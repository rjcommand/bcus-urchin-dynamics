library(tidyverse)
library(lubridate)
library(mgcv)
library(gratia)
library(forecast)

chla <- read_csv("data/chla_clean.csv")

chla_gam2 <- 
  chla %>% 
  mutate(WoY = week(Time_start),
         DoY = yday(Time_start),
         Year = factor(year(Time_start)))

# Buoy data set up
buoy_data <- read_csv("data/c46206.csv")

buoy_tidy <- 
  buoy_data %>% 
  rename("Time" = "DATE") %>% 
  mutate(Time = mdy_hm(Time, tz = "UTC")) %>% 
  dplyr::filter(SSTP > 0,
                SSTP < 20)
range(buoy_tidy$SSTP)  # Looks better!
# dotchart(buoy.data.untidy$SSTP)  # Looks much better!
# plot(buoy.data.untidy$Time, buoy.data.untidy$SSTP)


## Calculate the anomaly for the entire data set
# Create empty dataframes
df_part = data.frame()  # To store each chunk of Time and Oxygen (n = 350) 
df_full = data.frame()  # To store the summarized data - this gets built up with each successive df_part that is read in

add_time <- function(data) {
  
  if (nrow(data) == 1) {
    data %>% 
      mutate(Start_time = time_start,
             End_time = time_end) %>% 
      select(weekly_mean_SSTP, Start_time, End_time)
  } else {
    data %>% 
      mutate(Time_type = c("Start_time", "End_time")) %>% 
      pivot_wider(names_from = Time_type, values_from = Time)
  }
}


for (i in 1:nrow(chla_gam2)) {
  time_start <- chla_gam2$Time_start[i]
  time_end <- chla_gam2$Time_end[i]
  
  df_part <- data.frame(buoy_tidy[buoy_tidy$Time >= time_start & 
                                    buoy_tidy$Time < time_end, ])
  
  time_range <- df_part$Time[df_part$Time >= time_start & 
                               df_part$Time < time_end]
  if (!any(time_range >= time_start & time_range < time_end)) {
    df_part <-
      tibble(
        Start_time = time_start,
        End_time = time_end,
        weekly_mean_SSTP = NA_integer_
      ) 
  } else {
    df_part <- 
      df_part %>% 
      #group_by(Time = cut(Time, "8 days")) %>% 
      summarize(Time = cut(Time, "8 days"),
                weekly_mean_SSTP = mean(SSTP, na.rm = TRUE)) %>% 
      #mutate(SSTa = weekly_mean_SSTP - mean(weakly_mean_SSTP))
      distinct() %>% 
      add_time()
  }
  
  
  df_full <- rbind(df_full, df_part)  # Append the most recent chunk to the full data frame
  
}

df_full %>% 
  mutate(weekly_mean_SSTP = if_else(is.na(Start_time), NA_real_, weekly_mean_SSTP)) %>% View

pre_data <- 
  data.frame(
    Start_time = rev(c(seq.Date(ymd("2013-05-09"), ymd("1988-01-01"), by = -8), NA)),
    End_time = rev(seq.Date(ymd("2013-05-17"), ymd("1988-01-01"), by = -8))
  ) %>% 
  filter(!is.na(Start_time))

df_part2 <- data.frame()
df_full2 <- data.frame()
for (i in 1:nrow(pre_data)) {
  time_start <- pre_data$Start_time[i]
  time_end <- pre_data$End_time[i]
  
  df_part2 <- data.frame(buoy_tidy[buoy_tidy$Time >= time_start & 
                                     buoy_tidy$Time < time_end, ])
  
  time_range <- df_part2$Time[df_part2$Time >= time_start & 
                                df_part2$Time < time_end]
  
  if (!all(time_range >= time_start & time_range < time_end) || is_empty(time_range)) {
    df_part2 <-
      tibble(
        Start_time = time_start,
        End_time = time_end,
        weekly_mean_SSTP = NA_integer_
      ) 
  } else {
    df_part2 <- 
      df_part2 %>% 
      summarize(Time = cut(Time, "8 days"),
                weekly_mean_SSTP = mean(SSTP, na.rm = TRUE)) %>% 
      distinct() %>% 
      add_time()
  }
  
  
  df_full2 <- rbind(df_full2, df_part2)  # Append the most recent chunk to the full data frame
  
}


sstp_clean2 <- 
  df_full %>% 
  mutate(weekly_mean_SSTP = ifelse(is.na(Start_time), NA, weekly_mean_SSTP)) %>% 
  mutate(SSTa_new = weekly_mean_SSTP - mean(weekly_mean_SSTP, na.rm = TRUE))

chla_model_data <- 
  chla_gam2 %>% 
    mutate(Start_time = ymd(paste0(year(Time_start), "-", month(Time_start), "-", day(Time_start))),
           End_time = ymd(paste0(year(Time_end), "-", month(Time_end), "-", day(Time_end)))) %>% 
    full_join(sstp_clean2 %>% 
                mutate(Start_time = ymd(paste0(year(Start_time), "-", month(Start_time), "-", day(Start_time))),
                       End_time = ymd(paste0(year(End_time), "-", month(End_time), "-", day(End_time))))) %>% 
  mutate(time_step = 1:nrow(.))

hist(chla_model_data$Chl_a)
# Heavy right skew, does it benefit from log-transform?
hist(log(chla_model_data$Chl_a))
# Yep! We can say that Chlorophyll-a follows a log-normal distribution
chla_model_data %>% 
  filter(Start_time > "2013-01-01") %>% 
  pull(Chl_a) %>% 
  log() %>% 
  hist(., main = "Histogram of log-transformed Chlorophyll-a concentration")
# Even the time-truncated data do.
# Thus, we can model log(chlorophyll-a concentration) as a function of
# temporal and environmental covariates using GAMM to account for residual
# temporal autocorrelation.

# Add a variable for month
chla_model_data <- 
  chla_model_data %>% 
  mutate(nMonth = month(End_time)) %>% 
  filter(Start_time > "2013-01-01")

## fix the knots are just passed the ends of the month numbers
## this allows Dec and Jan to have their own estimates
knots <- list(nMonth = c(0.5, 12.5))
# Following Gavin Simpson's blog post re: modeling seasonal data with GAMM
ctrl <- list(niterEM = 0, msVerbose = TRUE, optimMethod="L-BFGS-B")  
m1c <- mgcv::gamm(log(Chl_a) ~
                    s(nMonth, bs = "cc", k = 12) + 
                    s(SSTa_new) + 
                    Year,
                  data = chla_model_data,
                  knots = knots,
                  control = ctrl)
#knots = knots)

mgcv::gam.check(m1c$gam)
acf(residuals(m1c$lme, type = "normalized"), lag = 200)
# There is residual temporal autocorrelation, thus a correlation term is needed.
pacf(residuals(m1c$lme, type = "normalized"), lag = 200)

#Let's "cheat" a bit and see what auto.arima says to give us a starting point
auto.arima(residuals(m1c$lme, type = "normalized"))
# Says AR3

m1c_MA1 <- mgcv::gamm(log(Chl_a) ~
                        s(nMonth, bs = "cc", k = 12) + 
                        s(SSTa_new) + 
                        Year,
                      data = chla_model_data,
                      correlation = nlme::corARMA(q = 1, form = ~1|Year),
                      knots = knots,
                      control = ctrl)
mgcv::gam.check(m1c_MA1$gam)
acf(residuals(m1c_MA1$lme, type = "normalized"), lag = 200)
pacf(residuals(m1c_MA1$lme, type = "normalized"), lag = 200)

m1c_AR1 <- mgcv::gamm(log(Chl_a) ~
                        s(nMonth, bs = "cc", k = 12) + 
                        s(SSTa_new) + 
                        Year,
                      data = chla_model_data,
                      correlation = nlme::corARMA(p = 1, form = ~1|Year),
                      knots = knots,
                      control = ctrl)
mgcv::gam.check(m1c_AR1$gam)
acf(residuals(m1c_AR1$lme, type = "normalized"), lag = 200)
pacf(residuals(m1c_AR1$lme, type = "normalized"), lag = 200)

m1c_AR2 <- mgcv::gamm(log(Chl_a) ~
                        s(nMonth, bs = "cc", k = 12) + 
                        s(SSTa_new) + 
                        Year,
                      data = chla_model_data,
                      correlation = nlme::corARMA(p = 2, form = ~1|Year),
                      knots = knots,
                      control = ctrl)
mgcv::gam.check(m1c_AR2$gam)
acf(residuals(m1c_AR1$lme, type = "normalized"), lag = 200)
pacf(residuals(m1c_AR1$lme, type = "normalized"), lag = 200)

m1c_AR3 <- mgcv::gamm(log(Chl_a) ~
                        s(nMonth, bs = "cc", k = 12) + 
                        s(SSTa_new) + 
                        Year,
                      data = chla_model_data,
                      correlation = nlme::corARMA(p = 3, form = ~1|Year),
                      knots = knots,
                      control = ctrl)
mgcv::gam.check(m1c_AR3$gam)
acf(residuals(m1c_AR1$lme, type = "normalized"), lag = 200)
pacf(residuals(m1c_AR1$lme, type = "normalized"), lag = 200)

anova(m1c$lme, m1c_AR1$lme, m1c_AR2$lme, m1c_AR3$lme)

m1c_AR1MA1 <- mgcv::gamm(log(Chl_a) ~
                           s(nMonth, bs = "cc", k = 12) + 
                           s(SSTa_new) + 
                           Year,
                         data = chla_model_data,
                         correlation = nlme::corARMA(p = 1, q = 1, form = ~1|Year),
                         knots = knots,
                         control = ctrl)

m1c_AR2MA1 <- mgcv::gamm(log(Chl_a) ~
                           s(nMonth, bs = "cc", k = 12) + 
                           s(SSTa_new) + 
                           Year,
                         data = chla_model_data,
                         correlation = nlme::corARMA(p = 2, q = 1, form = ~1|Year),
                         knots = knots,
                         control = ctrl)
mgcv::gam.check(m1c_AR2MA1$gam)
acf(residuals(m1c_AR2MA1$lme, type = "normalized"), lag = 200)
pacf(residuals(m1c_AR2MA1$lme, type = "normalized"), lag = 200)

anova(m1c$lme, m1c_MA1$lme, m1c_AR1$lme, m1c_AR2MA1$lme)
anova(m1c$lme, m1c_MA1$lme, m1c_AR2MA1$lme, m1c_AR2MA2$lme, m1c_AR1MA2$lme, m1c_MA2$lme)
anova(m1c$lme, m1c_MA1$lme, m1c_MA2$lme, m1c_AR1MA1$lme, m1c_AR2MA1$lme, m1c_AR1MA2$lme)
anova(m1c$lme, m1c_MA1$lme, m1c_AR2MA1$lme)

# AR2 + MA1 is sufficient

# Check this model
mgcv::gam.check(m1c_AR1$gam)
acf(residuals(m1c_AR1$lme, type = "normalized"), lag = 200)
pacf(residuals(m1c_AR1$lme, type = "normalized"), lag = 200)


## Plots ----
# Autocorrelation plot
acf_dat <- acf(residuals(m1c_AR1$lme, type = "normalized"), lag = 200, plot = FALSE)
sig_level <- exp(2 * 1.96 / sqrt(260 - 3) - 1) / exp( 2 * 1.96 / sqrt(260 - 3) + 1)

acf_df <- data.frame(lag = acf_dat$lag[, , 1], acf = acf_dat$acf[, , 1])
acf_plot <- 
  acf_df %>% 
  ggplot() +
  geom_segment(aes(x = lag, xend = lag, y = 0, yend = acf)) + 
  geom_hline(aes(yintercept = 0)) +
  geom_hline(aes(yintercept = sig_level), colour = "blue", linetype = "dashed") +
  geom_hline(aes(yintercept = -sig_level), colour = "blue", linetype = "dashed") + 
  labs(x = "Lag", y = "ACF") + 
  theme_bw() +
  theme(
    panel.grid = element_blank()
  )

# Residual plots for Chla model
p_s <- cowplot::plot_grid(
  gratia::qq_plot(m1c_AR1$gam) + 
    theme_bw() +
    theme(
      panel.grid = element_blank()
    ),
  gratia::residuals_hist_plot(m1c_AR1$gam) + 
    theme_bw() +
    theme(
      panel.grid = element_blank()
    ),
  gratia::residuals_linpred_plot(m1c_AR1$gam) + 
    theme_bw() +
    theme(
      panel.grid = element_blank()
    ),
  gratia::observed_fitted_plot(m1c_AR1$gam) + 
    theme_bw() +
    theme(
      panel.grid = element_blank()
    ),
  ncol = 2, nrow = 2,
  rel_widths = c(1, 1, 1, 1, 2),
  align = "vh", axis = "lb",
  labels = LETTERS[1:4],
  label_x = 0.08, label_y = 0.8
) %>% 
  cowplot::plot_grid(., acf_plot, nrow = 2, rel_heights = c(2, 1), labels = c("", "E"),
                     label_x = 0.045, label_y = 0.93, 
                     align = "v", axis = "r")

ggsave("figures/figS8.png", p_s, width = 15, height = 12, units = "in", dpi = 300)

# Check concurvity
mgcv::concurvity(m1c_AR1$gam, full = FALSE)

# Model summary
summary(m1c_AR1$gam)


# Effects plots
ssta_effect <- 
  gratia::draw(m1c_AR1$gam, select = "s(SSTa_new)", 
               residuals = TRUE,
               resid_col = "black") + 
  #lims(y = c(-0.4, 0.4)) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16)
  ) +
  ggtitle("") +
  labs(x = expression(paste("SST anomaly (", C, ")")))

month_effect <- 
  gratia::draw(m1c_AR1$gam, select = "s(nMonth)", 
               residuals = TRUE,
               resid_col = "black") + 
  scale_x_continuous(breaks = seq(1, 12, by = 1), labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16)
  ) +
  ggtitle("") +
  labs(x = "")

year_effect <- 
  gratia::parametric_effects(m1c_AR1$gam) %>% 
  ggplot() +
  geom_point(aes(x = level, y = partial)) +
  geom_segment(aes(x = level, xend = level, y = partial - se, yend = partial + se)) +
  geom_rect(aes(xmin = "2014", xmax = "2016", ymin = -Inf, ymax = Inf), fill = "orange", alpha = 0.01) + 
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16)
  ) +
  ggtitle("") +
  labs(x = "",
       y = "Partial effect")

# Assemble effects plots
p <- cowplot::plot_grid(month_effect, ssta_effect, nrow = 1, labels = c("A", "B"),
                   label_x = 0.12, label_y = 0.9, align = "hv", axis = "b",
                   label_size = 18) %>% 
  cowplot::plot_grid(., year_effect, align = "vh", axis = "r", 
                     nrow = 2, labels = c("", "C"), label_x = 0.055, label_y = 0.91,
                     label_size = 18)

ggsave("figures/fig7.png", p, width = 15, height = 12, units = "in", dpi = 300)


#### END ####

## Extra fun stuff... ----

pdat <- with(chla_model_data,
             data.frame(Year = as.factor(rep(c(2002, 2013), each = 100)),
                        nMonth = rep(seq(1, 12, length = 100), times = 2),
                        SSTa_new = c(seq(min(chla_model_data$SSTa_new[chla_model_data$Year == "2002"], na.rm = TRUE), 
                                         max(chla_model_data$SSTa_new[chla_model_data$Year == "2002"], na.rm = TRUE), length.out = 100),
                                     seq(min(chla_model_data$SSTa_new[chla_model_data$Year == "2013"], na.rm = TRUE), 
                                         max(chla_model_data$SSTa_new[chla_model_data$Year == "2013"], na.rm = TRUE), length.out = 100))))

pred <- predict(m1c_AR2MA1$gam, newdata = pdat, se.fit = TRUE)
crit <- qt(0.975, df = df.residual(m1c_AR2MA1$gam)) # ~95% interval critical t
pdat <- transform(pdat, fitted = pred$fit, se = pred$se.fit, fYear = as.factor(Year))
pdat <- transform(pdat,
                  upper = fitted + (crit * se),
                  lower = fitted - (crit * se))
p1 <- ggplot(pdat, aes(x = nMonth, y = fitted, group = fYear)) +
  geom_ribbon(mapping = aes(ymin = lower, ymax = upper,
                            fill = fYear), alpha = 0.2) + # confidence band
  geom_line(aes(colour = fYear)) +    # predicted temperatures
  theme_bw() +                        # minimal theme
  theme(legend.position = "top") +    # push legend to the top
  labs(y = expression(Chlorophyll-a ~ (mg/mL)), x = NULL) +
  scale_fill_discrete(name = "Year") + # correct legend name
  scale_colour_discrete(name = "Year") +
  scale_x_continuous(breaks = 1:12,   # tweak where the x-axis ticks are
                     labels = month.abb, # & with what labels
                     minor_breaks = NULL)
p1



# Yearly trend by month
pdat2 <- with(chla_model_data,
              data.frame(Year = rep(2002:2020, each = 12),
                         nMonth = rep(1:12, times = 19),
                         SSTa_new = SSTa_new))

pred2 <- predict(chla_m6_AR1$gam, newdata = pdat2, se.fit = TRUE)
## add predictions & SEs to the new data ready for plotting
pdat2 <- transform(pdat2,
                   fitted = pred2$fit,  # predicted values
                   se = pred2$se.fit,   # standard errors
                   fMonth = factor(month.abb[nMonth], # month as a factor
                                   levels = month.abb))
pdat2 <- transform(pdat2,
                   upper = fitted + (crit * se), # upper and...
                   lower = fitted - (crit * se)) # lower confidence bounds

p2 <- ggplot(pdat2, aes(x = Year, y = fitted, group = fMonth)) +
  geom_line(aes(colour = fMonth)) +   # draw trend lines
  theme_bw() +                        # minimal theme
  theme(legend.position = "none") +   # no legend
  labs(y = expression(Chlorophyll-a ~ (mg/mL)), x = NULL) +
  facet_wrap(~ fMonth, ncol = 6) +    # facet on month
  scale_y_continuous(breaks = seq(0, 10, by = 1),
                     minor_breaks = NULL) # nicer ticks
p2

# By Quarter
pdat2$Quarter <- NA
pdat2$Quarter[pdat2$nMonth %in% c(12,1,2)] <- "Winter"
pdat2$Quarter[pdat2$nMonth %in% 3:5] <- "Spring"
pdat2$Quarter[pdat2$nMonth %in% 6:8] <- "Summer"
pdat2$Quarter[pdat2$nMonth %in% 9:11] <- "Autumn"
pdat2 <- transform(pdat2,
                   Quarter = factor(Quarter,
                                    levels = c("Spring","Summer","Autumn","Winter")))

p3 <- ggplot(pdat2, aes(x = Year, y = fitted, group = fMonth)) +
  geom_line(aes(colour = fMonth)) +   # draw trend lines
  theme_bw() +                        # minimal theme
  theme(legend.position = "top") +    # legend on top
  scale_fill_discrete(name = "Month") + # nicer legend title
  scale_colour_discrete(name = "Month") +
  labs(y = expression(Chlorophyll-a ~ (mg/mL)), x = NULL) +
  facet_grid(Quarter ~ ., scales = "free_y") # facet by Quarter
p3

# How has chla concentration changed over the past 19 years?
pdat <- with(chla_model_data,
            data.frame(Year = rep(c(2002, 2020), each = 12),
                       nMonth = rep(1:12, times = 2)))
pred <- predict(chla_m6_AR1$gam, newdata = pdat)
pdat <- transform(pdat, fitted = pred, fYear = as.factor(Year))
dif <- with(pdat, data.frame(Month = 1:12,
                             Difference = fitted[Year == 2020] - fitted[Year == 2002]))

ggplot(dif, aes(x = Difference, y = Month)) +
  geom_point() +
  labs(x = expression(Chlorophyll-a ~ difference ~ (mg/mL)),
       y = "Month") +
  theme_bw() +                        # minimal theme
  scale_y_continuous(breaks = 1:12,   # tweak where the x-axis ticks are
                     labels = month.abb, # & with what labels
                     minor_breaks = NULL) #+
  #scale_x_continuous(breaks = seq(0, 1.2, by = 0.1),
  #                   minor_breaks = NULL)

layout(matrix(1:3, ncol = 3))
op <- par(mar = rep(4, 4) + 0.1)
plot(chla_m6_AR1$gam, pers = TRUE, scale = 0)
par(op)
layout(1)


# Autocorrelation plot
acf_dat <- acf(residuals(chla_m6_AR1$lme, type = "normalized"), lag = 200, plot = FALSE)
sig_level <- exp(2 * 1.96 / sqrt(260 - 3) - 1) / exp( 2 * 1.96 / sqrt(260 - 3) + 1)

acf_df <- data.frame(lag = acf_dat$lag[, , 1], acf = acf_dat$acf[, , 1])
acf_plot <- 
  acf_df %>% 
  ggplot() +
  geom_segment(aes(x = lag, xend = lag, y = 0, yend = acf)) + 
  geom_hline(aes(yintercept = 0)) +
  geom_hline(aes(yintercept = sig_level), colour = "blue", linetype = "dashed") +
  geom_hline(aes(yintercept = -sig_level), colour = "blue", linetype = "dashed") + 
  labs(x = "Lag", y = "ACF") + 
  theme(
    panel.grid = element_blank()
  )

# Residual plots for Oxygen model
cowplot::plot_grid(
  gratia::qq_plot(chla_m6_AR1$gam) + 
    theme(
      panel.grid = element_blank()
    ),
  gratia::residuals_hist_plot(chla_m6_AR1$gam) + 
    theme(
      panel.grid = element_blank()
    ),
  gratia::residuals_linpred_plot(chla_m6_AR1$gam) + 
    theme(
      panel.grid = element_blank()
    ),
  gratia::observed_fitted_plot(chla_m6_AR1$gam) + 
    theme(
      panel.grid = element_blank()
    ),
  ncol = 2, nrow = 2,
  rel_widths = c(1, 1, 1, 1, 2),
  align = "vh", axis = "lb",
  labels = LETTERS[1:4],
  label_x = 0.08, label_y = 0.8
) %>% 
  cowplot::plot_grid(., acf_plot, nrow = 2, rel_heights = c(2, 1), labels = c("", "E"),
                     label_x = 0.045, label_y = 0.93, 
                     align = "v", axis = "r")


preds.chl <- predict(chl.model, type = "response", se = TRUE)
chla.pred.plot <- cbind(na.omit(this_data), preds.chl)



library(forecast)
chla_m6_AR0 <- mgcv::gamm(Chl_a ~ 
                            te(time_step, nMonth, bs = c("cr","cc"), k = c(10, 12)),
                          data = chla_model_data2,
                          #correlation = nlme::corARMA(p = 1, form =~ 1|Year),
                          control = ctrl,
                          knots = knots)
arma_res <- auto.arima(resid(chla_m6_AR0$lme, type = "normalized"),
                       stationary = TRUE, seasonal = FALSE)

arma_res$coef
chla_m6_ARMA0 <- mgcv::gamm(Chl_a ~ 
                            #s(time_step, bs = "cr", k = 10) + 
                            #s(nMonth, bs = "cc", k = 12) + 
                            t2(time_step, nMonth, bs = c("cr","cc"), k = c(10, 12)),
                          data = chla_model_data2,
                          control = ctrl,
                          knots = knots)
arma_res <- auto.arima(resid(chla_m6_ARMA0$lme, type = "normalized"),
                       stationary = TRUE, seasonal = FALSE)

arma_res$coef

chla_m6_MA1 <- mgcv::gamm(Chl_a ~ 
                            s(time_step, bs = "tp", k = 10) + 
                            s(nMonth, bs = "cc", k = 12) + 
                            ti(time_step, nMonth, bs = c("cr","cc"), k = c(10, 12)),
                          data = chla_model_data2,
                          correlation = nlme::corARMA(p = 1, form =~ 1|Year),
                          control = ctrl,
                          knots = knots)
mgcv::gam.check(chla_m6_MA1$gam)


chla_m6_4 <- mgcv::gam(Chl_a ~ 
                            s(Year, bs = "tp", k = 10) + 
                            s(nMonth, bs = "cc", k = 12) + 
                            ti(Year, nMonth, bs = c("cr","cc"), k = c(20, 12)),
                          data = chla_model_data2,
                          knots = knots,
                         family = Gamma())
mgcv::gam.check(chla_m6_4)
summary(chla_m6_4)
gratia::draw(chla_m6_4)


chla_model_data3 <- 
  chla_model_data %>% 
  filter(Start_time > "2013-01-01")
m <- mgcv::gam(Chl_a ~
                 s(DoY, bs = "cc", k = 12) +
                 #s(DoY, Year, bs = "fs") + 
                 s(SSTa_new) + 
                 Year,
               data = chla_model_data,
               knots = knots,
               family = Gamma(link = "log"))
 
mgcv::gam.check(m)
summary(m)
gratia::draw(m, residuals = TRUE)
acf(resid(m, "pearson"), lag = 200)

m1a <- mgcv::gam(Chl_a ~
                   s(time_step) +
                   s(DoY, bs = "cc", k = 12) +
                   #s(DoY, Year, bs = "fs") + 
                   s(SSTa_new) + 
                   Year,
                 data = chla_model_data,
                 knots = knots,
                 family = Gamma(link = "log"))

m1b <- mgcv::gamm(log(Chl_a) ~
                   s(time_step) +
                   s(DoY, Year, bs = "fs") + 
                   s(SSTa_new) + 
                   Year,
                 data = chla_model_data,
                 control = ctrl)
                 #knots = knots)

mgcv::gam.check(m1b$gam)
acf(residuals(m1b$lme, type = "normalized"), lag = 200)

hist(chla_model_data$Chl_a)
hist(log(chla_model_data$Chl_a))

auto.arima(residuals(m1b$lme, type = "normalized"))
m1b_AR1 <- mgcv::gamm(log(Chl_a) ~
                    s(time_step) +
                    s(DoY, Year, bs = "fs") + 
                    s(SSTa_new) + 
                    Year,
                  data = chla_model_data,
                  correlation = nlme::corAR1(form = ~1|Year),
                  control = ctrl)
mgcv::gam.check(m1b_AR1$gam)
acf(residuals(m1b_AR1$lme, type = "normalized"), lag = 200)
#knots = knots)
#

m1b_AR2 <- mgcv::gamm(log(Chl_a) ~
                        s(time_step) +
                        s(DoY, Year, bs = "fs") + 
                        s(SSTa_new) + 
                        Year,
                      data = chla_model_data,
                      correlation = nlme::corARMA(p = 2, form = ~1|Year),
                      control = ctrl)
mgcv::gam.check(m1b_AR2$gam)
acf(residuals(m1b_AR2$lme, type = "normalized"), lag = 200)

m1b_AR2MA1 <- mgcv::gamm(log(Chl_a) ~
                        s(time_step) +
                        s(DoY, Year, bs = "fs") + 
                        s(SSTa_new) + 
                        Year,
                      data = chla_model_data,
                      correlation = nlme::corARMA(p = 2, q = 1, form = ~1|Year),
                      control = ctrl)
mgcv::gam.check(m1b_AR3$gam)
acf(residuals(m1b_AR2MA1$lme, type = "normalized"), lag = 200)

anova(m1b$lme, m1b_AR1$lme, m1b_AR2$lme, m1b_AR2MA1$lme)
summary(m1b_AR2MA1$gam)
gratia::draw(m1b_AR2MA1$gam)

mgcv::concurvity(m1b_AR2MA1$gam, full = FALSE)


m1c <- mgcv::gamm(log(Chl_a) ~
                    s(nMonth, bs = "cc", k = 12) + 
                    s(SSTa_new) + 
                    Year,
                  data = chla_model_data,
                  knots = knots,
                  control = ctrl)
#knots = knots)

mgcv::gam.check(m1c$gam)
acf(residuals(m1c$lme, type = "normalized"), lag = 200)

hist(chla_model_data$Chl_a)
hist(log(chla_model_data$Chl_a))

auto.arima(residuals(m1c$lme, type = "normalized"))
m1c_MA1 <- mgcv::gamm(log(Chl_a) ~
                        s(nMonth, bs = "cc", k = 12) + 
                        s(SSTa_new) + 
                        Year,
                      data = chla_model_data,
                      correlation = nlme::corARMA(q = 1, form = ~1|Year),
                      control = ctrl)
mgcv::gam.check(m1c_MA1$gam)
acf(residuals(m1c_MA1$lme, type = "normalized"), lag = 200)
#knots = knots)
#

m1c_AR1 <- mgcv::gamm(log(Chl_a) ~
                        s(nMonth, bs = "cc", k = 12) + 
                        s(SSTa_new) + 
                        Year,
                      data = chla_model_data,
                      correlation = nlme::corARMA(p = 1, form = ~1|Year),
                      control = ctrl)
mgcv::gam.check(m1c_AR1$gam)
acf(residuals(m1c_AR1$lme, type = "normalized"), lag = 200)
pacf(residuals(m1c_AR1$lme, type = "normalized"), lag = 200)


m1c_AR2MA1 <- mgcv::gamm(log(Chl_a) ~
                           s(nMonth, bs = "cc", k = 12) + 
                           s(SSTa_new) + 
                           Year,
                         data = chla_model_data,
                         correlation = nlme::corARMA(p = 2, q = 1, form = ~1|Year),
                         control = ctrl)
mgcv::gam.check(m1c_AR2MA1$gam)
acf(residuals(m1c_AR2MA1$lme, type = "normalized"), lag = 200)

anova(m1c$lme, m1c_MA1$lme, m1c_AR1$lme, m1c_AR2MA1$lme)
anova(m1c$lme, m1c_MA1$lme, m1c_AR2MA1$lme)

m1c_AR2MA2 <- mgcv::gamm(log(Chl_a) ~
                           s(nMonth, bs = "cc", k = 12) + 
                           s(SSTa_new) + 
                           Year,
                         data = chla_model_data,
                         correlation = nlme::corARMA(p = 2, q = 3, form = ~1|time_step),
                         control = ctrl)
mgcv::gam.check(m1c_AR2MA2$gam)
acf(residuals(m1c_AR2MA2$lme, type = "normalized"), lag = 200)
pacf(residuals(m1c_AR2MA2$lme, type = "normalized"), lag = 200)

summary(m1c_AR2MA1$gam)
gratia::draw(m1c_AR2MA1$gam)

mgcv::concurvity(m1c_AR2MA1$gam, full = FALSE)

chla_model_data4 <- 
  chla_model_data3 %>% 
  mutate(Year = as.numeric(Year))

m1 <- mgcv::gam(Chl_a ~ 
                  s(nMonth, bs = "cc", k = 12) + 
                  s(SSTa_new) +
                  ti(Year, SSTa_new, bs = c("cc", "cr"), k = c(6, 6)),
                data = chla_model_data4,
                knots = knots, 
                family = Gamma(link = "log"))
mgcv::gam.check(m1)
summary(m1)
gratia::draw(m1)

m2 <- mgcv::gam(Chl_a ~
                 s(DoY, bs = "cc", k = 12) +
                 s(SSTa_new) + 
                 ti(DoY, SSTa_new, bs = c("cc", "cr"), k = c(12, 4)) +
                s(Year, bs = "re"),
               data = chla_model_data3,
               knots = knots,
               family = Gamma(link = "log"))

mgcv::gam.check(m2)
summary(m2)
gratia::draw(m2)

acf(resid(m2, "pearson"), lag = 200)


m4 <- mgcv::gam(Chl_a ~
                  te(time_step, nMonth, bs = c("cr", "cc"), k = c(10, 12)) +
                  s(SSTa_new),
                data = chla_model_data2,
                knots = knots,
                family = Gamma(link = "log"))
mgcv::gam.check(m4)
acf(resid(m4, "pearson"), lag = 200)
summary(m4)
gratia::draw(m4)

m6 <- mgcv::gam(Chl_a ~
                  te(time_step, nMonth, bs = c("cr", "cc"), k = c(10, 12)) +
                  te(time_step, weekly_mean_SSTP, bs = c("cc", "cr"), k = c(10, 4)),
                data = chla_model_data2,
                knots = knots,
                family = Gamma(link = "log"))
mgcv::gam.check(m6)
acf(resid(m6, "pearson"), lag = 200)
summary(m6)
gratia::draw(m6)


m8 <- mgcv::gam(Chl_a ~
                  s(time_step, bs = "cr", k = 10) +
                  s(nMonth, bs = "cc", k = 12) +
                  s(weekly_mean_SSTP, bs = "cr", k = 4) +
                  ti(time_step, nMonth, bs = c("cr", "cc"), k = c(10, 12)) +
                  ti(time_step, weekly_mean_SSTP, bs = c("cc", "cr"), k = c(10, 4)),
                data = chla_model_data2,
                knots = knots,
                family = Gamma(link = "log"))

mgcv::gam.check(m8)
acf(resid(m8, "pearson"), lag = 200)
summary(m8)
gratia::draw(m8)

plot(chla_model_data2$SSTa_new)
