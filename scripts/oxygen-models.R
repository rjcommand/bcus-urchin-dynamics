library(tidyverse)
library(lubridate)

oxygen <- read_csv(here::here("data/weekly.bcus.oxygen.csv"))

#### Full oxygen time series GAM analysis ####
oxygen_gam <- 
  oxygen %>% 
  mutate(time_step = 1:nrow(oxygen),
         Year = as.factor(year(Start_time)),
         WoY = week(Start_time)
         )
View(oxygen_gam)
# Chlorophyll-data
chla <- read_csv("data/chla_clean.csv")

chla_gam <- 
  chla %>% 
  mutate(WoY = week(Time_start),
         Year = factor(year(Time_start))) %>% 
  filter(Time_start >= "2013-05-16" & Time_end <= "2020-02-10")

# Combine
oxygen_model_data <- 
  oxygen_gam %>% 
  left_join(chla_gam) 

oxygen_model_data <- 
  oxygen_model_data %>% 
  mutate(Start_time = case_when(
    time_step == 180 ~ as.Date("2017-10-08"),
    time_step == 270 ~ as.Date("2020-02-02"),
    TRUE ~ as.Date(Start_time)
  ),
  End_time = case_when(
    time_step == 180 ~ as.Date("2017-10-16"),
    time_step == 270 ~ as.Date("2020-02-10"),
    TRUE ~ as.Date(End_time)
  ),
  Year = year(Start_time)
  )


str(oxygen_model_data)
# Load buoy data
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

# time_range <- df_part$Time[df_part$Time >= chla_gam$Time_start[28] & 
#                              df_part$Time < chla_gam$Time_end[28]]
# if (!any(time_range >= chla_gam$Time_start[28] & time_range < chla_gam$Time_end[28])) {
#   df_part <-
#     tibble(
#       Start_time = time_start,
#       End_time = time_end,
#       weekly_mean_SSTP = NA_character_
#     ) 
# }
# df_part <- data.frame(buoy_tidy[buoy_tidy$Time >= chla_gam$Time_start[27] & 
#                        buoy_tidy$Time < chla_gam$Time_end[27], ]) %>% 
#   summarize(Time = cut(Time, "8 days"),
#             weekly_mean_SSTP = mean(SSTP, na.rm = TRUE)) %>% 
#   distinct() %>% 
#   add_time()

for (i in 1:nrow(chla_gam)) {
  time_start <- chla_gam$Time_start[i]
  time_end <- chla_gam$Time_end[i]
  
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


sstp_clean <- 
  rbind(df_full2, df_full) %>% 
  mutate(weekly_mean_SSTP = ifelse(is.na(Start_time), NA, weekly_mean_SSTP)) %>% 
  mutate(SSTa_new = weekly_mean_SSTP - mean(weekly_mean_SSTP, na.rm = TRUE))

all_data <- left_join(oxygen_model_data, sstp_clean)

# Remove 3 outliers
oxygen_model_data_no_outliers3 <- 
  all_data %>% 
  filter(Chl_a <= 7)

# Get DoY as a numeric and Year as a factor variables
omno_doy <- 
  oxygen_model_data_no_outliers3 %>% 
  mutate(DoY = yday(Start_time),
         Year = as.factor(Year))
ctrl <- list(niterEM = 0, msVerbose = TRUE, optimMethod="L-BFGS-B")

omt1 <- mgcv::gamm(Oxygen ~ s(time_step, k = 20) + 
                     Year,
                   data = omno_doy,
                   method = "REML",
                   correlation = nlme::corARMA(p = 2, form = ~ 1),
                   control = ctrl)

omt2 <- mgcv::gamm(Oxygen ~ s(DoY, k = 12, bs = "cc") + 
                     Year,
                   data = omno_doy,
                   method = "REML",
                   correlation = nlme::corARMA(p = 2, form = ~ 1),
                   control = ctrl)

omt3 <- mgcv::gamm(Oxygen ~ s(time_step, k = 20) + s(DoY, k = 12, bs = "cc") + 
                     Year,
                   data = omno_doy,
                   method = "REML",
                   correlation = nlme::corARMA(p = 2, form = ~ 1),
                   control = ctrl)

anova(omt1$lme, omt2$lme, omt3$lme)
nrow(omno_doy)

mgcv::gam.check(omt3$gam)
summary(omt3$gam)

omt3_env_0 <- mgcv::gamm(Oxygen ~ 
                         s(time_step, k = 20) + 
                         s(DoY, k = 12, bs = "cc") + 
                         s(Chl_a) + 
                         s(SSTa_new) + 
                         s(Year, bs = "re"),
                       data = omno_doy,
                       method = "REML",
                       control = ctrl)

omt3_env_AR1 <- mgcv::gamm(Oxygen ~ 
                         s(time_step, k = 20) + 
                         s(DoY, k = 12, bs = "cc") + 
                         s(Chl_a) + 
                         s(SSTa_new) + 
                         s(Year, bs = "re"),
                       data = omno_doy,
                       method = "REML",
                       correlation = nlme::corARMA(p = 1, form = ~ 1),
                       control = ctrl)

omt3_env_AR2 <- mgcv::gamm(Oxygen ~ 
                         s(time_step, k = 20) + 
                         s(DoY, k = 12, bs = "cc") + 
                         s(Chl_a) + 
                         s(SSTa_new) + 
                         s(Year, bs = "re"),
                       data = omno_doy,
                       method = "REML",
                       correlation = nlme::corARMA(p = 2, form = ~ 1),
                       control = ctrl)

omt3_env_AR3 <- mgcv::gamm(Oxygen ~ 
                         s(time_step, k = 20) + 
                         s(DoY, k = 12, bs = "cc") + 
                         s(Chl_a) + 
                         s(SSTa_new) + 
                         s(Year, bs = "re"),
                       data = omno_doy,
                       method = "REML",
                       correlation = nlme::corARMA(p = 3, form = ~ 1),
                       control = ctrl)
anova(omt3_env_0$lme, omt3_env_AR1$lme, omt3_env_AR2$lme, omt3_env_AR3$lme)
omt3_env <- mgcv::gamm(Oxygen ~ 
                         s(time_step, k = 20) + 
                         s(DoY, k = 12, bs = "cc") + 
                         s(Chl_a) + 
                         s(SSTa_new) + 
                         s(Year, bs = "re"),
                       data = omno_doy,
                       method = "REML",
                       correlation = nlme::corARMA(p = 2, form = ~ 1),
                       control = ctrl)
mgcv::gam.check(omt3_env$gam)
summary(omt3_env$gam)
gratia::draw(omt3_env$gam, residuals = TRUE)

# To check whether the missing data affects the observed patterns, fit a model
# on both the 'pre' missing and 'post' missing data set.
premiss <- 
  omno_doy %>% 
  filter(Start_time < "2015-01-09")
omt3_env_pre <- mgcv::gamm(Oxygen ~ 
                             s(time_step) + 
                             s(DoY, k = 12, bs = "cc") + 
                             s(Chl_a) + 
                             s(SSTa_new) + 
                             s(Year, bs = "re"),
                           data = premiss,
                           method = "REML",
                           correlation = nlme::corARMA(p = 2, form = ~ 1),
                           control = ctrl)
mgcv::gam.check(omt3_env_pre$gam)
summary(omt3_env_pre$gam)
gratia::draw(omt3_env_pre$gam, residuals = TRUE)

postmiss <- 
  omno_doy %>% 
  filter(Start_time >= "2015-01-09")

omt3_env_post <- mgcv::gamm(Oxygen ~ 
                              s(time_step, k = 20) + 
                              s(DoY, k = 12, bs = "cc") + 
                              s(Chl_a) + 
                              s(SSTa_new) + 
                              s(Year, bs = "re"),
                            data = postmiss,
                            method = "REML",
                            correlation = nlme::corARMA(p = 2, form = ~ 1),
                            control = ctrl)
mgcv::gam.check(omt3_env_post$gam)
summary(omt3_env_post$gam)
gratia::draw(omt3_env_post$gam, residuals = TRUE)


# Autocorrelation plot
acf_dat <- acf(residuals(omt3_env_AR2$lme, type = "normalized"), lag = 200, plot = FALSE)
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
ggsave("figures/figS3.png", acf_plot, width = 15, height = 12, units = "in", dpi = 300)
# Residual plots for Oxygen model
cowplot::plot_grid(
  gratia::qq_plot(omt3_env$gam) + 
    theme(
      panel.grid = element_blank()
    ),
  gratia::residuals_hist_plot(omt3_env$gam) + 
    theme(
      panel.grid = element_blank()
    ),
  gratia::residuals_linpred_plot(omt3_env$gam) + 
    theme(
      panel.grid = element_blank()
    ),
  gratia::observed_fitted_plot(omt3_env$gam) + 
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


#### END ####

## Old -----
View(all_data)
View(oxygen_model_data)
all(na.omit(year(oxygen_model_data$Start_time) == year(oxygen_model_data$Time_start)))
all(na.omit(month(oxygen_model_data$Start_time) == month(oxygen_model_data$Time_start)))
all(na.omit(day(oxygen_model_data$Start_time) == day(oxygen_model_data$Time_start)))


model0 <- mgcv::gam(Oxygen ~ s(WoY, bs = "cc", k = 12) + Year + s(Chl_a) + s(SSTa_new),
                    data = all_data,
                    method = "REML")
mgcv::gam.check(model0)
acf(residuals(model0, "pearson"), lag = 400)
acf(model0$residuals, lag = 200)

mmodel0 <- mgcv::gamm(Oxygen ~ #s(time_step, k = 40) + 
                        s(WoY, bs = "cc", k = 12) + 
                        Year + 
                        s(Chl_a) + 
                        s(SSTa_new),
                    data = all_data,
                    method = "REML",
                    correlation = nlme::corARMA(p = 2))
mgcv::gam.check(mmodel0$gam)
summary(mmodel0$gam)
summary(mmodel0$lme)
acf(residuals(mmodel0$lme, type = "normalized"), lag = 200)
acf(mmodel0$gam$residuals, lag = 200)



# Remove only the point greater than 15
oxygen_model_data_no_outliers2 <- 
  all_data %>% 
  filter(Chl_a <= 15)
mmodel2 <- mgcv::gamm(Oxygen ~ #s(time_step, k = 40) + 
                        s(WoY, bs = "cc", k = 12) + 
                        Year + 
                        s(Chl_a) + 
                        s(SSTa_new),
                      data = oxygen_model_data_no_outliers2,
                      method = "REML",
                      correlation = nlme::corARMA(p = 2))
mgcv::gam.check(mmodel2$gam)
summary(mmodel2$gam)
gratia::draw(mmodel2$gam, residuals = TRUE)

oxygen_model_data_no_outliers3 <- 
  all_data %>% 
  filter(Chl_a <= 7)
mmodel3 <- mgcv::gamm(Oxygen ~ #s(time_step, k = 40) + 
                        s(WoY, bs = "cc", k = 12) + 
                        Year + 
                        s(Chl_a) + 
                        s(SSTa_new),
                      data = oxygen_model_data_no_outliers3,
                      method = "REML",
                      correlation = nlme::corARMA(p = 2))
mgcv::gam.check(mmodel3$gam)
summary(mmodel3$gam)
gratia::draw(mmodel3$gam, residuals = TRUE)


oxygen_model_data_no_outliers3$OYear <- ordered(oxygen_model_data_no_outliers3$Year)

mmodel4 <- mgcv::gamm(Oxygen ~ 
                        s(DoY, bs = "cc", k = 12) + 
                        OYear + 
                        s(Chl_a) + 
                        s(SSTa_new),
                      data = omno_doy,
                      method = "REML",
                      correlation = nlme::corARMA(p = 2))
mgcv::gam.check(mmodel4$gam)
summary(mmodel4$gam)
gratia::draw(mmodel4$gam, residuals = TRUE)


# Detrend oxygen
all_data %>% 
  mutate(Oxygen_diff = diff(Chl_a))
?diff
diff(c(1, 2, 3))
all_data$Chl_a_diff <- c(diff(all_data$Chl_a), NA)
plot(diff(all_data$Chl_a_diff))
View(all_data)


premiss <- 
  omno_doy %>% 
  filter(Start_time < "2015-01-09")

mmodel5 <- mgcv::gamm(Oxygen ~ 
                        s(time_step, k = 20) + 
                        s(DoY, bs = "cc", k = 12) + 
                        OYear + 
                        s(Chl_a) + 
                        s(SSTa_new),
                      data = premiss,
                      method = "REML",
                      correlation = nlme::corARMA(p = 2))
mgcv::gam.check(mmodel5$gam)
summary(mmodel5$gam)
gratia::draw(mmodel5$gam, residuals = TRUE)

postmiss <- 
  omno_doy %>% 
  filter(Start_time >= "2015-01-09")

mmodel6 <- mgcv::gamm(Oxygen ~ 
                        s(time_step, k = 20) + 
                        s(DoY, bs = "cc", k = 12) + 
                        OYear + 
                        s(Chl_a) + 
                        s(SSTa_new),
                      data = postmiss,
                      method = "REML",
                      correlation = nlme::corARMA(p = 2))
mgcv::gam.check(mmodel6$gam)
summary(mmodel6$gam)
gratia::draw(mmodel6$gam, residuals = TRUE)



mmodel7 <- mgcv::gam(Oxygen ~ 
                        s(time_step) +
                        s(WoY, bs = "cc", k = 12) + 
                        OYear + 
                        s(Chl_a) + 
                        s(SSTa_new),
                      data = postmiss,
                      method = "REML")#,
                      #correlation = nlme::corARMA(p = 2))
mgcv::gam.check(mmodel7)
summary(mmodel7)
gratia::draw(mmodel7, residuals = TRUE)
acf(residuals(mmodel7, type = "pearson"), lag = 200)


omno_doy <- 
  oxygen_model_data_no_outliers3 %>% 
  mutate(DoY = yday(Start_time),
         Year = as.factor(Year))
ctrl <- list(niterEM = 0, msVerbose = TRUE, optimMethod="L-BFGS-B")

mmodel8 <- mgcv::gamm(Oxygen ~ 
                            s(time_step, k = 20) +
                            s(DoY, bs = "cc", k = 12) + 
                            Year + 
                            s(Chl_a) + 
                            s(SSTa_new),
                          data = omno_doy,
                          method = "REML",
                          #correlation = nlme::corARMA(p = 1, form = ~1),
                          control = ctrl)
mmodel8_AR1 <- mgcv::gamm(Oxygen ~ 
                        s(time_step, k = 20) +
                        s(DoY, bs = "cc", k = 12) + 
                        Year + 
                        s(Chl_a) + 
                        s(SSTa_new),
                      data = omno_doy,
                      method = "REML",
                      correlation = nlme::corARMA(p = 1, form = ~1|Year),
                      control = ctrl)
mmodel8_AR2 <- mgcv::gamm(Oxygen ~ 
                        s(time_step, k = 20) +
                        s(DoY, bs = "cc", k = 12) + 
                        Year + 
                        s(Chl_a) + 
                        s(SSTa_new),
                      data = omno_doy,
                      method = "REML",
                      correlation = nlme::corARMA(p = 2, form = ~1|Year),
                      control = ctrl)
mmodel8_AR3 <- mgcv::gamm(Oxygen ~ 
                       s(time_step, k = 20) +
                       s(DoY, bs = "cc", k = 12) + 
                       Year + 
                       s(Chl_a) + 
                       s(SSTa_new),
                     data = omno_doy,
                     method = "REML",
                     correlation = nlme::corARMA(p = 3, form = ~1|Year),
                     control = ctrl)
anova(mmodel8$lme, mmodel8_AR1$lme, mmodel8_AR2$lme, mmodel8_AR3$lme)
anova(mmodel8_AR1$lme, mmodel8_AR2$lme, mmodel8_AR3$lme)


mgcv::gam.check(mmodel8_AR2$gam)
summary(mmodel8_AR2$gam)
gratia::draw(mmodel8_AR2$gam, residuals = TRUE)
acf(residuals(mmodel8$gam, type = "pearson"), lag = 200)
acf(residuals(mmodel8_AR2$lme, type = "normalized"), lag = 200)
pacf(residuals(mmodel8_AR2$lme, type = "normalized"), lag = 200)



mmodel9 <- mgcv::gamm(Oxygen ~ 
                        s(time_step, k = 20) +
                        s(DoY, bs = "cc", k = 12) + 
                        Year,
                      data = omno_doy,
                      method = "REML",
                      #correlation = nlme::corARMA(p = 1, form = ~1),
                      control = ctrl)
mmodel9_AR1 <- mgcv::gamm(Oxygen ~ 
                            s(time_step, k = 20) +
                            s(DoY, bs = "cc", k = 12) + 
                            Year,
                          data = omno_doy,
                          method = "REML",
                          correlation = nlme::corARMA(p = 1, form = ~1|Year),
                          control = ctrl)
mmodel9_AR2 <- mgcv::gamm(Oxygen ~ 
                            s(time_step, k = 20) +
                            s(DoY, bs = "cc", k = 12) + 
                            Year,
                          data = omno_doy,
                          method = "REML",
                          correlation = nlme::corARMA(p = 2, form = ~1|Year),
                          control = ctrl)
mmodel9_AR3 <- mgcv::gamm(Oxygen ~ 
                            s(time_step, k = 20) +
                            s(DoY, bs = "cc", k = 12) + 
                            Year,
                          data = omno_doy,
                          method = "REML",
                          correlation = nlme::corARMA(p = 3, form = ~1|Year),
                          control = ctrl)


mmodel9_2 <- mgcv::gamm(Oxygen ~ 
                        s(time_step, k = 20) +
                        s(DoY, bs = "cc", k = 12) + 
                        Year,
                      data = omno_doy,
                      method = "REML",
                      #correlation = nlme::corARMA(p = 1, form = ~1),
                      control = ctrl)
mmodel9_AR1_2 <- mgcv::gamm(Oxygen ~ 
                            s(time_step, k = 20) +
                            s(DoY, bs = "cc", k = 12) + 
                            Year,
                          data = omno_doy,
                          method = "REML",
                          correlation = nlme::corARMA(p = 1, form = ~1),
                          control = ctrl)
mmodel9_AR2_2 <- mgcv::gamm(Oxygen ~ 
                            s(time_step, k = 20) +
                            s(DoY, bs = "cc", k = 12) + 
                            Year,
                          data = omno_doy,
                          method = "REML",
                          correlation = nlme::corARMA(p = 2, form = ~1),
                          control = ctrl)
mmodel9_AR3_2 <- mgcv::gamm(Oxygen ~ 
                            s(time_step, k = 20) +
                            s(DoY, bs = "cc", k = 12) + 
                            Year,
                          data = omno_doy,
                          method = "REML",
                          correlation = nlme::corARMA(p = 3, form = ~1),
                          control = ctrl)

anova(mmodel9_2$lme, mmodel9_AR1_2$lme, mmodel9_AR2_2$lme, mmodel9_AR3_2$lme)
anova(mmodel9$lme, mmodel9_AR2_2$lme)
mgcv::gam.check(mmodel9_AR2$gam)
summary(mmodel9_AR2$gam)
summary(mmodel8_AR2$gam)



omt1 <- mgcv::gamm(Oxygen ~ s(time_step, k = 20) + 
                     Year,
                   data = omno_doy,
                   method = "REML",
                   correlation = nlme::corARMA(p = 2, form = ~ 1),
                   control = ctrl)

omt2 <- mgcv::gamm(Oxygen ~ s(DoY, k = 12, bs = "cc") + 
                     Year,
                   data = omno_doy,
                   method = "REML",
                   correlation = nlme::corARMA(p = 2, form = ~ 1),
                   control = ctrl)

omt3 <- mgcv::gamm(Oxygen ~ s(time_step, k = 20) + s(DoY, k = 12, bs = "cc") + 
                     Year,
                   data = omno_doy,
                   method = "REML",
                   correlation = nlme::corARMA(p = 2, form = ~ 1),
                   control = ctrl)

anova(omt1$lme, omt2$lme, omt3$lme)
nrow(omno_doy)

mgcv::gam.check(omt3$gam)
summary(omt3$gam)

omt3_env <- mgcv::gamm(Oxygen ~ 
                         s(time_step, k = 20) + 
                         s(DoY, k = 12, bs = "cc") + 
                         s(Chl_a) + 
                         s(SSTa_new) + 
                         s(Year, bs = "re"),
                       data = omno_doy,
                       method = "REML",
                       correlation = nlme::corARMA(p = 2, form = ~ 1),
                       control = ctrl)
mgcv::gam.check(omt3_env$gam)
summary(omt3_env$gam)
gratia::draw(omt3_env$gam, residuals = TRUE)

omt3_env_pre <- mgcv::gamm(Oxygen ~ 
                         s(time_step, k = 20) + 
                         s(DoY, k = 12, bs = "cc") + 
                         s(Chl_a) + 
                         s(SSTa_new) + 
                         s(Year, bs = "re"),
                       data = premiss,
                       method = "REML",
                       correlation = nlme::corARMA(p = 2, form = ~ 1),
                       control = ctrl)
mgcv::gam.check(omt3_env_pre$gam)
summary(omt3_env_pre$gam)
gratia::draw(omt3_env_pre$gam, residuals = TRUE)

omt3_env_post <- mgcv::gamm(Oxygen ~ 
                         s(time_step, k = 20) + 
                         s(DoY, k = 12, bs = "cc") + 
                         s(Chl_a) + 
                         s(SSTa_new) + 
                         s(Year, bs = "re"),
                       data = postmiss,
                       method = "REML",
                       correlation = nlme::corARMA(p = 2, form = ~ 1),
                       control = ctrl)
mgcv::gam.check(omt3_env_post$gam)
summary(omt3_env_post$gam)
gratia::draw(omt3_env_post$gam, residuals = TRUE)


omt3_env2 <- mgcv::gamm(Oxygen ~ 
                         s(DoY, k = 12, bs = "cc") + 
                         s(Chl_a) + 
                         s(SSTa_new) + 
                         Year,
                       data = omno_doy,
                       method = "REML",
                       correlation = nlme::corARMA(p = 2, form = ~ 1),
                       control = ctrl)
mgcv::gam.check(omt3_env2$gam)
summary(omt3_env2$gam)
gratia::draw(omt3_env$gam, residuals = TRUE)
# omt3_env2 was the model I used in the paper originally. However, there is still 
# a 'cone' pattern in the residuals vs linear predictors plot, and some large 
# deviance residuals. The residuals look much better when I include the long-term 
# trend, however the smoothing parameters are not satisfactory. Adjusting K does 
# not yield any meaningful changes here...

# Autocorrelation plot
acf_dat <- acf(residuals(omt3_env$lme, type = "normalized"), lag = 200, plot = FALSE)
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
  gratia::qq_plot(omt3_env$gam) + 
    theme(
      panel.grid = element_blank()
    ),
  gratia::residuals_hist_plot(omt3_env$gam) + 
    theme(
      panel.grid = element_blank()
    ),
  gratia::residuals_linpred_plot(omt3_env$gam) + 
    theme(
      panel.grid = element_blank()
    ),
  gratia::observed_fitted_plot(omt3_env$gam) + 
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



gratia::draw(omt3$gam, residuals = TRUE)
acf(residuals(omt3$gam, type = "pearson"), lag = 200)
acf(residuals(omt3$lme, type = "normalized"), lag = 200)
pacf(residuals(omt3$lme, type = "normalized"), lag = 200)

omt3_ <- mgcv::gamm(Oxygen ~ s(time_step, k = 20) + s(DoY, k = 12, bs = "cc") + 
                     Year,
                   data = omno_doy,
                   method = "REML",
                   correlation = nlme::corARMA(p = 2, form = ~ 1),
                   control = ctrl)
mgcv::gam.check(omt3_k40$gam)
summary(omt3_k40$gam)


ggplot(data = omno_doy) +
  geom_point(aes(x = DoY, y = Oxygen)) +
  facet_wrap(~Year)






# Without removing outliers
m1 <- mgcv::gamm(Oxygen ~ s(WoY, bs = "cc") + Year + s(Chl_a),
           data = oxygen_model_data,
           family = mgcv::scat,
           correlation = nlme::corAR1(form = ~time_step))
?mgcv::family.mgcv
mgcv::gam.check(m1$gam)
plot(m1$gam, residuals = TRUE)
summary(m1$gam)
# Smooth term of Chlorophyll-a is significant
gratia::draw(m1)

# Remove the 3 outliers
oxygen_model_data_no_outliers <- 
  oxygen_model_data %>% 
  filter(Chl_a <= 7)

m2 <- mgcv::gamm(Oxygen ~ s(WoY) + s(Year, bs = "re") + Chl_a,
                 data = oxygen_model_data_no_outliers,
                 family = mgcv::scat,
                 correlation = nlme::corAR1(form = ~time_step))

mgcv::gam.check(m2$gam)
plot(m2$gam, residuals = TRUE)
summary(m2$gam)
# Linear effect of Chlorophyll-a is significant, but no evidence that a smooth term is necessary

# Remove only the point greater than 15
oxygen_model_data_no_outliers2 <- 
  oxygen_model_data %>% 
  filter(Chl_a <= 15)

m3 <- mgcv::gamm(Oxygen ~ s(WoY) + s(Year, bs = "re") + Chl_a,
                 data = oxygen_model_data_no_outliers2,
                 family = mgcv::scat,
                 correlation = nlme::corAR1(form = ~time_step))

acf(residuals(m3$lme, type = "pearson"))
mgcv::gam.check(m3$gam)
plot(m3$gam, residuals = TRUE)
summary(m3$gam)
# Linear effect of Chlorophyll-a is significant, but no evidence that a smooth term is necessary


# Break up the sections of non-missing data
premiss <- 
  oxygen_model_data %>% 
  filter(Start_time < "2015-01-09")

m4 <- mgcv::gamm(Oxygen ~ s(WoY) + s(Year, bs = "re") + s(Chl_a),
                 data = premiss,
                 family = mgcv::scat,
                 correlation = nlme::corAR1(form = ~time_step))
mgcv::gam.check(m4$gam)
plot(m4$gam, residuals = TRUE)
summary(m4$gam)

View(premiss)

postmiss <- 
  oxygen_model_data %>% 
  filter(Start_time > "2015-01-09")

m5 <- mgcv::gamm(Oxygen ~ s(WoY) + s(Year, bs = "re") + Chl_a,
                 data = postmiss,
                 family = mgcv::scat,
                 correlation = nlme::corAR1(form = ~time_step))
mgcv::gam.check(m5$gam)
plot(m5$gam, residuals = TRUE)
summary(m5$gam)


m6 <- mgcv::gamm(Oxygen ~ s(WoY, bs = "cc") + s(WoY, Year, bs = "fs", m = 2) + Chl_a,
                 data = oxygen_model_data,
                 family = mgcv::scat)#,
                 #correlation = nlme::corAR1(form = ~time_step))
mgcv::gam.check(m6$gam)
plot(m6$gam, residuals = TRUE)
summary(m6$gam)

gratia::draw(m6) +
  scale_colour_brewer(palette = "Set2") + 
  theme(legend.position = "left")


m7 <- mgcv::gamm(Oxygen ~ s(time_step, bs = "gp", k = 40) + s(WoY, Year, bs = "fs") + s(Chl_a),
                 data = oxygen_model_data,
                 family = mgcv::scat)#,
#correlation = nlme::corAR1(form = ~time_step))
mgcv::gam.check(m7$gam)
plot(m7$gam, residuals = TRUE)
summary(m7$gam)

gratia::draw(m7, residuals = TRUE) +
  scale_colour_brewer(palette = "Set2") + 
  theme(legend.position = "left")

m8 <- mgcv::gamm(Oxygen ~ 
                   s(time_step, bs = "gp", k = 40) + 
                   s(WoY, Year, bs = "fs") + 
                   s(Chl_a, Year, bs = "fs"),
                 data = oxygen_model_data,
                 family = mgcv::scat)#,
#correlation = nlme::corAR1(form = ~time_step))
mgcv::gam.check(m8$gam)
plot(m8$gam, residuals = TRUE)
summary(m8$gam)

gratia::draw(m8) +
  scale_colour_brewer(palette = "Set2") + 
  theme(legend.position = "left")



m9 <- mgcv::gamm(Oxygen ~ 
                   s(time_step, bs = "gp", k = 40) + 
                   s(WoY, Year, bs = "fs") + 
                   s(Chl_a, Year, bs = "fs"),
                 data = oxygen_model_data_no_outliers,
                 family = mgcv::scat)#,
#correlation = nlme::corAR1(form = ~time_step))
mgcv::gam.check(m9$gam)
plot(m9$gam, residuals = TRUE)
summary(m9$gam)

gratia::draw(m9) +
  theme(legend.position = "left")


m10 <- mgcv::gamm(Oxygen ~ 
                   #s(time_step, bs = "gp", k = 40) + 
                   s(WoY) + 
                   s(Chl_a) + 
                    Year,
                 data = premiss,
                 family = mgcv::scat)#,
#correlation = nlme::corAR1(form = ~time_step))
mgcv::gam.check(m10$gam)
plot(m10$gam, residuals = TRUE)
summary(m10$gam)

gratia::draw(m10) +
  theme(legend.position = "left")


m10 <- mgcv::gamm(Oxygen ~ 
                    s(WoY, k = 12, bs = "cc") + 
                    s(Chl_a) + 
                    s(SSTa_new) + 
                    Year,
                  data = all_data,
                  family = mgcv::scat,
                  method = "REML",
                  correlation = nlme::corARMA(p = 2))#,
#correlation = nlme::corAR1(form = ~time_step))
mgcv::gam.check(m10$gam)
plot(m10$gam, residuals = TRUE)
summary(m10$gam)

gratia::draw(m10, residuals = TRUE) +
  theme(legend.position = "left")

plot(gratia::fixed_effects(m10))



# Split the pre and post missing data chunks
premiss <- 
  all_data %>% 
    filter(Start_time < "2015-01-09")
m11 <- mgcv::gamm(Oxygen ~ 
                    s(WoY, k = 12, bs = "cc") + 
                    s(Chl_a, k = 12) + 
                    s(SSTa_new, by = Year) + 
                    Year,
                  data = premiss,
                  family = mgcv::scat,
                  method = "REML",
                  correlation = nlme::corARMA(p = 2))#,
#correlation = nlme::corAR1(form = ~time_step))
mgcv::gam.check(m11$gam)
plot(m11$gam, residuals = TRUE)
summary(m11$gam)

gratia::draw(m11, residuals = TRUE) +
  theme(legend.position = "left")

plot(gratia::fixed_effects(m11))


m12 <- mgcv::gamm(Oxygen ~ 
                    s(WoY, by = Year) + 
                    s(Chl_a) + 
                    s(SSTa_new) + 
                    Year,
                  data = premiss,
                  family = mgcv::scat,
                  method = "REML",
                  correlation = nlme::corARMA(p = 2))#,
#correlation = nlme::corAR1(form = ~time_step))
mgcv::gam.check(m11$gam)
plot(m12$gam, residuals = TRUE)
summary(m12$gam)

gratia::draw(m12, residuals = TRUE) +
  theme(legend.position = "left")

plot(gratia::fixed_effects(m12))


postmiss <- 
  all_data %>% 
  filter(Start_time >= "2015-01-09")
m13 <- mgcv::gamm(Oxygen ~ 
                    s(WoY, k = 12, bs = "cc") + 
                    s(Chl_a, k = 12) + 
                    s(SSTa_new, by = Year) + 
                    Year,
                  data = postmiss,
                  family = mgcv::scat,
                  method = "REML",
                  correlation = nlme::corARMA(p = 2))#,
#correlation = nlme::corAR1(form = ~time_step))
mgcv::gam.check(m13$gam)
plot(m13$gam, residuals = TRUE)
summary(m13$gam)

gratia::draw(m11, residuals = TRUE) +
  theme(legend.position = "left")


m14 <- mgcv::gam(Oxygen ~ 
                    s(WoY) + 
                    s(Chl_a, Year, bs = "fs") + 
                    s(SSTa_new) + 
                    Year,
                  data = all_data,
                  family = mgcv::scat,
                  method = "REML")#,
                  #correlation = nlme::corARMA(p = 2))#,
#correlation = nlme::corAR1(form = ~time_step))
mgcv::gam.check(m14)
plot(m14, residuals = TRUE)
summary(m14)

gratia::draw(m14, residuals = TRUE) +
  theme(legend.position = "left")

gratia::evaluate_parametric_term(m14, "Year") %>% 
  ggplot() +
  geom_point(aes(x = value, y = partial)) +
  geom_segment(aes(x = value, xend = value, y = partial - se, yend = partial + se))

acf(residuals(m14, "pearson"), lag = 200)


m15 <- mgcv::gam(Oxygen ~ 
                   s(WoY, bs = "cc") + 
                   s(Chl_a, Year, bs = "fs") + 
                   s(SSTa_new) + 
                   Year,
                 data = all_data,
                 family = mgcv::scat,
                 method = "REML")#,
#correlation = nlme::corARMA(p = 2))#,
#correlation = nlme::corAR1(form = ~time_step))
mgcv::gam.check(m15)
plot(m15, residuals = TRUE)
summary(m15)

gratia::draw(m15, residuals = TRUE) +
  theme(legend.position = "left")

gratia::evaluate_parametric_term(m14, "Year") %>% 
  ggplot() +
  geom_point(aes(x = value, y = partial)) +
  geom_segment(aes(x = value, xend = value, y = partial - se, yend = partial + se))

acf(residuals(m15, "pearson"), lag = 200)


m16 <- mgcv::gamm(Oxygen ~ 
                   s(WoY, bs = "cc") + 
                   s(Chl_a) + 
                   s(SSTa_new) + 
                   Year,
                 data = all_data,
                 family = mgcv::scat,
                 method = "REML",
                 correlation = nlme::corARMA(p = 2))#,
#correlation = nlme::corAR1(form = ~time_step))
mgcv::gam.check(m16$gam)
plot(m16$gam, residuals = TRUE)
summary(m16$gam)

premiss$End_time
View(postmiss)
gratia::draw(m16$gam, residuals = TRUE) +
  theme_bw()

gratia::evaluate_parametric_term(m16$gam, "Year") %>% 
  ggplot() +
  geom_point(aes(x = value, y = partial)) +
  geom_segment(aes(x = value, xend = value, y = partial - se, yend = partial + se))

acf(residuals(m15, "pearson"), lag = 200)


m17 <- mgcv::gamm(Oxygen ~ 
                    s(WoY, by = Year) + 
                    s(Chl_a) + 
                    s(SSTa_new) + 
                    Year,
                  data = all_data,
                  family = mgcv::scat,
                  method = "REML",
                  correlation = nlme::corARMA(p = 2))#,
#correlation = nlme::corAR1(form = ~time_step))
mgcv::gam.check(m17$gam)
plot(m17$gam, residuals = TRUE)
summary(m17$gam)

gratia::draw(m17$gam, residuals = TRUE) +
  theme_bw()

all_data_no_outlier <- 
  all_data %>% 
  filter(Chl_a < 15) 
m18 <- mgcv::gam(Oxygen ~ 
                    s(WoY, bs = "cc") + 
                    s(Chl_a) + 
                    #s(SSTa_new) + 
                    Year,
                  data = all_data_no_outlier,
                  family = mgcv::scat(),
                  method = "REML")#,
                  #correlation = nlme::corARMA(p = 2))#,
#correlation = nlme::corAR1(form = ~time_step))
mgcv::gam.check(m18)
plot(m18, residuals = TRUE)
summary(m18)

gratia::draw(m18, residuals = TRUE) +
  theme_bw()

gratia::evaluate_parametric_term(m18, "Year") %>% 
  ggplot() +
  geom_point(aes(x = value, y = partial)) +
  geom_segment(aes(x = value, xend = value, y = partial - se, yend = partial + se))


acf(residuals(m18, "pearson"), lag = 200)


m21 <- mgcv::gam(Oxygen ~ 
                   s(WoY, bs = "cc", k = 12) + 
                   Year,
                 data = all_data,
                 family = mgcv::scat(),
                 method = "REML")
mgcv::gam.check(m21)
summary(m21)

adno_pre <- 
  all_data_no_outlier %>% 
  filter(Start_time < "2015-01-09")
m19 <- mgcv::gam(Oxygen ~ 
                   s(WoY, bs = "cc", k = 12) + 
                   s(Chl_a) + 
                   s(SSTa_new) + 
                   Year,
                 data = adno_pre,
                 family = mgcv::scat(),
                 method = "REML")#,
#correlation = nlme::corARMA(p = 2))#,
#correlation = nlme::corAR1(form = ~time_step))
mgcv::gam.check(m19)
plot(m19, residuals = TRUE)
summary(m19)

gratia::draw(m19, residuals = TRUE) +
  theme_bw()

gratia::evaluate_parametric_term(m19, "Year") %>% 
  ggplot() +
  geom_point(aes(x = value, y = partial)) +
  geom_segment(aes(x = value, xend = value, y = partial - se, yend = partial + se))

adno_post <- 
  all_data_no_outlier %>% 
  filter(Start_time >= "2015-01-09")
m20 <- mgcv::gam(Oxygen ~ 
                   s(WoY, bs = "cc") + 
                   s(Chl_a) + 
                   s(SSTa_new) + 
                   Year,
                 data = adno_post,
                 family = mgcv::scat(),
                 method = "REML")
mgcv::gam.check(m20)
plot(m20, residuals = TRUE)
summary(m20)

gratia::draw(m20, residuals = TRUE) +
  theme_bw()

gratia::evaluate_parametric_term(m20, "Year") %>% 
  ggplot() +
  geom_point(aes(x = value, y = partial)) +
  geom_segment(aes(x = value, xend = value, y = partial - se, yend = partial + se))


ggplot(all_data) +
  geom_point(aes(x = Start_time, y = Oxygen)) +
  geom_smooth(aes(x = Start_time, y = Oxygen), method = "gam")

p1 <- premiss %>% 
  ggplot() +
  geom_point(aes(x = Start_time, y = Oxygen), alpha = 0.7) +
  geom_smooth(aes(x = Start_time, y = Oxygen), method = "gam", colour = "black") +
  lims(y = c(0.75, 2)) +
  theme_classic()

p2 <- postmiss %>% 
  ggplot() +
  geom_point(aes(x = Start_time, y = Oxygen), alpha = 0.7) +
  geom_smooth(aes(x = Start_time, y = Oxygen), method = "gam", colour = "black") +
  lims(y = c(0.75, 2)) +
  theme_classic()

p3 <- premiss %>% 
  ggplot() +
  geom_point(aes(x = Chl_a, y = Oxygen), alpha = 0.7) + 
  geom_smooth(aes(x = Chl_a, y = Oxygen), method = "gam", colour = "black") +
  lims(y = c(0.75, 2)) +
  theme_classic()

p4 <- postmiss %>% 
  ggplot() +
  geom_point(aes(x = Chl_a, y = Oxygen), alpha = 0.7) + 
  geom_smooth(aes(x = Chl_a, y = Oxygen), method = "gam", colour = "black") +
  lims(y = c(0.75, 2)) +
  theme_classic()

cowplot::plot_grid(p1, p2, p3, p4, ncol = 2)
