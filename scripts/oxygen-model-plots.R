# Get fitted values from the GAMM
oxy.preds <- predict(mmodel4$gam, type = "response", se = TRUE)
oxy.pred.plot <- cbind(na.omit(oxygen_model_data_no_outliers3), oxy.preds)
View(oxy.pred.plot)
oxygen_trend_plot <- ggplot(data = oxygen_model_data_no_outliers3) +
  #geom_line(data = all_data, aes(x = Time_start, y = Oxygen)) +
  geom_smooth(aes(x = Time_start, y = Oxygen), formula = y ~ s(x, bs = "tp", k = 20), method = "gam", colour = "black") +
  geom_point(aes(x = Time_start, y = Oxygen)) +
  
  scale_x_datetime(date_labels = "%b-%Y", date_breaks = "1 year") +
  labs(x = "",
       y = "Oxygen (ml/l)") +
  geom_hline(aes(yintercept = mean(Oxygen, na.rm = TRUE)), linetype = "dashed") +
  theme_classic() +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 18))

ggplot(data = omno_doy) +
  #geom_line(data = all_data, aes(x = Time_start, y = Oxygen)) +
  geom_smooth(aes(x = Time_start, y = Oxygen), method = "gam", colour = "black") +
  geom_smooth(aes(x = Time_start, y = Oxygen), formula = y ~ s(x, bs = "tp", k = 20), method = "gam", colour = "red") +
  
  geom_point(aes(x = Time_start, y = Oxygen)) +
  
  scale_x_datetime(date_labels = "%b-%Y", date_breaks = "1 year") +
  labs(x = "",
       y = "Oxygen (ml/l)") +
  geom_hline(aes(yintercept = mean(Oxygen, na.rm = TRUE)), linetype = "dashed") +
  theme_classic() +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 18))

oxy.pred.plot %>% 
  as_tibble() %>% 
  ggplot() +
  geom_smooth(aes(x = Start_time, y = fit), method = "gam", colour = "black") +
  geom_point(aes(x = Start_time, y = Oxygen), alpha = 0.4) +
  scale_x_date(date_labels = "%b-%Y", date_breaks = "1 year") +
  labs(x = expression(paste("Surface [Chl-a] (", mg/m^{3}, ")")),
       y = "Oxygen (ml/l)") +
  geom_hline(aes(yintercept = mean(Oxygen, na.rm = TRUE)), linetype = "dashed") +
  theme_classic() +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 18))

# New figure 6
oxy_chla_plot <- oxy.pred.plot %>% 
  as_tibble() %>% 
ggplot() +
  geom_smooth(aes(x = Chl_a, y = fit), method = "gam", colour = "black") +
  geom_point(aes(x = Chl_a, y = Oxygen), alpha = 0.4) +
  labs(x = expression(paste("Surface [Chl-a] (", mg/m^{3}, ")")),
       y = "Oxygen (ml/l)") +
  geom_hline(aes(yintercept = mean(Oxygen, na.rm = TRUE)), linetype = "dashed") +
  geom_vline(aes(xintercept = 2.4)) +
  theme_classic() +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 18))
ggsave("figures/fig6.png", oxy_chla_plot, width = 15, height = 12, units = "in", dpi = 300)

oxygen_preds_8 <- predict(omt3_env$gam, type = "response", se = TRUE)
oxygen_preds_8_plot <- cbind(na.omit(omno_doy), oxygen_preds_8)
oxygen_preds_8_pre <- predict(omt3_env$gam, type = "response", se = TRUE, newdata = premiss)
oxygen_preds_8_post <- predict(omt3_env$gam, type = "response", se = TRUE, newdata = na.omit(postmiss))
oxygen_preds_8_pre_plot <- cbind(premiss, oxygen_preds_8_pre)
oxygen_preds_8_post_plot <- cbind(na.omit(postmiss), oxygen_preds_8_post)


oxy.preds.pre <- predict(omt3_env_pre$gam, type = "response", se = TRUE)
oxy.pred.pre.plot <- cbind(na.omit(premiss), oxy.preds.pre)
oxy.preds.post <- predict(omt3_env_post$gam, type = "response", se = TRUE)
oxy.pred.post.plot <- cbind(na.omit(postmiss), oxy.preds.post)
oxy.preds <- predict(omt3_env$gam, type = "response", se = TRUE)
oxy.pred.plot <- cbind(na.omit(omno_doy), oxy.preds)
# New Figure 4
oxy.pred.pre.plot %>% 
  as_tibble() %>% 
  ggplot() +
  #geom_smooth(data = oxygen_model_data_no_outliers3, aes(x = Start_time, y = Oxygen), formula = y ~ s(x, bs = "tp", k = 20), method = "gam", colour = "black") + 
  geom_rect(aes(xmin = as.Date("2014-01-01"), xmax = as.Date("2016-01-01"), 
  ymin = -Inf, ymax = Inf), fill = "orange", alpha = 0.008) +
  
  geom_smooth(data = oxy.pred.plot, aes(x = Start_time, y = fit), 
              method = "gam", colour = "seagreen", fill = "seagreen", alpha = 0.4) + 
  geom_smooth(aes(x = Start_time, y = fit), method = "gam", colour = "black") +
  geom_point(aes(x = Start_time, y = Oxygen), alpha = 0.4) +
  geom_smooth(data = oxy.pred.post.plot, aes(x = Start_time, y = fit), method = "gam", colour = "black") +
  geom_point(data = oxy.pred.post.plot, aes(x = Start_time, y = Oxygen), alpha = 0.4, colour = "black") +
  
  # Legend
  geom_rect(aes(xmin = as.Date("2019-01-25"), xmax = as.Date("2019-04-01"), 
                ymin = 1.78, ymax = 1.82), fill = "seagreen", alpha = 0.01) +
  geom_segment(aes(x = as.Date("2019-01-25"), xend = as.Date("2019-04-01"), 
                   y = 1.8, yend = 1.8), size = 1.2, colour = "seagreen") +
  geom_text(aes(x = as.Date("2018-10-01"), y = 1.8), label = "Full model") +
  
  geom_rect(aes(xmin = as.Date("2019-01-25"), xmax = as.Date("2019-04-01"), 
                ymin = 1.72, ymax = 1.76), fill = "grey30", alpha = 0.01) +
  geom_segment(aes(x = as.Date("2019-01-25"), xend = as.Date("2019-04-01"), 
                   y = 1.74, yend = 1.74), size = 1.2, colour = "black") +
  geom_text(aes(x = as.Date("2018-10-01"), y = 1.74), label = "Split model") +
  
  scale_x_date(date_labels = "%b-%y", date_breaks = "1 year") +
  
  labs(x = "",
       y = "Oxygen (ml/l)") +
  geom_hline(aes(yintercept = mean(Oxygen, na.rm = TRUE)), linetype = "dashed") +
  theme_minimal() +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        panel.border = element_rect(fill = NA, colour = "black", size = 1),
        panel.grid = element_blank(),
        axis.ticks = element_line(size = 1),
        legend.position = c(0.8, 0.6)
  )


oxygen_preds_8_pre_plot %>% 
  as_tibble() %>% 
  ggplot() +
  geom_rect(aes(xmin = as.Date("2014-01-01"), xmax = as.Date("2016-01-01"), 
                ymin = -Inf, ymax = Inf), fill = "orange", alpha = 0.008) +
  #geom_smooth(data = oxygen_model_data_no_outliers3, aes(x = Start_time, y = Oxygen), formula = y ~ s(x, bs = "tp", k = 20), method = "gam", colour = "black") + 
  geom_smooth(data = oxygen_preds_8_plot, aes(x = Start_time, y = fit), 
              method = "gam", colour = "seagreen", fill = "seagreen", alpha = 0.4) + 
  geom_smooth(aes(x = Start_time, y = fit), method = "gam", colour = "black") +
  geom_point(aes(x = Start_time, y = Oxygen), alpha = 0.4) +
  geom_smooth(data = oxygen_preds_8_post_plot, aes(x = Start_time, y = fit), method = "gam", colour = "black") +
  geom_point(data = oxygen_preds_8_post_plot, aes(x = Start_time, y = Oxygen), alpha = 0.4, colour = "black") +

  geom_rect(aes(xmin = as.Date("2019-01-25"), xmax = as.Date("2019-04-01"), 
                ymin = 1.78, ymax = 1.82), fill = "seagreen", alpha = 0.01) +
  geom_segment(aes(x = as.Date("2019-01-25"), xend = as.Date("2019-04-01"), 
                   y = 1.8, yend = 1.8), size = 1.2, colour = "seagreen") +
  geom_text(aes(x = as.Date("2018-10-01"), y = 1.8), label = "Full model") +
  
  geom_rect(aes(xmin = as.Date("2019-01-25"), xmax = as.Date("2019-04-01"), 
                ymin = 1.72, ymax = 1.76), fill = "grey30", alpha = 0.01) +
  geom_segment(aes(x = as.Date("2019-01-25"), xend = as.Date("2019-04-01"), 
                   y = 1.74, yend = 1.74), size = 1.2, colour = "black") +
  geom_text(aes(x = as.Date("2018-10-01"), y = 1.74), label = "Split model") +
  
  scale_x_date(date_labels = "%b-%y", date_breaks = "1 year") +
  labs(x = "",
       y = "Oxygen (ml/l)") +
  geom_hline(aes(yintercept = mean(Oxygen, na.rm = TRUE)), linetype = "dashed") +
  theme_minimal() +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        panel.border = element_rect(fill = NA, colour = "black", size = 1),
        panel.grid = element_blank(),
        axis.ticks = element_line(size = 1),
        legend.position = c(0.8, 0.6)
  )


p <- 
  ggplot() +

  geom_rect(data = NULL, aes(xmin = as.Date("2014-01-01"), xmax = as.Date("2016-01-01"), 
                ymin = -Inf, ymax = Inf), fill = "orange", alpha = 0.4) +
  
  geom_smooth(data = oxygen_preds_8_plot, aes(x = Start_time, y = fit), 
              method = "gam", colour = "black") + 
  geom_point(data = oxygen_preds_8_plot, aes(x = Start_time, y = Oxygen), alpha = 0.4) +
  
  geom_hline(data = oxygen_preds_8_plot, aes(yintercept = mean(Oxygen, na.rm = TRUE)), linetype = "dashed") +
  
  
  scale_x_date(date_labels = "%b-%y", date_breaks = "1 year") +
  labs(x = "",
       y = "Oxygen (ml/l)") +
  theme_bw() +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        panel.grid = element_blank()
  )

ggsave("figures/fig4.png", p, width = 15, height = 12, units = "in", dpi = 300)


chla_effect <- 
  gratia::draw(omt3_env, select = "s(Chl_a)", 
               residuals = TRUE,
               resid_col = "black") + 
  lims(y = c(-0.4, 0.4)) +
  theme_bw() +
  theme(
    panel.grid = element_blank()
  ) +
  ggtitle("") +
  labs(x = expression(paste("Surface [Chl-a] (", mg/m^3, ")")))

doy_effect <- 
  gratia::draw(omt3_env, select = "s(DoY)", 
               residuals = TRUE,
               resid_col = "black") + 
  lims(y = c(-0.4, 0.4)) +
  geom_vline(aes(xintercept = 172), linetype = "dashed", colour = "blue") +
  geom_vline(aes(xintercept = 355), linetype = "dashed", colour = "grey30") +
  theme_bw() +
  theme(
    panel.grid = element_blank()
  ) +
  ggtitle("") +
  labs(x = "Day of year")

time_effect <- 
  gratia::draw(omt3_env, select = "s(time_step)", 
               residuals = TRUE,
               resid_col = "black") + 
  lims(y = c(-0.4, 0.4)) +
  theme_bw() +
  theme(
    panel.grid = element_blank()
  ) +
  ggtitle("") +
  labs(x = "Time")

ssta_effect <- 
  gratia::draw(omt3_env, select = "s(SSTa_new)", 
               residuals = TRUE,
               resid_col = "black") + 
  lims(y = c(-0.4, 0.4)) +
  theme_bw() +
  theme(
    panel.grid = element_blank()
  ) +
  ggtitle("") +
  labs(x = "SSTa")

oxy_effects_plots <- 
  cowplot::plot_grid(doy_effect, chla_effect, time_effect, ssta_effect, nrow = 2,
                   align = "hv", axis = "lb", labels = c("A", "B", "C", "D"),
                   label_x = 0.1, label_y = 0.9, label_size = 18)
ggsave("figures/figure5.png", oxy_effects_plots, width = 15, height = 12, units = "in", dpi = 300)
