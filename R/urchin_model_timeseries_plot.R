urchin_model_timeseries_plot <- function (modeling_data, model) {
  yr <- levels(modeling_data$Year)  # get names of factor levels for Year
  num_obs <- numeric()  # Create an empty vector to store the number of observations per year
  for (i in yr) {
    num_obs[i] <- nrow(subset(modeling_data, Year == i))
  }
  urch_plot_data <- data.frame(sample_time = modeling_data$sample_time, 
                               Year = c(rep("2013-2014", num_obs[[1]]), 
                                        rep("2014-2015", num_obs[[2]]), 
                                        rep("2018", num_obs[[3]]), 
                                        rep("2019-2020", num_obs[[4]])),
                               FOV = c(rep(modeling_data$FOV[modeling_data$Year == "2013-2014"], 1),
                                       rep(modeling_data$FOV[modeling_data$Year == "2014-2015"], 1),
                                       rep(modeling_data$FOV[modeling_data$Year == "2018"], 1),
                                       rep(modeling_data$FOV[modeling_data$Year == "2019-2020"], 1)),
                               Oxygen = c(seq(min(modeling_data$Oxygen, na.rm = TRUE), max(modeling_data$Oxygen, na.rm = TRUE), length.out = num_obs[[1]]),
                                          seq(min(modeling_data$Oxygen, na.rm = TRUE), max(modeling_data$Oxygen, na.rm = TRUE), length.out = num_obs[[2]]),
                                          seq(min(modeling_data$Oxygen, na.rm = TRUE), max(modeling_data$Oxygen, na.rm = TRUE), length.out = num_obs[[3]]),
                                          seq(min(modeling_data$Oxygen, na.rm = TRUE), max(modeling_data$Oxygen, na.rm = TRUE), length.out = num_obs[[4]])),
                               Magnitude = c(seq(min(modeling_data$Magnitude, na.rm = TRUE), max(modeling_data$Magnitude, na.rm = TRUE), length.out = num_obs[[1]]),
                                             seq(min(modeling_data$Magnitude, na.rm = TRUE), max(modeling_data$Magnitude, na.rm = TRUE), length.out  = num_obs[[2]]),
                                             seq(min(modeling_data$Magnitude, na.rm = TRUE), max(modeling_data$Magnitude, na.rm = TRUE), length.out  = num_obs[[3]]),
                                             seq(min(modeling_data$Magnitude, na.rm = TRUE), max(modeling_data$Magnitude, na.rm = TRUE), length.out  = num_obs[[4]])),
                               backscatter = c(seq(min(modeling_data$backscatter, na.rm = TRUE), max(modeling_data$backscatter, na.rm = TRUE), length.out = num_obs[[1]]),
                                               seq(min(modeling_data$backscatter, na.rm = TRUE), max(modeling_data$backscatter, na.rm = TRUE), length.out = num_obs[[2]]),
                                               seq(min(modeling_data$backscatter, na.rm = TRUE), max(modeling_data$backscatter, na.rm = TRUE), length.out = num_obs[[3]]),
                                               seq(min(modeling_data$backscatter, na.rm = TRUE), max(modeling_data$backscatter, na.rm = TRUE), length.out = num_obs[[4]])))
  
  # Get predicted values and standard errors from model
  urch_env_m1I_fit <- predict(model, 
                              urch_plot_data, 
                              se.fit = TRUE)
  
  # Get fitted values
  urch_plot_data$mod_env_m1I_fit <- as.numeric(urch_env_m1I_fit$fit)
  
  # Add fitted values and standard errors to prediction dataframe
  urch_plot_data <- gather(urch_plot_data, model, fit, 
                           mod_env_m1I_fit)
  
  urch_plot_data <- mutate(urch_plot_data, se = c(as.numeric(urch_env_m1I_fit$se.fit)),
                           upper = exp(fit + (2 * se))/FOV,  # Divide by FOV to get upper-ci values as urchin density
                           lower = exp(fit - (2 * se))/FOV,  # Divide by FOV to get lower-ci values as urchin density
                           fit   = exp(fit)/FOV)  # Divide by FOV to get fitted values as urchin density
  
  # Get the date-time variable Time from the data_full data set for nicer plot
  modeling_data$Time <- ymd_hms(modeling_data$Time, tz ="UTC")
  urch_plot_data2 <- urch_plot_data
  urch_plot_data2$Time = rep(modeling_data$Time, 1)
  urch_plot_data2$Time <- ymd_hms(urch_plot_data2$Time, tz = "UTC")
  
  # Plot
  p <- map(unique(modeling_data$Year), function(x) {
    plot_dat <- 
      urch_plot_data2 %>% 
      dplyr::filter(Year == x)
    model_dat <- 
      modeling_data %>% 
      dplyr::filter(Year == x)
    axis_labels <- 
      switch(as.character(x),
             "2013-2014" = c("Sep-13", "Oct-13", "Nov-13", "Dec-13", "Jan-14", "Feb-14"),
             "2014-2015" = c("Sep-14", "Oct-14", "Nov-14", "Dec-14", "Jan-15", "Feb-15"),
             "2018" = c("Sep-18", "Oct-18", "Nov-18", "Dec-18", "Jan-19", "Feb-19"),
             "2019-2020" = c("Sep-19", "Oct-19", "Nov-19", "Dec-19", "Jan-20", "Feb-20")
      )
    ggplot(plot_dat) +
      geom_ribbon(aes(x = sample_time,
                      ymin = lower,
                      ymax = upper),
                  alpha = 0.2) +
      geom_point(data = model_dat, aes(x = sample_time, y = urchin_Density2), size = 0.6) +
      geom_line(aes(x = sample_time, y = fit)) +
      labs(y = "",#expression(paste("Urchin density (indv.", m^{-2}, ")")),
           x = "") +
      scale_x_continuous(limits = c(0, 691), labels = axis_labels, breaks = c(69, 189, 313, 433, 557, 681)) +
      theme_bw() +
      theme(
        panel.border = element_rect(fill = NULL, size = 1),
        panel.grid = element_blank(),
        axis.ticks = element_line(),
        legend.position = "",
        axis.title.y = element_text(size = 18),
        axis.text = element_text(size = 16),
        strip.text = element_text(size = 16)
      )
  }) %>% 
    cowplot::plot_grid(plotlist = ., ncol = 1, labels = LETTERS[1:4],
                       align = "v", axis = "l", label_x = 0.95, label_y = 0.95,
                       label_fontface = "bold", label_size = 18) +
    geom_text(aes(x = 0.02, y = 0.5), label = expression(paste("Urchin density (indv. ", m^{-2}, ")")), angle = 90, size = 6)
  
  if (!dir.exists(here::here("figures/"))) {
    dir.create("figures/")
  }
  ggsave("figures/fig2.png", p, width = 10, height = 12, units = "in", dpi = 300)
  p
}


