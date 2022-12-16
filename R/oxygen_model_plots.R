make_figure_4_data <- function(.model, omno_doy) {
  oxygen_preds_8 <- predict(.model$gam, type = "response", se = TRUE)
  oxygen_preds_8_plot <- cbind(na.omit(omno_doy), oxygen_preds_8)
  
  return(oxygen_preds_8_plot)
}

plot_fig4 <- function(oxygen_preds_8_plot) {
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
  return(p)
}


plot_fig5 <- function(.model) {
  chla_effect <- 
    gratia::draw(.model, select = "s(Chl_a)", 
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
    gratia::draw(.model, select = "s(DoY)", 
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
    gratia::draw(.model, select = "s(time_step)", 
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
    gratia::draw(.model, select = "s(SSTa_new)", 
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
  
}


make_figure_6_data <- function(.model, omno_doy) {
  # Get fitted values from the GAMM
  oxy.preds <- predict(.model$gam, type = "response", se = TRUE)
  oxy.pred.plot <- cbind(na.omit(omno_doy), oxy.preds)
  
  return(oxy.pred.plot)
}

plot_fig6 <- function(oxy.pred.plot) {
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
  return(oxy_chla_plot)
}
