chlorophyll_model_residuals_plots <- function(m1c_AR1) {
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
  
  return(p_s)
}
