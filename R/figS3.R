#' Figure 3 Autocorrelation plot for oxygen GAMM
#'
#' @param oxygen_model best fitting oxygen GAMM (omt3_env_AR2).
#'
#' @return Saves an autocorrelation plot to file (figures/figS3.png)
#' @export
#'
#' @examples
figS3 <- function(oxygen_model) { 
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

}


oxygen_model_residual_plots <- function(oxygen_model) {
  # Residual plots for Oxygen model
  cowplot::plot_grid(
    gratia::qq_plot(oxygen_model$gam) + 
      theme(
        panel.grid = element_blank()
      ),
    gratia::residuals_hist_plot(oxygen_model$gam) + 
      theme(
        panel.grid = element_blank()
      ),
    gratia::residuals_linpred_plot(oxygen_model$gam) + 
      theme(
        panel.grid = element_blank()
      ),
    gratia::observed_fitted_plot(oxygen_model$gam) + 
      theme(
        panel.grid = element_blank()
      ),
    ncol = 2, nrow = 2,
    rel_widths = c(1, 1, 1, 1, 2),
    align = "vh", axis = "lb",
    labels = LETTERS[1:4],
    label_x = 0.08, label_y = 0.8
  )
}