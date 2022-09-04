#' Title
#'
#' @param model 
#'
#' @return
#' @export
#'
#' @examples
urchin_model_smooth_plot <- function(model) {
  # Get the estimated standard deviation of each smooth term from the model
  vcomp <- data.frame(gam.vcomp(model))
  vcomp <- vcomp[c(1:4), ]
  vcomp$rn <- factor(c("2019-2020", "2018", "2014-2015", "2013-2014"))
  vcomp$facet <- factor("Sample time")
  
  # Get the effects smooth plots
  pdat_oxy <- smooth_estimates(model, "s(Oxygen)")
  oxygen_smooth_plot <- ggplot(data = pdat_oxy) +
    geom_line(aes(x = Oxygen, y = est)) +
    geom_point(data = modeling_data, aes(x = Oxygen, y = est)) +
    geom_ribbon(aes(x = Oxygen, y = est, 
                    ymin = est - 1.96 * se,
                    ymax = est + 1.96 * se), alpha = 0.2) +
    labs(x = "Oxygen (mL/L)", y = "Effect") +
    theme_bw() +
    theme(panel.border = element_rect(fill = NULL, size = 1),
          panel.grid = element_blank(),
          axis.text = element_text(size = 16),
          axis.title = element_text(size = 18))
  
  pdat_backscatter <- smooth_estimates(model, "s(backscatter)")
  backscatter_smooth_plot <- ggplot(data = pdat_backscatter) +
    geom_line(aes(x = backscatter, y = est)) +
    geom_ribbon(aes(x = backscatter, y = est, 
                    ymin = est - 1.96 * se,
                    ymax = est + 1.96 * se), alpha = 0.2) +
    labs(x = "ADCP Backscatter (Hz)", y = "Effect") +
    theme_bw() +
    theme(panel.border = element_rect(fill = NULL, size = 1),
          panel.grid = element_blank(),
          axis.text = element_text(size = 16),
          axis.title = element_text(size = 18))
  
  pdat_magnitude <- smooth_estimates(model, "s(Magnitude)")
  magnitude_smooth_plot <- ggplot(data = pdat_magnitude) +
    geom_line(aes(x = Magnitude, y = est)) +
    geom_ribbon(aes(x = Magnitude, y = est, 
                    ymin = est - 1.96 * se,
                    ymax = est + 1.96 * se), alpha = 0.2) +
    labs(x = expression(paste("Current velocity magnitude (m ", s^{-1}, ")")), y = "Effect") +
    theme_bw() +
    theme(panel.border = element_rect(fill = NULL, size = 1),
          panel.grid = element_blank(),
          axis.text = element_text(size = 16),
          axis.title = element_text(size = 18))
  
  p <- cowplot::plot_grid(oxygen_smooth_plot, backscatter_smooth_plot, magnitude_smooth_plot, labels = LETTERS[1:3], ncol = 3)
  
  ggsave("figures/fig3-old.png", p, width = 12, height = 9, units = "in", res = 300)
  p
}

new_urchin_model_smooth_plot <- function (model, modeling_data) {
  b <- getViz(model)
  p <- list()
  ind <- c(6, 8, 7)
  smths <- c("s(Oxygen)", "s(backscatter)", "s(Magnitude)")
  xs <- c("Oxygen", "backscatter", "Magnitude")
  xlab <- c("Oxygen (mL/L)", "ADCP Backscatter( Hz)", "Current velocity magnitude (m/s)")
  for (i in 1:3) {
    smth_est <- smooth_estimates(model, smths[i], data = modeling_data, n = nrow(modeling_data))
    smth_est <- 
      smth_est %>% 
      mutate(xcol = smth_est %>% select(-smooth, -type, -by, -est, -se) %>% pull())
    p[i] <- 
      plot(sm(b, ind[i])) + 
      lims(y = c(-3, 3))  +
      l_rug(mapping = aes(x = x), alpha = 0.8) +
      geom_line(data = smth_est, aes(x = xcol, y = est), size = 1) + 
      l_points(shape = 19, size = 1, alpha = 0.1) + 
      geom_ribbon(data = smth_est, 
                  aes(x = xcol, y = est, ymin = est - 1.96 * se, ymax = est + 1.96 * se),
                  alpha = 0.2) + 
      labs(x = xlab[i],
           y = "Effect") + 
      theme_classic() + 
      theme(
        panel.border = element_rect(fill = NA, size = 1),
        panel.grid = element_blank(),
        axis.ticks = element_line(),
        axis.title.y = element_text(size = 18),
        axis.text = element_text(size = 16),
      )
  }
  
  p <- cowplot::plot_grid(plotlist = p, nrow = 1, labels = LETTERS[1:3],
                          label_x = 0.15, label_y = 0.97) 
  
  ggsave("figures/fig3.png", p, width = 15, height = 7, units = "in", dpi = 300)
  p
}

