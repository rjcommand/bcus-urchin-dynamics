plot_fig7 <- function(m1c_AR1) {
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
  return(p)
}

