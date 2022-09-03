get_aic_table <- function(models) {
  aics <- map_dbl(models, AIC)
  names(aics) <- c("G", "I", "S", "GS", "GI", 
                      "I + Global smooths", 
                      "GI + Global smooths")
  
  aic_table <- 
    aics %>% 
    as_tibble(rownames = "Model") %>% 
    rename("AIC" = "value") %>% 
    dplyr::mutate(data_source = c(rep("Temporal models", 5), rep("Temporal models + Env. cov.", 2))) %>%
    group_by(data_source) %>%
    dplyr::mutate(deltaAIC = AIC - min(AIC)) %>%
    ungroup() %>%
    dplyr::select(-data_source) %>%
    mutate(across(where(is.numeric), ~round(., digits = 0)))
}
