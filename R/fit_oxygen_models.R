prepare_oxygen_model_data <- function(oxygen_model_data) {
  # Remove 3 outliers
  oxygen_model_data_no_outliers3 <- 
    oxygen_model_data %>% 
    dplyr::filter(Chl_a <= 7)
  
  # Get DoY as a numeric and Year as a factor variables
  omno_doy <- 
    oxygen_model_data_no_outliers3 %>% 
    dplyr::mutate(DoY = lubridate::yday(Start_time),
           Year = as.factor(Year))
  
  return(omno_doy)
}


fit_oxygen_models <- function(omno_doy) {
  # Set up model fitting parameters
  ctrl <- list(niterEM = 0, msVerbose = TRUE, optimMethod = "L-BFGS-B")
  
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
  
  # anova(omt1$lme, omt2$lme, omt3$lme)
  
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
  # anova(omt3_env_0$lme, omt3_env_AR1$lme, omt3_env_AR2$lme, omt3_env_AR3$lme)
  
  omt3_env_final <- mgcv::gamm(Oxygen ~ 
                           s(time_step, k = 20) + 
                           s(DoY, k = 12, bs = "cc") + 
                           s(Chl_a) + 
                           s(SSTa_new) + 
                           s(Year, bs = "re"),
                         data = omno_doy,
                         method = "REML",
                         correlation = nlme::corARMA(p = 2, form = ~ 1),
                         control = ctrl)
  return(
    list(
      omt1 = omt1, 
      omt2 = omt2, 
      omt3 = omt3,
      omt3_env_0 = omt3_env_0, 
      omt3_env_AR1 = omt3_env_AR1, 
      omt3_env_AR2 = omt3_env_AR2, 
      omt3_env_AR3 = omt3_env_AR3,
      omt3_env_final = omt3_env_final
    )
  )
}