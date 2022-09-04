fit_urchin_models <- function(.modeling_data) {
  # G: Global trend for time + Group-level random effect (Year)
  m0 <- gam(Number_of_urchins ~ 
              s(sample_time, bs = "tp", k = 12, m = 2) + 
              s(Year, bs = "re") + 
              offset(log(FOV)),
            data = .modeling_data,
            family = nb(),
            method = "REML")
  
  # I: No global common trend for time; 
  # Group-level (Year) smooth for time (different wiggliness) + Group-level random effect (Year)
  m1 <- gam(Number_of_urchins ~ 
              s(sample_time, by = Year, bs = "tp", k = 12, m = 2) + 
              s(Year, bs = "re") + 
              offset(log(FOV)),
            data = .modeling_data,
            family = nb(),
            method = "REML")
  
  # S: No global common trend for time; 
  # Group-level (Year) smooth for time (same wiggliness, factor smooth)
  m2 <- gam(Number_of_urchins ~  
              s(sample_time, Year, bs = "fs", k = 12, m = 2) + 
              offset(log(FOV)),
            data = .modeling_data,
            family = nb(),
            method = "REML")
  
  # GS: Global common trend for time + Group-level (Year) smooth for time (same wiggliness, factor smooth)
  m3 <- gam(Number_of_urchins ~  
              s(sample_time, bs = "tp", k = 5, m = 2) + 
              s(sample_time, Year, k = 12, m = 2, bs = "fs") + 
              offset(log(FOV)),
            data = .modeling_data,
            family = nb(),
            method = "REML",
            select = TRUE)
  
  # GI: Global common trend for time + Group-level (Year) smooth for time (different wiggliness) + Group-level random effect (Year)
  m4 <- gam(Number_of_urchins ~ 
              s(sample_time, bs = "tp", k = 5, m = 2) + 
              s(sample_time, by = Year, bs = "tp", k = 12, m = 1) +
              s(Year, bs = "re") + 
              offset(log(FOV)),
            data = .modeling_data,
            family = nb(),
            method = "REML",
            select = TRUE)
  
  # 1.  Temporal model I + global trends for environmental covariates  
  env_m1I <- gam(Number_of_urchins ~ s(sample_time, by = Year, k = 12, bs = "tp", m = 2) + 
                   s(Year, bs = "re") +
                   s(Oxygen, k = 5, bs = "tp") +
                   s(Magnitude, k = 5, bs = "tp") +
                   s(backscatter, k = 5, bs = "tp") +
                   offset(log(FOV)),
                 data = .modeling_data,
                 family = nb(),
                 method = "REML")

  # 2. Temporal model GI + global trends for environmental covariates
  env_m1GI <- gam(Number_of_urchins ~ s(sample_time, k = 5, bs = "tp") +
                    s(sample_time, by = Year, k = 12, bs = "tp", m = 1) + 
                    s(Year, bs = "re") +
                    s(Oxygen, k = 5, bs = "tp") +
                    s(Magnitude, k = 5, bs = "tp") +
                    s(backscatter, k = 5, bs = "tp") +
                    offset(log(FOV)),
                  data = .modeling_data,
                  family = nb(),
                  method = "REML",
                  select = TRUE)
  
  return(list(m0 = m0, m1 = m1, m2 = m2, m3 = m3, m4 = m4, env_m1I = env_m1I, env_m1GI = env_m1GI))
}

