
pre_prepare_chlorophyll_model_data <- function(chla_clean) {
  chla_gam2 <- 
    chla_clean %>% 
    dplyr::mutate(WoY = lubridate::week(Time_start),
                  DoY = lubridate::yday(Time_start),
                  Year = factor(lubridate::year(Time_start)))
}


prepare_buoy_data_for_chlorophyll_model <- function(buoy_raw_file = "data/c46206.csv",
                                                    chla_gam2) {
  # Buoy data set up
  buoy_data <- read_csv(buoy_raw_file)
  
  buoy_tidy <- 
    buoy_data %>% 
    rename("Time" = "DATE") %>% 
    dplyr::mutate(Time = mdy_hm(Time, tz = "UTC")) %>% 
    dplyr::filter(SSTP > 0,
                  SSTP < 20)
  range(buoy_tidy$SSTP)  # Looks better!
  # dotchart(buoy.data.untidy$SSTP)  # Looks much better!
  # plot(buoy.data.untidy$Time, buoy.data.untidy$SSTP)
  
  
  ## Calculate the anomaly for the entire data set
  # Create empty dataframes
  df_part = data.frame()  # To store each chunk of Time and Oxygen (n = 350) 
  df_full = data.frame()  # To store the summarized data - this gets built up with each successive df_part that is read in
  
  add_time <- function(data) {
    
    if (nrow(data) == 1) {
      data %>% 
        dplyr::mutate(Start_time = time_start,
               End_time = time_end) %>% 
        dplyr::select(weekly_mean_SSTP, Start_time, End_time)
    } else {
      data %>% 
        dplyr::mutate(Time_type = c("Start_time", "End_time")) %>% 
        pivot_wider(names_from = Time_type, values_from = Time)
    }
  }
  
  
  for (i in 1:nrow(chla_gam2)) {
    time_start <- chla_gam2$Time_start[i]
    time_end <- chla_gam2$Time_end[i]
    
    df_part <- data.frame(buoy_tidy[buoy_tidy$Time >= time_start & 
                                      buoy_tidy$Time < time_end, ])
    
    time_range <- df_part$Time[df_part$Time >= time_start & 
                                 df_part$Time < time_end]
    if (!any(time_range >= time_start & time_range < time_end)) {
      df_part <-
        tibble(
          Start_time = time_start,
          End_time = time_end,
          weekly_mean_SSTP = NA_integer_
        ) 
    } else {
      df_part <- 
        df_part %>% 
        #group_by(Time = cut(Time, "8 days")) %>% 
        summarize(Time = cut(Time, "8 days"),
                  weekly_mean_SSTP = mean(SSTP, na.rm = TRUE)) %>% 
        #mutate(SSTa = weekly_mean_SSTP - mean(weakly_mean_SSTP))
        distinct() %>% 
        add_time()
    }
    
    
    df_full <- rbind(df_full, df_part)  # Append the most recent chunk to the full data frame
    
  }
  
  df_full %>% 
    dplyr::mutate(weekly_mean_SSTP = if_else(is.na(Start_time), NA_real_, weekly_mean_SSTP)) %>% View
  
  pre_data <- 
    data.frame(
      Start_time = rev(c(seq.Date(ymd("2013-05-09"), ymd("1988-01-01"), by = -8), NA)),
      End_time = rev(seq.Date(ymd("2013-05-17"), ymd("1988-01-01"), by = -8))
    ) %>% 
    dplyr::filter(!is.na(Start_time))
  
  df_part2 <- data.frame()
  df_full2 <- data.frame()
  for (i in 1:nrow(pre_data)) {
    time_start <- pre_data$Start_time[i]
    time_end <- pre_data$End_time[i]
    
    df_part2 <- data.frame(buoy_tidy[buoy_tidy$Time >= time_start & 
                                       buoy_tidy$Time < time_end, ])
    
    time_range <- df_part2$Time[df_part2$Time >= time_start & 
                                  df_part2$Time < time_end]
    
    if (!all(time_range >= time_start & time_range < time_end) || is_empty(time_range)) {
      df_part2 <-
        tibble(
          Start_time = time_start,
          End_time = time_end,
          weekly_mean_SSTP = NA_integer_
        ) 
    } else {
      df_part2 <- 
        df_part2 %>% 
        summarize(Time = cut(Time, "8 days"),
                  weekly_mean_SSTP = mean(SSTP, na.rm = TRUE)) %>% 
        distinct() %>% 
        add_time()
    }
    
    
    df_full2 <- rbind(df_full2, df_part2)  # Append the most recent chunk to the full data frame
    
  }
  
  
  sstp_clean2 <- 
    df_full %>% 
    dplyr::mutate(weekly_mean_SSTP = ifelse(is.na(Start_time), NA, weekly_mean_SSTP)) %>% 
    dplyr::mutate(SSTa_new = weekly_mean_SSTP - mean(weekly_mean_SSTP, na.rm = TRUE))
  
  return(sstp_clean2)
}

combine_chla_and_buoy_data <- function(chla_gam2, sstp_clean2) {
  chla_model_data <- 
    chla_gam2 %>% 
    dplyr::mutate(Start_time = ymd(paste0(lubridate::year(Time_start), "-", 
                                   lubridate::month(Time_start), "-", 
                                   lubridate::day(Time_start))),
           End_time = ymd(paste0(lubridate::year(Time_end), "-", 
                                 lubridate::month(Time_end), "-", 
                                 lubridate::day(Time_end)))) %>% 
    full_join(sstp_clean2 %>% 
                mutate(Start_time = ymd(paste0(lubridate::year(Start_time), "-", 
                                               lubridate::month(Start_time), "-", 
                                               lubridate::day(Start_time))),
                       End_time = ymd(paste0(lubridate::year(End_time), "-", 
                                             lubridate::month(End_time), "-", 
                                             lubridate::day(End_time))))) %>% 
    dplyr::mutate(time_step = 1:nrow(.))
  
  return(chla_model_data)
}


prepare_chlorophyll_model_data <- function(chla_model_data) {
  # Add a variable for month
  chla_model_data <- 
    chla_model_data %>% 
    dplyr::mutate(nMonth = lubridate::month(End_time)) %>% 
    dplyr::filter(Start_time > "2013-01-01")
  
  return(chla_model_data)
}


fit_chlorophyll_models <- function(chla_model_data) {
  ## fix the knots are just passed the ends of the month numbers
  ## this allows Dec and Jan to have their own estimates
  knots <- list(nMonth = c(0.5, 12.5))
  # Following Gavin Simpson's blog post re: modeling seasonal data with GAMM
  ctrl <- list(niterEM = 0, msVerbose = TRUE, optimMethod="L-BFGS-B")  
  m1c <- mgcv::gamm(log(Chl_a) ~
                      s(nMonth, bs = "cc", k = 12) + 
                      s(SSTa_new) + 
                      Year,
                    data = chla_model_data,
                    knots = knots,
                    control = ctrl)
  
  m1c_AR1 <- mgcv::gamm(log(Chl_a) ~
                          s(nMonth, bs = "cc", k = 12) + 
                          s(SSTa_new) + 
                          Year,
                        data = chla_model_data,
                        correlation = nlme::corARMA(p = 1, form = ~1|Year),
                        knots = knots,
                        control = ctrl)
  
  m1c_AR2 <- mgcv::gamm(log(Chl_a) ~
                          s(nMonth, bs = "cc", k = 12) + 
                          s(SSTa_new) + 
                          Year,
                        data = chla_model_data,
                        correlation = nlme::corARMA(p = 2, form = ~1|Year),
                        knots = knots,
                        control = ctrl)

  m1c_AR3 <- mgcv::gamm(log(Chl_a) ~
                          s(nMonth, bs = "cc", k = 12) + 
                          s(SSTa_new) + 
                          Year,
                        data = chla_model_data,
                        correlation = nlme::corARMA(p = 3, form = ~1|Year),
                        knots = knots,
                        control = ctrl)

  return(
    list(
      m1c = m1c,
      m1c_AR1 = m1c_AR1,
      m1c_AR2 = m1c_AR2,
      m1c_AR3 = m1c_AR3
  ))
}