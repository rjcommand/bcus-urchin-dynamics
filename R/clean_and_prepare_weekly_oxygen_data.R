#' Clean and prepare the weekly oxygen data for the benthic oxygen GAMM
#'
#' @param chla_gam A \code{data.frame} of cleaned and prepared 8-day binned chlorophyll-a
#' data.
#' @param path_to_oxygen_files \code{character} string indicating the path to
#' the directory where raw oxygen data files are stored.
#'
#' @return A \code{data.frame} of weekly oxygen data that are binned to match
#' the temporal extent of the chlorophyll-a data.
#' @export
#'
#' @examples
clean_and_prepare_weekly_oxygen_data <- function(path_to_oxygen_files = "data/Search_28111952 (1)", 
                                                 chla_gam) {
  # Write a function to load in the data
  rbindlist_fread <- function(path, pattern = "*.csv") {
    files <- list.files(path, pattern, full.names = TRUE)
    rbindlist(lapply(files, function(x) fread(x, header = FALSE, sep = ",", skip = 97)))
  }
  # Load in the data
  test <- rbindlist_fread(path = path_to_oxygen_files)  # Just over a minute, so fast!

  # Set the column names
  colnames(test) <- c("Time", "Oxygen", "QC")
  
  # The dataset is too large to clean as one, so I'll break it down first, clean it, and then put it all together
  # Write a function to split the dataframe up into equal sections of 1000000 rows
  chunks <- function (data) {
    data_list <- split(data, (seq(nrow(data))-1) %/% 1000000)
  }
  
  # data_rows <- c(seq(9035, nrow(test), 1000000), 349657843)
  # # Get the chunks
  # data_Chunks <- lapply(test, chunks)
  
  # Create empty dataframes
  df_part = data.frame()  # To store each chunk of Time and Oxygen (n = 350) 
  df_full = data.frame()  # To store the summarized data - this gets built up with each successive df_part that is read in
  
  add_time <- function(data) {
    if (nrow(data) == 1) {
      data %>% 
        dplyr::mutate(Start_time = chla_gam$Time_start[i],
               End_time = chla_gam$Time_end[i]) %>% 
        dplyr::select(Oxygen, Start_time, End_time)
    } else {
      data %>% 
        dplyr::mutate(Time_type = c("Start_time", "End_time")) %>% 
        pivot_wider(names_from = Time_type, values_from = Time)
    }
  }
  
  # formerly: 180:nrow(chla_gam)
  for (i in 1:nrow(chla_gam)) {
    df_part <- data.frame(test[Time >= chla_gam$Time_start[i] & Time < chla_gam$Time_end[i]])
    df_part <- 
      df_part %>% 
      summarize(Time = cut(Time, "8 days"),
                Oxygen = mean(Oxygen, na.rm = TRUE)) %>% 
      distinct() %>% 
      add_time()
    # mutate(Time_type = c("Start_time", "End_time")) %>% 
    # pivot_wider(names_from = Time_type, values_from = Time)
    
    df_full <- rbind(df_full, df_part)  # Append the most recent chunk to the full data frame
    
  }
  
  return(df_full)
}


#' Title
#'
#' @param weekly_oxygen 
#' @param chla_gam 
#'
#' @return
#' @export
#'
#' @examples
combine_oxygen_and_chla <- function(weekly_oxygen, chla_gam) {
  oxygen_gam <- 
    weekly_oxygen %>% 
    dplyr::mutate(time_step = 1:nrow(.),
           Year = as.factor(lubridate::year(Start_time)),
           WoY = lubridate::week(Start_time)
    )
  
  # Combine
  oxygen_model_data <- 
    oxygen_gam %>% 
    left_join(chla_gam)#, by = c("Start_time" = "Time_start", "End_time" = "Time_end")) 
  
  oxygen_model_data <- 
    oxygen_model_data %>% 
    dplyr::mutate(Start_time = case_when(
      time_step == 180 ~ as.Date("2017-10-08"),
      time_step == 270 ~ as.Date("2020-02-02"),
      TRUE ~ as.Date(Start_time)
    ),
    End_time = case_when(
      time_step == 180 ~ as.Date("2017-10-16"),
      time_step == 270 ~ as.Date("2020-02-10"),
      TRUE ~ as.Date(End_time)
    ),
    Year = lubridate::year(Start_time)
    )
  return(oxygen_model_data)
}


#' Title
#'
#' @param buoy_raw_file 
#' @param chla_gam 
#'
#' @return
#' @export
#'
#' @examples
clean_and_prepare_buoy_data <- function(buoy_raw_file, chla_gam) {
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
  
  # time_range <- df_part$Time[df_part$Time >= chla_gam$Time_start[28] & 
  #                              df_part$Time < chla_gam$Time_end[28]]
  # if (!any(time_range >= chla_gam$Time_start[28] & time_range < chla_gam$Time_end[28])) {
  #   df_part <-
  #     tibble(
  #       Start_time = time_start,
  #       End_time = time_end,
  #       weekly_mean_SSTP = NA_character_
  #     ) 
  # }
  # df_part <- data.frame(buoy_tidy[buoy_tidy$Time >= chla_gam$Time_start[27] & 
  #                        buoy_tidy$Time < chla_gam$Time_end[27], ]) %>% 
  #   summarize(Time = cut(Time, "8 days"),
  #             weekly_mean_SSTP = mean(SSTP, na.rm = TRUE)) %>% 
  #   distinct() %>% 
  #   add_time()
  
  for (i in 1:nrow(chla_gam)) {
    time_start <- chla_gam$Time_start[i]
    time_end <- chla_gam$Time_end[i]
    
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
  
  #df_full %>% 
  #  dplyr::mutate(weekly_mean_SSTP = if_else(is.na(Start_time), NA_real_, weekly_mean_SSTP)) %>% View
  
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
  
  
  sstp_clean <- 
    rbind(df_full2, df_full) %>% 
    dplyr::mutate(weekly_mean_SSTP = ifelse(is.na(Start_time), NA, weekly_mean_SSTP)) %>% 
    dplyr::mutate(SSTa_new = weekly_mean_SSTP - mean(weekly_mean_SSTP, na.rm = TRUE))
  
  return(sstp_clean)

}


combine_oxygen_chla_buoy_data <- function(oxygen_model_data, sstp_clean) {
  all_data <- left_join(oxygen_model_data, sstp_clean)
  
  return(all_data)
}