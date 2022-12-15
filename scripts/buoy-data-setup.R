# Buoy data set up
buoy_data <- read_csv("data/c46206.csv")

buoy_tidy <- 
  buoy_data %>% 
  rename("Time" = "DATE") %>% 
  mutate(Time = mdy_hm(Time, tz = "UTC")) %>% 
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
        mutate(Start_time = time_start,
               End_time = time_end) %>% 
        select(weekly_mean_SSTP, Start_time, End_time)
    } else {
      data %>% 
        mutate(Time_type = c("Start_time", "End_time")) %>% 
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

df_full %>% 
  mutate(weekly_mean_SSTP = if_else(is.na(Start_time), NA_real_, weekly_mean_SSTP)) %>% View

pre_data <- 
  data.frame(
    Start_time = rev(c(seq.Date(ymd("2013-05-09"), ymd("1988-01-01"), by = -8), NA)),
    End_time = rev(seq.Date(ymd("2013-05-17"), ymd("1988-01-01"), by = -8))
  ) %>% 
  filter(!is.na(Start_time))

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
  mutate(weekly_mean_SSTP = ifelse(is.na(Start_time), NA, weekly_mean_SSTP)) %>% 
  mutate(SSTa_new = weekly_mean_SSTP - mean(weekly_mean_SSTP, na.rm = TRUE))


View(sstp_clean)
# DONE #



equal_check <- 
  rbind(df_full2, df_full) %>% 
  left_join(buoy_ssta, by = c("Start_time" = "DATE"))

equal_check %>% 
  mutate(SSTa_new = weekly_mean_SSTP.x - mean(weekly_mean_SSTP.x, na.rm = TRUE)) %>%
  ggplot() +
  geom_point(aes(x = Start_time, y = SSTa_new)) +
  geom_point(aes(x = Start_time, y = SSTa), colour = "red") +
    geom_smooth(aes(x = Start_time, y = SSTa_new))


(na.omit(equal_check$weekly_mean_SSTP.x == equal_check$weekly_mean_SSTP.y))
