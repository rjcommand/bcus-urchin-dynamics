library(lubridate)
data <- read_csv("data/full-data.csv")

#' Clean BCUS data to prepare for modeling
#'
#' @param .bcus_data 
#'
#' @return
#' @export
#'
#' @examples
clean_bcus_data <- function(.bcus_data) {
  data <- 
    .bcus_data %>% 
    dplyr::mutate(Time = ymd_hms(Time), 
           Year = as.factor(Year))
  
  data_st <- map_df(unique(data$Year), function(x) {
    df <- 
      data %>% 
      dplyr::filter(Year == x) %>% 
      dplyr::mutate(
        sample_time = 1:nrow(.),
        FOV = case_when(
          Year == "2013-2014" ~ 0.44,
          Year == "2014-2015" ~ 14.4,
          Year == "2018" ~ 9.33,
          Year == "2019-2020" ~ 14.93
        ),
        urchin_Density2 = Number_of_urchins / FOV)
    
    if (x == "2019-2020")
      df$sample_time <- 125:688
    
    return(df)
  })
  return(data_st)
}

