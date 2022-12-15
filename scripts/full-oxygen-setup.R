## Oxygen data setup ----
## Load in libraries ----
library(tidyverse)
library(lubridate)
library(data.table)


# Write a function to load in the data
rbindlist_fread <- function(path, pattern = "*.csv") {
  files = list.files(path, pattern, full.names = TRUE)
  rbindlist(lapply(files, function(x) fread(x, header = FALSE, sep = ",", skip = 97)))
}
# Load in the data
test <- rbindlist_fread(path = "data/Search_28111952 (1)")  # Just over a minute, so fast!
# Look at the structure
str(test)
# Set the column names
colnames(test) <- c("Time", "Oxygen", "QC")
# Take a look
head(test)

# The dataset is too large to clean as one, so I'll break it down first, clean it, and then put it all together
# Write a function to split the dataframe up into equal sections of 1000000 rows
chunks <- function (data) {
  data_list <- split(data, (seq(nrow(data))-1) %/% 1000000)
}

split(test, (seq(nrow(test)) - 1) %/% 1000000)

data_rows <- c(seq(9035, nrow(test), 1000000), 349657843)
# Get the chunks
data_Chunks <- lapply(test, chunks)
chunks(test)
# Look at the chunks
str(data_Chunks)
# There are 350 lists for each variable, and each list has 1000000 rows
# I want to make a dataframe that includes Time and weekly mean Oxygen data

# Create empty dataframes
df_part = data.frame()  # To store each chunk of Time and Oxygen (n = 350) 
df_full = data.frame()  # To store the summarized data - this gets built up with each successive df_part that is read in

add_time <- function(data) {
  if (nrow(data) == 1) {
    data %>% 
      mutate(Start_time = chla_gam$Time_start[i],
             End_time = chla_gam$Time_end[i]) %>% 
      select(Oxygen, Start_time, End_time)
  } else {
    data %>% 
      mutate(Time_type = c("Start_time", "End_time")) %>% 
      pivot_wider(names_from = Time_type, values_from = Time)
  }
}


for (i in 180:nrow(chla_gam)) {
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
View(df_full)

ggplot(df_full) +
  geom_point(aes(x = Start_time, y = Oxygen))

plot(chla_gam$Chl_a, df_full$Oxygen)



write.csv(df_full, "data/weekly.bcus.oxygen.csv", row.names = FALSE)
oxygen <- read.csv("data/weekly.bcus.oxygen.csv", header = TRUE)