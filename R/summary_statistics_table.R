
summary_statistics_table <- function(.bcus_clean) {
  max_urch <- max(.bcus_clean$Number_of_urchins, na.rm = TRUE)
  n_urch_pres <- table(.bcus_clean$Number_of_urchins > 0)
  max_urch_density <- which.max(.bcus_clean$urchin_Density2)
  
  year = levels(.bcus_clean$Year)
  q = data.frame()
  for (i in year) {
    q[i, "Mean density"] = paste(round(mean(.bcus_clean$urchin_Density2[.bcus_clean$Year == i], na.rm = TRUE), 3), "(", round(se(.bcus_clean$urchin_Density2[.bcus_clean$Year == i]), 3), ")")
    q[i, "Total urchins counted"] = sum(.bcus_clean$Number_of_urchins[.bcus_clean$Year == i], na.rm = TRUE)
    q[i, "Max density in one video"] = max(.bcus_clean$urchin_Density2[.bcus_clean$Year == i], na.rm = TRUE)
    q[i, "Max count in one video"] = max(.bcus_clean$Number_of_urchins[.bcus_clean$Year == i], na.rm = TRUE)
  }
  return(q)
}
