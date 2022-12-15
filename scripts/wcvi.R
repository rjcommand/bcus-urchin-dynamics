
## Data set up ----
#### WCVI Trawl Survey data
## Load in libraries ----
library(lubridate)  # To work with date-time formats
library(tidyverse)  # To tidy the data
library(PMCMRplus)  # For non-parametric multiple comparisons
ggplot2::theme_set(theme_bw())

## Load in data ----
data.sp <- read.csv("data/WCVI_catch.csv", header = TRUE)
data.ll <- read.csv("data/WCVI_effort.csv", header = TRUE)

## Define functions ----
# Define standard error of the mean function
se <- function(x, na.rm) {
  sd(x, na.rm = TRUE)/sqrt(sum(!is.na(x)))
}


## Data setup ----
## Join the effort dataset with the catch dataset
all.data <- left_join(data.sp, data.ll) %>% 
  rename("Time" = "Survey.Year")

all_trawls <- all.data
# Filter for strongylocentrotus fragilis
urchin_trawls <- all.data %>% 
  dplyr::filter(English.common.name == "FRAGILE URCHIN") 

# Per Anderson et al 2019 p.256:
# "If there is no retained weight recorded but there is a retained count, then
# BEST_RETAINED_WT is calculated as the retained count multiplied by the best 
# available average kg per piece from, in preferential order, ROUND_KG_PER_OFFLOAD_PIECE, 
# ROUND_KG_PER_RETAINED_PIECE, EST_ROUND_KG_PER_PIECE. Similarly, if there is no 
# retained count recorded but there is a retained weight, then the retained weight 
# is divided by the best available average weight per piece to give BEST_RETAINED_COUNT."
urchin_trawls_IMPUTED <- 
  urchin_trawls %>% 
  mutate(BEST_RETAINED_COUNT = ifelse(is.na(Catch.count..pieces.) & !is.na(Catch.weight..kg.), Catch.weight..kg./mean(Catch.weight..kg., na.rm = TRUE), Catch.count..pieces.)) %>% 
  mutate(BEST_RETAINED_COUNT_rounded = round(BEST_RETAINED_COUNT)) %>% 
  select(Catch.count..pieces., Catch.weight..kg., BEST_RETAINED_COUNT, BEST_RETAINED_COUNT_rounded)
mean(urchin_trawls$Catch.weight..kg., na.rm = TRUE)

## There are some data points that have weight but not count.
## Here I assume that if there was a weight measured for the species that there must have been at least one urchin present in the trawl to be measured.
## Thus, I impute a value of 1 for count when [count = NA & weight = non-zero numeric]
## Write a function to do this:
fixMissing <- function (x, y) {
  x[is.na(x) & !is.na(y)] <- 1
  x
}
# Apply the imputation function
urchin_trawls$Catch.count..pieces. <- fixMissing(urchin_trawls$Catch.count..pieces., urchin_trawls$Catch.weight..kg.)
all_trawls$Catch.count..pieces. <- fixMissing(all_trawls$Catch.count..pieces., all_trawls$Catch.weight..kg.)
# Check
# cbind(data.all$Catch.count..pieces., data.all$Catch.weight..kg.)
# Now NA counts only occur where there were no urchins observed in the trawls


## Check for outliers ----
# Data points are considered to be outliers if they are greater than 75% quantile + (3 * the IQR)
# Only the 75% quantile is used because there are only outliers on the deep-end of the depth distribution
# This is done for urchin_trawls and all_trawls

## Urchin trawls

# Create an empty data frame
outliers_table <- data.frame()
# Create a vector for years
years <- unique(urchin_trawls$Time)
# Loop over years
for (i in 1:length(years)) {
  # Get the data for a given year
  data = urchin_trawls$Bottom.depth..m.[urchin_trawls$Time == years[i]]
  # Set column 1 equal to the year
  outliers_table[i, 1] <- years[i]
  # Set column 2 equal to the "outer fence", determined by 3 * the IQR + the 75% quantile (summary(data)[[5]]) 
  outliers_table[i, 2] <- (IQR(data, na.rm = TRUE) * 3) + summary(data)[[5]]
  # Set column names to be useful
  colnames(outliers_table) <- c("years", "outer_fence")
}
# Take a look
outliers_table

# Check which years have outliers
urchin_trawls %>% dplyr::filter(Time == 2004, Bottom.depth..m. > as.numeric(outliers_table[1, 2]))  # Outliers
urchin_trawls %>% dplyr::filter(Time == 2006, Bottom.depth..m. > as.numeric(outliers_table[2, 2]))  # Outliers
urchin_trawls %>% dplyr::filter(Time == 2008, Bottom.depth..m. > as.numeric(outliers_table[3, 2]))  # None
urchin_trawls %>% dplyr::filter(Time == 2010, Bottom.depth..m. > as.numeric(outliers_table[4, 2]))  # None
urchin_trawls %>% dplyr::filter(Time == 2012, Bottom.depth..m. > as.numeric(outliers_table[5, 2]))  # Outliers
urchin_trawls %>% dplyr::filter(Time == 2014, Bottom.depth..m. > as.numeric(outliers_table[6, 2]))  # Outliers
urchin_trawls %>% dplyr::filter(Time == 2016, Bottom.depth..m. > as.numeric(outliers_table[7, 2]))  # Outliers
urchin_trawls %>% dplyr::filter(Time == 2018, Bottom.depth..m. > as.numeric(outliers_table[8, 2]))  # Outliers

# Convert to data.frame type to make use of base R subsetting (not the best but it works)
urchin_trawls <- data.frame(urchin_trawls)
# Remove the outlier rows
urchin_trawls <- urchin_trawls[-c(as.numeric(row.names(urchin_trawls[urchin_trawls$Time == 2004 & urchin_trawls$Bottom.depth..m. > as.numeric(outliers_table[1, 2]), ])),
                                  na.omit(as.numeric(row.names(urchin_trawls[urchin_trawls$Time == 2006 & urchin_trawls$Bottom.depth..m. > as.numeric(outliers_table[2, 2]), ]))),
                                  na.omit(as.numeric(row.names(urchin_trawls[urchin_trawls$Time == 2008 & urchin_trawls$Bottom.depth..m. > as.numeric(outliers_table[3, 2]), ]))),
                                  na.omit(as.numeric(row.names(urchin_trawls[urchin_trawls$Time == 2010 & urchin_trawls$Bottom.depth..m. > as.numeric(outliers_table[4, 2]), ]))),
                                  na.omit(as.numeric(row.names(urchin_trawls[urchin_trawls$Time == 2012 & urchin_trawls$Bottom.depth..m. > as.numeric(outliers_table[5, 2]), ]))),
                                  na.omit(as.numeric(row.names(urchin_trawls[urchin_trawls$Time == 2014 & urchin_trawls$Bottom.depth..m. > as.numeric(outliers_table[6, 2]), ]))),
                                  na.omit(as.numeric(row.names(urchin_trawls[urchin_trawls$Time == 2016 & urchin_trawls$Bottom.depth..m. > as.numeric(outliers_table[7, 2]), ]))),
                                  na.omit(as.numeric(row.names(urchin_trawls[urchin_trawls$Time == 2018 & urchin_trawls$Bottom.depth..m. > as.numeric(outliers_table[8, 2]), ])))
), ]

## END ##

## All trawls

# Apply the same outlier test for all trawls conducted over the study period (not just S. fragilis trawls)
# Create a new data frame to avoid overwriting
trawl_depth <- all_trawls

# Create an empty data frame
outliers_table2 <- data.frame()
# Create a vector of years
years2 <- unique(trawl_depth$Time)
# Loop over each year
for (i in 1:length(years2)) {
  # Get the bottom depth data for a given year
  data = trawl_depth$Bottom.depth..m.[trawl_depth$Time == years2[i]]
  # Set column 1 equal to the year
  outliers_table2[i, 1] <- years2[i]
  # Set column 2 equal to the "outer fence", determined by 3 * the IQR + the 75% quantile (summary(data)[[5]])
  outliers_table2[i, 2] <- (IQR(data, na.rm = TRUE) * 3) + summary(data)[[5]]
  # Set the column names to be useful
  colnames(outliers_table2) <- c("years", "outer_fence")
}
# Take a look at the table
outliers_table2

# Check which years have outliers
trawl_depth %>% dplyr::filter(Time == 2004, Bottom.depth..m. > as.numeric(outliers_table2[1, 2]))  # Outliers
trawl_depth %>% dplyr::filter(Time == 2006, Bottom.depth..m. > as.numeric(outliers_table2[2, 2]))  # Outliers
trawl_depth %>% dplyr::filter(Time == 2008, Bottom.depth..m. > as.numeric(outliers_table2[3, 2]))  # None
trawl_depth %>% dplyr::filter(Time == 2010, Bottom.depth..m. > as.numeric(outliers_table2[4, 2]))  # Outliers
trawl_depth %>% dplyr::filter(Time == 2012, Bottom.depth..m. > as.numeric(outliers_table2[5, 2]))  # Outliers
trawl_depth %>% dplyr::filter(Time == 2014, Bottom.depth..m. > as.numeric(outliers_table2[6, 2]))  # Outliers
trawl_depth %>% dplyr::filter(Time == 2016, Bottom.depth..m. > as.numeric(outliers_table2[7, 2]))  # None
trawl_depth %>% dplyr::filter(Time == 2018, Bottom.depth..m. > as.numeric(outliers_table2[8, 2]))  # Outliers

# Remove the outlier rows
trawl_depth <- trawl_depth[-c(as.numeric(row.names(trawl_depth[trawl_depth$Time == 2004 & trawl_depth$Bottom.depth..m. > as.numeric(outliers_table2[1, 2]), ])),
                              na.omit(as.numeric(row.names(trawl_depth[trawl_depth$Time == 2006 & trawl_depth$Bottom.depth..m. > as.numeric(outliers_table2[2, 2]), ]))),
                              na.omit(as.numeric(row.names(trawl_depth[trawl_depth$Time == 2008 & trawl_depth$Bottom.depth..m. > as.numeric(outliers_table2[3, 2]), ]))),
                              na.omit(as.numeric(row.names(trawl_depth[trawl_depth$Time == 2010 & trawl_depth$Bottom.depth..m. > as.numeric(outliers_table2[4, 2]), ]))),
                              na.omit(as.numeric(row.names(trawl_depth[trawl_depth$Time == 2012 & trawl_depth$Bottom.depth..m. > as.numeric(outliers_table2[5, 2]), ]))),
                              na.omit(as.numeric(row.names(trawl_depth[trawl_depth$Time == 2014 & trawl_depth$Bottom.depth..m. > as.numeric(outliers_table2[6, 2]), ]))),
                              na.omit(as.numeric(row.names(trawl_depth[trawl_depth$Time == 2016 & trawl_depth$Bottom.depth..m. > as.numeric(outliers_table2[7, 2]), ]))),
                              na.omit(as.numeric(row.names(trawl_depth[trawl_depth$Time == 2018 & trawl_depth$Bottom.depth..m. > as.numeric(outliers_table2[8, 2]), ])))
), ]

## Data prep ----
# Set up each data frame for plotting

# For the density data
wcvi_density <- urchin_trawls %>% 
  dplyr::filter(!is.na(Catch.count..pieces.),
                !is.na(Bottom.depth..m.)) %>% 
  group_by(Time) %>% 
  summarize(urchin_Count_per_Year = mean(Catch.count..pieces., na.rm = TRUE),  # How many urchins caught in each year?
            trawled_Area_per_Year = mean(Distance.towed..m. * Trawl.door.spread..m., na.rm = TRUE),  # How much area was trawled each year?
            urchin_Density_per_Year = urchin_Count_per_Year / trawled_Area_per_Year,  # Urchin density each year?
            sd.urchin_Density = sd(Catch.count..pieces./sum(Distance.towed..m. * Trawl.door.spread..m., na.rm = TRUE)), na.rm = TRUE)


# For the depth distribution data
wcvi_depth <- urchin_trawls %>% 
  dplyr::filter(Catch.count..pieces. > 0,
                !is.na(Bottom.depth..m.)) %>% 
  group_by(Time, Bottom.depth..m.) %>% 
  summarise(urchin_Count_per_Year = sum(Catch.count..pieces., na.rm = TRUE),  # How many urchins caught in each year?
            trawled_Area_per_Year = sum(Distance.towed..m. * Trawl.door.spread..m., na.rm = TRUE),  # How much area was trawled each year?
            urchin_Density_per_Year = urchin_Count_per_Year / trawled_Area_per_Year,  # Urchin density each year?
            sd.urchin_Density = sd(Catch.count..pieces./sum(Distance.towed..m. * Trawl.door.spread..m., na.rm = TRUE), na.rm = TRUE))


# For density by depth bins:
# Add a column for depth bin
urchin_trawls$depth_bin <- cut(urchin_trawls$Bottom.depth..m., breaks = c(49, 100, 200, 300, 400, 600), 
                               labels = c("50-100 m", "101-200 m", "201-300 m", "301-400 m", "> 400 m"))

# Make the data frame
wcvi_depth_bins <- urchin_trawls %>% 
  dplyr::filter(Catch.count..pieces. > 0,
                !is.na(Bottom.depth..m.)) %>% 
  group_by(Time, depth_bin) %>% 
  summarise(urchin_Count_per_Year = mean(Catch.count..pieces., na.rm = TRUE),  # How many urchins caught in each year?
            trawled_Area_per_Year = mean(Distance.towed..m. * Trawl.door.spread..m., na.rm = TRUE),  # How much area was trawled each year?
            urchin_Density_per_Year = urchin_Count_per_Year / trawled_Area_per_Year,  # Urchin density each year?
            sd.urchin_Density = se(Catch.count..pieces./sum(Distance.towed..m. * Trawl.door.spread..m., na.rm = TRUE)))


## Plotting ----
# Figure 7. Average yearly urchin density over time
wcvi_density_plot <- ggplot(data = wcvi_density, aes(x = as.factor(Time), y = urchin_Density_per_Year)) +
  geom_point() +
  geom_errorbar(aes(ymin = urchin_Density_per_Year - sd.urchin_Density, 
                    ymax = urchin_Density_per_Year + sd.urchin_Density), 
                width = 0.1) +
  labs(y = expression(paste("Urchin density (indv.", m^-2, ")")), x = "Time") +
  scale_fill_brewer(name = "", palette = "Dark2") +
  scale_colour_brewer(name = "",
                      palette = "Dark2") +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        axis.title.x = element_blank())

# Plot and save!
tiff("figures/figure7-trawl-density.tiff", units = "in", width = 15, height = 8, res = 300)
wcvi_density_plot
dev.off()


# Figure 10. Urchin depth distribution over time 

# How many independent trawls were done in each year?
n_Trawls <- urchin_trawls %>% 
  dplyr::filter(Catch.count..pieces. > 0,
                !is.na(Catch.count..pieces.),
                !is.na(Bottom.depth..m.)) %>% 
  group_by(Time) %>% 
  summarise(n_Trips = length(unique(Trip.identifier)),
            n_Sets = length(Set.number))

# Calculate the mean depth for each year
mean.Depth <- wcvi_depth %>% 
  group_by(Time) %>% 
  summarize(mean_Depth = mean(Bottom.depth..m., na.rm = TRUE)) %>% 
  mutate(n_Trawls = n_Trawls$n_Sets)

# Create the plot
wcvi_depth_distribution_plot <- ggplot(data = wcvi_depth, aes(x = factor(Time), y = Bottom.depth..m.)) +
  geom_boxplot() +
  geom_point(data = mean.Depth, aes(x = factor(Time), y = mean_Depth), shape = 3) +
  geom_text(data = n_Trawls, aes(x = factor(Time), y = 50, label = n_Sets), position = position_dodge(width = 0.9)) +
  labs(x = "Year",
       y = "Depth (m)") +
  scale_y_reverse() +
  #theme_classic() +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        axis.title.x = element_blank())

# Plot and save!
tiff("figures/figure10-trawl-depth-distribution.tiff", units = "in", width = 15, height = 8, res = 300)
wcvi_depth_distribution_plot
dev.off()


# Figure 8. Density over time in 5 depth bins
# Plot by depth bins for each year
# Add a column for number of trawls in each depth bin in each year
n_Trawls_Depth <- urchin_trawls %>% 
  dplyr::filter(Catch.count..pieces. > 0,
                !is.na(Catch.count..pieces.)) %>% 
  group_by(Time, depth_bin) %>% 
  summarise(n_Trawls = length(depth_bin)) %>% 
  filter(!is.na(depth_bin))

# Depth bins plot
wcvi_depth_density_plot <- ggplot(data = subset(wcvi_depth_bins, !is.na(depth_bin))) +
  # Make the error bars on the bar plot
  geom_errorbar(aes(x = factor(Time), y = urchin_Density_per_Year, 
                    ymin = urchin_Density_per_Year - sd.urchin_Density,
                    ymax = urchin_Density_per_Year + sd.urchin_Density, 
                    group = depth_bin), 
                width = 0.2, position = position_dodge(0.9)) +
  # Make the bar plot
  geom_bar(aes(x = factor(Time), y = urchin_Density_per_Year, 
               fill = depth_bin, group = depth_bin), 
           stat = "identity", position = "dodge") +
  # Add trawl number labels
  geom_text(data = n_Trawls_Depth, 
            aes(x = factor(Time), y = 0, 
                group = depth_bin, label = n_Trawls), 
            position = position_dodge(width = 0.9), 
            vjust = -1, size = 4) +
  # Change the axis labels
  labs(y = expression(paste("Urchin density (indv.", m^-2, ")")), x = "") +
  # Change the y-axis breaks and align the bottom of the bars to y = 0
  scale_y_continuous(breaks = seq(0, 0.00005, by = 0.0000025), expand = expansion(mult = c(0, 0.1))) +
  # Sett the legend title to "Depth Bins" and the colour palette of the bars
  scale_fill_brewer("Depth Bins", type = "seq", palette = "Blues") +
  # Remove the lines from the background
  theme_classic() +
  # Change the axis text size and reposition/reorient the legend
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        legend.position = c(0.5, 0.94),
        legend.direction = "horizontal",
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16)) +
  # Move the legend title to be centered above the legend
  guides(fill = guide_legend(title.position = "top", title.hjust = 0.5))

# Plot and save!
tiff("figures/figure8-trawl-density-depth-bins.tiff", units = "in", width = 15, height = 8, res = 300)
wcvi_depth_density_plot
dev.off()


## Figure 9. Urchin trawls vs all trawls comparison 

# Data setup
# Get the depth distribution range each year for all trawls
# trawl_depth is created when checking for outliers in the entire trawl dataset
tdf <- trawl_depth %>% 
  dplyr::filter(!is.na(Bottom.depth..m.),
                !is.na(Catch.count..pieces.),
                Catch.count..pieces. > 0) %>% 
  group_by(Time) %>% 
  summarize(up.lim = quantile(Bottom.depth..m., na.rm = TRUE)[[1]],
            q.25 = quantile(Bottom.depth..m., na.rm = TRUE)[[2]],
            q.75 = quantile(Bottom.depth..m., na.rm = TRUE)[[4]],
            low.lim = quantile(Bottom.depth..m., na.rm = TRUE)[[5]],
            mean.depth = mean(Bottom.depth..m., na.rm = TRUE),
            median.depth = median(Bottom.depth..m., na.rm = TRUE))


## Combine all trawl summary stats and s. fragilis trawl summary stats
gf <- wcvi_depth %>% 
  group_by(Time) %>% 
  summarize(up.lim = quantile(Bottom.depth..m., na.rm = TRUE)[[1]],
            q.25 = quantile(Bottom.depth..m., na.rm = TRUE)[[2]],
            q.75 = quantile(Bottom.depth..m., na.rm = TRUE)[[4]],
            low.lim = quantile(Bottom.depth..m., na.rm = TRUE)[[5]],
            mean.depth = mean(Bottom.depth..m., na.rm = TRUE),
            median.depth = median(Bottom.depth..m., na.rm = TRUE))

# Add a column for "treatment"
tdf$Treatment <- "All trawls"
gf$Treatment <- "S. fragilis only"

# Join the urchin only and all trawls data sets
trawl_depth_dist <- tdf %>% 
  full_join(gf)



## Figures comparing depth distribution parameters between all trawl and s. fragilis trawls ----

# Create a vector of column names from the data frame
all_vars <- colnames(trawl_depth_dist)[-c(1, 8)]

# Create a vector of y-axis labels
y_names = c("Upper limit", "25% Quantile", "75% Quantile", "Lower limit", "Mean depth", "Median depth")

# Set the names of the vector of y-axis labels equal to the column names for easy indexing in the plot function
y_names = set_names(y_names, all_vars)

# Write a function to loop over all variables in a data frame
plot_function <- function(x, y) {
  ggplot(data = trawl_depth_dist, aes(x = .data[[x]], y = .data[[y]], colour = .data[["Treatment"]]) ) +
    geom_point() +
    geom_smooth(method = "lm", se = TRUE) +
    scale_x_continuous("", breaks = seq(2004, 2018, by = 2), labels = seq(2004, 2018, by = 2)) +
    scale_y_reverse(paste(y_names[y], "(m)")) +  # Set the y-axis labels equal to the associated element in y_names
    theme_classic() +
    theme(legend.title = element_blank(),
          legend.position = "",
          axis.text = element_text(size = 14),
          axis.title = element_text(size = 16))
}

# Write a function to loop over all the variables and create a list of Time vs variable plots
plots <- map(all_vars, function (var) {
  plot_function(x = "Time", y = var)
})

# Plot the list and save!
tiff("figures/figure9-trawl-data-compare.tiff", units = "in", width = 15, height = 8, res = 300)
cowplot::plot_grid(plotlist = plots, labels = LETTERS[1:length(plots)])
dev.off()


## Data analysis ----

## Summary statistics
gf$up.lim
119 - 92
92 - 99.5
99.5 - 68.5
68.5 - 94
94 - 66.4
66.4 - 53.6
53.6 - 70
mean(c(27, -7.5, 31, -25.5, 27.6, 12.8, -16.4)) / 2


gf$q.25
144.5 - 137.27
137.27 - 132.75
132.75 - 130.50
130.50 - 132.25
132.25 - 127.4
127.4 - 126.75
126.75 - 116.5
mean(c(7.23, 4.52, 2.25, -1.75, 4.85, 0.65, 10.25)) / 2

gf$low.lim
gf$mean.depth
gf$median.depth

## All trawls vs urchin trawls ----
# Can differences in depth distribution parameters be attributed to changes in depth of all trawls?

# Are there differences in depth distribution parameters between S. fragilis and all trawls?
lm1 <- lm(up.lim ~ Time * Treatment, data = trawl_depth_dist)
summary(lm1)  # Yes!

lm2 <- lm(q.25 ~ Time * Treatment, data = trawl_depth_dist)
summary(lm2)  # Yes!

lm3 <- lm(q.75 ~ Time * Treatment, data = trawl_depth_dist)
summary(lm3)  # No

lm4 <- lm(low.lim ~ Time * Treatment, data = trawl_depth_dist)
summary(lm4)  # Yes, Omnibus F-test indicates differences, but none of the explanatory variables are significant (better than intercept-only model)

lm5 <- lm(mean.depth ~ Time * Treatment, data = trawl_depth_dist)
summary(lm5)  # No

lm6 <- lm(median.depth ~ Time * Treatment, data = trawl_depth_dist)
summary(lm6)  # Yes, Omnibus F-test indicates differences, but none of the explanatory variables are significant (better than intercept-only model)


# Figure S5. REsidual and QQ plots for linear models comparing all trawls with urchin trawls
# Create a function to make the residuals plots
resid_plot <- function (model) {
  ggplot(model, aes(x = .fitted, y = .resid)) + 
    geom_point() + 
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey30") + 
    xlab("Fitted Values") + ylab("Residuals")
}
# Create a function to make the qq plots
quantile_plot <- function (model) {
  ggplot(model, aes(qqnorm(.stdresid)[[1]], .stdresid)) + 
    geom_point() + geom_abline() +
    xlab("Theoretical Quantiles") + ylab("Std. Residuals")
}

# Write a function to loop over all the variables and create a list of Time vs variable plots
models = list(lm1, lm2, lm3, lm4, lm5, lm6)
# Create the residuals plots
resid_plots <- map(models, function (model) {
  resid_plot(model = model)
})
# Create the q-q plots
quantile_plots <- map(models, function (model) {
  quantile_plot(model = model)
})

# Create an empty list
list_of_residual_plots <- list()
# Create an index
index = 1:6

for (i in index) {
  list_of_residual_plots[[i]] <- cowplot::plot_grid(plot_list = resid_plots[[i]], quantile_plots[[i]])
}

# Save the plot
tiff("figures/figureS5-trawldata-lm-residuals.tiff", units = "in", width = 8, height = 15, res = 300)
cowplot::plot_grid(plotlist = list_of_residual_plots, labels = LETTERS[1:length(list_of_residual_plots)], ncol = 1)
dev.off()


## Komolgorov-Smirnov test ----
ks.test(x = trawl_depth_dist$up.lim[trawl_depth_dist$Treatment == "S. fragilis only"], 
        y = trawl_depth_dist$up.lim[trawl_depth_dist$Treatment == "All trawls"])

ks.test(x = trawl_depth_dist$q.25[trawl_depth_dist$Treatment == "S. fragilis only"], 
        y = trawl_depth_dist$q.25[trawl_depth_dist$Treatment == "All trawls"])

ks.test(x = trawl_depth_dist$q.75[trawl_depth_dist$Treatment == "S. fragilis only"], 
        y = trawl_depth_dist$q.75[trawl_depth_dist$Treatment == "All trawls"])

ks.test(x = trawl_depth_dist$low.lim[trawl_depth_dist$Treatment == "S. fragilis only"], 
        y = trawl_depth_dist$low.lim[trawl_depth_dist$Treatment == "All trawls"])

ks.test(x = trawl_depth_dist$mean.depth[trawl_depth_dist$Treatment == "S. fragilis only"], 
        y = trawl_depth_dist$mean.depth[trawl_depth_dist$Treatment == "All trawls"])

ks.test(x = trawl_depth_dist$median.depth[trawl_depth_dist$Treatment == "S. fragilis only"], 
        y = trawl_depth_dist$median.depth[trawl_depth_dist$Treatment == "All trawls"])


## Are there differences in density over time?
anova.data <- urchin_trawls %>% 
  dplyr::filter(!is.na(Bottom.depth..m.),
                !is.na(Catch.count..pieces.)) %>% 
  mutate(trawled_area = Distance.towed..m. * Trawl.door.spread..m.,
         urchin_Density = Catch.count..pieces. / trawled_area)

# Kruskal-Wallis test for density over time
kw_density <- kruskal.test(urchin_Density ~ factor(Time), data = anova.data)
kw_test <- data.frame("")
kable(kw_density)
kw_mc <- PMCMRplus::kwAllPairsDunnTest(urchin_Density ~ factor(Time), data = anova.data, p.adjust.method = "bonferroni")
# Yes, there were statistically significant observed changes in density over time

# Create a data frame of all possible combinations of years
times = expand.grid("x" = seq(2004, 2018, by = 2), "y" = seq(2004, 2018, by = 2))
# Include only unique combinations (remove equal years and duplicates)
times = subset(times, x != y & x < y)
# Rearrange the dataframe to be in descending order
times = arrange(times, desc(times))

# Create a new variable
kw_times <- times
# Create a column with year comparison labels
kw_times$Comparison = paste(kw_times$x, "vs", kw_times$y)
# Create a column with p.values from the kwAllPairsDunnTest() function, reversing the order to align with the times data frame
kw_times$p.value = c(rev(na.omit(kw_mc$p.value[,7])), 
                     rev(na.omit(kw_mc$p.value[,6])), 
                     rev(na.omit(kw_mc$p.value[,5])), 
                     rev(na.omit(kw_mc$p.value[,4])), 
                     rev(na.omit(kw_mc$p.value[,3])), 
                     rev(na.omit(kw_mc$p.value[,2])), 
                     rev(na.omit(kw_mc$p.value[,1])))
# Remove the extra columns
kw_times <- kw_times %>% dplyr::select(-x, -y)

# Make the table
kable(kw_times, booktabs = TRUE, digits = 3,
      caption = "Dunn's post-hoc multiple comparisons with Bonferroni correction for *S. fragilis* density over time from the West Coast Vancouver Island synoptic trawl survey") %>% 
  kable_styling(full_width = FALSE) %>% 
  row_spec(bold = TRUE, row = c(7, 8, 9, 11, 12, 13))


# Pairwise K-S test for differences in depth distribution between years
# Komolgorov-Smirnov test
ks_func <- function (data, time.1, time.2) {
  tmp_dat_x <- subset(data["Bottom.depth..m."], data["Time"] == time.1)
  tmp_dat_y <- subset(data["Bottom.depth..m."], data["Time"] == time.2)
  ks_depth <- ks.test(x = tmp_dat_x[,1],
                      y = tmp_dat_y[,1])
  ks_summary <- data.frame("Comparison" = paste(time.1, "vs", time.2),
                           "Test statistic" = ks_depth$statistic, 
                           "P-value" = ks_depth$p.value)
}
# Create an empty data frame
ttt <- data.frame()
# Loop over each row (time comparison)
for (i in 1:nrow(times)) {
  # Get the test statistic and p-value
  temp <- ks_func(anova.data, time.1 = times[i, 1], time.2 = times[i, 2])
  # Add it to the empty dataframe
  ttt[i, 1:3] <- temp
}
# Remove row names
rownames(ttt) = NULL

# Makde the table
kable(ttt, digits = 3, booktabs = TRUE,
      caption = "Komolgorov-Smirnov output for depth distribution of S. fragilis") %>% 
  row_spec(bold = TRUE, row = c(22, 23, 24, 26))

post_hoc_trawl <- full_join(kw_times, ttt) %>% 
  rename("Dunn's P-value" = "p.value",
         "K-S Test statistic" = "Test.statistic",
         "K-S P-value" = "P.value") %>% 
  mutate(`Dunn's P-value` = if_else(`Dunn's P-value` < 0.001, "<0.001", as.character(round(`Dunn's P-value`, 3))),
         `K-S P-value` = if_else(`K-S P-value` < 0.001, "<0.001", as.character(round(`K-S P-value`, 3)))
  )

saveRDS(post_hoc_trawl, "data/export-for-thesis/table2-6_post-hoc-trawl.rds")
kable(post_hoc_trawl, digits = 3, booktabs = TRUE,
      caption = "Dunn’s post-hoc multiple comparisons with Bonferroni correction for S. fragilis density over time from the West Coast Vancouver Island synoptic bottom trawl surveys. Komolgorov- Smirnov (K-S) comparison of S. fragilis depth distributions for each pair of survey years. Bold rows indicate statistical significance at the α = 0.05 level.") %>% 
  row_spec(bold = TRUE, row = c(22, 23, 24, 26))
post_hoc_trawl


# Trawl maps ----
library(terra)
library(basemaps)
rsts <- rast("data/rasters/GEBCO_04_Sep_2022_e718d7da94cd/gebco_2022_n50.9745_s47.0023_w-128.9853_e-123.982.tif")

plot(rsts)

bmap <- basemap(rsts, map_service = "carto", map_type = "light_no_labels")
basemap2 = as(bmap, "Raster")

bm_df <- as.data.frame(basemap2, xy = TRUE)


rast_df <- as.data.frame(rasts, xy = TRUE)

utm_coords <- sp::SpatialPoints(cbind(bm_df$x, bm_df$y), proj4string = sp::CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0 +lon_0=0 +x_0=0 +y_0=0 +k=1 +units=m +nadgrids=@null +wktext +no_defs"))
latlon_coords <- sp::spTransform(utm_coords, sp::CRS("+proj=longlat"))
bm_df_ll <- cbind(bm_df, latlon_coords)

rast_df2 <- 
  rast_df %>% 
  rename("gebco_dem" = "gebco_2022_n50.9745_s47.0023_w-128.9853_e-123.982") %>% 
  mutate(gebco_dem2 = if_else(gebco_dem > 0, NA_integer_, gebco_dem),
         gebco_dem3 = gebco_dem2 * -1)

ggplot() +
  geom_raster(data = bm_df_ll, aes(x = coords.x1, y = coords.x2)) +
  geom_raster(data = rast_df2, aes(x = x, y = y, fill = gebco_dem3)) +
  #geom_contour(data = rast_df2, aes(x = x, y = y, z = gebco_dem3), 
  #             colour = "grey30",
  #             breaks = c(200, 400, 1000, 2000, 3000),
  #             ) +
  metR::geom_contour2(data = rast_df2, 
                      aes(x = x, y = y, z = gebco_dem3, label = stat(level)),
                      colour = "grey30",
                      breaks = c(400, 1000, 2000, 3000),
                      size = 0.4) +
  geom_point(data = all.data, aes(x = Start.longitude,
                                  y = Start.latitude), size = 0.5) +
  geom_point(data = urchin_trawls, aes(x = Start.longitude,
                                       y = Start.latitude), colour = "red", size = 0.5) +
  #geom_segment(data = all.data,
  #             aes(x = Start.longitude, xend = End.longitude, 
  #                 y = Start.latitude, yend = End.latitude)) +
  #geom_segment(data = urchin_trawls, 
  #             aes(x = Start.longitude, xend = End.longitude, 
  #                 y = Start.latitude, yend = End.latitude), 
  #             colour = "red") + 
  geom_point(aes(x = -126.174724, y = 48.427027), size = 3, colour = "Yellow") +
  scale_fill_distiller(type = "seq", palette = "Blues", direction = 1) +
  scale_x_continuous(limits = c(-129, -124.5), expand = c(0, 0, 0, 0), breaks = c(-129, -128, -127, -126, -125), labels = c("129°W", "128°W", "127°W", "126°W", "125°W")) +
  scale_y_continuous(limits = c(48, 50.75), expand = c(0, 0, 0, 0), breaks = c(48, 48.5, 49, 49.5, 50, 50.5), labels = c("48°N", "48° 30'N", "49°N", "49° 30'N", "50°N", "50° 30'N")) +
  theme_bw() +
  theme(
    panel.border = element_rect(colour = "black", size = 2),
    axis.title = element_blank(),
    legend.position = c(0.85, 0.8),
    legend.direction = "horizontal"
    )

