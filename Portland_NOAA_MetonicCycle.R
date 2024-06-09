# Project: Atlantic Coast Joint Venture
# Sub-Project: Geospatial Analysis of the Efficacy of Runnels
# Author: Grant McKown (james.mckown@unh.edu)

#Purpose: Calculate and graph the 18.6 year metonic cycle from the Portland, Maine NOAA tidal gauge.
        # The metonic cycle is a decadal tidal cycle that can differ the mean high water tidal range by over 10 inches
        # and severely impact the vegetation community with sea-level rise. The Portland NOAA tidal guage has a 100 year 
        # monthly tidal range data set and is representative of New England conditions. 

# Set up the code with the appropriate packages

#Library & Packages
rm(list = ls())

#Stats & Data Organization Packages
library(tidyr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(scales)


# Read in the raw data of the Portland NOAA Tidal Gauge (January 1920 - June 2023)
  # Tidal data was imported into a CSV from the NOAA data inventory on June 14th, 2023 by author
  # Data is in Station Datum

Tides <- read.csv(file = "Raw_Data/Portland_NOAA_TidalGauge_Monthly_Metric2.csv")

glimpse(Tides)


# Data preparation
  # First, we will rename some of the columns for easier understanding and tidiness of the data set
  # Second, we will calculate the difference in Mean High Water and Mean Low Water, which will better illustrate
    # the 18.6 year metonic cycle. 

Tides <- Tides %>%
  rename(MHW = MHW..m.,
         MLW = MLW..m.,
         MSL = MSL..m.) %>%
  mutate(Tides_Diff = MHW - MLW,
         Date = lubridate::mdy(Date))

glimpse(Tides)


#Graph the metonic cycle using a smooth line graph for three series over time:
  # (1) Mean Sea Level 
  # (2) Trend of Mean Sea Level
  # (3) Difference in Mean High Water and Mean Low Water (18.6 year Metonic Cycle)


Portland_Tidal_Composite <- ggplot() +
  geom_point(data = Tides,
             aes(x = Date, y = MSL),
             size = 2, shape = 19, colour = "darkorange") + 
  geom_smooth(data = Tides,
              aes(x = Date, y = MSL),
              method = lm, size = 1, colour = "black", linetype = "dashed") + 
  geom_point(data = Tides,
             aes(x = Date, y = Tides_Diff),
             size = 2, shape = 19, colour = "darkblue") + 
  labs(x = "",
       y = "") + 
  scale_x_date(labels = date_format("%Y"), 
               limits = as.Date(c("1950-01-01", "2025-01-01")),
               breaks = ("10 years"),
               expand = c(0,0)) + 
  scale_y_continuous(limits = c(-0.4, 3.0), 
                     breaks = seq(-0.4, 3.0, 0.2)) + 
  theme_bw() +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    strip.text.x = element_text(size = 20, colour = "black"),
    strip.background = element_blank(),
    axis.title = element_text(size = 20, colour = "black"),
    axis.text = element_text(size = 15, colour = "black"))


Portland_Tidal_Composite


ggsave(Portland_Tidal_Composite, limitsize = FALSE, 
       dpi = 300, height = 7.5, width = 12.5,
        filename = "Processed_Figures/Portland_NOAA_MetonicCycle2.jpg")
