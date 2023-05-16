#Project: Atlantic Coast Joint Venture
#Script: Geospatial Post-Analysis of Salt Marsh Health (UVVR & Vegetated Area)

#Author: Grant McKown, Research Assistant, (jgrantmck@gmail.com)

#Created: December 2022
#Last Updated: March 2023


#Code Organization:

#Book 1: Setting up the R code and Datasets

# Chapter 1: Setting Up Code Environment

# Chapter 2: Preparing the Database and Timeline Datasets before Analysis
# Page 1: Reading in the overarching UVVR - Veg Area Database
# Page 2: Separating the Database into separate UVVR & Veg Area Databases, Subsetting Moody Marsh
# Page 3: Calculate Timeline for the Image Capture Dates Respective of Runnel Construction
# Page 4: Calculate the Percent Vegetated Area Compared to Earliest Time 
# Page 5: Combine the UVVR and Veg Database & Export
# Page 6: Create the UVVR and Vegetated Area Timelines


#Book 2: UVVR Analysis

# Chapter 1: UVVR - Piecewise Linear Mixed Model
# Page 1: Calculate the Piecewise Mixed Linear Regression for Pre- and Post-Restoration
# Page 2: Combine & Export the results of the Pre- Post-restoration mixed models
# Page 3: Predict the means and Confidence Interval with ggpredict() function
# Page 4: Calculate slopes of pre- and post-restoration models
# Page 5: Plot Piecewise Linear Mixed Model


#Chapter 2: UVVR - Linear Mixed Model
# Page 1: Calculate the Linear Mixed Model & Summaries
# Page 2: Export the Linear Mixed Model Summaries
# Page 3: Predict the Linear Mixed Model Means and Confidence Intervals with ggpredict()
# Page 4: Calculate the Slopes of each Treatment
# Page 5: Plot the Linear Mixed Model

# Chapter 3: UVVR - Spline Mixed Model
# Page 1: Calculate the Spline Mixed Model & Summaries
# Page 2: Export the Spline Mixed Model Summaries
# Page 3: Predict the Spline Mixed Model Means and Confidence Intervals with ggpredict()
# Page 4: Calculate the Slopes of each Treatment
# Page 5: Plot the Spline Mixed Model

#Chapter 4: UVVR - Comparison of Linear and Spline Regressions, ANCOVA post-restoration
#Page 1: Model Validation with Lilkliehood Ratio Test between Linear and Spline Mixed Models
#Page 2: One-way ANOVA to verify if Runnel restoration were applied to most degraded tidesheds


#Book 3: Vegetated Area Analysis

# Chapter 1: Vegetated Area - Piecewise Linear Mixed Model
# Page 1: Calculate the Piecewise Mixed Linear Regression for Pre- and Post-Restoration
# Page 2: Combine & Export the results of the Pre- Post-restoration mixed models
# Page 3: Predict the means and Confidence Interval with ggpredict() function
# Page 4: Calculate slopes of pre- and post-restoration models
# Page 5: Plot Piecewise Linear Mixed Model

#Chapter 2: Vegetated Area - Linear Mixed Model
# Page 1: Calculate the Linear Mixed Model & Summaries
# Page 2: Export the Linear Mixed Model Summaries
# Page 3: Predict the Linear Mixed Model Means and Confidence Intervals with ggpredict()
# Page 4: Calculate the Slopes of each Treatment
# Page 5: Plot the Linear Mixed Model

# Chapter 3: Vegetated Area - Spline Mixed Model
# Page 1: Calculate the Spline Mixed Model & Summaries
# Page 2: Export the Spline Mixed Model Summaries
# Page 3: Predict the Spline Mixed Model Means and Confidence Intervals with ggpredict()
# Page 4: Calculate the Slopes of each Treatment
# Page 5: Plot the Spline Mixed Model

#Chapter 4: Vegetated Area - Comparison of Linear and Spline Regressions, ANCOVA post-restoration
#Page 1: Model Validation with Lilkliehood Ratio Test between Linear and Spline Mixed Models
#Page 2: One-way ANOVA to verify if Runnel restoration were applied to most degraded tidesheds

# Chapter 5: Vegetated Area - Determine Full Vegetated Marsh Loss/Gains Since 2010
# Page 1: Calculate the Percent Change Pre- and Post-Restoration in Vegetated Area
# Page 2: Calculate the Actual Vegetated Area Loss
# Page 3: Set up and Populate Vegetated Area Loss Data Frame



#_____________________________________________________________________________________________________________

# CHAPTER 1: SETTING UP THE R-SCRIPT CODE

#Library & Packages
rm(list = ls())

#Stats & Data Organization Packages
library(tidyr)
library(dplyr)
library(broom)
library(purrr)
library(broom.mixed)
library(modelr)

#Data Visualization Packages
library(patchwork)
library(gridExtra)
library(drc)
library(ggfortify)
library(ggplot2)
library(viridis)
library(mgcv)
library(ggformula)
library(wesanderson)

#Data Analysis Packages
library(lme4)
library(rstatix)
library(splines2)
library(splines)
library(afex)
library(ggeffects)

set.seed(1009)

setwd("E:/Coastal Habitat Restoration Team/ACJV Sites - RI_Mass_Maine/Data Analysis/ACJV_UVVR_NewEngland")









#---------------------------------------------------------------------------------------------------------------

# CHAPTER 2: STTING UP UVVR AND VEG AREA DATA FRAMES


# Page 1: Reading in the overarching UVVR - Veg Area Database
Database <- read.csv("Raw_Data\\ACJV_UVVR_AllTidesheds_Finalized.csv")

glimpse(Database)


# Page 2: Separating the Database into separate UVVR & Veg Area Databases, Subsetting Moody Marsh
#All of the NAIP Imagery for Mass, RI were taken on 2010, 2012, 2014, 2016, 2018, and 2021 timelines
#However, the NAIP imagery of ME were taken on 2011, 2013, 2015, 2018, and 2021.
#The Moody Marsh site in ME needs to be subsetted from the Mass, RI sites to create the timeline


# Step 1: Subsetting the Databases
Database.UVVR <- Database[ , c(1:14)]

glimpse(Database.UVVR)

Database.Veg <- Database [ , c(1:8, 15:20)]

glimpse(Database.Veg)

# Step 2: Re-organize each database using the Gather Function
# To perform the analysis, the multitude of UVVRXX Columns need to be reorganized into individual row entries

Database.UVVR <- gather(Database.UVVR, key = "Image_Date", value = "UVVR", 
                        UVVR10, UVVR12, UVVR14, UVVR16, UVVR18, UVVR21)


Database.Veg <- gather(Database.Veg, key = "Image_Date", value = "Veg_Area", 
                       Veg10, Veg12, Veg14, Veg16, Veg18, Veg21)

# Step 3: Subset Moody Marsh Data from the UVVR and Veg Area Databases 
# NAIP collection dates are unique in Maine compared to Rhode Island and Massachusetts
# 2010 = Na (Maine), 2012 = 2011 (Maine), 2014 = 2013 (Maine), and 2016 = 2015 (Maine), 2021 = NA (Maine)

Moody.UVVR <- filter(Database.UVVR, Site == "Moody Marsh", Image_Date != "UVVR10" & Image_Date != "UVVR21")

Database.UVVR <- filter(Database.UVVR, Site != "Moody Marsh")


Moody.Veg <- filter(Database.Veg, Site == "Moody Marsh", Image_Date != "Veg10" & Image_Date != "Veg21")

Database.Veg <- filter(Database.Veg, Site != "Moody Marsh", Image_Date != "UVVR10" & Image_Date != "UVVR21")

# Step 4: Recalculate the new Year Columns into the Years (aka UVVR10 --> 2010)
# To perform the future analysis, we need to convert the UVVRXX into proper years

#UVVR Database
Database.UVVR <- Database.UVVR %>%
  mutate(Image_Date = recode(Image_Date,
                             "UVVR10" = 2010,
                             "UVVR12" = 2012,
                             "UVVR14" = 2014,
                             "UVVR16" = 2016,
                             "UVVR18" = 2018,
                             "UVVR21" = 2021))

Database.UVVR$Image_Date <- as.integer(Database.UVVR$Image_Date)

#UVVR Moody Marsh Database (Please note different years due to unique NAIP collection years in Maine)
Moody.UVVR <- Moody.UVVR %>%
  mutate(Image_Date = recode(Image_Date,
                             "UVVR12" = 2011,
                             "UVVR14" = 2013,
                             "UVVR16" = 2015,
                             "UVVR18" = 2018))

Moody.UVVR$Image_Date <- as.integer(Moody.UVVR$Image_Date)

#Vegetated Area Database
Database.Veg <- Database.Veg %>%
  mutate(Image_Date = recode(Image_Date,
                             "Veg10" = 2010,
                             "Veg12" = 2012,
                             "Veg14" = 2014,
                             "Veg16" = 2016,
                             "Veg18" = 2018,
                             "Veg21" = 2021))

Database.Veg$Image_Date <- as.integer(Database.Veg$Image_Date)

#Veg Area Moody Marsh Database (Please note different years due to unique NAIP collection years in Maine)
Moody.Veg <- Moody.Veg %>%
  mutate(Image_Date = recode(Image_Date,
                             "Veg12" = 2011,
                             "Veg14" = 2013,
                             "Veg16" = 2015,
                             "Veg18" = 2018))


Moody.Veg$Image_Date <- as.integer(Moody.Veg$Image_Date)




# Page 3: Calculate Timeline for the Image Capture Dates Respective of Runnel Construction
#Timeline for Pre-Restoration = (-10 - 0 yrs); Post-Restoration = (1 - 10 yrs)

# Step 1: Calculate the Timeline for Image Capture Dates
# Mutate function to easily accomplish this: Age = (Age in 2021 Column) - (2021 - Image_Date Column)

Database.UVVR <- Database.UVVR %>%
  mutate(Timeline = (Age_2021) - (2021 - Image_Date))

glimpse(Database.UVVR)

Moody.UVVR <- Moody.UVVR %>%
  mutate(Timeline = (Age_2021) - (2021 - Image_Date))

glimpse(Moody.UVVR)

Database.Veg <- Database.Veg %>%
  mutate(Timeline = (Age_2021) - (2021 - Image_Date))

glimpse(Database.Veg)

Moody.Veg <- Moody.Veg %>% 
  mutate(Timeline = (Age_2021) - (2021 - Image_Date))

glimpse(Moody.Veg)

#Step 2: Append the Moody Marsh Data frames into UVVR and Veg Data frames

Database.UVVR <- bind_rows(Moody.UVVR, Database.UVVR)

glimpse(Database.UVVR)

Database.Veg <- bind_rows(Database.Veg, Moody.Veg)

glimpse(Database.Veg)

#Step 3: Removal of the Potters Pond 2010 classification from the dataset
#The image classification of Potters Pond in 2010 is iffy at best and should be removed

row.PP.10 <- which(Database.UVVR$Site == "Potters Pond" & Database.UVVR$Image_Date == 2010)

Database.UVVR <- slice(Database.UVVR, -row.PP.10)

row.PP.10 <- which(Database.Veg$Site == "Potters Pond" & Database.Veg$Image_Date == 2010)

Database.Veg <- slice(Database.Veg, -row.PP.10)

#Step 4: Removal of all Timeline values less than -10 & the Random Dataset
#The analysis is only concerned for monitoring from -10 yrs --> +8 yrs in relation to restoration
#Since the Random tidesheds are not the focus of the study, they are removed

Database.UVVR <- Database.UVVR %>%
  filter(Timeline >= -10, Treatment != "Random")

Database.Veg <- Database.Veg %>%
  filter(Timeline >= - 10, Treatment != "Random")

#Step 5: Remove Moody Marsh Data frames to clean up the environment

rm(Moody.UVVR, Moody.Veg, row.PP.10)



#Page 4: Calculate the Percent Vegetated Area Compared to Earliest Time
  #The code determines the Vegetated Area at the earliest time for each tideshed in pre-restoration
  #The code calculates the Change in Percent Vegetated Area based on the size of the tideshed
  #Lastly, the code removes some pesky NAs from the database

Database.Veg <- Database.Veg %>%
  group_by(Site, Tideshed) %>%
  mutate(Veg_Earliest = Veg_Area[which.min(Timeline)],
         
         Veg_Percent = (Veg_Area / Hectares) * 100,
         
         Veg_PChange = ifelse(Timeline == min(Timeline), NA,
                              ((Veg_Area - Veg_Earliest)/ Hectares) * 100),
         
         Veg_PChange = round(Veg_PChange, 2)) %>%
  filter(is.na(Timeline) == FALSE) %>%
  ungroup() 

  glimpse(Database.Veg)  



Database.UVVR <- Database.UVVR %>%
  group_by(Site, Tideshed) %>%
  mutate(UVVR_Earliest = UVVR[which.min(Timeline)]) %>%
  ungroup()

glimpse(Database.UVVR)

#Page 5: Combine the UVVR and Veg Database & Export
#Instead of having two separate UVVR and Veg databases, we can create one large database
#We then can remove all of the duplicate tideshed characteristic columns

#First, arrange both databases by Site, Tideshed, and Timeline to make sure all rows are the same
Database.Veg <- arrange(Database.Veg, Site, Tideshed, Timeline)

Database.UVVR <- arrange(Database.UVVR, Site, Tideshed, Timeline)

#Bind the two databases together with cbind()
Database <- cbind(Database.UVVR, Database.Veg)

#Determine and remove all duplicate characteristic columns
duplicates <- duplicated(colnames(Database))

Database <- Database[!duplicates]

glimpse(Database)

# Create a unique ID for each individual Tideshed throughout the study
#Aggregate South & North Sites into Full Sites

Database <- Database %>%
  group_by(Site, Tideshed) %>%
  mutate(Tideshed_ID = as.factor(cur_group_id()),
         
         Site = ifelse(Site == "Sapowet - South", "Sapowet", Site),
         Site = ifelse(Site == "Sapowet - North", "Sapowet", Site),
         
         Site = ifelse(Site == "Plum Island - South", "Plum Island", Site),
         Site = ifelse(Site == "Plum Island - North", "Plum Island", Site)) %>%
  ungroup()


# Lastly, I want to categorize the No Action Control and Runnel Tidesheds as 'Health' or 'Degraded'
  #based on the UVVR Score right immediately prior to restoration (-2 - 0 years)
  #Degraded tidesheds are considered with UVVR scores > 0.13
  #All reference tidesheds are considered 'Healthy' since very few have UVVR scores > 0.13


Database <- Database %>%
  group_by(Tideshed_ID) %>%
    mutate(Timeline.Baseline = max(Timeline[Timeline <= 0]),
           
           UVVR.Baseline = UVVR[which(Timeline == Timeline.Baseline)],
      
           BaselineCondition = ifelse(Treatment == "Reference", "Healthy", 
                                      ifelse(UVVR.Baseline >= 0.13, "Degraded", "Healthy"))) %>%
ungroup()


#Export the new combined Database
#We will keep the Database.Veg and Database.UVVR for later analyses for simplicity

Database <- filter(Database, Treatment != "Ditch Remediation")

write.csv(Database, "E:\\Coastal Habitat Restoration Team\\ACJV Sites - RI_Mass_Maine\\Data Analysis\\Output R CSVs Analysis\\ACJV_Overall_Database.csv")

#Remove the UVVR & Veg Database data frame, since we no longer have any use for it
rm(Database.UVVR, Database.Veg)



#Page 6: Create the UVVR and Vegetated Area Timelines

#Step 1: Create the Timelines with dplyr, summarise(), and mutate() functions
#The code calculates the average and standard error UVVR score  & Percent Vegetated Area
# for each timeline of the tidesheds within each treatment
#The code also rounds the Veg_percent to 2 decimal points for easy reading

Timeline.Metrics <- Database %>%
  group_by(Timeline, Treatment) %>%
  summarise(
    UVVR.avg = mean(UVVR, na.rm = TRUE),
    UVVR.se = sd(UVVR, na.rm = TRUE)/sqrt(n()),
    
    Veg_Percent.avg = mean(Veg_Percent, na.rm = TRUE),
    Veg_Percent.se = sd(Veg_Percent, na.rm = TRUE)/sqrt(n()),
    
    Veg_PChange.avg = mean(Veg_PChange, na.rm = TRUE),
    Veg_PChange.se = sd(Veg_PChange, na.rm = TRUE)/sqrt(n()),
    
    
    Count = n() )  %>%
  mutate(
    Veg_Percent.avg = round(Veg_Percent.avg, 2),
    Veg_Percent.se = round(Veg_Percent.se, 2),
    
    Veg_PChange.avg = round(Veg_PChange.avg, 2),
    Veg_PChange.se = round(Veg_PChange.se, 2)) %>%
  ungroup()


glimpse(Timeline.Metrics)



#Export the Timeline Database

write.csv(Timeline.Metrics, "E:\\Coastal Habitat Restoration Team\\ACJV Sites - RI_Mass_Maine\\Data Analysis\\Output R CSVs Analysis\\ACJV_Overall_Timeline.csv")



#Step 2: Create the Timelines based on Baseline Condition with dplyr, summarise(), and mutate() functions
#The code calculates the average and standard error UVVR score  & Percent Vegetated Area
# for each timeline of the tidesheds within each treatment
#The code also rounds the Veg_percent to 2 decimal points for easy reading

Timeline.Health <- Database %>%
  group_by(Timeline, Treatment, BaselineCondition) %>%
  summarise(
    UVVR.avg = mean(UVVR, na.rm = TRUE),
    UVVR.se = sd(UVVR, na.rm = TRUE)/sqrt(n()),
    
    Veg_Percent.avg = mean(Veg_Percent, na.rm = TRUE),
    Veg_Percent.se = sd(Veg_Percent, na.rm = TRUE)/sqrt(n()),
    
    Veg_PChange.avg = mean(Veg_PChange, na.rm = TRUE),
    Veg_PChange.se = sd(Veg_PChange, na.rm = TRUE)/sqrt(n()),
    
    
    Count = n() )  %>%
  mutate(
    Veg_Percent.avg = round(Veg_Percent.avg, 2),
    Veg_Percent.se = round(Veg_Percent.se, 2),
    
    Veg_PChange.avg = round(Veg_PChange.avg, 2),
    Veg_PChange.se = round(Veg_PChange.se, 2)) %>%
  ungroup()


glimpse(Timeline.Health)

#Export the Timeline Database

write.csv(Timeline.Health, "E:\\Coastal Habitat Restoration Team\\ACJV Sites - RI_Mass_Maine\\Data Analysis\\Output R CSVs Analysis\\ACJV_Health_Timeline.csv")




Database.stats <- Database %>%
  group_by(Tideshed_ID) %>%
  filter(Timeline == min(Timeline)) %>%
  ungroup() %>%
  group_by(Site) %>%
   summarise(Area = sum(Hectares),
            Avg = mean(Hectares),
            SE = sd(Hectares)/sqrt(n()),
            n = n())


rm(Database.stats)


#_________________________________________________________________________________________________________

#Book 2: UVVR MIXED MODEL ANALYSIS

#___________________________________________________________________________________________________________



#CHAPTER 1 - MIXED MODEL LINEAR REGRESSIONS OF UVVR

#Page 1 - Create Full Linear Model and Summary Data frames
#Mixed model is created with the lmer() function and the outputs of the mixed model are recorded with
#various broom.mixed package functions. Broom.mixed converts complex lme4 outputs to tables
#For the mixed model, UVVR is the response variable, Timeline and Treatment are fixed effects,
#and Site and Tideshed_ID are random effects. 
#Note: Individual tidesheds are uniquely ID'ed, rendering a nesting structure for the data obsolete


UVVR.Linear <- lmer(UVVR ~ Timeline * Treatment + (1|Site) + (1|Tideshed_ID), 
                    data = Database, na.action = na.omit)

UVVR.Linear.tidy <- broom.mixed::tidy(UVVR.Linear)

UVVR.Linear.tidy

UVVR.Linear.anova <- as.data.frame(anova(UVVR.Linear)) %>%
  mutate(coeff = row.names(.))

UVVR.Linear.anova

UVVR.Linear.glance <- broom.mixed::glance(UVVR.Linear)

UVVR.Linear.glance

#Step 2 - Check the assumptions of homogeneity and normal distribution with visual inspection of residuals

#Assumption 1 - Linearity
#Plot residuals of the model and observed values

plot(resid(UVVR.Linear), Database$UVVR)


#Homogeneity of Variance

plot(UVVR.Linear)

residuals(UVVR.Linear)

qqnorm(residuals(UVVR.Linear))


#Page 2 - Export the Full Linear Model data frames

write.csv(UVVR.Linear.glance, "E:\\Coastal Habitat Restoration Team\\ACJV Sites - RI_Mass_Maine\\Data Analysis\\Output R CSVs Analysis\\UVVR_Linear_glance.csv")

write.csv(UVVR.Linear.tidy, "E:\\Coastal Habitat Restoration Team\\ACJV Sites - RI_Mass_Maine\\Data Analysis\\Output R CSVs Analysis\\UVVR_Linear_tidy.csv")

write.csv(UVVR.Linear.anova, "E:\\Coastal Habitat Restoration Team\\ACJV Sites - RI_Mass_Maine\\Data Analysis\\Output R CSVs Analysis\\UVVR_Linear_anova.csv")

rm(UVVR.Linear.glance, UVVR.Linear.tidy, UVVR.Linear.anova)



#Page 3 - Predict the Linear Model for each Treatment

#Using the ggpredict() function, the mean value and confidence interval can be calculated for each
#treatment at each timeline point. ggpredict() function is a powerful tool!
#For visualization purposes, the confidence interval was calculated for the variance of the fixed effects,
#while keeping the random effects constant. Essentially, the confidence interval becomes incredibly large
#and not useful for visualization purposes
#Lastly, the columns are renamed for easy ggploting. For UVVR, the lower confidence interval is set to zero
#if negative, since you can not have a UVVR score of zero. 


Linear.preds <- ggpredict(UVVR.Linear, c("Timeline", "Treatment"), 
                          type = "fixed", interval = 'confidence') %>%
  rename(Timeline = x,
         Treatment = group,
         UVVR = predicted) %>%
  mutate(conf.low = ifelse(conf.low < 0, 0, conf.low)) %>%
  arrange(Treatment)



#Page 4 - Calculate the Slope of Linear Mixed Model with predicted values
#Using the predicted mean values, the slopes of each treatment can be calculated

Linear.Slope <- Linear.preds %>%
  group_by(Treatment) %>%
  summarise(slope = (UVVR[which.max(Timeline)] - UVVR[which.min(Timeline)]) / (max(Timeline) - min(Timeline)) ) %>%
  ungroup()

write.csv(Linear.Slope, "E:\\Coastal Habitat Restoration Team\\ACJV Sites - RI_Mass_Maine\\Data Analysis\\Output R CSVs Analysis\\UVVR_Linear_Slopes.csv")



#Page 5 - Graph the predicted linear mixed model for each treatment via the facet wrap function
#There are two data sets being graphed: Predicted values of mixed model and the mean values (with error bars) 
#The treatments are graphed in the following order: No Action, Reference, Ditch, and Runnel 
#These corresponds with basic R colors that correspond well with each treatment


Linear.preds$Treatment <- factor(Linear.preds$Treatment,
                                 levels = c("No Action", "Reference", "Runnel"))

Linear.Graph.UVVR <- ggplot() +   
  geom_vline(xintercept = 0, size = 1, colour = "grey", 
             linetype = "dashed") + 
  geom_line(data = Linear.preds,
            aes(x = Timeline, y = UVVR, colour = Treatment),
            size = 1.5) +
  geom_ribbon(data = Linear.preds,
              aes(x = Timeline, ymin = conf.low, ymax = conf.high, fill = Treatment),
              alpha = 0.25) +
  geom_errorbar(data = Timeline.Metrics,
                aes(x = Timeline, ymin = UVVR.avg - UVVR.se, ymax = UVVR.avg + UVVR.se,
                    colour = Treatment),
                size = 1, width = 0.5) +
  geom_point(data = Timeline.Metrics,
             aes(x = Timeline, y = UVVR.avg, fill = Treatment), 
             shape = 21, size = 5) +
  labs(x = "Age Relative to Restoration (yrs)", 
       y = "UVVR Score") + 
  scale_x_continuous(limits = c(-10.5, 8.5), 
                     breaks = seq(-10, 8, 2)) + 
  scale_y_continuous(limits = c(0, 1.0), 
                     breaks = seq(0, 1.0, 0.20)) + 
  scale_fill_manual(values = wes_palette("FantasticFox1", n = 3)) + 
  scale_colour_manual(values = wes_palette("FantasticFox1", n = 3)) + 
  theme_bw() +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    strip.text.x = element_text(size = 22.5, colour = "black"),
    strip.background = element_blank(),
    axis.title = element_text(size = 22.5, colour = "black"),
    axis.text = element_text(size = 22.5, colour = "black")) + 
  facet_wrap(~Treatment,
             nrow = 3, ncol = 1)


Linear.Graph.UVVR
























#________________________________________________________________________________________________________________________________

#CHAPTER 2: UVVR - SPLINE REGRESSIONS

# To complete a spline regression, we need to define the "knots" of the time/location on the 
#x-axis where the regression shifts
# For this analysis, the Knot will be placed at Time = 0 yrs relative to Runnel Construction Date, 
# since we are curious about the shift in the slope pre- and post-restoration

#The full model will be a mixed spline model. #Mixed model is created with the lmer() function and the outputs of the mixed model are recorded with
#various broom.mixed package functions. Broom.mixed converts complex lme4 outputs to tables
#For the mixed model, UVVR is the response variable, Timeline and Treatment are fixed effects,
#and Site and Tideshed_ID are random effects. 
#Note: Individual tidesheds are uniquely ID'ed, rendering a nesting structure for the data obsolete


# Page 1 - Create the Splines Model with the Splines Package and bs() function

#The bs() function allows us to designate the knot locations for the spline model
#Later on in the code, we will individually create a Spline Model for each management treatment,
#but for now, this is a simple and quick way to calculate the ANOVA table for the regressions
#Spline regressions are completed with raw data (e.g., all tidesheds, not means)
#The statistical terms are rounded to 4 digits
#Lastly, the slope of each segment is calculated. The slope of the second segment is calculated as summation of 
#the estimates of each segment. First estimate = Slope of segment 1, Second estimate = change in slope

UVVR.Spline <- lmer(UVVR ~ bs(Timeline, knots = 0, degree = 1) * Treatment + (1|Site) + (1|Tideshed_ID),
                    data = Database, na.action = na.omit)

Spline.tidy <- broom.mixed::tidy(UVVR.Spline)

Spline.tidy

Spline.glance <- broom.mixed::glance(UVVR.Spline)

Spline.glance

Spline.anova <- data.frame(anova(UVVR.Spline)) %>%
  mutate(coeff = rownames(.))

Spline.anova



#Step 2 - Check the assumptions of homogeneity and normal distribution with visual inspection of residuals

#Assumption 1 - Linearity
#Plot residuals of the model and observed values

plot(resid(UVVR.Spline), Database$UVVR)


#Assumption 2 - Homogeneity of Variance

plot(UVVR.Spline)

#Assumption 3 - Normality of Residuals

qqnorm(residuals(UVVR.Spline))




#Page 2 - Export the Spline Mixed Model Model Summaries

write.csv(Spline.glance, "E:\\Coastal Habitat Restoration Team\\ACJV Sites - RI_Mass_Maine\\Data Analysis\\Output R CSVs Analysis\\UVVR_Spline_glance.csv")

write.csv(Spline.tidy, "E:\\Coastal Habitat Restoration Team\\ACJV Sites - RI_Mass_Maine\\Data Analysis\\Output R CSVs Analysis\\UVVR_Spline_tidy.csv")

write.csv(Spline.anova, "E:\\Coastal Habitat Restoration Team\\ACJV Sites - RI_Mass_Maine\\Data Analysis\\Output R CSVs Analysis\\UVVR_Spline_anova.csv")

rm(Spline.glance, Spline.tidy, Spline.anova)


#Page 3 - Predict Spline Regression (without random effects) with ggpredict()

#Using the ggpredict() function, the mean value and confidence interval can be calculated for each
#treatment at each timeline point. ggpredict() function is a powerful tool!
#For visualization purposes, the confidence interval was calculated for the variance of the fixed effects,
#while keeping the random effects constant. Essentially, the confidence interval becomes incredibly large
#and not useful for visualization purposes
#Lastly, the columns are renamed for easy ggploting. For UVVR, the lower confidence interval is set to zero
#if negative, since you can not have a UVVR score of zero. 


Spline.preds <- ggpredict(UVVR.Spline, terms = c("Timeline [all]", "Treatment [all]"),
                          type = "fixed", interval = 'confidence') %>%
  rename(Timeline = x,
         Treatment = group,
         UVVR = predicted) %>%
  arrange(Treatment) %>%
  mutate(conf.low = ifelse(conf.low < 0, 0, conf.low))


#Page 4 - Calcualte the Slopes of the Spline Models with predicted values
#Using the predicted mean values, the slopes of each treatment can be calculated pre- and post-restoration

Spline.Slope <- Spline.preds %>%
  group_by(Treatment) %>%
  summarise(slope.pre = (UVVR[which(Timeline == 0)] - UVVR[which(Timeline == min(Timeline))]) / (0 - min(Timeline)),
            
            slope.post = (UVVR[which(Timeline == max(Timeline))] - UVVR[which(Timeline == min(Timeline = 0))]) / (max(Timeline) - 0)) %>%
  ungroup()

write.csv(Spline.Slope, "E:\\Coastal Habitat Restoration Team\\ACJV Sites - RI_Mass_Maine\\Data Analysis\\Output R CSVs Analysis\\UVVR_Spline_Slopes.csv")


# Page 5 - Graph the mixed Spline Model (without Ditch Remediation) with facet wrap function

#There are two data sets being graphed: Predicted values of mixed model and the mean values (with error bars) 
#The treatments are graphed in the following order: No Action, Reference, Ditch, and Runnel 
#These corresponds with basic R colors that correspond well with each treatment


Spline.preds$Treatment <- factor(Spline.preds$Treatment,
                                 levels = c("No Action", "Reference","Runnel"))

Spline.Graph.UVVR <- ggplot() + 
  geom_vline(xintercept = 0, size = 1, colour = "grey", 
             linetype = "dashed") + 
  geom_ribbon(data = filter(Spline.preds),
              aes(x = Timeline, ymin = conf.low, ymax = conf.high, fill = Treatment),
              alpha = 0.25) +
  geom_line(data = filter(Spline.preds),
            aes(x = Timeline, y = UVVR, colour = Treatment),
            size = 1.5) +
  geom_errorbar(data = filter(Timeline.Metrics),
                aes(x = Timeline, ymin = UVVR.avg - UVVR.se, ymax = UVVR.avg + UVVR.se,
                    colour = Treatment),
                size = 1, width = 0.5) +
  geom_point(data = filter(Timeline.Metrics),
             aes(x = Timeline, y = UVVR.avg, fill = Treatment), 
             shape = 21, size = 5.5) +
  labs(x = "Age Relative to Restoration (yrs)", 
       y = "UVVR Score") + 
  scale_x_continuous(limits = c(-10.5, 8.5), 
                     breaks = seq(-10, 8, 2)) + 
  scale_y_continuous(limits = c(0, 1.0), 
                     breaks = seq(0, 1.0, 0.2)) +
  scale_fill_manual(values = wes_palette("FantasticFox1", n = 3)) + 
  scale_colour_manual(values = wes_palette("FantasticFox1", n = 3)) + 
  theme_bw() +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.x = element_blank(),
    strip.text.x = element_text(size = 22.5, colour = "black"),
    strip.background = element_blank(),
    axis.title = element_text(size = 22.5, colour = "black"),
    axis.text = element_text(size = 22.5, colour = "black")) + 
  facet_wrap(~Treatment, nrow = 3, ncol = 1)

Spline.Graph.UVVR






#_____________________________________________________________________________________________________________

#Chapter 3: Spline Mixed Model (3 way interaction) of UVVR

#After review of the data, it seemed there was a complete divergence between the No Action Control and Runnel
  #tidesheds when analyzed between Healthy (UVVR < 0.13) and Degraded ( UVVR > 0.13) tideshed areas prior
  # to restoration. If you look at the runnel tidesheds, there were a handful of sites with still vigorous
  #vegetation cover (Jacob's Point, Broad Cove, Essex, Sapowet, etc.) at some interior marshes. This does not
  #mean that the tidesheds are degraded - could be waterlogged, vegetation dominated by S. alterniflora - but
  #from the standpoint of the UVVR and the classification, the salt marsh is healthy.

#To analyze this trend more thoroughly, we will create a mixed linear spline model with the effects:
    # Fixed: Timeline, Treatment, Baseline Condition, and Three-way Interaction
    # Random: Site, Unique Tideshed ID

#Only the runnel and no action control treatments were subdivided into Healthy and Degraded. Few tidesheds of
  #the reference treatment were considered Degraded, so all were retroactively classified as Healthy. Due to the 
  #lack of a sub-treatment, the reference treatment was removed from further investigation.

#The code below is adopted from Chapter 2 in this Book and is modified to incorporate the three-way interaction,
  #most of the variable names remained the same except for figures (facetted figures later in code)


# Page 1 - Create the Splines Model with the Splines Package and bs() function


UVVR.Spline <- lmer(UVVR ~ bs(Timeline, knots = 0, degree = 1) * Treatment * BaselineCondition + (1|Site) + (1|Tideshed_ID),
                    data = filter(Database, Treatment != 'Reference'), na.action = na.omit)

Spline.tidy <- broom.mixed::tidy(UVVR.Spline)

Spline.tidy

Spline.glance <- broom.mixed::glance(UVVR.Spline)

Spline.glance

Spline.anova <- data.frame(anova(UVVR.Spline)) %>%
  mutate(coeff = rownames(.))

Spline.anova



#Step 2 - Check the assumptions of homogeneity and normal distribution with visual inspection of residuals

#Assumption 1 - Linearity
#Plot residuals of the model and observed values

plot(resid(UVVR.Spline), Database$UVVR)


#Assumption 2 - Homogeneity of Variance

plot(UVVR.Spline)

#Assumption 3 - Normality of Residuals

qqnorm(residuals(UVVR.Spline))


summary(UVVR.Spline)

#Page 2 - Export the Spline Mixed Model Model Summaries

write.csv(Spline.glance, "E:\\Coastal Habitat Restoration Team\\ACJV Sites - RI_Mass_Maine\\Data Analysis\\Output R CSVs Analysis\\UVVR_Spline_3way_glance.csv")

write.csv(Spline.tidy, "E:\\Coastal Habitat Restoration Team\\ACJV Sites - RI_Mass_Maine\\Data Analysis\\Output R CSVs Analysis\\UVVR_Spline_3way_tidy.csv")

write.csv(Spline.anova, "E:\\Coastal Habitat Restoration Team\\ACJV Sites - RI_Mass_Maine\\Data Analysis\\Output R CSVs Analysis\\UVVR_Spline_3way_anova.csv")

rm(Spline.glance, Spline.tidy, Spline.anova)


#Page 3 - Predict Spline Regression (without random effects) with ggpredict()

#Using the ggpredict() function, the mean value and confidence interval can be calculated for each
  #treatment at each timeline point. ggpredict() function is a powerful tool!
#For visualization purposes, the confidence interval was calculated for the variance of the fixed effects,
  #while keeping the random effects constant. Essentially, the confidence interval becomes incredibly large
  #and not useful for visualization purposes
#Lastly, the columns are renamed for easy ggploting. For UVVR, the lower confidence interval is set to zero
  #if negative, since you can not have a UVVR score of zero. 


Spline.preds <- ggpredict(UVVR.Spline, terms = c("Timeline [all]", "Treatment [all]", "BaselineCondition [all]"),
                          type = "fixed", interval = 'confidence') %>%
  rename(Timeline = x,
         Treatment = group,
         UVVR = predicted,
         BaselineCondition = facet) %>%
  arrange(Treatment) %>%
  mutate(conf.low = ifelse(conf.low < 0, 0, conf.low))


#Page 4 - Calcualte the Slopes of the Spline Models with predicted values
#Using the predicted mean values, the slopes of each treatment can be calculated pre- and post-restoration

Spline.Slope <- Spline.preds %>%
  group_by(Treatment, BaselineCondition) %>%
  summarise(slope.pre = (UVVR[which(Timeline == 0)] - UVVR[which(Timeline == min(Timeline))]) / (0 - min(Timeline)),
            
            slope.post = (UVVR[which(Timeline == max(Timeline))] - UVVR[which(Timeline == min(Timeline = 0))]) / (max(Timeline) - 0)) %>%
  ungroup()

Spline.Slope

write.csv(Spline.Slope, "E:\\Coastal Habitat Restoration Team\\ACJV Sites - RI_Mass_Maine\\Data Analysis\\Output R CSVs Analysis\\UVVR_Spline_Slopes_Condition.csv")


# Page 5 - Graph the mixed Spline Model (without Ditch Remediation) with facet wrap function

#There are two data sets being graphed: Predicted values of mixed model and the mean values (with error bars) 
#The treatments are graphed in the following order: No Action, Reference, Ditch, and Runnel 
#These corresponds with basic R colors that correspond well with each treatment


Spline.preds$Treatment <- factor(Spline.preds$Treatment,
                                 levels = c("No Action", "Reference","Runnel"))

Spline.preds$BaselineCondition <- factor(Spline.preds$BaselineCondition,
                                 levels = c("Degraded", "Healthy"))

Health.UVVR <- ggplot() + 
geom_vline(xintercept = 0, size = 1, colour = "grey", 
           linetype = "dashed") + 
  geom_ribbon(data = filter(Spline.preds),
              aes(x = Timeline, ymin = conf.low, ymax = conf.high, fill = BaselineCondition),
              alpha = 0.25) +
  geom_line(data = filter(Spline.preds),
            aes(x = Timeline, y = UVVR, colour = BaselineCondition),
            size = 1.5) +
  geom_errorbar(data = filter(Timeline.Health, Treatment != "Reference"),
                aes(x = Timeline, ymin = UVVR.avg - UVVR.se, ymax = UVVR.avg + UVVR.se,
                    colour = BaselineCondition),
                size = 1, width = 0.5) +
  geom_point(data = filter(Timeline.Health, Treatment != "Reference"),
             aes(x = Timeline, y = UVVR.avg, fill = BaselineCondition), 
             size = 5.5, shape = 21) +
  labs(x = "Age Relative to Restoration (yrs)", 
       y = "UVVR Score") + 
  scale_x_continuous(limits = c(-10.5, 8.5), 
                     breaks = seq(-10, 8, 2)) + 
  scale_y_continuous(limits = c(0, 1.25), 
                     breaks = seq(0, 1.25, 0.25)) +
  scale_fill_manual(values = c(wes_palette("FantasticFox1")[1], wes_palette("FantasticFox1")[3])) + 
  scale_colour_manual(values = c(wes_palette("FantasticFox1")[1], wes_palette("FantasticFox1")[3])) + 
  theme_bw() +
  theme(
    legend.position = c(0.12, 0.92),
    legend.background = element_rect(
      size = 0.5, linetype = "solid", colour = "black" ),
    legend.text = element_text(size = 15, colour = "black"),
    legend.title = element_blank(),
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.x = element_blank(),
    strip.text.x = element_text(size = 22.5, colour = "black"),
    strip.background = element_blank(),
    axis.title = element_text(size = 22.5, colour = "black"),
    axis.text = element_text(size = 22.5, colour = "black")) + 
  facet_wrap(~Treatment, nrow = 3, ncol = 1)

Health.UVVR




  





#_________________________________________________________________________________________________________________________________

#CHAPTER 3: UVVR - Univariate Statistics (ANOVA, Least Likliehood Ratio Test, etc.)


#Page 1 - Model Comparison: Compare Linear and Spline models to defend use of Spline

#We are using a Likelihood Ratio Test (via the anova() function) to compare Linear and Spline mixed models
  #ability to account for the variance.
#Ditch Remediation treatment is not included in the Linear mixed model, since it can not be accounted for
  #in the Spline model

UVVR.Linear.mod <- lmer(UVVR ~ Timeline * Treatment + (1|Site) + (1|Tideshed_ID), 
                        data = filter(Database), na.action = na.omit)


Model.compare <- anova(UVVR.Linear.mod, UVVR.Spline)

Model.compare

write.csv(Model.compare,  "E:\\Coastal Habitat Restoration Team\\ACJV Sites - RI_Mass_Maine\\Data Analysis\\Output R CSVs Analysis\\UVVR_LikliehoodTest_Models.csv")


#Page 2 - Determine if runnel restoration was applied to the most degraded tidesheds 
  #One-way ANOVAs will be applied to the UVVR scores of all the treatment tidesheds at the latest
   #pre-restoration time (e.g., -2 -> 0 yrs)
  #Essentially, we want to see if the runnel restoration activities were applied to tidesheds that were
    #more degraded than references and similar to no action tidesheds
  # The one-way mixed ANOVA will be completed in JMP, due to ease of completing a unequal sample size
    #Dunnett's post-hoc analysis. In the R code, we are organizing the data to input into JMP and creating figures


#Step 1 - Determine the latest pre-restoration date for each tideshed 
#Each tideshed had aerial imagery at some point from -2 -> 0 yrs before restoration


Tideshed.pre <- Database %>%
  group_by(Tideshed_ID) %>%
   filter(Timeline <= 0) %>%
     filter(Timeline == max(Timeline)) %>%
ungroup()

Tideshed.pre$Treatment <- factor(Tideshed.pre$Treatment, 
                                 levels = c("No Action", "Reference", "Runnel"))


write.csv(Tideshed.pre, "E:\\Coastal Habitat Restoration Team\\ACJV Sites - RI_Mass_Maine\\Data Analysis\\Output R CSVs Analysis\\UVVR_Veg_PreRestoration_Baseline.csv" )


#Next, for reporting purposes, the summary statistics will be calculated for the Vegetated Area and UVVR

Tideshed.pre.sum <- Tideshed.pre %>%
  group_by(Treatment) %>%
  summarise(UVVR.avg = mean(UVVR),
            UVVR.se = sd(UVVR)/sqrt(n()),
            
            Veg.avg = mean(Veg_Percent),
            Veg.se = sd(Veg_Percent)/sqrt(n()),
            
            Veg.PChange.avg = mean(Veg_PChange, na.rm = TRUE),
            Veg.PChange.se = sd(Veg_PChange, na.rm = TRUE)/sqrt(n())) %>%
  ungroup()

Tideshed.pre.sum$Treatment <- factor(Tideshed.pre.sum$Treatment, 
                                     levels = c("No Action", "Reference", "Runnel"))

write.csv(Tideshed.pre.sum, "E:\\Coastal Habitat Restoration Team\\ACJV Sites - RI_Mass_Maine\\Data Analysis\\Output R CSVs Analysis\\UVVR_Veg_PreRestoration_Baseline_SumStats.csv" )



#Step 3 - Visualize the One-Way ANOVA of UVVR

PreRestoration.UVVR.Graph <- ggplot() + 
  geom_boxplot(data = Tideshed.pre,
               aes(x = Treatment, y = UVVR, fill = Treatment),
               size = 0.75) +
  labs(x = '',
       y = "Average UVVR Score") + 
  scale_y_continuous(limits = c(0, 1.0), breaks = seq(0, 1.0, 0.2)) +
  theme_bw() +
  theme(
    legend.position = 'none',
    legend.background = element_rect(
      size = 0.5, linetype = "solid", colour = "black" ),
    legend.text = element_text(size = 15, colour = "black"),
    legend.title = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.title = element_text(size = 17.5, colour = "black"),
    axis.text.y = element_text(size = 15, colour = "black"),
    axis.text.x = element_text(size = 17.5, colour = "black"))

PreRestoration.UVVR.Graph



rm(pre.anova.tidy, Piece.Linear.Pre, Linear.Slope, Linear.preds)




















#___________________________________________________________________________________________________________

#Book 3: Vegetated Area Mixed Model Analysis

#___________________________________________________________________________________________________________


#CHAPTER 1 - MIXED MODEL LINEAR REGRESSIONS of Vegetated Area

#Page 1 - Create Full Linear Model and Summary Data frames
#Mixed model is created with the lmer() function and the outputs of the mixed model are recorded with
#various broom.mixed package functions. Broom.mixed converts complex lme4 outputs to tables
#For the mixed model, UVVR is the response variable, Timeline and Treatment are fixed effects,
#and Site and Tideshed_ID are random effects. 
#Note: Individual tidesheds are uniquely ID'ed, rendering a nesting structure for the data obsolete



Veg.Linear <- lmer(Veg_Percent ~ Timeline * Treatment + (1|Site) + (1|Tideshed_ID), 
                   data = Database, na.action = na.omit)

Veg.Linear.tidy <- broom.mixed::tidy(Veg.Linear)

Veg.Linear.anova <- as.data.frame(anova(Veg.Linear)) %>%
  mutate(coeff = row.names(.))

Veg.Linear.glance <- broom.mixed::glance(Veg.Linear)



#Step 2 - Check the assumptions of homogeneity and normal distribution with visual inspection of residuals

#Assumption 1 - Linearity
#Plot residuals of the model and observed values

plot(resid(Veg.Linear), Database$UVVR)


#Assumption 2 - Homogeneity of Variance

plot(Veg.Linear)

#Assumption - Normality of residuals

qqnorm(residuals(Veg.Linear))



#Page 2 - Export the Full Linear Model data frames

write.csv(Veg.Linear.glance, "E:\\Coastal Habitat Restoration Team\\ACJV Sites - RI_Mass_Maine\\Data Analysis\\Output R CSVs Analysis\\Veg_Linear_glance.csv")

write.csv(Veg.Linear.tidy, "E:\\Coastal Habitat Restoration Team\\ACJV Sites - RI_Mass_Maine\\Data Analysis\\Output R CSVs Analysis\\Veg_Linear_tidy.csv")

write.csv(Veg.Linear.anova, "E:\\Coastal Habitat Restoration Team\\ACJV Sites - RI_Mass_Maine\\Data Analysis\\Output R CSVs Analysis\\Veg_Linear_anova.csv")

rm(Veg.Linear.glance, Veg.Linear.tidy, Veg.Linear.anova)



#Page 3 - Predict the Linear Model for each Treatment

#Using the ggpredict() function, the mean value and confidence interval can be calculated for each
  #treatment at each timeline point. ggpredict() function is a powerful tool!
#For visualization purposes, the confidence interval was calculated for the variance of the fixed effects,
  #while keeping the random effects constant. Essentially, the confidence interval becomes incredibly large
  #and not useful for visualization purposes


Linear.preds <- ggpredict(Veg.Linear, c("Timeline", "Treatment"), 
                          type = "fixed", interval = 'confidence') %>%
  rename(Timeline = x,
         Treatment = group,
         Veg_Percent = predicted) %>%
  arrange(Treatment)



#Page 4 - Calculate the Slope of Linear Mixed Model with predicted values

#Using the predicted mean values, the slopes of each treatment can be calculated

Linear.Slope <- Linear.preds %>%
  group_by(Treatment) %>%
   summarise(slope = (Veg_Percent[which.max(Timeline)] - Veg_Percent[which.min(Timeline)]) / (max(Timeline) - min(Timeline)) ) %>%
ungroup()

Linear.Slope

write.csv(Linear.Slope, "E:\\Coastal Habitat Restoration Team\\ACJV Sites - RI_Mass_Maine\\Data Analysis\\Output R CSVs Analysis\\Veg_Linear_Slopes.csv")



#Page 5 - Graph the predicted linear mixed model for each treatment via the facet wrap function

#There are two data sets being graphed: Predicted values of mixed model and the mean values (with error bars) 
#The treatments are graphed in the following order: No Action, Reference, Ditch, and Runnel 
#These corresponds with basic R colors that correspond well with each treatment


Linear.preds$Treatment <- factor(Linear.preds$Treatment,
                                 levels = c("No Action", "Reference", "Runnel"))

Linear.Graph.Veg <- ggplot() +   
  geom_vline(xintercept = 0, size = 1, colour = "grey", 
             linetype = "dashed") + 
  geom_line(data = Linear.preds,
            aes(x = Timeline, y = Veg_Percent, colour = Treatment),
            size = 1.5) +
  geom_ribbon(data = Linear.preds,
              aes(x = Timeline, ymin = conf.low, ymax = conf.high, fill = Treatment),
              alpha = 0.25) +
  geom_errorbar(data = Timeline.Metrics,
                aes(x = Timeline, ymin = Veg_Percent.avg - Veg_Percent.se, ymax = Veg_Percent.avg + Veg_Percent.se, 
                    colour = Treatment),
                width = 0.5, size = 1) +
  geom_point(data = Timeline.Metrics,
             aes(x = Timeline, y = Veg_Percent.avg, fill = Treatment), 
             shape = 21, size = 5) +
  labs(x = "Age Relative to Restoration (yrs)", 
       y = "Vegetated Area (%)") + 
  scale_x_continuous(limits = c(-10.5, 8.5), 
                     breaks = seq(-10, 8, 2)) + 
  scale_y_continuous(limits = c(60, 105), 
                     breaks = seq(60, 100, 10)) +
  scale_fill_manual(values = wes_palette("FantasticFox1", n = 3)) + 
  scale_colour_manual(values = wes_palette("FantasticFox1", n = 3)) + 
  theme_bw() +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    strip.text.x = element_text(size = 22.5, colour = "black"),
    strip.background = element_blank(),
    axis.title = element_text(size = 22.5, colour = "black"),
    axis.text = element_text(size = 22.5, colour = "black")) + 
  facet_wrap(~Treatment, 
             ncol = 1, nrow = 3)


Linear.Graph.Veg


Linear.Graph <- Linear.Graph.UVVR + Linear.Graph.Veg


Linear.Graph


ggsave(Linear.Graph, height = 12, width = 16, dpi = 600,
       limitsize = FALSE, units = "in",
       filename = "E:\\Coastal Habitat Restoration Team\\ACJV Sites - RI_Mass_Maine\\Data Analysis\\Output R Figures\\Final Figures\\Manuscript\\Linear_MixedModels_VegPercent.jpg")
















#________________________________________________________________________________________________________________________________

#CHAPTER 2: Vegetated Area - SPLINE REGRESSIONS

# To complete a spline regression, we need to define the "knots" of the time/location on the 
#x-axis where the regression shifts
# For this analysis, the Knot will be placed at Time = 0 yrs relative to Runnel Construction Date, 
# since we are curious about the shift in the slope pre- and post-restoration

#The full model will be a mixed spline model. #Mixed model is created with the lmer() function and the outputs of the mixed model are recorded with
#various broom.mixed package functions. Broom.mixed converts complex lme4 outputs to tables
#For the mixed model, UVVR is the response variable, Timeline and Treatment are fixed effects,
#and Site and Tideshed_ID are random effects. 
#Note: Individual tidesheds are uniquely ID'ed, rendering a nesting structure for the data obsolete


# Page 1 - Create the Splines Model with the Splines Package and bs() function

#The bs() function allows us to designate the knot locations for the spline model
#Later on in the code, we will individually create a Spline Model for each management treatment,
#but for now, this is a simple and quick way to calculate the ANOVA table for the regressions
#Spline regressions are completed with raw data (e.g., all tidesheds, not means)
#The statistical terms are rounded to 4 digits
#Lastly, the slope of each segment is calculated. The slope of the second segment is calculated as summation of 
#the estimates of each segment. First estimate = Slope of segment 1, Second estimate = change in slope



Veg.Spline <- lmer(Veg_Percent ~ bs(Timeline, knots = 0, degree = 1) * Treatment + (1|Site) + (1|Tideshed_ID),
                   data = Database, na.action = na.omit)

Spline.tidy <- broom.mixed::tidy(Veg.Spline)

Spline.tidy


Spline.glance <- broom.mixed::glance(Veg.Spline)

Spline.glance


Spline.anova <- data.frame(anova(Veg.Spline)) %>%
  mutate(coeff = rownames(.))

Spline.anova



#Step 2 - Check the assumptions of homogeneity and normal distribution with visual inspection of residuals

#Assumption 1 - Linearity
#Plot residuals of the model and observed values

plot(resid(Veg.Spline), Database$Veg_Percent)


#Assumption 2 - Homogeneity of Variance

plot(Veg.Spline)



#Assumption 3 - Residuals of the Model are Normally Distributed

qqnorm(residuals(Veg.Spline))


#Page 2 - Export the Spline Mixed Model Model Summaries

write.csv(Spline.glance, "E:\\Coastal Habitat Restoration Team\\ACJV Sites - RI_Mass_Maine\\Data Analysis\\Output R CSVs Analysis\\Veg_Spline_glance.csv")

write.csv(Spline.tidy, "E:\\Coastal Habitat Restoration Team\\ACJV Sites - RI_Mass_Maine\\Data Analysis\\Output R CSVs Analysis\\Veg_Spline_tidy.csv")

write.csv(Spline.anova, "E:\\Coastal Habitat Restoration Team\\ACJV Sites - RI_Mass_Maine\\Data Analysis\\Output R CSVs Analysis\\Veg_Spline_anova.csv")

rm(Spline.glance, Spline.tidy, Spline.anova)


#Page 3 - Predict Spline Regression (without random effects) with ggpredict()

#Using the ggpredict() function, the mean value and confidence interval can be calculated for each
  #treatment at each timeline point. ggpredict() function is a powerful tool!
#For visualization purposes, the confidence interval was calculated for the variance of the fixed effects,
  #while keeping the random effects constant. Essentially, the confidence interval becomes incredibly large
  #and not useful for visualization purposes



Spline.preds <- ggpredict(Veg.Spline, terms = c("Timeline [all]", "Treatment [all]"),
                          type = "fixed", interval = 'confidence') %>%
  rename(Timeline = x,
         Treatment = group,
         Veg_Percent = predicted) %>%
  arrange(Treatment)


#Page 4 - Calculate the Slopes of the Spline Models with predicted values
#Using the predicted mean values, the slopes of each treatment can be calculated pre- and post-restoration

Spline.Slope <- Spline.preds %>%
  group_by(Treatment) %>%
  summarise(slope.pre = (Veg_Percent[which(Timeline == 0)] - Veg_Percent[which(Timeline == min(Timeline))]) / (0 - min(Timeline)),
            
            slope.post = (Veg_Percent[which(Timeline == max(Timeline))] - Veg_Percent[which(Timeline == min(Timeline = 0))]) / (max(Timeline) - 0)) %>%
  ungroup()

Spline.Slope

write.csv(Spline.Slope, "E:\\Coastal Habitat Restoration Team\\ACJV Sites - RI_Mass_Maine\\Data Analysis\\Output R CSVs Analysis\\Veg_Spline_Slopes.csv")


# Page 5 - Graph the mixed Spline Model (without Ditch Remediation) with facet wrap function

#There are two data sets being graphed: Predicted values of mixed model and the mean values (with error bars) 
#The treatments are graphed in the following order: No Action, Reference, Ditch, and Runnel 
#These corresponds with basic R colors that correspond well with each treatment


Spline.preds$Treatment <- factor(Spline.preds$Treatment,
                                 levels = c("No Action", "Reference",  "Runnel"))

Spline.Veg <- ggplot() + 
  geom_vline(xintercept = 0, size = 1, colour = "grey", 
             linetype = "dashed") + 
  geom_line(data = Spline.preds,
            aes(x = Timeline, y = Veg_Percent, colour = Treatment),
            size = 1.5) +
  geom_ribbon(data = Spline.preds,
              aes(x = Timeline, ymin = conf.low, ymax = conf.high, fill = Treatment),
              alpha = 0.25) +
  geom_errorbar(data = Timeline.Metrics,
                aes(x = Timeline, ymin = Veg_Percent.avg - Veg_Percent.se, 
                    ymax = Veg_Percent.avg + Veg_Percent.se,
                    colour = BaselineCondition),
                size = 1, width = 0.5) +
  geom_point(data = Timeline.Metrics,
             aes(x = Timeline, y = Veg_Percent.avg, fill = Treatment), 
             size = 5.5, shape = 21) +
  labs(x = "Age Relative to Restoration (yrs)", 
       y = "Vegetated Area (%)") + 
  scale_x_continuous(limits = c(-10.5, 8.5), 
                     breaks = seq(-10, 8, 2)) + 
  scale_y_continuous(limits = c(60, 105), 
                     breaks = seq(60, 100, 10)) +
  scale_fill_manual(values = wes_palette("FantasticFox1", type = "discrete", n = 3)) + 
  scale_colour_manual(values = wes_palette("FantasticFox1", type = "discrete", n = 3)) + 
  theme_bw() +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    strip.text.x = element_text(size = 22.5, colour = "black"),
    strip.background = element_blank(),
    axis.title = element_text(size = 22.5, colour = "black"),
    axis.text = element_text(size = 22.5, colour = "black")) + 
  facet_wrap(~Treatment,
             ncol = 1, nrow = 3)


Spline.Graph.Veg

Spline.Graph <- Spline.Graph.UVVR + Spline.Graph.Veg

Spline.Graph


ggsave(Spline.Graph, height = 12, width = 16, dpi = 600,
       limitsize = FALSE, units = "in",
       filename = "E:\\Coastal Habitat Restoration Team\\ACJV Sites - RI_Mass_Maine\\Data Analysis\\Output R Figures\\Final Figures\\Manuscript\\Spline_MixedModels_VegPercent.jpg")




rm(Spline.Graph, Spline.Graph.UVVR, Spline.Graph.Veg, Spline.preds, Spline.Slope, UVVR.Linear,
   UVVR.Spline, Veg.Linear, Veg.Spline, UVVR.Linear.anova, UVVR.Linear.glance, UVVR.Linear.tidy,
   Linear.Slope, Linear.preds, Linear.Graph.UVVR, Linear.Graph.Veg, Linear.Graph)







#___________________________________________________________________________________________________________

#Chapter 3: Spline Mixed Model on Baseline Condition for Vegetated Area

#After review of the data, it seemed there was a complete divergence between the No Action Control and Runnel
  #tidesheds when analyzed between Healthy (UVVR < 0.13) and Degraded ( UVVR > 0.13) tideshed areas prior
  #to restoration. If you look at the runnel tidesheds, there were a handful of sites with still vigorous
  #vegetation cover (Jacob's Point, Broad Cove, Essex, Sapowet, etc.) at some interior marshes. This does not
  #mean that the tidesheds are degraded - could be waterlogged, vegetation dominated by S. alterniflora - but
  #from the standpoint of the UVVR and the classification, the salt marsh is healthy.

#To analyze this trend more thoroughly, we will create a mixed linear spline model with the effects:
  # Fixed: Timeline, Treatment, Baseline Condition, and Three-way Interaction
  # Random: Site, Unique Tideshed ID

#Only the runnel and no action control treatments were subdivided into Healthy and Degraded. Few tidesheds of
  #the reference treatment were considered Degraded, so all were retroactively classified as Healthy. Due to the 
  #lack of a sub-treatment, the reference treatment was removed from further investigation.

#The code below is adopted from Chapter 2 in this Book and is modified to incorporate the three-way interaction,
  #most of the variable names remained the same except for figures (facetted figures later in code)


Veg.Spline <- lmer(Veg_Percent ~ bs(Timeline, knots = 0, degree = 1) * Treatment * BaselineCondition + (1|Site) + (1|Tideshed_ID),
                   data = filter(Database, Treatment != "Reference"), na.action = na.omit)

Spline.tidy <- broom.mixed::tidy(Veg.Spline)

Spline.tidy


Spline.glance <- broom.mixed::glance(Veg.Spline)

Spline.glance


Spline.anova <- data.frame(anova(Veg.Spline)) %>%
  mutate(coeff = rownames(.))

Spline.anova



#Step 2 - Check the assumptions of homogeneity and normal distribution with visual inspection of residuals

#Assumption 1 - Linearity
#Plot residuals of the model and observed values

plot(resid(Veg.Spline), Database$Veg_PChange)


#Assumption 2 - Homogeneity of Variance

plot(Veg.Spline)



#Assumption 3 - Residuals of the Model are Normally Distributed

qqnorm(residuals(Veg.Spline))


#Page 2 - Export the Spline Mixed Model Model Summaries

write.csv(Spline.glance, "E:\\Coastal Habitat Restoration Team\\ACJV Sites - RI_Mass_Maine\\Data Analysis\\Output R CSVs Analysis\\Veg_Spline_Health_glance.csv")

write.csv(Spline.tidy, "E:\\Coastal Habitat Restoration Team\\ACJV Sites - RI_Mass_Maine\\Data Analysis\\Output R CSVs Analysis\\Veg_Spline_Health_tidy.csv")

write.csv(Spline.anova, "E:\\Coastal Habitat Restoration Team\\ACJV Sites - RI_Mass_Maine\\Data Analysis\\Output R CSVs Analysis\\Veg_Spline_Health_anova.csv")

rm(Spline.glance, Spline.tidy, Spline.anova)


#Page 3 - Predict Spline Regression (without random effects) with ggpredict()

#Using the ggpredict() function, the mean value and confidence interval can be calculated for each
#treatment at each timeline point. ggpredict() function is a powerful tool!
#For visualization purposes, the confidence interval was calculated for the variance of the fixed effects,
#while keeping the random effects constant. Essentially, the confidence interval becomes incredibly large
#and not useful for visualization purposes



Spline.preds <- ggpredict(Veg.Spline, terms = c("Timeline [all]", "Treatment [all]", "BaselineCondition [all]"),
                          type = "fixed", interval = 'confidence') %>%
  rename(Timeline = x,
         Treatment = group,
         Veg_Percent = predicted,
         BaselineCondition = facet) %>%
  arrange(Treatment)


#Page 4 - Calculate the Slopes of the Spline Models with predicted values
#Using the predicted mean values, the slopes of each treatment can be calculated pre- and post-restoration

Spline.Slope <- Spline.preds %>%
  group_by(Treatment, BaselineCondition) %>%
  summarise(slope.pre = (Veg_Percent[which(Timeline == 0)] - Veg_Percent[which(Timeline == min(Timeline))]) / (0 - min(Timeline)),
            
            slope.post = (Veg_Percent[which(Timeline == max(Timeline))] - Veg_Percent[which(Timeline == min(Timeline = 0))]) / (max(Timeline) - 0)) %>%
  ungroup()

Spline.Slope

write.csv(Spline.Slope, "E:\\Coastal Habitat Restoration Team\\ACJV Sites - RI_Mass_Maine\\Data Analysis\\Output R CSVs Analysis\\Veg_Spline_Slopes_Health.csv")


# Page 5 - Graph the mixed Spline Model (without Ditch Remediation) with facet wrap function

#There are two data sets being graphed: Predicted values of mixed model and the mean values (with error bars) 
#The treatments are graphed in the following order: No Action, Reference, Ditch, and Runnel 
#These corresponds with basic R colors that correspond well with each treatment


Spline.preds$Treatment <- factor(Spline.preds$Treatment,
                                 levels = c("No Action", "Reference",  "Runnel"))

Spline.preds$BaselineCondition <- factor(Spline.preds$BaselineCondition,
                                 levels = c("Degraded", "Healthy"))

Health.Veg <- ggplot() + 
  geom_vline(xintercept = 0, size = 1, colour = "grey", 
             linetype = "dashed") + 
  geom_line(data = Spline.preds,
            aes(x = Timeline, y = Veg_Percent, colour = BaselineCondition),
            size = 1.5) +
  geom_ribbon(data = Spline.preds,
              aes(x = Timeline, ymin = conf.low, ymax = conf.high, fill = BaselineCondition),
              alpha = 0.25) +
  geom_errorbar(data = filter(Timeline.Health, Treatment != "Reference"),
                aes(x = Timeline, ymin = Veg_Percent.avg - Veg_Percent.se, 
                    ymax = Veg_Percent.avg + Veg_Percent.se,
                    colour = BaselineCondition),
                size = 1, width = 0.5) +
  geom_point(data = filter(Timeline.Health, Treatment != "Reference"),
             aes(x = Timeline, y = Veg_Percent.avg, fill = BaselineCondition), 
             size = 5.5, shape = 21) +
  labs(x = "Age Relative to Restoration (yrs)", 
       y = "Vegetated Area (%)") + 
  scale_x_continuous(limits = c(-10.5, 8.5), 
                     breaks = seq(-10, 8, 2)) + 
  scale_y_continuous(limits = c(60, 105), 
                     breaks = seq(60, 100, 10)) +
  scale_fill_manual(values = c(wes_palette("FantasticFox1")[1], wes_palette("FantasticFox1")[3])) + 
  scale_colour_manual(values = c(wes_palette("FantasticFox1")[1], wes_palette("FantasticFox1")[3])) + 
  theme_bw() +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    strip.text.x = element_text(size = 22.5, colour = "black"),
    strip.background = element_blank(),
    axis.title = element_text(size = 22.5, colour = "black"),
    axis.text = element_text(size = 22.5, colour = "black")) + 
  facet_wrap(~Treatment,
             ncol = 1, nrow = 3)

Health.Veg


Health.Graph <- Health.UVVR + Health.Veg


Health.Graph

ggsave(Health.Graph, height = 8, width = 16, dpi = 600,
       limitsize = FALSE, units = "in",
       filename = "E:\\Coastal Habitat Restoration Team\\ACJV Sites - RI_Mass_Maine\\Data Analysis\\Output R Figures\\Final Figures\\Manuscript\\Health_Graph_NoRef.jpg")



rm(Spline.Graph, Spline.Graph.UVVR, Spline.Graph.Veg, Spline.preds, Spline.Slope, UVVR.Linear,
   UVVR.Spline, Veg.Linear, Veg.Spline, UVVR.Linear.anova, UVVR.Linear.glance, UVVR.Linear.tidy,
   Linear.Slope, Linear.preds, Linear.Graph.UVVR, Linear.Graph.Veg, Linear.Graph,
   Health.Graph, Health.UVVR, Health.Veg)

















#_________________________________________________________________________________________________________________________________

#CHAPTER 4: Vegetated Area - Univariate Statistics (ANOVA, Least Likelihood Ratio Test, etc.)


#Page 1 - Model Comparison: Compare Linear and Spline models to defend use of Spline
#We are using a Likelihood Ratio Test (via the anova() function) to compare Linear and Spline mixed models
#ability to account for the variance.
#Ditch Remediation treatment is not included in the Linear mixed model, since it can not be accounted for
#in the Spline model

Veg.Linear.mod <- lmer(Veg_Percent ~ Timeline * Treatment + (1|Site) + (1|Tideshed_ID), 
                       data = Database, na.action = na.omit)


Model.compare <- anova(Veg.Linear.mod, Veg.Spline)

Model.compare

write.csv(Model.compare,  "E:\\Coastal Habitat Restoration Team\\ACJV Sites - RI_Mass_Maine\\Data Analysis\\Output R CSVs Analysis\\Veg_LikliehoodTest_Models.csv")


rm(Veg.Linear.mod, UVVR.Linear.mod, Model.compare)


#Page 2 - Determine if runnel restoration was applied to the most degraded tidesheds 
  #One-way ANOVAs will be applied to the UVVR scores of all the treatment tidesheds at the latest
    # pre-restoration time (e.g., -2 -> 0 yrs)
  #Essentially, we want to see if the runnel restoration activities were applied to tidesheds that were
    #more degraded than references and similar to no action tidesheds
  #I am not exactly how to compare models with mixed models, so Site will be used as a fixed blocking effect
    #in the one-way ANOVA. Tideshed and Timeline are not needed as covariate effects in the analysis

#Step 1 - Determine the latest pre-restoration date for each tideshed 
  #Each tideshed had aerial imagery at some point from -2 -> 0 yrs before restoration

#Note: This was already completed in the UVVR one-way ANOVA


#Step 2 - Visualize the One-Way ANOVA
#The calculations for the Percent Vegetated Area were completed in the One-Way ANOVA calculations for UVVR

PreRestoration.Veg.Graph <- ggplot() + 
  geom_boxplot(data = Tideshed.pre,
               aes(x = Treatment, y = Veg_Percent, fill = Treatment),
               size = 0.75) +
  labs(x = '',
       y = "Vegetated Area (%)") + 
  scale_y_continuous(limits = c(-60, 100), breaks = seq(60, 100, 10)) +
  theme_bw() +
  theme(
    legend.position = 'none',
    legend.background = element_rect(
      size = 0.5, linetype = "solid", colour = "black" ),
    legend.text = element_text(size = 15, colour = "black"),
    legend.title = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.title = element_text(size = 17.5, colour = "black"),
    axis.text.y = element_text(size = 15, colour = "black"),
    axis.text.x = element_text(size = 17.5, colour = "black"))

PreRestoration.Veg.Graph

PreRestoration.UVVR.Graph

#No changes needed for either graph when making the full two-panel graph with patchwork package
#Top - UVVR, Bottom - Vegetated Area

PreRestoration.Graph <- PreRestoration.UVVR.Graph / PreRestoration.Veg.Graph

PreRestoration.Graph


rm(PreRestoration.Graph, PreRestoration.UVVR.Graph, PreRestoration.Veg.Graph, 
   Tideshed.pre, Tideshed.pre.sum)








#_____________________________________________________________________________________________________________



#CHAPTER 5: DETERMINE TOTAL VETETATED AREA LOSS/GAINS SINCE 2010

#Page 1: Calculate Percent Vegetated Area Pre- and Post-Restoration
  #In Pre-Restoration, the vegetated area of years -12 -> 0, are divided by the vegetated area of the earliest year
  #In Post-Restoration, the vegetated area of years 1 --> 10, are divided by the vegetated area of the last year in pre-restoration

#Pre-Restoration Calculations
Database.Veg.Pre <- Database %>%
  filter(Timeline <=  0 & Timeline >= -10) %>%
     group_by(Site, Tideshed) %>%
        mutate(Veg_Earliest = Veg_Area[which.min(Timeline)]) %>%
ungroup(Site, Tideshed)


glimpse(Database.Veg.Pre)  

#Post-Restoration Calculations

Database.Veg.Post <- Database %>%
  filter(Timeline >= -2) %>%
     group_by(Site, Tideshed) %>%
        mutate(Veg_Earliest = ifelse(sum(Timeline == 0) == 1, 
                               Veg_Area[Timeline == 0], Veg_Area[which.min(Timeline)])) %>%
ungroup(Site, Tideshed) %>%
  filter(Timeline > 0)


glimpse(Database.Veg.Post)


#Step 2: Bind the Veg Databases together

Database.Veg <- rbind(Database.Veg.Pre, Database.Veg.Post)

glimpse(Database.Veg)


#Page 2: Calculate the Change in Vegetated Area Pre- and Post-restoration

#Step 1: Calculate the ins/Loss of Vegetated Area

Database.Veg <- Database.Veg %>%
  mutate(Veg_Change = Veg_Area - Veg_Earliest)



#Page 2: Calculate the Vegetated Area Gains/Loss

  #Step 1: Subset the Pre- and Post-restoration Time frames and filter out the Time frames to Calculate

#Gains and Losses of Vegetated Area

#Pre-restoration Gains/Losses for each Tideshed
Database.Veg.Pre <- Database.Veg %>%
  filter(Timeline <= 0 & Timeline >= -10) %>%
   group_by(Site, Tideshed) %>%
      filter(Timeline == max(Timeline)) %>%
ungroup()

#Post-restoration Gains/Losses for each Tideshed
Database.Veg.Post <- Database.Veg %>%
  filter(Timeline > 0) %>%
   group_by(Site, Tideshed) %>%
     filter(Timeline == max(Timeline)) %>%
ungroup()

#Step 3: Bind the two data frames back together 
Database.Veg <- rbind(Database.Veg.Pre, Database.Veg.Post)

#Step 4: Remove the pre- and post-restoration data frames
rm("Database.Veg.Post", "Database.Veg.Pre")

#Step 5: Calculate the Vegetated Area Gains/Loss for each Time frame

#Calculate the Gains and Losses for Each Timeframe
Veg.Gains.Losses <- Database.Veg %>%
  mutate(Timeframe = ifelse(Timeline <= 0, "Pre-restoration", "Post-restoration")) %>%
    group_by(Treatment, Timeframe) %>%
    summarise(
     Hectares = sum(Veg_Change),
      Acres = Hectares / 0.405,
      Count = n()) %>%
ungroup()


#Step 7: Remove the pre- and post-restoration data frames

rm("Database.Veg.Pre", "Database.Veg.Post")

#Step 8: Export the Veg Gains/Losses Table to Excel

write.csv(Veg.Gains.Losses, "E:\\Coastal Habitat Restoration Team\\ACJV Sites - RI_Mass_Maine\\Data Analysis\\Output R CSVs Analysis\\Veg_Gains_Losses.csv")


#Page 4: Bar Graph of the Vegetated Gains and Losses 

Veg.Gains.Losses$Treatment <- factor(Veg.Gains.Losses$Treatment, levels = c("No Action", "Reference","Runnel"))

Veg.Gains.Losses$Timeframe <- factor(Veg.Gains.Losses$Timeframe, levels = c("Pre-restoration", "Post-restoration"))


Veg.Gains.Bar <- ggplot(Veg.Gains.Losses, 
                        aes(x = Timeframe, y = Hectares, fill  = Treatment)) + 
  geom_bar(position = position_dodge(), stat = "identity", 
           colour = "black", size = 1) +
  geom_text(aes(label = Count, y = Hectares - 0.4), 
            position = position_dodge(0.9), 
            fontface ="bold", size = 6, colour = "black") +
  labs(x = "", y = "Change in Vegetated Area (ha)") +
  scale_y_continuous(limits = c(-5, 5), breaks = seq(-5, 5, 1)) +
  scale_fill_manual(values = wes_palette("FantasticFox1", n = 3)) + 
  theme_bw() +
  theme(
    legend.position = c(0.12,0.85),
    legend.background = element_rect(
      size = 0.5, linetype = "solid", colour = "black" ),
    legend.text = element_text(size = 15, colour = "black"),
    legend.title = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.title = element_text(size = 17, colour = "black"),
    axis.text.y = element_text(size = 17, colour = "black"),
    axis.text.x = element_text(size = 17, colour = "black"))

Veg.Gains.Bar





#Step 9 Calculate the Vegetated Area Gains/Loss for Site for Each Timeframe

#Pre-restoration
Veg.Gains.Losses.Site <- Database.Veg %>%
  mutate(Timeframe = ifelse(Timeline <= 0, "Pre-restoration", "Post-restoration")) %>%
    group_by(Site, Treatment, Timeframe) %>%
      summarise(
          Hectares = sum(Veg_Change),
          Acres = Hectares / 0.405,
          Count = n()) %>%
ungroup()

#Step 6: Bind the two data frames back together

Veg.Gains.Losses.Site <- Veg.Gains.Losses.Site %>%
  select(-Acres)%>%
    spread(Timeframe, Hectares)



#Step 8: Export the Veg Gains/Losses Table to Excel

write.csv(Veg.Gains.Losses.Site, "E:\\Coastal Habitat Restoration Team\\ACJV Sites - RI_Mass_Maine\\Data Analysis\\Output R CSVs Analysis\\Veg_Gains_Losses_Site.csv")









#____________________________________________________________________________________________________________


#Chapter 6: Relationship between the change in UVVR and Percent Vegetated Area



#Page 1 - Calculate and Graph the Change Vegetated Area (%) and 
  # the Change in UVVR based on comparisons to the Earliest Date

Database.change <- Database %>%
  mutate(UVVR.change = UVVR - UVVR_Earliest) %>%
    group_by(Tideshed_ID) %>%
      filter(Timeline == max(Timeline)) %>%
ungroup()




#Page 2 - Linear Mixed Model
  #The linear mixed model will include Site and Tideshed_ID as random effects
  #Outliers of UVVR change > 1.0 will be removed from the analysis

UVVR.veg <- lmer(Veg_PChange ~ UVVR.change + (1|Site),
                 data = Database.change, na.action = na.omit)

UVVR.veg.anova <- anova(UVVR.veg)

UVVR.veg.anova


UVVR.veg.sum <- broom.mixed::tidy(UVVR.veg)

UVVR.veg.sum


UVVR.veg.glance <- glance(UVVR.veg)

UVVR.veg.glance

#Page 3 - Predict the Linear Regression of the change in UVVR and Vegetated Area
    #Using ggpredict to create the prediction and 95% confidence interval
    #Confidence intervals are created by keeping the random effect fixed

Change.preds <- ggpredict(UVVR.veg, c("UVVR.change"), 
                          type = "fixed", interval = 'confidence') %>%
  rename(UVVR.change = x,
         Veg_PChange = predicted)

#page 4 - Export the predict values of the regression and the statistical output of the regression


write.csv(Change.preds, "E:\\Coastal Habitat Restoration Team\\ACJV Sites - RI_Mass_Maine\\Data Analysis\\Output R CSVs Analysis\\Veg_UVVR_Prediction.csv")

write.csv(UVVR.veg.anova, "E:\\Coastal Habitat Restoration Team\\ACJV Sites - RI_Mass_Maine\\Data Analysis\\Output R CSVs Analysis\\Veg_UVVR_Regression.csv")

write.csv(UVVR.veg.sum, "E:\\Coastal Habitat Restoration Team\\ACJV Sites - RI_Mass_Maine\\Data Analysis\\Output R CSVs Analysis\\Veg_UVVR_Regression_Table.csv")


#Page 4 - Graph the linear regression with ggplot()
  #The predicted values from ggpredict() are used to create the regression line and the confidence interval
    #ribbon

UVVR.Veg.Figure <- ggplot() +
  geom_point(data = Database.change,
             aes(x = UVVR.change, y = Veg_PChange, fill = Treatment),
             size = 4, shape = 21) + 
  geom_ribbon(data = Change.preds,
              aes(x = UVVR.change, ymax = conf.high, ymin = conf.low),
              colour = 'black', fill = 'grey60', alpha = 0.5, linetype = 2) +
  geom_line(data = Change.preds,
            aes(x = UVVR.change, y = Veg_PChange),
            size = 1.5, colour = 'black') + 
  scale_fill_manual(values = wes_palette("FantasticFox1", n = 3)) + 
  labs(x = "Change in UVVR Score", 
       y = "Change in Vegetated Area (%)") + 
  theme_bw() +
  theme(
    legend.position = "none",
    legend.text = element_text(size = 15, colour = "black"),
    legend.title = element_blank(),
    legend.background = element_rect(
      size = 0.5, linetype = "solid", colour = "black"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.title = element_text(size = 17, colour = "black"),
    axis.text = element_text(size = 17, colour = "black"),
    strip.text = element_text(size = 17, colour = "black"))

UVVR.Veg.Figure

rm(Change.preds, UVVR.veg.anova, UVVR.veg.glance, UVVR.veg.sum, Veg.Gains.Losses, 
   Veg.Gains.Losses.Site, Tideshed.pre, Tideshed.pre.sum, pre.anova.tidy, PreRestoration.Graph, PreRestoration.UVVR.Graph,
   PreRestoration.Veg.Graph)





















#_______________________________________________________________________________________________________________

#Chapter 8 - Impact of Initial Conditions on the Restoration Trajectory

  #To evaluate how initial marsh degradation may have had an impact on the degree of vegetation recovery
    # from runnels, we will focus on tidesheds with at least 5 years of post-restoration data
  #Sites include: Moody Marsh, Pine Island, Plum Island, Canochet, Jacobs Point, Middlebridge
                # Narrow Riv, Round Marsh, and Winnapaug


  #To do this, we will create mixed linear regressions (Site = random effect) for
    # 1) Percent Change in UVVR vs Initial UVVR Scores
    # 2) Percent Change in Vegetated Area vs Initial Percent Vegetated Area

  #Luckily, the percent change in vegetated area is essentially calculated for us. However, we will need to 
    #calculate the percent change in UVVR between the latest date prior to restoration to the latest date
    # post-restoration. Luckily, we already know what the latest date is for each tideshed from our
    # pre-restoration health analysis! 


#Page 1 - Create the data set for the analysis
  #We will filter the data set to the Runnel Tidesheds of the sites above ( >= 5 years post-restoration),
    # calculate the percent change in UVVR for all of the tidesheds, then filter down to only the tideshed
    # entries for the max date post-restoration


Database.Intl <- Database %>%
  filter(Treatment == "Runnel") %>%
    group_by(Tideshed_ID) %>%
      mutate(Timeline.Baseline = max(Timeline[Timeline <= 0]),
             
             UVVR.Baseline = UVVR[which(Timeline == Timeline.Baseline)],
             
             UVVR.Change = (UVVR - UVVR.Baseline) / Timeline,
                            
             UVVR.PChange = (((UVVR - UVVR.Baseline)/(UVVR.Baseline))/Timeline) * 100,
             
             Veg.Baseline = Veg_Area[which(Timeline == Timeline.Baseline)],
             
             Veg.PBaseline = (Veg.Baseline) / (Hectares) * 100, 
             
             Veg.Change = (Veg_Area - Veg.Baseline) / Timeline,
            
             Veg.PChange = ((((Veg_Area - Veg.Baseline)/(Veg.Baseline)) / Timeline) * 100 ),
             
             BaselineCondition = BaselineCondition) %>%
    
    filter(Timeline >= 5) %>%
    filter(Timeline == max(Timeline)) %>%
    filter(Tideshed_ID != 169) %>%
      mutate(UVVR.PChange = ifelse(UVVR.PChange == "NaN", 0, UVVR.PChange)) %>%
    select(Site, Tideshed, Tideshed_ID, Timeline, Hectares, BaselineCondition, Timeline.Baseline, UVVR, UVVR.Baseline, UVVR.Change, 
           UVVR.PChange, Veg_Area, Veg_Percent, Veg.Baseline, Veg.PBaseline, Veg.Change, Veg.PChange) %>%
ungroup()


write.csv(Database.Intl, "E:\\Coastal Habitat Restoration Team\\ACJV Sites - RI_Mass_Maine\\Data Analysis\\Output R CSVs Analysis\\Veg_UVVR_InitialConditions_Dataset.csv")




#Page 2 - Mixed Linear Regression of the Percent Change in UVVR Score

#Step 1 - Linear Regression and Output Stats for Raw UVVR Score
  #A heavily vegetated runnel tideshed in Round Marsh was removed due to a raw UVVR change in 0.02 resulted in
    #a percent change in ~+200%. It was removed as an outlier for this regression. 

UVVR.PChange.Model <- lmer(UVVR.PChange ~ UVVR.Baseline + (1|Site),
                          data = Database.Intl)


UVVR.PChange.Model.anova <- anova(UVVR.PChange.Model)

UVVR.PChange.Model.anova


UVVR.PChange.Model.tidy <- broom.mixed::tidy(UVVR.PChange.Model)

UVVR.PChange.Model.tidy


UVVR.PChange.Model.glance <- glance(UVVR.PChange.Model)

UVVR.PChange.Model.glance


write.csv(UVVR.PChange.Model.anova, "E:\\Coastal Habitat Restoration Team\\ACJV Sites - RI_Mass_Maine\\Data Analysis\\Output R CSVs Analysis\\Percent_UVVR_PChange_tidy.csv")

write.csv(UVVR.PChange.Model.tidy, "E:\\Coastal Habitat Restoration Team\\ACJV Sites - RI_Mass_Maine\\Data Analysis\\Output R CSVs Analysis\\Percent_UVVR_PChange_anova.csv")

write.csv(UVVR.PChange.Model.glance, "E:\\Coastal Habitat Restoration Team\\ACJV Sites - RI_Mass_Maine\\Data Analysis\\Output R CSVs Analysis\\Percent_UVVR_PChange_glance.csv")

rm(UVVR.PChange.Model.anova, UVVR.PChange.Model.tidy, UVVR.PChange.Model.glance)


#Step 2 - Predict the Linear Regression for graphing purposes


UVVR.PChange.preds <- ggpredict(UVVR.PChange.Model, c("UVVR.Baseline"), 
                               type = "fixed", interval = 'confidence') %>%
  rename(UVVR.Baseline.p = x,
         UVVR.PChange.p = predicted)


#Step 3 - Graph the mixed linear regression of the percent change in UVVR Score
  #Since the regression was non-significant, the regression line and ribbon are nto included in the graph

UVVR.PChange.graph <- ggplot() +
  geom_point(data = Database.Intl,
             aes(x = UVVR.Baseline, y = UVVR.PChange, fill = BaselineCondition),
             size = 6, shape = 21) + 
  labs(y = "Annual Change of UVVR (%)", 
       x = "Pre-restoration UVVR") + 
  scale_x_continuous(limits = c(0, 1), 
                     breaks = seq(0, 1, 0.2)) + 
  scale_y_continuous(limits = c(-20, 10),
                     breaks = seq(-20, 10, 5)) + 
  scale_fill_manual(values = c(wes_palette("FantasticFox1")[1], wes_palette("FantasticFox1")[3])) + 
  theme_bw() +
  theme(
    legend.position = c(0.88, 0.85),
    legend.text = element_text(size = 15),
    legend.title = element_blank(),
    legend.background = element_rect(
      size = 0.5, linetype = "solid", colour = "black"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.title = element_text(size = 17, colour = "black"),
    axis.text = element_text(size = 17, colour = "black"),
    strip.text = element_text(size = 17, colour = "black"))


UVVR.PChange.graph





#Page 3 - Mixed Linear Regression of the Percent Change in Vegetated Area

#Step 1 - Linear Regression and Output Stats for Percent Change in Vegetated Area

Veg.PChange.Model <- lmer(Veg.PChange ~ Veg.PBaseline + (1|Site),
                         data = Database.Intl)


Veg.PChange.Model.anova <- anova(Veg.PChange.Model)

Veg.PChange.Model.anova


Veg.PChange.Model.tidy <- broom.mixed::tidy(Veg.PChange.Model)

Veg.PChange.Model.tidy


Veg.PChange.Model.glance <- glance(Veg.PChange.Model)

Veg.PChange.Model.glance


write.csv(Veg.PChange.Model.anova, "E:\\Coastal Habitat Restoration Team\\ACJV Sites - RI_Mass_Maine\\Data Analysis\\Output R CSVs Analysis\\Percent_Veg_PChange_tidy.csv")

write.csv(Veg.PChange.Model.tidy, "E:\\Coastal Habitat Restoration Team\\ACJV Sites - RI_Mass_Maine\\Data Analysis\\Output R CSVs Analysis\\Percent_Veg_PChange_anova.csv")

write.csv(Veg.PChange.Model.glance, "E:\\Coastal Habitat Restoration Team\\ACJV Sites - RI_Mass_Maine\\Data Analysis\\Output R CSVs Analysis\\Percent_Veg_PChange_glance.csv")

rm(Veg.PChange.Model.anova, Veg.PChange.Model.glance, Veg.PChange.Model.tidy)


#Step 2 - Predict the Linear Regression for graphing purposes


Veg.PChange.preds <- ggpredict(Veg.PChange.Model, c("Veg.PBaseline"), 
                              type = "fixed", interval = 'confidence') %>%
  rename(Veg.PBaseline = x,
         Veg.PChange = predicted)


#Step 3 - Graph the mixed linear regression of the Percent PChange in vegetated area

Veg.PChange.graph <- ggplot() +
  geom_ribbon(data = Veg.PChange.preds,
              aes(x = Veg.PBaseline, ymax = conf.high, ymin = conf.low),
              colour = 'black', fill = 'grey60', alpha = 0.5, linetype = 2) +
  geom_line(data = Veg.PChange.preds,
            aes(x = Veg.PBaseline, y = Veg.PChange),
            size = 1.5, colour = 'black') + 
  geom_point(data = Database.Intl,
             aes(x = Veg.PBaseline, y = Veg.PChange, fill = BaselineCondition),
             size = 6, shape = 21) +
  labs(y = "Annual Change of Vegetated Area (%)", 
       x = "Pre-restoration Vegetated Area (%)") + 
  scale_x_continuous(limits = c(50, 100), 
                     breaks = seq(50, 100, 10)) + 
  scale_y_continuous(limits = c(-2, 6),
                     breaks = seq(-2, 6, 2)) +
  scale_fill_manual(values = c(wes_palette("FantasticFox1")[1], wes_palette("FantasticFox1")[3])) + 
  theme_bw() +
  theme(
    legend.position = "none",
    legend.text = element_text(size = 15),
    legend.title = element_blank(),
    legend.background = element_rect(
      size = 0.5, linetype = "solid", colour = "black"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.title = element_text(size = 17, colour = "black"),
    axis.text = element_text(size = 17, colour = "black"),
    strip.text = element_text(size = 17, colour = "black"))


Veg.PChange.graph



# Lastly, I am going to create one good compilation figure of the statistics for:
  # 1) Relationship of UVVR to Percent Vegetated Area
  # 2) Total Change in Vegetated Area
  # 3) Impact of Baseline UVVR on UVVR Recovery
  # 4) Impact of Baseline Vegetated Area on Vegetated Area Recovery


Stats.figure <- (Veg.Gains.Bar + UVVR.Veg.Figure) / (UVVR.PChange.graph + Veg.PChange.graph)

Stats.figure

ggsave(Stats.figure, height = 10, width = 17, dpi = 600,
       limitsize = FALSE, units = "in",
       filename = "E:\\Coastal Habitat Restoration Team\\ACJV Sites - RI_Mass_Maine\\Data Analysis\\Output R Figures\\Final Figures\\Manuscript\\Stats_Compilation.jpg")





rm(UVVR.Change.Model, UVVR.Change.Model.anova, UVVR.Change.Model.glance, UVVR.Change.Model.tidy,
   UVVR.change.preds, UVVR.Change.Raw.graph, UVVR.PChange.graph, UVVR.PChange.Model, UVVR.PChange.Model.anova,
   UVVR.PChange.Model.glance, UVVR.PChange.Model.tidy)

rm(Veg.Change.Model.anova, Veg.Change.Model.anova, Veg.Change.Model.glance, Veg.Change.Model.tidy, Veg.Change.Model,
   Veg.change.preds, Veg.Change.Raw.graph, Veg.PChange.Model.anova, Veg.PChange.Model.glance, Veg.PChange.Model.tidy, Veg.PChange.preds,
   Veg.PChange.Raw.graph, Veg.PChange.Model)


#_______________________________________________________________________________________________________


#Chapter 8: Creating Individual Graphs of UVVR for each Runnel Tideshed

#To first investigate what may be driving the variability in the runnel treatments, we need to visualize
  #the UVVR score and percent vegetated area for the runnel treatment for each site



#Page 1 - Create a timeline of runnel treatments for each site over the restoration timeframe for
  #UVVR scores with UVVR scores < 1.5 & Facet Graph

  #For the UVVR Graph, the sites are divided between those that have UVVR scores of < 1.5 (Timeline.Runnel)
      # and those with > 1.5 (Timeline.Runnel2)
  #The tidesheds that contribute to such high UVVR scores are those that are entirely pools at some point
  #Sites are: Plum Island, Potters Pond, Weekapaug Foundation, and Winnapaug Town Land

  #For the Veg Graphs, we will graph the Percent Area which is Vegetated, not the percent change
        
  #First, we will create the UVVR and Veg Percent Graphs for the sites with UVVR scores of < 1.5

#Step 1 - Create the timeline of runnel treatments of the restoration timeframe with tidesheds averaged
  #for similar runnel install dates within each site

Timeline.Runnel <- Database %>%
  filter(Treatment == "Runnel") %>%
    group_by(State, Runnel_Install, Timeline, State) %>%
      summarise(
        UVVR.m = sum(UVVR, na.rm = TRUE),
        UVVR.se = sd(UVVR, na.rm = TRUE)/sqrt(n()),
      
        
        Veg_Percent.m = sum(Veg_Percent, na.rm = TRUE),
        Veg_Percent.se = sd(Veg_Percent, na.rm = TRUE)/sqrt(n())) %>%
    ungroup() %>%
        rename(
          UVVR = UVVR.m,
          Veg_Percent = Veg_Percent.m) %>%
  filter(Site != "Plum Island" & Site != "Potters Pond" & Site != "Weekapaug Foundation" & Site != "Winnapaug Town Land")

glimpse(Timeline.Runnel)          

Timeline.Runnel$Runnel_Install <- as.character(Timeline.Runnel$Runnel_Install)



#Step 2 - Create a timeline of runnel treatments for each site over the restoration timeframe for
  #UVVR scores with UVVR scores < 1.5 & Facet Graph with tidesheds individually shown

  #For the UVVR Graph, the sites are divided between those that have UVVR scores of < 1.5 (Timeline.Runnel)
    # and those with > 1.5 (Timeline.Runnel2)
  #The tidesheds that contribute to such high UVVR scores are those that are entirely pools at some point
  #Sites are: Plum Island, Potters Pond, Weekapaug Foundation, and Winnapaug Town Land

# Create the timeline of runnel treatments of the restoration time frame with tidesheds individually
  # shown for similar runnel install dates within each site


Timeline.Tidesheds <- Database %>%
  mutate(Site_Tideshed = paste(Site, Tideshed, sep = " - ")) %>%
    filter(Site_Tideshed != "Plum Island - 8" & Site_Tideshed != "Potters Pond - 3" & 
           Site_Tideshed != "Weekapaug Foundation - 7" &  Site_Tideshed != "Winnapaug Town Land - 21" &
             Site_Tideshed != "Winnapaug Town Land - 22" & Site_Tideshed != "Moody Marsh - 11")



#Step 2 -  UVVR Graph for the Tidesheds Individually shown with UVVR scores > 1.5


Timeline.Tidesheds$Tideshed_ID <- as.character(Timeline.Tidesheds$Tideshed_ID)


UVVR.Tidesheds <- ggplot() +   
  geom_vline(xintercept = 0, size = 1, colour = "grey", 
             linetype = "dashed") + 
  geom_point(data = Timeline.Tidesheds,
             aes(x = Timeline, y = UVVR, fill = Treatment), 
             shape = 21, size = 5.5) +
  labs(x = "Age Relative to Restoration (yrs)", 
       y = "Average UVVR Score") + 
  scale_x_continuous(limits = c(-10.5, 8.5), 
                     breaks = seq(-10, 8, 2)) +
  scale_y_continuous(limits = c(0, 1.0),
                     breaks = seq(0, 1.0, 0.25)) +
  scale_fill_viridis("viridis", discrete = TRUE) + 
  
  theme_bw() +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.title = element_text(size = 20, colour = "black"),
    axis.text = element_text(size = 20, colour = "black"),
    strip.text = element_text(size = 16, colour = "black")) +
  facet_wrap_paginate(~Site_Tideshed, scales = "free",
                      ncol = 4, nrow = 3, page = 16)


UVVR.Tidesheds

ggsave(UVVR.Tidesheds, height = 9, width = 16, dpi = 300,
       limitsize = FALSE, units = "in",
       filename = "E:\\Coastal Habitat Restoration Team\\ACJV Sites - RI_Mass_Maine\\Data Analysis\\Output R Figures\\Final Figures\\Manuscript\\Runnel_Tidesheds_page16.jpg")



#Page 1 - Graph the tidesheds with UVVR scores > 1.0

#Lastly, there are a handful of sites with UVVR scores outside of the typical boundary of 0 - 1.0, so we will graph
  #these separately and allow for free scaling of the y-axis. To accomplish this, we will simply remove the
  #the scale_x_continuous() function of the ggplot


Timeline.Tidesheds <- Database %>%
  mutate(Site_Tideshed = paste(Site, Tideshed, sep = " - ")) %>%
  filter(Site_Tideshed == "Plum Island - 8" | Site_Tideshed == "Potters Pond - 3" | 
           Site_Tideshed == "Weekapaug Foundation - 7" |  Site_Tideshed == "Winnapaug Town Land - 21" |
           Site_Tideshed == "Winnapaug Town Land - 22" | Site_Tideshed == "Moody Marsh - 11")



#Step 2 -  UVVR Graph for the Tidesheds Individually shown with UVVR scores > 1.5


Timeline.Tidesheds$Tideshed_ID <- as.character(Timeline.Tidesheds$Tideshed_ID)


UVVR.Tidesheds <- ggplot() +   
  geom_vline(xintercept = 0, size = 1, colour = "grey", 
             linetype = "dashed") + 
  geom_point(data = Timeline.Tidesheds,
             aes(x = Timeline, y = UVVR, fill = Treatment), 
             shape = 21, size = 5.5) +
  labs(x = "Age Relative to Restoration (yrs)", 
       y = "Average UVVR Score") + 
  scale_x_continuous(limits = c(-10.5, 8.5), 
                     breaks = seq(-10, 8, 2)) +
  scale_fill_viridis("viridis", discrete = TRUE) + 
  
  theme_bw() +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.title = element_text(size = 20, colour = "black"),
    axis.text = element_text(size = 20, colour = "black"),
    strip.text = element_text(size = 16, colour = "black")) +
  facet_wrap_paginate(~Site_Tideshed, scales = "free",
                      ncol = 4, nrow = 3, page = 1)


UVVR.Tidesheds

ggsave(UVVR.Tidesheds, height = 9, width = 16, dpi = 300,
       limitsize = FALSE, units = "in",
       filename = "E:\\Coastal Habitat Restoration Team\\ACJV Sites - RI_Mass_Maine\\Data Analysis\\Output R Figures\\Final Figures\\Manuscript\\Runnel_Tidesheds_page17.jpg")



