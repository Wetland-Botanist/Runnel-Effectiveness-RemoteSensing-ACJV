#Project: Atlantic Coast Joint Venture
#Script: Geospatial Post-Analysis of Salt Marsh Health (UVVR & Vegetated Area)

#Author: Grant McKown, Research Assistant, (jgrantmck@gmail.com)


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











#---------------------------------------------------------------------------------------------------------------

# CHAPTER 2: STTING UP UVVR AND VEG AREA DATA FRAMES


# Page 1: Reading in the overarching UVVR - Veg Area Database
Database <- read.csv()


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
#The code then divides the vegetated area for each date by the earliest vegetated area (Percent Vegetated Area)
#Lastly, the code removes some pesky NAs from the database

Database.Veg <- Database.Veg %>%
  group_by(Site, Tideshed) %>%
  mutate(Veg_Earliest = Veg_Area[which.min(Timeline)]) %>%
  mutate(Veg_Percent = (Veg_Area / Veg_Earliest) * 100,
         Veg_Percent = round(Veg_Percent, 2)) %>%
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


#Export the new combined Database
#We will keep the Database.Veg and Database.UVVR for later analyses for simplicity

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
    Count = n() )  %>%
  mutate(
    Veg_Percent.avg = round(Veg_Percent.avg, 2),
    Veg_Percent.se = round(Veg_Percent.se, 2)) %>%
  ungroup()


glimpse(Timeline.Metrics)



#Export the Timeline Database

write.csv(Timeline.Metrics, "E:\\Coastal Habitat Restoration Team\\ACJV Sites - RI_Mass_Maine\\Data Analysis\\Output R CSVs Analysis\\ACJV_Overall_Timeline.csv")




Database.stats <- Database %>%
  group_by(Tideshed_ID) %>%
  filter(Timeline == min(Timeline)) %>%
  ungroup() %>%
  summarise(Area = sum(Veg_Area))





#_________________________________________________________________________________________________________

#Book 2: UVVR ANALYSIS

#___________________________________________________________________________________________________________


# CHAPTER 1: UVVR - Piecewise Linear Mixed Models

#Page 1 - Calculate the Piecewise Mixed Linear Regression for Pre- and Post-Restoration
#Provides useful information on model and to calculate slopes and intercepts for each treatment
#using the do(), glance(), and tidy() functions, the output will be a nice data frame of the stats for each model
#Using the broom.mixed package since it is a mixed effects linear model (lme4 object)
#Note: p-values are not calculated for mixed effects linear models!
#For the mixed model, UVVR is the response variable, Timeline and Treatment are fixed effects,
#and Site and Tideshed_ID are random effects. 
#Note: Individual tidesheds are uniquely ID'ed, rendering a nesting structure for the data obsolete


#Step 1 - Pre-Restoration models and calculations
UVVR.Pre.glance <- Database %>% 
  filter(Timeline <= 0) %>%
  do(broom.mixed::glance(lmer(UVVR ~ Timeline * Treatment + (1|Site) + (1|Tideshed_ID), 
                              data = .))) %>%
  mutate(Timeframe = "Pre") %>%
  ungroup()


UVVR.Pre.tidy <- Database %>% 
  filter(Timeline <= 0) %>%
  do(broom.mixed::tidy(lmer(UVVR ~ Timeline * Treatment + (1|Site) + (1|Tideshed_ID), 
                            data = .))) %>%
  mutate(Timeframe = "Pre") %>%
  ungroup()


UVVR.Pre.anova <- Database %>% 
  filter(Timeline <= 0) %>%
  do(as.data.frame(anova((lmer(UVVR ~ Timeline * Treatment + (1|Site) + (1|Tideshed_ID), 
                               data = .))))) %>%
  mutate(Timeframe = "Pre",
         coeff = rownames(.)) %>%
  ungroup()



#Step 2 - Post-restoration models and calculations

UVVR.Post.glance <- Database %>% 
  filter(Timeline >= 0, Treatment != "Ditch Remediation") %>%
  do(broom.mixed::glance(lmer(UVVR ~ Timeline * Treatment + (1|Site) + (1|Tideshed_ID), 
                              data = .))) %>%
  mutate(Timeframe = "Post") %>%
  ungroup()


UVVR.Post.tidy <- Database %>% 
  filter(Timeline >= 0, Treatment != "Ditch Remediation") %>%
  do(broom.mixed::tidy(lmer(UVVR ~ Timeline * Treatment + (1|Site) + (1|Tideshed_ID), 
                            data = .))) %>%
  mutate(Timeframe = "Post") %>%
  ungroup()


UVVR.Post.anova <- Database %>% 
  filter(Timeline >= 0) %>%
  do(as.data.frame(anova((lmer(UVVR ~ Timeline * Treatment + (1|Site) + (1|Tideshed_ID), 
                               data = .))))) %>%
  mutate(Timeframe = "Post",
         coeff = rownames(.)) %>%
  ungroup()


#Page 2: Combine & Export the results of the Pre- Post-restoration mixed models

# Step 1 - Bind the Pre- and Post- Piece Meal Linear Regressions to one data frame
UVVR.Linear.Piece.glance <- rbind(UVVR.Pre.glance, UVVR.Post.glance )

UVVR.Linear.Piece.tidy <- rbind(UVVR.Pre.tidy, UVVR.Post.tidy)

UVVR.Linear.Piece.anova <- rbind(UVVR.Pre.anova, UVVR.Post.anova)

glimpse(UVVR.Linear.Piece.glance)

glimpse(UVVR.Linear.Piece.tidy)

glimpse(UVVR.Linear.Piece.anova)


# Step 2 -  Export the Piece-meal UVVR Linear Regression Results to CSV File
write.csv(UVVR.Linear.Piece.glance, "E:\\Coastal Habitat Restoration Team\\ACJV Sites - RI_Mass_Maine\\Data Analysis\\Output R CSVs Analysis\\UVVR_Piecemeal_Linear_glance.csv")

write.csv(UVVR.Linear.Piece.tidy, "E:\\Coastal Habitat Restoration Team\\ACJV Sites - RI_Mass_Maine\\Data Analysis\\Output R CSVs Analysis\\UVVR_Piecemeal_Linear_tidy.csv")

write.csv(UVVR.Linear.Piece.anova, "E:\\Coastal Habitat Restoration Team\\ACJV Sites - RI_Mass_Maine\\Data Analysis\\Output R CSVs Analysis\\UVVR_Piecemeal_Linear_anova.csv")






# Step 3 - Remove the Linear Regressions Data frames to clean up Environment

rm(UVVR.Pre.tidy, UVVR.Pre.glance, UVVR.Post.glance, UVVR.Post.tidy, UVVR.Linear.Piece.glance, UVVR.Linear.Piece.tidy,
   UVVR.Linear.Piece.anova, UVVR.Post.anova, UVVR.Pre.anova)



#Page 4: Predict the means and Confidence Interval with ggpredict() function
#To calculate the Means and Confidence Interval, the random effects of Site & Tideshed were
#held constant at zero in order to calculate for just the Timeline & Treatment
#Holding the random effects at zero should not affect the Means of the fitted values, just the variance
# of the 

Piece.Linear.Pre <- lmer(UVVR ~ Timeline * Treatment + (1|Site) + (1|Tideshed_ID), 
                         data = filter(Database, Timeline <= 0))

Piece.Linear.Post <- lmer(UVVR ~ Timeline * Treatment + (1|Site) + (1|Tideshed_ID), 
                          data = filter(Database, Timeline >= 0, Treatment != "Ditch Remediation"))

Predict.Pre <- ggpredict(Piece.Linear.Pre, terms = c("Timeline", "Treatment"), 
                         type = "fixed", interval = "confidence")

Predict.Post <- ggpredict(Piece.Linear.Post, terms = c("Timeline", "Treatment"), 
                          type = "fixed", interval = "confidence")


Piece.Predict <- rbind(Predict.Pre, Predict.Post) %>%
  rename(Timeline = x,
         Treatment = group,
         UVVR = predicted) %>%
  arrange(Treatment) %>%
  slice(-which(Treatment == "Ditch Remediation" & Timeline > -1)) %>%
  mutate(conf.low = ifelse(conf.low < 0, 0, conf.low))

rm(Predict.Pre, Predict.Post)



#Page 5 - Graph the predicted piecewise linear mixed model for each treatment via the facet wrap function
#There are two data sets being graphed: Predicted values of mixed model and the mean values (with error bars) 
#The treatments are graphed in the following order: No Action, Reference, Ditch, and Runnel 
#These corresponds with basic R colors that correspond well with each treatment



Piece.Predict$Treatment <- factor(Piece.Predict$Treatment,
                                  levels = c("No Action", "Reference", "Ditch Remediation", "Runnel"))

Timeline.Metrics$Treatment <- factor(Timeline.Metrics$Treatment,
                                     levels = c("No Action", "Reference", "Ditch Remediation", "Runnel"))

Linear.Graph <- ggplot() +   
  geom_vline(xintercept = 0, size = 1, colour = "grey", 
             linetype = "dashed") + 
  geom_ribbon(data = Piece.Predict,
              aes(x = Timeline, ymin = conf.low, ymax = conf.high, fill = Treatment),
              alpha = 0.25) + 
  geom_line(data = Piece.Predict,
            aes(x = Timeline, y = UVVR, colour = Treatment),
            size = 1.5) + 
  geom_errorbar(data = Timeline.Metrics,
                aes(x = Timeline, ymin = UVVR.avg - UVVR.se, ymax = UVVR.avg + UVVR.se,
                    colour = Treatment),
                size = 0.75) +
  geom_point(data = Timeline.Metrics,
             aes(x = Timeline, y = UVVR.avg, fill = Treatment), 
             shape = 21, size = 5) +
  labs(x = "Age Relative to Restoration (yrs)", 
       y = "Average UVVR Score") + 
  scale_x_continuous(limits = c(-10.5, 8.5), 
                     breaks = seq(-10, 8, 2)) + 
  scale_y_continuous(limits = c(0, 0.90), 
                     breaks = seq(0, 0.90, 0.10)) +
  theme_bw() +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.title = element_text(size = 15, colour = "black"),
    axis.text = element_text(size = 15, colour = "black"),
    strip.text = element_text(size = 16, colour = "black")) +
  facet_wrap(~Treatment)


Linear.Graph 








#_________________________________________________________________________________________________________

#CHAPTER 2 - MIXED MODEL LINEAR REGRESSIONS OF UVVR

#Page 1 - Create Full Linear Model and Summary Data frames
#Mixed model is created with the lmer() function and the outputs of the mixed model are recorded with
#various broom.mixed package functions. Broom.mixed converts complex lme4 outputs to tables
#For the mixed model, UVVR is the response variable, Timeline and Treatment are fixed effects,
#and Site and Tideshed_ID are random effects. 
#Note: Individual tidesheds are uniquely ID'ed, rendering a nesting structure for the data obsolete


UVVR.Linear <- lmer(UVVR ~ Timeline * Treatment + (1|Site) + (1|Tideshed_ID), 
                    data = Database, na.action = na.omit)

UVVR.Linear.tidy <- broom.mixed::tidy(UVVR.Linear)

UVVR.Linear.anova <- as.data.frame(anova(UVVR.Linear)) %>%
  mutate(coeff = row.names(.))

UVVR.Linear.glance <- broom.mixed::glance(UVVR.Linear)


#Step 2 - Check the assumptions of homogeneity and normal distribution with visual inspection of residuals

#Assumption 1 - Linearity
#Plot residuals of the model and observed values

plot(resid(UVVR.Linear), Database$UVVR)


#Homogeneity of Variance

plot(UVVR.Linear)

levene_test()


#Residuals of the Model are Normally Distributed

qqmath(UVVR.Linear, id = 0.5)



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
  slice(-which(Treatment == "Ditch Remediation" & Timeline > -1)) %>%
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
                                 levels = c("No Action", "Reference", "Ditch Remediation", "Runnel"))

Linear.Graph <- ggplot() +   
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
                size = 0.75) +
  geom_point(data = Timeline.Metrics,
             aes(x = Timeline, y = UVVR.avg, fill = Treatment), 
             shape = 21, size = 5) +
  labs(x = "Age Relative to Restoration (yrs)", 
       y = "Average UVVR Score") + 
  scale_x_continuous(limits = c(-10.5, 8.5), 
                     breaks = seq(-10, 8, 2)) + 
  scale_y_continuous(limits = c(0, 0.90), 
                     breaks = seq(0, 0.90, 0.10)) +
  scale_color_manual(values = wes_palette("FantasticFox1", n = 4))
theme_bw() +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.title = element_text(size = 15, colour = "black"),
    axis.text = element_text(size = 15, colour = "black"),
    strip.text = element_text(size = 16, colour = "black")) + 
  facet_wrap(~Treatment)


Linear.Graph 
























#________________________________________________________________________________________________________________________________

#CHAPTER 3: UVVR - SPLINE REGRESSIONS

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
                    data = filter(Database, Treatment != "Ditch Remediation"), na.action = na.omit)

Spline.tidy <- broom.mixed::tidy(UVVR.Spline)

Spline.glance <- broom.mixed::glance(UVVR.Spline)

Spline.anova <- data.frame(anova(UVVR.Spline)) %>%
  mutate(coeff = rownames(.))


#Step 2 - Check the assumptions of homogeneity and normal distribution with visual inspection of residuals

#Assumption 1 - Linearity
#Plot residuals of the model and observed values

plot(resid(UVVR.Spline), Database$UVVR)


#Assumption 2 - Homogeneity of Variance

plot(UVVR.Spline)

levene_test()


#Assumption 3 - Residuals of the Model are Normally Distributed

qqmath(UVVR.Spline, id = 0.5)






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
                                 levels = c("No Action", "Reference", "Ditch Remediation", "Runnel"))

Spline.Graph <- ggplot() + 
  geom_vline(xintercept = 0, size = 1, colour = "grey", 
             linetype = "dashed") + 
  geom_ribbon(data = Spline.preds,
              aes(x = Timeline, ymin = conf.low, ymax = conf.high, fill = Treatment),
              alpha = 0.25) +
  geom_line(data = Spline.preds,
            aes(x = Timeline, y = UVVR, colour = Treatment),
            size = 1.5) +
  geom_errorbar(data = Timeline.Metrics,
                aes(x = Timeline, ymin = UVVR.avg - UVVR.se, ymax = UVVR.avg + UVVR.se,
                    colour = Treatment),
                size = 0.75) +
  geom_point(data = Timeline.Metrics,
             aes(x = Timeline, y = UVVR.avg, fill = Treatment), 
             shape = 21, size = 5.5) +
  labs(x = "Age Relative to Restoration (yrs)", 
       y = "Average UVVR Score") + 
  scale_x_continuous(limits = c(-10.5, 8.5), 
                     breaks = seq(-10, 8, 2)) + 
  scale_y_continuous(limits = c(0, 0.9), 
                     breaks = seq(0, 0.9, 0.1)) +
  theme_bw() +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.x = element_blank(),
    axis.title = element_text(size = 15, colour = "black"),
    axis.text = element_text(size = 15, colour = "black"),
    strip.text = element_text(size = 16, colour = "black"))+
  facet_wrap(~Treatment)

Spline.Graph



#Step 5 - Graph the Spline Mixed Model (with Ditch Remediation)

#The graph now includes the linear mixed model (predicted values and confidence interval) of the 
#Ditch Remediation Treatment for full visualization purposes

Spline.Graph <- ggplot() + 
  geom_vline(xintercept = 0, size = 1, colour = "grey", 
             linetype = "dashed") + 
  geom_errorbar(data = Timeline.Metrics,
                aes(x = Timeline, ymin = UVVR.avg - UVVR.se, ymax = UVVR.avg + UVVR.se,
                    colour = Treatment),
                size = 0.75) +
  geom_point(data = Timeline.Metrics,
             aes(x = Timeline, y = UVVR.avg, fill = Treatment), 
             shape = 21, size = 5.5) +
  geom_ribbon(data = Spline.preds,
              aes(x = Timeline, ymin = conf.low, ymax = conf.high, fill = Treatment),
              alpha = 0.25) +
  geom_line(data = filter(Linear.preds, Treatment == "Ditch Remediation"),
            aes(x = Timeline, y = UVVR, colour = Treatment),
            size = 1.5) + 
  geom_ribbon(data = filter(Linear.preds, Treatment == "Ditch Remediation"),
              aes(x = Timeline, ymin = conf.low, ymax = conf.high, fill = Treatment),
              alpha = 0.25) + 
  geom_line(data = Spline.preds,
            aes(x = Timeline, y = UVVR, colour = Treatment),
            size = 1.5) +
  labs(x = "Age Relative to Restoration (yrs)", 
       y = "Average UVVR Score") + 
  scale_x_continuous(limits = c(-10.5, 8.5), 
                     breaks = seq(-10, 8, 2)) + 
  scale_y_continuous(limits = c(0, 0.9), 
                     breaks = seq(0, 0.9, 0.1)) +
  theme_bw() +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.x = element_blank(),
    axis.title = element_text(size = 15, colour = "black"),
    axis.text = element_text(size = 15, colour = "black"),
    strip.text = element_text(size = 16, colour = "black"))+
  facet_wrap(~Treatment)

Spline.Graph











#_________________________________________________________________________________________________________________________________

#CHAPTER 4: UVVR - Univariate Statistics (ANOVA, Least Likliehood Ratio Test, etc.)


#Page 1 - Model Comparison: Compare Linear and Spline models to defend use of Spline
#We are using a Likelihood Ratio Test (via the anova() function) to compare Linear and Spline mixed models
#ability to account for the variance.
#Ditch Remediation treatment is not included in the Linear mixed model, since it can not be accounted for
#in the Spline model

UVVR.Linear.mod <- lmer(UVVR ~ Timeline * Treatment + (1|Site) + (1|Tideshed_ID), 
                        data = filter(Database, Treatment != "Ditch Remediation"), na.action = na.omit)


Model.compare <- anova(UVVR.Linear.mod, UVVR.Spline)

write.csv(Model.compare,  "E:\\Coastal Habitat Restoration Team\\ACJV Sites - RI_Mass_Maine\\Data Analysis\\Output R CSVs Analysis\\UVVR_LikliehoodTest_Models.csv")


#Page 2 - Determine if runnel restoration was applied to the most degraded tidesheds 
#One-way ANOVAs will be applied to the UVVR scores of all the treatment tidesheds at the latest
#pre-restoration time (e.g., -2 -> 0 yrs)
#Essentially, we want to see if the runnel restoration activities were applied to tidesheds that were
#more degraded than references and similar to no action tidesheds
#I am not exactly how to compare models with mixed models, so Site will be used as a fixed blocking effect
#in the one-way ANOVA. Tideshed and Timeline are not needed as covariate effects in the analysis

#Step 1 - Determine the latest pre-restoration date for each tideshed 
#Each tideshed had aerial imagery at some point from -2 -> 0 yrs before restoration


Tideshed.pre <- Database %>%
  group_by(Tideshed_ID) %>%
  filter(Timeline <= 0) %>%
  filter(Timeline == max(Timeline)) %>%
  ungroup()

Tideshed.pre$Treatment <- factor(Tideshed.pre$Treatment, 
                                 levels = c("No Action", "Reference", "Ditch Remediation", "Runnel"))


write.csv(Tideshed.pre, "E:\\Coastal Habitat Restoration Team\\ACJV Sites - RI_Mass_Maine\\Data Analysis\\Output R CSVs Analysis\\UVVR_Veg_PreRestoration_Baseline.csv" )


#Next, for reporting purposes, the summary statistics will be calculated for the Vegetated Area and UVVR

Tideshed.pre.sum <- Tideshed.pre %>%
  group_by(Treatment) %>%
  summarise(UVVR.avg = mean(UVVR),
            UVVR.se = sd(UVVR)/sqrt(n()),
            
            Veg.avg = mean(Veg_Percent),
            Veg.se = sd(Veg_Percent)/sqrt(n())) %>%
  ungroup()

Tideshed.pre.sum$Treatment <- factor(Tideshed.pre.sum$Treatment, 
                                     levels = c("No Action", "Reference", "Ditch Remediation", "Runnel"))

write.csv(Tideshed.pre.sum, "E:\\Coastal Habitat Restoration Team\\ACJV Sites - RI_Mass_Maine\\Data Analysis\\Output R CSVs Analysis\\UVVR_Veg_PreRestoration_Baseline_SumStats.csv" )



#Step 2 - Comparison of the Intercepts at Year = 0 for the Linear and Spline Models as a gut check
#First, re-calculate the predicted values of the Linear Regression to calculate intercept of
#Ditch Remediation at Timeline = 0
#Pull out the intercepts of the other treatments in the Spline predicted values

Ditch.Lin.intercept <- ggpredict(UVVR.Linear, c("Timeline", "Treatment"), 
                                 type = "fixed", interval = 'confidence') %>%
  rename(Timeline = x,
         Treatment = group,
         UVVR = predicted) %>%
  filter(Timeline == 0, Treatment == "Ditch Remediation") %>%
  select(Treatment, UVVR)


Spline.intercepts <- Spline.preds %>%
  filter(Timeline == 0) %>%
  select(Treatment, UVVR)




#Step 3 - One-way ANOVA with blocking factor of Site
#Tukey Test with the TukeyHSD() function is applied to the model for pairwise comparisons between treatments
#Results of the model and Tukey Test are exported to csvs

anova <- lm(UVVR ~ Treatment + Site, data = Tideshed.pre)

pre.anova <- tidy(anova(anova))

pre.anova

tukey.test <- TukeyHSD(aov(anova))

Tukey.test <- tukey.test$Treatment

Tukey.test

write.csv(pre.anova, "E:\\Coastal Habitat Restoration Team\\ACJV Sites - RI_Mass_Maine\\Data Analysis\\Output R CSVs Analysis\\UVVR_PreRestoration_ANOVA.csv")

write.csv(Tukey.test, "E:\\Coastal Habitat Restoration Team\\ACJV Sites - RI_Mass_Maine\\Data Analysis\\Output R CSVs Analysis\\UVVR_Prerestoration_ANOVA_Tukey.csv")


rm(anova, pre.anova, Tukey.test, Spline.intercepts, Ditch.Lin.intercept, Tideshed.pre, tukey.test)

rm(Spline.Graph, Spline.preds, Spline.Slope, Model.compare, Model.Verify, Piece.predict, Piece.Predict,
   UVVR.Linear.mod, Veg.Pre.glance, Linear.Predict)


#Step 4 - Visualize the One-Way ANOVA

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
























#___________________________________________________________________________________________________________

#Book 3: Vegetated Area Analysis

#___________________________________________________________________________________________________________

# CHAPTER 1: Vegetated Area - Piecewise Linear Mixed Model


#Page 1 - Calculate the Piecewise Mixed Linear Regression for Pre- and Post-Restoration
#Provides useful information on model and to calculate slopes and intercepts for each treatment
#using the do(), glance(), and tidy() functions, the output will be a nice data frame of the stats for each model
#Using the broom.mixed package since it is a mixed effects linear model (lme4 object)
#Note: p-values are not calculated for mixed effects linear models!
#For the mixed model, UVVR is the response variable, Timeline and Treatment are fixed effects,
#and Site and Tideshed_ID are random effects. 
#Note: Individual tidesheds are uniquely ID'ed, rendering a nesting structure for the data obsolete


#Step 1 - Pre-Restoration models and calculations
Veg.Pre.glance <- Database %>% 
  filter(Timeline <= 0) %>%
  do(broom.mixed::glance(lmer(Veg_Percent ~ Timeline * Treatment + (1|Site) + (1|Tideshed_ID), 
                              data = .))) %>%
  mutate(Timeframe = "Pre") %>%
  ungroup()


Veg.Pre.tidy <- Database %>% 
  filter(Timeline <= 0) %>%
  do(broom.mixed::tidy(lmer(Veg_Percent ~ Timeline * Treatment + (1|Site) + (1|Tideshed_ID), 
                            data = .))) %>%
  mutate(Timeframe = "Pre") %>%
  ungroup()


Veg.Pre.anova <- Database %>% 
  filter(Timeline <= 0) %>%
  do(as.data.frame(anova((lmer(Veg_Percent ~ Timeline * Treatment + (1|Site) + (1|Tideshed_ID), 
                               data = .))))) %>%
  mutate(Timeframe = "Pre",
         coeff = rownames(.)) %>%
  ungroup()



#Step 2 - Post-restoration models and calculations

Veg.Post.glance <- Database %>% 
  filter(Timeline >= 0, Treatment != "Ditch Remediation") %>%
  do(broom.mixed::glance(lmer(Veg_Percent ~ Timeline * Treatment + (1|Site) + (1|Tideshed_ID), 
                              data = .))) %>%
  mutate(Timeframe = "Post") %>%
  ungroup()


Veg.Post.tidy <- Database %>% 
  filter(Timeline >= 0, Treatment != "Ditch Remediation") %>%
  do(broom.mixed::tidy(lmer(Veg_Percent ~ Timeline * Treatment + (1|Site) + (1|Tideshed_ID), 
                            data = .))) %>%
  mutate(Timeframe = "Post") %>%
  ungroup()


Veg.Post.anova <- Database %>% 
  filter(Timeline >= 0) %>%
  do(as.data.frame(anova((lmer(Veg_Percent ~ Timeline * Treatment + (1|Site) + (1|Tideshed_ID), 
                               data = .))))) %>%
  mutate(Timeframe = "Post",
         coeff = rownames(.)) %>%
  ungroup()


#Page 2: Combine & Export the results of the Pre- Post-restoration mixed models

# Step 1 - Bind the Pre- and Post- Piece Meal Linear Regressions to one data frame
Veg.Linear.Piece.glance <- rbind(Veg.Pre.glance, Veg.Post.glance )

Veg.Linear.Piece.tidy <- rbind(Veg.Pre.tidy, Veg.Post.tidy)

Veg.Linear.Piece.anova <- rbind(Veg.Pre.anova, Veg.Post.anova)

glimpse(Veg.Linear.Piece.glance)

glimpse(Veg.Linear.Piece.tidy)

glimpse(Veg.Linear.Piece.anova)


# Step 2 -  Export the Piece-meal UVVR Linear Regression Results to CSV File
write.csv(Veg.Linear.Piece.glance, "E:\\Coastal Habitat Restoration Team\\ACJV Sites - RI_Mass_Maine\\Data Analysis\\Output R CSVs Analysis\\Veg_Piecemeal_Linear_glance.csv")

write.csv(Veg.Linear.Piece.tidy, "E:\\Coastal Habitat Restoration Team\\ACJV Sites - RI_Mass_Maine\\Data Analysis\\Output R CSVs Analysis\\Veg_Piecemeal_Linear_tidy.csv")

write.csv(Veg.Linear.Piece.anova, "E:\\Coastal Habitat Restoration Team\\ACJV Sites - RI_Mass_Maine\\Data Analysis\\Output R CSVs Analysis\\Veg_Piecemeal_Linear_anova.csv")






# Step 3 - Remove the Linear Regressions Data frames to clean up Environment

rm(Veg.Pre.tidy, Veg.Pre.glance, Veg.Post.glance, Veg.Post.tidy, Veg.Linear.Piece.glance, Veg.Linear.Piece.tidy,
   Veg.Linear.Piece.anova, Veg.Post.anova, Veg.Pre.anova)



#Page 4: Predict the means and Confidence Interval with ggpredict() function
#To calculate the Means and Confidence Interval, the random effects of Site & Tideshed were
#held constant at zero in order to calculate for just the Timeline & Treatment
#Holding the random effects at zero should not affect the Means of the fitted values, just the variance
# of the 

Piece.Linear.Pre <- lmer(Veg_Percent ~ Timeline * Treatment + (1|Site) + (1|Tideshed_ID), 
                         data = filter(Database, Timeline <= 0))

Piece.Linear.Post <- lmer(Veg_Percent ~ Timeline * Treatment + (1|Site) + (1|Tideshed_ID), 
                          data = filter(Database, Timeline >= 0, Treatment != "Ditch Remediation"))

Predict.Pre <- ggpredict(Piece.Linear.Pre, terms = c("Timeline", "Treatment"), 
                         type = "fixed", interval = "confidence")

Predict.Post <- ggpredict(Piece.Linear.Post, terms = c("Timeline", "Treatment"), 
                          type = "fixed", interval = "confidence")


Piece.Predict <- rbind(Predict.Pre, Predict.Post) %>%
  rename(Timeline = x,
         Treatment = group,
         Veg_Percent = predicted) %>%
  arrange(Treatment) %>%
  slice(-which(Treatment == "Ditch Remediation" & Timeline > -1)) %>%
  mutate(conf.low = ifelse(conf.low < 0, 0, conf.low))

rm(Predict.Pre, Predict.Post)



#Page 5 - Graph the predicted piecewise linear mixed model for each treatment via the facet wrap function
#There are two data sets being graphed: Predicted values of mixed model and the mean values (with error bars) 
#The treatments are graphed in the following order: No Action, Reference, Ditch, and Runnel 
#These corresponds with basic R colors that correspond well with each treatment



Piece.Predict$Treatment <- factor(Piece.Predict$Treatment,
                                  levels = c("No Action", "Reference", "Ditch Remediation", "Runnel"))

Timeline.Metrics$Treatment <- factor(Timeline.Metrics$Treatment,
                                     levels = c("No Action", "Reference", "Ditch Remediation", "Runnel"))

Linear.Graph <- ggplot() +   
  geom_vline(xintercept = 0, size = 1, colour = "grey", 
             linetype = "dashed") + 
  geom_ribbon(data = Piece.Predict,
              aes(x = Timeline, ymin = conf.low, ymax = conf.high, fill = Treatment),
              alpha = 0.25) + 
  geom_line(data = Piece.Predict,
            aes(x = Timeline, y = Veg_Percent, colour = Treatment),
            size = 1.5) + 
  geom_errorbar(data = Timeline.Metrics,
                aes(x = Timeline, ymin = Veg_Percent.avg - Veg_Percent.se, ymax = Veg_Percent.avg + Veg_Percent.se,
                    colour = Treatment),
                size = 0.75) +
  geom_point(data = Timeline.Metrics,
             aes(x = Timeline, y = Veg_Percent.avg, fill = Treatment), 
             shape = 21, size = 5) +
  labs(x = "Age Relative to Restoration (yrs)", 
       y = "Percent of Vegetated Area (%)") + 
  scale_x_continuous(limits = c(-10.5, 8.5), 
                     breaks = seq(-10, 8, 2)) + 
  scale_y_continuous(limits = c(85, 137), 
                     breaks = seq(85, 137, 10)) +
  theme_bw() +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.title = element_text(size = 15, colour = "black"),
    axis.text = element_text(size = 15, colour = "black"),
    strip.text = element_text(size = 16, colour = "black")) +
  facet_wrap(~Treatment)


Linear.Graph 











#_________________________________________________________________________________________________________

#CHAPTER 2 - MIXED MODEL LINEAR REGRESSIONS of Vegetated Area

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

levene_test()


#Assumption 3 - Residuals of the Model are Normally Distributed

qqmath(Veg.Linear, id = 0.5)


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
  slice(-which(Treatment == "Ditch Remediation" & Timeline > -1)) %>%
  arrange(Treatment)



#Page 4 - Calculate the Slope of Linear Mixed Model with predicted values
#Using the predicted mean values, the slopes of each treatment can be calculated

Linear.Slope <- Linear.preds %>%
  group_by(Treatment) %>%
  summarise(slope = (Veg_Percent[which.max(Timeline)] - Veg_Percent[which.min(Timeline)]) / (max(Timeline) - min(Timeline)) ) %>%
  ungroup()

write.csv(Linear.Slope, "E:\\Coastal Habitat Restoration Team\\ACJV Sites - RI_Mass_Maine\\Data Analysis\\Output R CSVs Analysis\\Veg_Linear_Slopes.csv")



#Page 5 - Graph the predicted linear mixed model for each treatment via the facet wrap function
#There are two data sets being graphed: Predicted values of mixed model and the mean values (with error bars) 
#The treatments are graphed in the following order: No Action, Reference, Ditch, and Runnel 
#These corresponds with basic R colors that correspond well with each treatment


Linear.preds$Treatment <- factor(Linear.preds$Treatment,
                                 levels = c("No Action", "Reference", "Ditch Remediation", "Runnel"))

Linear.Graph <- ggplot() +   
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
                size = 0.75) +
  geom_point(data = Timeline.Metrics,
             aes(x = Timeline, y = Veg_Percent.avg, fill = Treatment), 
             shape = 21, size = 5) +
  labs(x = "Age Relative to Restoration (yrs)", 
       y = "Percent of Vegetated Area (%)") + 
  scale_x_continuous(limits = c(-10.5, 8.5), 
                     breaks = seq(-10, 8, 2)) + 
  scale_y_continuous(limits = c(85, 137), 
                     breaks = seq(85, 137, 10)) +
  theme_bw() +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.title = element_text(size = 15, colour = "black"),
    axis.text = element_text(size = 15, colour = "black"),
    strip.text = element_text(size = 16, colour = "black")) +
  facet_wrap(~Treatment)


Linear.Graph 






















#________________________________________________________________________________________________________________________________

#CHAPTER 3: Vegetated Area - SPLINE REGRESSIONS

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
                   data = filter(Database, Treatment != "Ditch Remediation"), na.action = na.omit)

Spline.tidy <- broom.mixed::tidy(Veg.Spline)

Spline.glance <- broom.mixed::glance(Veg.Spline)

Spline.anova <- data.frame(anova(Veg.Spline)) %>%
  mutate(coeff = rownames(.))



#Step 2 - Check the assumptions of homogeneity and normal distribution with visual inspection of residuals

#Assumption 1 - Linearity
#Plot residuals of the model and observed values

plot(resid(Veg.Spline), Database$Veg_Percent)


#Assumption 2 - Homogeneity of Variance

plot(Veg.Spline)

levene_test()


#Assumption 3 - Residuals of the Model are Normally Distributed

qqmath(Veg.Spline, id = 0.5)



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


#Page 4 - Calcualte the Slopes of the Spline Models with predicted values
#Using the predicted mean values, the slopes of each treatment can be calculated pre- and post-restoration

Spline.Slope <- Spline.preds %>%
  group_by(Treatment) %>%
  summarise(slope.pre = (Veg_Percent[which(Timeline == 0)] - Veg_Percent[which(Timeline == min(Timeline))]) / (0 - min(Timeline)),
            
            slope.post = (Veg_Percent[which(Timeline == max(Timeline))] - Veg_Percent[which(Timeline == min(Timeline = 0))]) / (max(Timeline) - 0)) %>%
  ungroup()

write.csv(Spline.Slope, "E:\\Coastal Habitat Restoration Team\\ACJV Sites - RI_Mass_Maine\\Data Analysis\\Output R CSVs Analysis\\Veg_Spline_Slopes.csv")


# Page 5 - Graph the mixed Spline Model (without Ditch Remediation) with facet wrap function

#There are two data sets being graphed: Predicted values of mixed model and the mean values (with error bars) 
#The treatments are graphed in the following order: No Action, Reference, Ditch, and Runnel 
#These corresponds with basic R colors that correspond well with each treatment


Spline.preds$Treatment <- factor(Spline.preds$Treatment,
                                 levels = c("No Action", "Reference", "Ditch Remediation", "Runnel"))

Spline.Graph <- ggplot() + 
  geom_vline(xintercept = 0, size = 1, colour = "grey", 
             linetype = "dashed") + 
  geom_ribbon(data = Spline.preds,
              aes(x = Timeline, ymin = conf.low, ymax = conf.high, fill = Treatment),
              alpha = 0.25) +
  geom_line(data = Spline.preds,
            aes(x = Timeline, y = Veg_Percent, colour = Treatment),
            size = 1.5) +
  geom_errorbar(data = Timeline.Metrics,
                aes(x = Timeline, ymin = Veg_Percent.avg - Veg_Percent.se, ymax = Veg_Percent.avg + Veg_Percent.se,
                    colour = Treatment),
                size = 0.75) +
  geom_point(data = Timeline.Metrics,
             aes(x = Timeline, y = Veg_Percent.avg, fill = Treatment), 
             shape = 21, size = 5.5) +
  labs(x = "Age Relative to Restoration (yrs)", 
       y = "Percent of Vegetated Area (%)") + 
  scale_x_continuous(limits = c(-10.5, 8.5), 
                     breaks = seq(-10, 8, 2)) + 
  scale_y_continuous(limits = c(85, 137), 
                     breaks = seq(85, 137, 10)) +
  theme_bw() +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.title = element_text(size = 15, colour = "black"),
    axis.text = element_text(size = 15, colour = "black"),
    strip.text = element_text(size = 16, colour = "black")) +
  facet_wrap(~Treatment)


Linear.Graph 




#Step 5 - Graph the Spline Mixed Model (with Ditch Remediation)

#The graph now includes the linear mixed model (predicted values and confidence interval) of the 
#Ditch Remediation Treatment for full visualization purposes

Spline.Graph <- ggplot() + 
  geom_vline(xintercept = 0, size = 1, colour = "grey", 
             linetype = "dashed") + 
  geom_errorbar(data = Timeline.Metrics,
                aes(x = Timeline, ymin = Veg_Percent.avg - Veg_Percent.se, ymax = Veg_Percent.avg + Veg_Percent.se,
                    colour = Treatment),
                size = 0.75) +
  geom_point(data = Timeline.Metrics,
             aes(x = Timeline, y = Veg_Percent.avg, fill = Treatment), 
             shape = 21, size = 5.5) +
  geom_ribbon(data = Spline.preds,
              aes(x = Timeline, ymin = conf.low, ymax = conf.high, fill = Treatment),
              alpha = 0.25) +
  geom_line(data = filter(Linear.preds, Treatment == "Ditch Remediation"),
            aes(x = Timeline, y = Veg_Percent, colour = Treatment),
            size = 1.5) + 
  geom_ribbon(data = filter(Linear.preds, Treatment == "Ditch Remediation"),
              aes(x = Timeline, ymin = conf.low, ymax = conf.high, fill = Treatment),
              alpha = 0.25) + 
  geom_line(data = Spline.preds,
            aes(x = Timeline, y = Veg_Percent, colour = Treatment),
            size = 1.5) +
  labs(x = "Age Relative to Restoration (yrs)", 
       y = "Percent of Vegetated Area (%)") + 
  scale_x_continuous(limits = c(-10.5, 8.5), 
                     breaks = seq(-10, 8, 2)) + 
  scale_y_continuous(limits = c(85, 137), 
                     breaks = seq(85, 137, 10)) +
  theme_bw() +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.title = element_text(size = 15, colour = "black"),
    axis.text = element_text(size = 15, colour = "black"),
    strip.text = element_text(size = 16, colour = "black")) +
  facet_wrap(~Treatment)


Linear.Graph 









#_________________________________________________________________________________________________________________________________

#CHAPTER 4: Vegetated Area - Univariate Statistics (ANOVA, Least Liklihood Ratio Test, etc.)


#Page 1 - Model Comparison: Compare Linear and Spline models to defend use of Spline
#We are using a Likelihood Ratio Test (via the anova() function) to compare Linear and Spline mixed models
#ability to account for the variance.
#Ditch Remediation treatment is not included in the Linear mixed model, since it can not be accounted for
#in the Spline model

Veg.Linear.mod <- lmer(Veg_Percent ~ Timeline * Treatment + (1|Site) + (1|Tideshed_ID), 
                       data = filter(Database, Treatment != "Ditch Remediation"), na.action = na.omit)


Model.compare <- anova(Veg.Linear.mod, Veg.Spline)

write.csv(Model.compare,  "E:\\Coastal Habitat Restoration Team\\ACJV Sites - RI_Mass_Maine\\Data Analysis\\Output R CSVs Analysis\\Veg_LikliehoodTest_Models.csv")


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



#Step 2 - Comparison of the Intercepts at Year = 0 for the Linear and Spline Models as a gut check
#First, re-calculate the predicted values of the Linear Regression to calculate intercept of
#Ditch Remediation at Timeline = 0
#Pull out the intercepts of the other treatments in the Spline predicted values

Ditch.Lin.intercept <- ggpredict(UVVR.Linear, c("Timeline", "Treatment"), 
                                 type = "fixed", interval = 'confidence') %>%
  rename(Timeline = x,
         Treatment = group,
         UVVR = predicted) %>%
  filter(Timeline == 0, Treatment == "Ditch Remediation") %>%
  select(Treatment, UVVR)


Spline.intercepts <- Spline.preds %>%
  filter(Timeline == 0) %>%
  select(Treatment, UVVR)




#Step 3 - One-way ANOVA with blocking factor of Site
#Tukey Test with the TukeyHSD() function is applied to the model for pairwise comparisons between treatments
#Results of the model and Tukey Test are exported to csvs

pre.anova <- lm(Veg_Percent ~ Treatment + Site, data = Tideshed.pre)

pre.anova.tidy <- tidy(anova(anova))

pre.anova$model


pre.anova

tukey.test <- TukeyHSD(aov(anova))

Tukey.test <- tukey.test$Treatment

Tukey.test

write.csv(pre.anova, "E:\\Coastal Habitat Restoration Team\\ACJV Sites - RI_Mass_Maine\\Data Analysis\\Output R CSVs Analysis\\Veg_PreRestoration_ANOVA.csv")

write.csv(Tukey.test, "E:\\Coastal Habitat Restoration Team\\ACJV Sites - RI_Mass_Maine\\Data Analysis\\Output R CSVs Analysis\\Veg_Prerestoration_ANOVA_Tukey.csv")


rm(anova, pre.anova, Tukey.test, Spline.intercepts, Ditch.Lin.intercept, Tideshed.pre, tukey.test)

rm(Spline.Graph, Spline.preds, Spline.Slope, Model.compare, Model.Verify, Piece.predict, Piece.Predict,
   UVVR.Linear.mod, Veg.Pre.glance, Linear.Predict)


#Step 4 - Visualize the One-Way ANOVA
#The calculations for the Percent Vegetated Area were completed in the One-Way ANOVA calculations for UVVR

PreRestoration.Veg.Graph <- ggplot() + 
  geom_boxplot(data = Tideshed.pre,
               aes(x = Treatment, y = Veg_Percent, fill = Treatment),
               size = 0.75) +
  labs(x = '',
       y = "Percent of Vegetated Area") + 
  scale_y_continuous(limits = c(80, 120), breaks = seq(80, 120, 10)) +
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
















#-------------------------------------------------------------------------------------------------------

#CHAPTER 5: DETERMINE TOTAL VETETATED AREA LOSS/GAINS SINCE 2010

#Page 1: Calculate Percent Vegetated Area Pre- and Post-Restoration
#In Pre-Restoration, the vegetated area of years -12 -> 0, are divided by the vegetated area of the earliest year
#In Post-Restoration, the vegetated area of years 1 --> 10, are divided by the vegetated area of the last year in pre-restoration

#Pre-Restoration Calculations
Database.Veg.Pre <- Database %>%
  filter(Timeline <=  0 & Timeline >= -10) %>%
  group_by(Site, Tideshed) %>%
  mutate(Veg_Earliest = Veg_Area[which.min(Timeline)]) %>%
  mutate(Veg_Percent = (Veg_Area / Veg_Earliest) * 100) %>%
  ungroup(Site, Tideshed)


glimpse(Database.Veg.Pre)  

#Post-Restoration Calculations

Database.Veg.Post <- Database %>%
  filter(Timeline >= -2) %>%
  group_by(Site, Tideshed) %>%
  mutate(Veg_Earliest = ifelse(sum(Timeline == 0) == 1, 
                               Veg_Area[Timeline == 0], Veg_Area[which.min(Timeline)])) %>%
  mutate(Veg_Percent = ((Veg_Area/Veg_Earliest) * 100)) %>%
  ungroup(Site, Tideshed) %>%
  filter(Timeline > 0)


glimpse(Database.Veg.Post)


#Step 2: Bind the Veg Databases together

Database.Veg <- rbind(Database.Veg.Pre, Database.Veg.Post)

glimpse(Database.Veg)


#Page 2: Calculate the Percent Change in Vegetated Area Pre- and Post-restoration

#Step 1: Calculate the Percent Change and Gains/Loss of Vegetated Area

Database.Veg <- Database.Veg %>%
  mutate(Veg_Change = ((Veg_Area - Veg_Earliest)/Veg_Earliest) * 100) %>%
  mutate(Veg_Gains = (Veg_Change/100) * Veg_Earliest)

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

#Pre-restoration
Database.Veg.Pre <- Database.Veg %>%
  filter(Timeline <= 0) %>%
  group_by(Treatment) %>%
  summarise(
    Hectares = sum(Veg_Gains),
    Acres = Hectares / 0.405,
    Count = n()) %>%
  mutate(Timeline = "Pre-restoration") %>%
  ungroup()

#Post-restoration
Database.Veg.Post <- Database.Veg %>%
  filter(Timeline > 0) %>%
  group_by(Treatment) %>%
  summarise(
    Hectares = sum(Veg_Gains),
    Acres = Hectares / 0.405,
    Count = n()) %>%
  mutate(Timeline = "Post-restoration") %>%
  ungroup()

#Step 6: Bind the two data frames back together

Veg.Gains.Losses <- rbind(Database.Veg.Pre, Database.Veg.Post)


#Step 7: Remove the pre- and post-restoration data frames

rm("Database.Veg.Pre", "Database.Veg.Post")

#Step 8: Export the Veg Gains/Losses Table to Excel

write.csv(Veg.Gains.Losses, "E:\\Coastal Habitat Restoration Team\\ACJV Sites - RI_Mass_Maine\\Data Analysis\\Output R CSVs Analysis\\Veg_Gains_Losses.csv")


#Page 4: Bar Graph of the Vegetated Gains and Losses 

Veg.Gains.Losses$Treatment <- factor(Veg.Gains.Losses$Treatment, levels = c("No Action", "Reference", "Ditch Remediation", "Runnel"))

Veg.Gains.Losses$Timeline <- factor(Veg.Gains.Losses$Timeline, levels = c("Pre-restoration", "Post-restoration"))


Veg.Gains.Bar <- ggplot(Veg.Gains.Losses, 
                        aes(x = Timeline, y = Hectares, fill  = Treatment)) + 
  geom_bar(position = position_dodge(), stat = "identity", colour = "black", size = 1) +
  geom_text(aes(label = Count, y = Hectares - 0.2), position = position_dodge(0.9), fontface ="bold", size = 6, colour = "black") +
  labs(x = "", y = "Change in Vegetated Marsh Area (ha)") +
  scale_y_continuous(limits = c(-5, 5), breaks = seq(-5, 5, 1)) +
  theme_bw() +
  theme(
    legend.position = c(0.08,0.90),
    legend.background = element_rect(
      size = 0.5, linetype = "solid", colour = "black" ),
    legend.text = element_text(size = 15, colour = "black"),
    legend.title = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.title = element_text(size = 17.5, colour = "black"),
    axis.text.y = element_text(size = 15, colour = "black"),
    axis.text.x = element_text(size = 17.5, colour = "black"))

Veg.Gains.Bar



rm("Veg.Gains.Bar")  



#Step 9 Calculate the Vegetated Area Gains/Loss for Site for Each Timeframe

#Pre-restoration
Database.Veg.Pre <- Database.Veg %>%
  filter(Timeline <= 0) %>%
  group_by(Site, Treatment) %>%
  summarise(
    Hectares = sum(Veg_Gains),
    Acres = Hectares / 0.405,
    Count = n()) %>%
  mutate(Timeline = "Pre-restoration") %>%
  ungroup()

#Post-restoration
Database.Veg.Post <- Database.Veg %>%
  filter(Timeline > 0) %>%
  group_by(Site, Treatment) %>%
  summarise(
    Hectares = sum(Veg_Gains),
    Acres = Hectares / 0.405,
    Count = n()) %>%
  mutate(Timeline = "Post-restoration") %>%
  ungroup()

#Step 6: Bind the two data frames back together

Veg.Gains.Losses.Site <- rbind(Database.Veg.Pre, Database.Veg.Post)

Veg.Gains.Losses.Site <- Veg.Gains.Losses.Site %>%
  select(-Acres)%>%
  spread(Timeline, Hectares )


#Step 7: Remove the pre- and post-restoration data frames

rm("Database.Veg.Pre", "Database.Veg.Post")

#Step 8: Export the Veg Gains/Losses Table to Excel

write.csv(Veg.Gains.Losses.Site, "E:\\Coastal Habitat Restoration Team\\ACJV Sites - RI_Mass_Maine\\Data Analysis\\Output R CSVs Analysis\\Veg_Gains_Losses_Site.csv")












#Chapter 6: Relationship between the change in UVVR and Percent Vegetated Area



#Page 1 - Calculate and Graph the Change in Percent Vegetated Area and the Change in UVVR based on comparisons to the Earliest Date

Database.change <- Database %>%
  mutate(UVVR.change = UVVR - UVVR_Earliest,
         Veg.change = Veg_Percent - 100) %>%
  group_by(Tideshed_ID) %>%
  filter(Timeline == max(Timeline)) %>%
  ungroup()




#Linear Mixed Model
#The linear mixed model will include Site and Tideshed_ID as random effects
#Outliers of UVVR change > 1.0 will be removed from the analysis

UVVR.veg <- lmer(Veg_Percent ~ UVVR + (1|Site),
                 data = filter(Database, UVVR <= 3))

UVVR.veg.anova <- anova(UVVR.veg)

UVVR.veg.sum <- broom.mixed::tidy(UVVR.veg)

UVVR.veg.glance <- glance(UVVR.veg)



Change.preds <- ggpredict(UVVR.veg, c("UVVR"), 
                          type = "fixed", interval = 'confidence') %>%
  rename(UVVR = x,
         Veg_Percent = predicted)


write.csv(Change.preds, "E:\\Coastal Habitat Restoration Team\\ACJV Sites - RI_Mass_Maine\\Data Analysis\\Output R CSVs Analysis\\Veg_UVVR_Prediction.csv")



ggplot() +
  geom_ribbon(data = Change.preds,
              aes(x = UVVR, ymax = conf.high, ymin = conf.low),
              colour = 'dodgerblue', fill = 'dodgerblue', alpha = 0.5) +
  geom_line(data = Change.preds,
            aes(x = UVVR, y = Veg_Percent),
            size = 1.5, colour = 'black') + 
  geom_point(data = Database.change,
             aes(x = UVVR, y = Veg_Percent),
             size = 3) + 
  labs(x = "UVVR Score", 
       y = "Percent Vegetated Area (%)") + 
  scale_x_continuous(limits = c(0, 1), 
                     breaks = seq(0, 3, 0.25)) + 
  scale_y_continuous(limits = c(50, 150), 
                     breaks = seq(50, 150, 10)) +
  theme_bw() +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.title = element_text(size = 15, colour = "black"),
    axis.text = element_text(size = 15, colour = "black"),
    strip.text = element_text(size = 16, colour = "black"))



