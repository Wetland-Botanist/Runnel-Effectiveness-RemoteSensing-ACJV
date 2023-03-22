# ACJV_UVVR_NewEngland
Geospatial Analysis of Vegetated Marsh Area and the Impact of Runnel Restoration in New England

Date Last Updated: 3/22/2023

Created by: JG McKown (jgrantmck@gmail.com)

Project Details:

Saltmarsh Sparrows are currently threatened with severe population declines and extinction due to the loss of critical breeding and nesting habitat of high marsh habitat in interior tidal wetlands. As part of a multi-state conservation plan, the Atlantic Coast Joint Venture called on the implementation of runnels, or shallow vegetated swells, to drain waterlogged interior marshes and recover lost vegetation. This project evaluates the impact of runnels from a landscape-perspective through imagery classification of the National Agricultural Imagery Program on 19 selected sites throughout New England. Selected sites had NAIP Imagery (2010 - 2021) classified into vegetated and unvegetated (pixel, supervised classification) in ArcGIS Pro. The code, in this repository, is the post-remote sensing analysis on the pre- and post-restoration trends of Reference, No Action, and Runnel Restored tidal watersheds throughout the sites focused on the unvegetated-vegetated ratio (UVVR) and percent vegetated area for each tidal watershed.  



Raw Data:

"ACJV_UVVR_AllTidesheds_Finalized.csv"

Raw data is the raw data of the finalized unvegetated - vegetated ratio (UVVR) scores and the vegetated area (ha) of each tideshed for each year. Additional site characteristic data is provided including Site Name, Tideshed Number within a site, Year restoration activities took place, and rate of restoration activites relative to 2021 (the most recent public aerial imagery collection date). Raw data was created and finalized in March 2023 by JG McKown. 


Note on Code:

The organization of the code is broken down by Book - Chapter - Page - Step. Each book represents three uniquely different aspects of the code - Data Prep, Analysis of UVVR, Analysis of Vegetated Area. Each chapter represents a different analysis requiring such as the Linear Mixed Model of UVVR. Each page represents a unique facet or series of steps within the chapter. Steps are individual code blocks. I organize my R code this way to allow for ease of understanding in bite-size chunks for the audience as well as myself. Code is heavily annotated to fully explain the steps, functions, and packages used in the analysis as well as reasoning for certain statistical analyses. 



