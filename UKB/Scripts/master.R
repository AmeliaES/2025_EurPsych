# -----------------------------
# Analysis in UK Biobank
# -----------------------------
# Requirements:
# Run scripts in the Scripts/Eddie folder to extract data. Please see the README.md in this folder.
# -----------------------------
# Does inflammation associate with trajectories of depression in mid to late life?
# Sources R scripts in the "Scripts" folder
# Reads in data and cleans it
# Plots the data - exploratory plots which are included in the supplementary materials
# Runs the multi-level trajectory models, formats the results tables
# Plots the main results figures
# Conducts the same analysis but splitting the trajectories into males and females separately
# -----------------------------
setwd("UKB")
# Set scipen
options(scipen = 999)
# -----------------------------
library(broom.mixed)
library(car)
library(cowplot)
library(data.table)
library(dplyr)
library(stringr)
library(tidyverse)
library(lme4)
library(gtsummary)
library(gt)
library(forcats)
library(tableone)
library(multcomp) #glht

# Source functions used for analysing ALSPAC data:
sapply(list.files("../ALSPAC/Scripts/FUNS", full.names = TRUE), source)
# Source functions specific for UKB analysis:
sapply(list.files("Scripts/FUNS", full.names = TRUE), source)

# -------------------------
# Set ggplot theme
my_theme <- function(base_size = 18, base_family = ""){
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(
      panel.background = element_rect(fill = "transparent"),
      plot.background = element_rect(fill = "transparent", color = NA),
      panel.grid.major = element_line(colour = "grey95"),
      panel.grid.minor = element_line(colour = "grey99"),
      panel.ontop = FALSE
    )
}

theme_set(my_theme())

###############################
# 1. Process IL6 Olink protein data from UKB, including creating categorical variables, eg. tertiles of IL6
###############################
# Create data and save in datastore
source("Scripts/collate_data.R")

# -----------------------------
# Explore data
# Run this to remove UKB-PPP participants, scale data and create tertiles/quartiles
# This inverse normal transforms and z-score scales IL-6
source("Scripts/data_explore.R")

###############################
# 2. Combine with depression data and create long format
###############################
# Creates plots and sum stats for covariate variables
# Creates demographic table
# This also z-score scales log(BMI), Townsend deprivation index and depression scores.
source("Scripts/data_merge_long.R")

###############################
# 3. Create trajectories of depression
###############################
source("Scripts/traj.R")

###############################
# 4. Test for associations with a categorical variable of IL6
###############################
source("Scripts/traj_inflam.R")
# -------
# Identify which markers are strongly associated with depression trajectories
# Read in main results and return rows where the p value is < 0.05 or "<0.0001"
# Return so that it is clear what is significant in unadjusted, sex adjusted and fully adjusted
files <- str_subset(list.files("Output/traj_inflam/table_all_estimates", full.names = T), "Cat")[4]

# Categorical
lapply(files, function(file){
  marker <- str_extract(file, "(?<=Cat)(.*?)(?=\\.csv)")
  res <- read.csv(file) %>%
    filter(str_detect(Parameter, marker))

  base <- res %>%
    filter(`p.value..Unadjusted.` == "<0.0001" | as.numeric(`p.value..Unadjusted.`) < 0.05) %>%
    pull(Parameter)

  sex <- res %>%
    filter(`p.value..Sex.Adjusted.` == "<0.0001" | as.numeric(`p.value..Sex.Adjusted.`) < 0.05) %>%
    pull(Parameter)

  full <- res %>%
    filter(`p.value..Fully.Adjusted.` == "<0.0001" | as.numeric(`p.value..Fully.Adjusted.`) < 0.05) %>%
    pull(Parameter)

  list(base = base, sex = sex, full = full)
})

# Continuous
files <- str_subset(list.files("Output/traj_inflam/table_all_estimates", full.names = T), "Cont")

lapply(files, function(file){
  marker <- str_extract(file, "(?<=Cont)(.*?)(?=\\.csv)")
  res <- read.csv(file) %>%
    filter(str_detect(Parameter, marker))

  base <- res %>%
    filter(`p.value..Unadjusted.` == "<0.0001" | as.numeric(`p.value..Unadjusted.`) < 0.05) %>%
    pull(Parameter)

  sex <- res %>%
    filter(`p.value..Sex.Adjusted.` == "<0.0001" | as.numeric(`p.value..Sex.Adjusted.`) < 0.05) %>%
    pull(Parameter)

  full <- res %>%
    filter(`p.value..Fully.Adjusted.` == "<0.0001" | as.numeric(`p.value..Fully.Adjusted.`) < 0.05) %>%
    pull(Parameter)

  list(base = base, sex = sex, full = full)
})

# -------
# Main figure:
source("Scripts/mainPlot_scoresAtAges.R")

# -------------------------
# Split by sex analysis
source("Scripts/traj_inflam_sex.R")
source("Scripts/mainPlot_scoresAtAges_sex.R")

# -----------------------------
# Return to original working directory
setwd("../")

