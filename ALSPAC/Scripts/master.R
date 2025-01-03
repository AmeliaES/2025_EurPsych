# -------------------------
# Analysis in ALSPAC
# -------------------------
# Does inflammation associate with trajectories of depression in adolescence to mid-life?
# Sources R scripts in the "Scripts" folder
# Reads in data and cleans it
# Plots the data - exploratory plots which are included in the supplementary materials
# Runs the multi-level trajectory models, formats the results tables
# Plots the main results figures
# Conducts the same analysis but splitting the trajectories into males and females separately

# -------------------------
setwd("ALSPAC")
# Set scipen
options(scipen = 999)
# -------------------------
# Make folders to store results:
# system("mkdir Output/traj_inflam")
# system("mkdir Output/traj_inflam/table_all_estimates")
# system("mkdir Output/traj_inflam/plotSplitTraj")
# system("mkdir Output/traj_inflam/plotScoresAtAges")
# system("mkdir Output/traj_inflam/tableScoresAtAges")

# -------------------------
# Load libraries 
# Most of these libraries are for plotting functions or for specific statistical functions
library(broom.mixed)
library(car)
library(cowplot)
library(data.table)
library(dplyr)
library(forcats)
library(ggcorrplot)
library(ggalluvial)
library(ggplot2)
library(gridExtra)
library(haven)
library(labelled)
library(lme4)
library(lmtest) # lrtest
library(MASS)
library(multcomp) #glht
library(stringi)
library(stringr)
library(tableone)
library(tibble)
library(tidyr)

# -------------------------
# Source functions
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

# -------------------------
# Read and prepare data
source("Scripts/data_prep.R")

# -------------------------
# Create exploratory plots:
source("Scripts/data_exploration.R")

# -------------------------
# Make depression trajectory for linear, quadratic, cubic and quartic.
# Compare model fit between those 4, choose the best model to take forward.
source("Scripts/traj.R")

# -------------------------
# Rum model for depression trajectories with inflammatory markers added
source("Scripts/traj_inflam.R")


# -------
# Plot main figures for paper
source("Scripts/mainPlot_scoresAtAges.R")

# -------------------------
# Split by sex analysis
source("Scripts/traj_inflam_sex.R")
source("Scripts/mainPlot_scoresAtAges_sex.R")

# -------------------------
# Return to original working directory
setwd("../")



