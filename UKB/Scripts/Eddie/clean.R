# We have ran the "extract.R" script and now we want to create our long format with the depression score and age at each time point
# ------------------------------------
# Set wd to where we hae cloned ALSPAC_trajectories
setwd("/exports/igmm/eddie/GenScotDepression/amelia/ALSPAC_trajectories")
SCRATCH <- "/exports/eddie/scratch/s1211670/"

# ------------------------------------
library(dplyr)
library(stringr)
library(tidyverse)
library(data.table)
library(lubridate)
# ------------------------------------
# Read in data
data <- read.csv(paste0(SCRATCH, "UKB/dataWide.csv"))

# ------------------------------------
# Read in linker file with the variables we want to extract
linker <- fread("UKB/Supplementary/fields_to_extract.csv")

# ------------------------------------
# Get depression variables for each time point
# For PHQ2 this will be Dep_Q1 and Dep_Q2 summed at each time point (TimePoint column)
# For PHQ4 this will be DepQ1, DepQ2, DepQ3, DepQ4 summed at each time point

# ------------------------------------
# Recode variables to numeric and code missing as NA
fieldRecode <- linker %>%
	filter(str_detect(Type, "Dep_")) %>%
	pull(Field_ID)

# Check what responses are in each column
data_factors <- data %>%
	dplyr::select(paste0("f.", fieldRecode)) %>%
	mutate_all(funs(as.factor(.)))

sapply(data_factors, levels)
# Great, they are all the same, good to check that


# ------------------------------------
# Calculate number of completed questionnaires for PHQ-2 (with and without excluding imaging appts: c("53.2.0", "53.3.0"))
linker %>%
	filter(str_detect(Type, "Dep_Q1|Dep_Q2"))
# If they have an NA at for both questions at each timePoint then they count as missing, else they have answered the questionnaire

times <- c("0", "1", "mh", "pain", "hwell", "mwell") # Remove the imaging time points (2, 3)

for(time in times){
Qfields <- linker %>%
	filter(str_detect(Type, "Dep_Q1|Dep_Q2")) %>%
	filter(TimePoint == time)%>%
		pull(Field_ID) %>%
		paste0("f.", .)

data <- data %>%
	 mutate(!!sym(paste0("NA_PHQ2", time)) := rowSums(is.na(select(., Qfields))) == 2) # TRUE if missing
}
data <- data %>%
	mutate(NCompleted = rowSums(!select(., starts_with("NA_PHQ2")))) %>% 
	dplyr::select(-starts_with("NA_PHQ2")) 

# ------------------------------------
# Recode:
# Not at all = 0, Several days = 1, More than half the days = 2, Nearly every day = 3, NA for Do not know and prefer not to answer
# Recode values in selected columns

data <- data %>%
  mutate(across(all_of(paste0("f.", fieldRecode)), ~case_when(
    . == "Not at all" ~ 0,
    . == "Several days" ~ 1,
    . == "More than half the days" ~ 2,
    . == "Nearly every day" ~ 3,
    . %in% c("Do not know", "Prefer not to answer") ~ -NA,
    TRUE ~ as.numeric(.)
  )))


# ------------------------------------
# Test what we expect to find for PHQ2 at the initial assessment time point to match against when we create the columns below
PHQType <- "Dep_Q1|Dep_Q2"
time <- "0"
cols <- linker %>%
	filter(str_detect(Type, PHQType)) %>%
	filter(TimePoint == time) %>%
	pull(Field_ID) %>%
	paste0("f.", .)
colName <- paste0("dep_", time)
data %>%
		mutate(!!sym(colName) := rowSums(.[,cols], na.rm = T)) %>%
		mutate(!!sym(colName) := ifelse( rowSums(is.na(.[,cols])) == length(cols) , NA, !!sym(colName))) %>%
		mutate(!!sym(paste0(colName, "_isNA")) := rowSums(is.na(.[,cols])) ) %>%
		dplyr::select(c(cols, colName, paste0(colName, "_isNA"))) %>%
		head(50)

# ------------------------------------
# Create columns for PHQ2 and PHQ4

for (PHQ in c("PHQ2", "PHQ4")){
	if(PHQ == "PHQ2"){
	PHQType <- "Dep_Q1|Dep_Q2"
	times <- c(as.character(0:3), "mh", "pain", "hwell", "mwell")
	}else if(PHQ == "PHQ4"){
	PHQType <- "Dep_Q1|Dep_Q2|Dep_Q3|Dep_Q4"
	times <- c(as.character(0:3), "mh","mwell")
	}

	for(time in times){

	cols <- linker %>%
		filter(str_detect(Type, PHQType)) %>%
		filter(TimePoint == time) %>%
		pull(Field_ID) %>%
		paste0("f.", .)

	colName <- paste0(PHQ, "_dep_", time)

	data <- data %>%
		mutate(!!sym(colName) := rowSums(.[,cols], na.rm = T)) %>%
		mutate(!!sym(colName) := ifelse( rowSums(is.na(.[,cols])) == length(cols) , NA, !!sym(colName))) %>%
		mutate(!!sym(paste0(colName, "_isNA")) := rowSums(is.na(.[,cols])) )
	}
}


# ------------------------------------
# Get age for each time point

# Remove time so it's just the date from any columns
dateCols <- linker %>%
		filter(str_detect(Type, "For_age_date")) %>%
		pull(Field_ID) %>%
		paste0("f.", .)
data %>%
	select(dateCols ) %>%
	head(40)

# col f.120128.0.0 also has a time
data %>%
  mutate(across(all_of(dateCols), ~ format(ymd(parse_date_time(., orders = c("ymd", "ymd HMS"))), "%Y-%m-%d")))%>%
	select(dateCols ) %>%
	head(60)
# Some imaging appointments are after the 2 online well being questionnaires

### Calculate age for each time point

# Names of new columns (in the same order to correspond with dateCols)
times <- paste0("age_", c(as.character(0:3), "mh", "pain", "hwell", "mwell"))

# The first 4 date cols for 21003.0, 21003.1, 21003.2, 21003.3 have already been derived by UKB so rename these columns
ageCols <- paste0("f.21003.", 0:3, ".0")
data <- data %>%
	rename_with(~ times[1:4][match(., ageCols)], all_of(ageCols))

# Derive age for the other columns:
# Year of birth column is f.34.0.0
data <- data %>%
mutate(across(all_of(dateCols[5:8]), ~ ., .names = "original_col_{.col}")) %>%
  mutate(across(all_of(dateCols[5:8]), ~ year(parse_date_time(., orders = c("ymd", "ymd HMS")))))%>%
  mutate(across(all_of(dateCols[5:8]),
                list(age = ~ . - f.34.0.0),
                .names = "{times[5:8]}"))

# ------------------------------------
# Rename covariates from UKB fields to names:
# Do qualifications separately, as there are multiple answers for this:
fieldCovar <- linker %>%
	filter(str_detect(Type, "Covariate")) %>%
	filter(!Variable_Name %in% "Qualifications") %>%
	pull(Field_ID) %>%
	paste0("f.", .)

covarName <- c("Sex", "Townsend",  "Age_completed_education", "BMI",
"Assessment_centre_baseline", "Assessment_centre_1", "Assessment_centre_2", "Assessment_centre_3")

data <- data %>%
	rename_with(~ covarName [match(., fieldCovar)], all_of(fieldCovar))

# Now do qualifications:
data <- data %>%
	rename_with(~ sub("^f.6138.", "Qualifications", .), starts_with("f.6138."))


# ------------------------------------
# Save in scratch
write.csv(data, paste0(SCRATCH, "UKB/dataWideDerived.csv"), row.names = F)



