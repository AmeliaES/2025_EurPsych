# Extract depression and covariate data from duckDB and save as wide format csv in Data Store

# ------------------------------------
## Install the package and load other libraries
# local({r <- getOption("repos")
#        r["CRAN"] <- "https://www.stats.bris.ac.uk/R/" 
#        options(repos=r)
# })
# install.packages("remotes")
# remotes::install_version('duckdb', '0.7.1-1', repos = "http://cran.us.r-project.org")
# install.packages("dbplyr")

# ------------------------------------
# Set wd to where we hae cloned ALSPAC_trajectories
setwd("/exports/igmm/eddie/GenScotDepression/amelia/ALSPAC_trajectories")
SCRATCH <- "/exports/eddie/scratch/s1211670/"

# ------------------------------------
library(dbplyr)
library(duckdb, lib.loc = "/home/s1211670/R")
library(dplyr)
library(stringr)
library(tidyverse)
library(data.table)

# ------------------------------------
# Load in function that takes field IDs and returns a flat table
flatTable <- function(fieldIDs, instance = 0, con){

  # Check arguments
  if(!is.numeric(fieldIDs)){
    stop("Please provide a numeric value for the fieldIDs.")
  }

  if(!instance %in% c(0:3, as.character(0:3) , "all")){
    stop('Please provide either 0,1,2,3 or "all" for the instance. See documentation for more details.')
  }

  # Load the data dictionary
  Dictionary <- tbl(con, 'Dictionary')

  # Make a dataframe that tells you which table each field ID is in. 
  # ie. This is data frame with 2 columns "Table" and "FieldID", where Table is the table in the duckDB where you can find it's corresponding "FieldID".
  fields_df <- Dictionary |>
    filter(FieldID %in% fieldIDs) |>
    select(c(Table, FieldID)) |>
    collect()

  # Create global environment variables for each table, (assigned from it's character string name) so we can query them later
  tables <- unique(fields_df$Table)
  for(tab in tables){
    assign(tab, tbl(con, tab))
  }

  # Create a list for each table with the field IDs selected
  # Reduce these and join by the ID column
  # Either do this for the instance specified (first "if" code chunk) or for all instances ("else if" code chunk)
  if(instance %in% c(0:3, as.character(0:3))){
    flat_table_df <- lapply(tables, function(tab){
      tmp <- fields_df |>
        filter(Table %in% tab) |>
        pull(FieldID) 
      tmp <- paste0("f.", tmp, ".", instance, ".")
      eval(as.symbol(tab)) |>
        select(c(f.eid, all_of(starts_with(tmp)))) 
      }) %>% reduce(., full_join, by = "f.eid")

  }else if(instance == "all"){
      flat_table_df <- lapply(tables, function(tab){
      tmp <- fields_df |>
        filter(Table %in% tab) |>
        pull(FieldID) 
      tmp <- paste0("f.", tmp, ".")
      eval(as.symbol(tab)) |>
        select(c(f.eid, any_of(starts_with(tmp)))) 
      }) %>% reduce(., full_join, by = "f.eid")
  }
  return(as.data.frame(flat_table_df))
}

# ------------------------------------
## Connect to the database
con <- DBI::dbConnect(duckdb::duckdb(),
  dbdir="/exports/igmm/eddie/GenScotDepression/data/ukb/phenotypes/fields/2022-11-phenotypes-ukb670429-v0.7.1/ukb670429.duckdb",
  read_only=TRUE)



# ------------------------------------
# Read in linker file with the variables we want to extract
linker <- fread("UKB/Supplementary/fields_to_extract.csv")

# Get field IDs
fieldIDs <- linker$Field_ID %>%
  str_split(., "\\.")%>%
  sapply(., "[[", 1) %>%
  unique() %>%
  as.numeric()

# ------------------------------------
# Run function and extract for all available instances. NB/ instance basically means assessment type, eg. baseline, repeat, imaging etc etc
data <- flatTable(fieldIDs = fieldIDs,
      instance = "all",
      con = con)

# ------------------------------------
# Check we have all the variables we need from the linker file
# Variables we have found:
linker$Field_ID[paste0("f.", linker$Field_ID) %in% colnames(data)]
# Variables we have not found:
linker$Field_ID[!paste0("f.", linker$Field_ID) %in% colnames(data)]


# ------------------------------------
# Data for the Mental well-being and Health and well-being are located in different tsv files so let's extract them here:
health <- fread(paste0(SCRATCH, "health-wellbeing.tsv"))
mental <- fread(paste0(SCRATCH, "MentalWellBeing.tsv"))

missing <- paste0("f.", linker$Field_ID[!paste0("f.", linker$Field_ID) %in% colnames(data)])

health <- health %>%
  dplyr::select(c("f.eid", starts_with(missing)))

mental <- mental %>%
  dplyr::select(c("f.eid", starts_with(missing)))


missing %in% c(colnames(health), colnames(mental))
# all true, yay!

# Combine with "data"

nrow(data) # 502389
nrow(health) # 502366
nrow(mental) # 502356

# Merge and keep participants that are in all 3 tables
data <- list(data, health, mental) %>% reduce(left_join, by = "f.eid")

# Check again we have all our variables:
# Variables we have found:
linker$Field_ID[paste0("f.", linker$Field_ID) %in% colnames(data)]
# Variables we have not found:
linker$Field_ID[!paste0("f.", linker$Field_ID) %in% colnames(data)]

# -----------------------------------
# Extract data on CRP, smoking status, date of death, and birth weight
extraVariables <- flatTable(fieldIDs = c(30710, 20116, 40000, 20022),
      instance = 0,
      con = con) %>%
    rename(CRP = f.30710.0.0,         
          DOD =f.40000.0.0,          # https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=40000
          birth_weight = f.20022.0.0) # in kg: https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=20022


data <- left_join(data, extraVariables, by = "f.eid")

# -----------------------------------
# Extract data on anti-inflammatory medication
# Type of medication: https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=20003
meds <- flatTable(con = con, fieldIDs = 20003)

# -----------------------------------
# Extract data on self-reported inflamamtory condition (same deifnition as I used in my previous UKB project)
# Only need to look at baseline appointment as that's when IL6 was measured
# Use coding6.tsv (non-cancer), coding3.tsv (cancer) and inflam_disease.csv to extract the conditions we want
# Create a column called inflam_condition that is TRUE/FALSE if the person has an inflammatory condition
# Also create individual columns for TRUE/FALSE for the individual conditions and their names as the columns

dataInflam <- flatTable(fieldIDs = c(20002, 20001) ,
      instance = 0,
      con = con)

codesNonCancer <- fread("UKB/Supplementary/coding6.tsv")
codesCancer <- fread("UKB/Supplementary/coding3.tsv")


illnesses <- fread("UKB/Supplementary/inflam_disease.csv")$Condition %>% 
  tolower() %>%
  str_remove("\\ \\(nos\\)") %>%
  str_replace("arthritis \\(unspecified\\)", "arthritis (nos)"  ) %>%
  str_remove("\\ \\(unspecified\\)") %>%
  str_remove_all("\\’") %>%
  str_remove("\\ \\(type not specified\\)") %>%
  str_replace("glomerulonephritis", "glomerulnephritis") %>%
  str_replace("\\/ ", "\\/") %>%
  str_replace("polio\\/", "polio\\ \\/\\ ") %>%
  str_replace("sjogrens", "sjogren\\'s") %>%
  str_replace("pelvic inflammatory disease", "pelvic inflammatory disease/ pid") %>%
  str_replace("schistosomiasis" , "schistosomiasis/bilharzia") %>%
  str_replace("tuberculosis", "tuberculosis (tb)") %>%
  str_replace("systemic lupus erythematosus \\(sle\\)", "systemic lupus erythematosis/sle") %>%
  str_replace("wegeners granulomatosis", "wegners granulmatosis") %>%
  str_replace( "plantar fasciitis"  , "plantar fascitis") %>%
  str_replace("polyarteritis nodosa", "microscopic polyarteritis") %>%
  str_replace("acute infective polyneuritis/guillain-barré syndrome", "acute infective polyneuritis/guillain-barre syndrome") %>%
  str_replace("chronic lymphocytic leukaemia", "leukaemia") %>%
  str_replace("chronic myeloid leukaemia", "acute myeloid leukaemia") %>%
  str_replace("hodgkins lymphoma/hodgkins disease","hodgkins lymphoma / hodgkins disease") %>%
  str_replace("non-hodgkin lymphoma", "non-hodgkins lymphoma")

# Find the code for each condition:
illnesses[illnesses %in% codesNonCancer$meaning]
illnesses[illnesses %in% codesCancer$meaning]

# Check we have got codes for all of them:
length(illnesses) == sum(length(illnesses[illnesses %in% codesCancer$meaning]), length(illnesses[illnesses %in% codesNonCancer$meaning]))

length(illnesses[illnesses %in% codesNonCancer$meaning])
nrow(codesNonCancer[codesNonCancer$meaning %in% illnesses,])

length(illnesses[illnesses %in% codesCancer$meaning])
nrow(codesCancer[codesCancer$meaning %in% illnesses,])

# Why have we dropped one in each?
illnesses[illnesses %in% codesCancer$meaning][!
codesCancer[codesCancer$meaning %in% illnesses,]$meaning %in% 
illnesses[illnesses %in% codesCancer$meaning]
]
# leukaemia is repeated

illnesses[illnesses %in% codesNonCancer$meaning][!
codesNonCancer[codesNonCancer$meaning %in% illnesses,]$meaning %in% 
illnesses[illnesses %in% codesNonCancer$meaning]
]

illnesses[illnesses %in% codesNonCancer$meaning][duplicated(illnesses[illnesses %in% codesNonCancer$meaning])]
# microscopic polyarteritis is repeated

# Subset codes to those conditions we want
codesNonCancer <- codesNonCancer[codesNonCancer$meaning %in% illnesses,]
codesCancer <- codesCancer[codesCancer$meaning %in% illnesses,]

# # Create a new column that is TRUE/FALSE for if an individual has an inflammatory condition:
# test <- dataInflam%>%
#   mutate(inflam_condition = if_any(starts_with("f.20002"), ~(. %in% codesNonCancer),na.rm = T) | if_any(starts_with("f.20001"), ~(. %in% codesCancer),na.rm = T))


# # Do a quick check to see if that looks sensible:
# IDs <- dataInflam %>%
#   select(c(f.eid, starts_with("f.20002.0"))) %>%
#   slice(which(dataInflam$f.20002.0.0 %in% codesNonCancer)) %>%
#   pull(f.eid)

# IDsTest <- test %>%
#   select(c(f.eid, starts_with("f.20002.0"), inflam_condition)) %>%
#   filter(inflam_condition) %>%
#   pull(f.eid)

# sort(IDs)[1:20]
# sort(IDsTest)[1:20]

# # All IDs should be in IDsTest but there should be extra IDs in IDsTest for the other cols
# all(IDs %in% IDsTest) # TRUE
# IDsTest[!IDsTest %in% IDs][1:20]

# dataInflam %>%
#   filter(f.eid %in% 1000014)%>%
#   select(c(f.eid, starts_with("f.20002.0")))

# codesNonCancer

# yes, as expected
# confident I've extracted inflamatory conditions

dataInflamAll <- dataInflam%>%
  mutate(inflam_condition = if_any(starts_with("f.20002"), ~(. %in% codesNonCancer$coding),na.rm = T) | if_any(starts_with("f.20001"), ~(. %in% codesCancer),na.rm = T)) %>%
  select(f.eid, inflam_condition)

sum(is.na(dataInflamAll$inflam_condition)) # 0

# ----
# Create columns for each individual inflammatory condition
dataInflamCond <- lapply(1:nrow(codesNonCancer), function(i){
colname <- codesNonCancer$meaning[i] %>%
  str_replace_all("\\/", ".") %>%
  str_replace_all(" ", "_")

dataInflamCond <- dataInflam %>%
  mutate("inflam_ind_{colname}" := if_any(starts_with("f.20002"), ~(. %in% codesNonCancer$coding[i]),na.rm = T) ) %>%
  dplyr::select(f.eid, !!sym(paste0("inflam_ind_", colname)))

return(dataInflamCond)
}) %>% reduce(left_join, by = "f.eid")

dataInflamCondCancer <- lapply(1:nrow(codesCancer), function(i){
colname <- codesCancer$meaning[i] %>%
  str_replace_all("\\/", ".") %>%
  str_replace_all(" ", "_")

dataInflamCond <- dataInflam %>%
  mutate("inflam_ind_{colname}" := if_any(starts_with("f.20001"), ~(. %in% codesCancer$coding[i]),na.rm = T) ) %>%
  dplyr::select(f.eid, !!sym(paste0("inflam_ind_", colname)))

return(dataInflamCond)
}) %>% reduce(left_join, by = "f.eid")


head(dataInflamCond)
head(dataInflamCondCancer)

# -----------------------------------
# Check this has all conditions we expect

ncol(dataInflamCond)-1
ncol(dataInflamCondCancer)-1
# 56 columns
# Why are two missing and what are they?
nrow(codesCancer)
nrow(codesNonCancer)


# -----------------------------------
lapply(list(data, meds, dataInflamAll, dataInflamCond, dataInflamCondCancer), nrow)
data <- list(data, meds, dataInflamAll, dataInflamCond, dataInflamCondCancer) %>% reduce(left_join, by = "f.eid")


# -----------------------------------
# Save data to scratch space
# system(paste0("mkdir ", SCRATCH ,"UKB"))
write.csv(data, paste0(SCRATCH, "UKB/dataWide.csv"), row.names = F)

# Great, we have identified all the variables we need, 
# On to the next step...




