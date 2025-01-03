# Convert estimates of scores and their differences to Z-scores
# Main table for manuscript

library(openxlsx)
library(tidyr)
library(stringr)
library(dplyr)

# Read in functions:
source("Scripts_both_cohorts/FUNS/makeLong.R")

# Read in scores and differences and sample size for ALSPAC and UKB
cohorts <- c("ALSPAC", "UKB")
scores <- c("", "PHQ2_")

# Run for main analysis and split by sex analysis
sapply(c("traj_inflam", "traj_inflam_sex"), function(analysisType){

  scoreFiles <- lapply(seq_along(cohorts), function(i) {
    list(
      scores = read.csv(paste0(cohorts[i], paste0("/Output/",analysisType,"/tableScoresAtAges/"), scores[i] ,"scoresIL6.csv")),
      dif = read.csv(paste0(cohorts[i],paste0("/Output/",analysisType,"/tableScoresAtAges/"),scores[i],"differenceIL6.csv")),
      N = as.numeric(read.csv(paste0(cohorts[i],paste0("/Output/",analysisType,"/table_all_estimates/"),scores[i],"resultsCatIL6.csv"))$`N..Fully.Adjusted.`[1])  # to get N in fully adjusted model
    )
  })

  # --------------------------------
  # Z-score = mean / SD
  # Z = mean / SE*sqrt(N)
  # Formula and workings to get SE from 95% CIs: https://stats.stackexchange.com/questions/512789/converting-between-confidence-interval-and-standard-error
  # Z = mean / ( ((CIu - CIl) /3.199) *sqrt(N) )

  calcZ <- function(mean, CIu, CIl, N){
    Z = round( mean / ( ((CIu - CIl) /3.199) *sqrt(N) ) , 3)
    return(Z)
  }

  # --------------------------------
  # Convert to long format and calculate Z scores:

  mainTabs <- lapply(1:2, function(i){
    data <- if(analysisType=="traj_inflam"){
      filter(makeLongDif(scoreFiles[[i]]$dif), str_detect(Tertile, "Top vs Bottom"))
    }else{
      filter(makeLongDif(scoreFiles[[i]]$dif), str_detect(Tertile, "Female_Top vs Female_Bottom|Male_Top vs Male_Bottom"))
    }
    data %>%
    mutate(`Difference (Z-score)` = calcZ(mean = Difference,
                     CIu = Upper_CI,
                     CIl = Lower_CI,
                     N = scoreFiles[[i]]$N)) %>%
      relocate(`Difference (Z-score)`, .before = Lower_CI) %>%
      unite("95% CI", c(Lower_CI, Upper_CI), sep = " - ") %>%
      rename(c( `Age (years)` = Tertile,
                `Difference (raw score)` = Difference,
                `P (uncorrected)` = uncorrected_p,
                `P (FDR)` = FDR_p))
  })

  mainTabs

  # ------
  # Format table to make it more readable
  lapply(1:2, function(i){
  data <- mainTabs[[i]]

  # Create a workbook
  wb <- createWorkbook()

  # Add a worksheet to the workbook
  ws <- addWorksheet(wb, "Sheet1")

  # Write data to the worksheet
  writeData(wb,1, data, startCol = 1, startRow = 1)

  # Add borders to the worksheet
  addStyle(wb, 1, style = createStyle(border = "TopBottomLeftRight", borderStyle = "thin", halign = "center", valign = "center"),
           rows = 1:(nrow(data)+1),
           cols = 1:ncol(data),
           gridExpand = TRUE)

  setColWidths(wb, 1, cols = 1:ncol(data), widths = "auto")

  # Save the workbook to an xlsx file
  saveWorkbook(wb, paste0("Manuscript/scoresDif_",analysisType, "_", cohorts[i] ,".xlsx"), overwrite = TRUE)

  # Save scores in long format
  if(analysisType == "traj_inflam"){
    results <- makeLongScores(scoreFiles[[i]]$scores)
  }else if(analysisType == "traj_inflam_sex"){
    results <- makeLongScoresSex(scoreFiles[[i]]$scores)
  }

  # Create a workbook
  wb2 <- createWorkbook()

  # Add a worksheet to the workbook
  ws2 <- addWorksheet(wb2, "Sheet1")

  # Write data to the worksheet
  writeData(wb2,1, results, startCol = 1, startRow = 1)

  # Add borders to the worksheet
  addStyle(wb2, 1, style = createStyle(border = "TopBottomLeftRight", borderStyle = "thin", halign = "center", valign = "center"),
           rows = 1:(nrow(results)+1),
           cols = 1:ncol(results),
           gridExpand = TRUE)

  setColWidths(wb2, 1, cols = 1:ncol(results), widths = "auto")

  # Save the workbook to an xlsx file
  saveWorkbook(wb2, paste0("Manuscript/scores_",analysisType, "_", cohorts[i] ,".xlsx"), overwrite = TRUE)

  })



})




