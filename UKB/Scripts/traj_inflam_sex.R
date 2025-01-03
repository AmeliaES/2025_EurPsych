# Run inflam models split by sex
# -------------------------
PHQ <- "PHQ2"

dataLong <- fread(paste0("/Volumes/GenScotDepression/users/amelia/UKB/UKB_dataLong_",PHQ,".csv"))  %>%
  mutate_at(c("Sex", "IL6_tertile"), as.factor)

# -------------------------
marker <- "IL6"
sex <- TRUE



  newColName <- paste0(marker, "_sex_tertile")

  dataLongSex <- dataLong %>%
    filter(!is.na(Sex)) %>%
    mutate(!!sym(newColName) :=
             ifelse(Sex == "Male" & !!sym(paste0(marker, "_tertile")) == "Bottom", "Male_Bottom",
                    ifelse( Sex == "Male" & !!sym(paste0(marker, "_tertile")) == "Middle", "Male_Middle",
                            ifelse(Sex == "Male" & !!sym(paste0(marker, "_tertile")) == "Top", "Male_Top",
                                   ifelse(Sex == "Female" & !!sym(paste0(marker, "_tertile")) == "Bottom", "Female_Bottom",
                                          ifelse( Sex == "Female" & !!sym(paste0(marker, "_tertile")) == "Middle", "Female_Middle",
                                                  ifelse(Sex == "Female" & !!sym(paste0(marker, "_tertile")) == "Top", "Female_Top",   NA                        ))))))
    ) %>%
    mutate(!!sym(newColName) := factor(!!sym(newColName), levels = c("Male_Bottom", "Male_Middle", "Male_Top",
                                                                     "Female_Bottom", "Female_Middle", "Female_Top")))


  # Run lmer (quartic models) for unadjusted and fully adjusted (maternal education and BMI )
  models <- quadraticInflamModelSex(inflam =  paste0(marker, "_sex"),
                                  data = dataLongSex,
                                  type = list("tertile"))

  resultsCat <- resultsTable(modelList = models$tertile,
                             colNames = c(" (Unadjusted)", " (Fully Adjusted)"))


  # Calculate predict column
  dataPred <- calcPredQuad(data = dataLongSex, model = models$tertile[[2]], condition = newColName)

  # ----
  # Return a plot of the trajectories split by categorical variable
  # Fully adjusted model = models$tertile[[3]]
  plotTertiles <- plotSplitTrajQuad(model = models$tertile[[2]],
                                data = dataPred,
                                condition = newColName,
                                conditionPlot = paste0(marker, "_tertile"),
                                plotColours = c(  "darkolivegreen3","dodgerblue2", "darkorchid3"),
                                legendTitle = marker,
                                legendLabels = c("Bottom Third", "Middle Third", "Top Third"),
                                sex = TRUE)

  # ----
  # Return a table and plot for scores at ages
  # And differences between these scores at ages pair wise for each level

  scoresAtAges <- plotScoresAgesCatQuad(model = models$tertile[[2]],
                                    data = dataPred,
                                    ageInputScore = c(40, 50, 60, 70, 80),
                                    condition = newColName,
                                    conditionPlot = paste0(marker, "_tertile"),
                                    plotColours = c( "darkolivegreen3","dodgerblue2", "darkorchid3"),
                                    legendTitle = marker,
                                    legendLabels = c("Bottom Third", "Middle Third", "Top Third"),
                                    sex = TRUE)

  source("Scripts/scoresAtAgesSexDif.R")
  write.csv(difTab, paste0("Output/traj_inflam_sex/tableScoresAtAges/PHQ2_difference",marker ,".csv"), row.names = F)

  # Save files
  write.csv(resultsCat, paste0("Output/traj_inflam_sex/table_all_estimates/resultsCat", marker ,".csv"), row.names = F)

    png(paste0("Output/traj_inflam_sex/plotSplitTraj/",marker, ".png"), width = 2300, height = 1000, units = "px", res = 300)
  print(plotTertiles)
  dev.off()

  png(paste0("Output/traj_inflam_sex/plotScoresAtAges/",marker, ".png"), width = 2300, height = 1000, units = "px", res = 300)
  print(scoresAtAges$plot)
  dev.off()

  write.csv(scoresAtAges$scores, paste0("Output/traj_inflam_sex/tableScoresAtAges/PHQ2_scores",marker ,".csv"), row.names = T)







