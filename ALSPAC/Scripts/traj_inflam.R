# Rum model for depression trajectories with IL-6
# -------------------------
  # Define marker
  marker <- "IL6"

  # ----
  # Run lmer (quartic models) for unadjusted, sex adjusted and fully adjusted (sex, maternal education and BMI )
  models <- quarticInflamModel(inflam =  marker,
                               data = dataLong,
                               type = list("tertile", "log", "INT"))

  dataLongAnyMeds <- dataLong %>%
    filter(any_meds == FALSE)
  modelsAnyMeds <- quarticInflamModel(inflam =  marker,
                               data = dataLongAnyMeds,
                               type = list("tertile"))

  dataLongMinTwo <- dataLong %>%
    filter(CompletedQ > 1)
  modelsMinTwo <- quarticInflamModel(inflam =  marker,
                                     data = dataLongMinTwo,
                                     type = list("tertile"))

  # ----
  # Get a nice results table to put in supplementary
  # tertiles (categorical variable):
  resultsCat <- resultsTable(modelList = models$tertile[1:3],
               colNames = c(" (Unadjusted)", " (Sex Adjusted)", " (Fully Adjusted)"))

  resultsCatTownsend <- resultsTable(modelList = models$tertile[3:4],
                             colNames = c(" (Fully Adjusted)", " (Townsend Adjusted)"))

  resultsCatAnyMeds <- resultsTable(modelList = modelsAnyMeds$tertile[1:3],
                                    colNames = c(" (Unadjusted)", " (Sex Adjusted)", " (Fully Adjusted)"))

  resultsCatMinTwo <- resultsTable(modelList = modelsMinTwo$tertile[1:3],
                                    colNames = c(" (Unadjusted)", " (Sex Adjusted)", " (Fully Adjusted)"))

  # Continuous variable:
  resultsContLog <- resultsTable(modelList = models$log[1:3],
               colNames = c(" (Unadjusted)", " (Sex Adjusted)", " (Fully Adjusted)"))

  resultsContINT <- resultsTable(modelList = models$INT[1:3],
                              colNames = c(" (Unadjusted)", " (Sex Adjusted)", " (Fully Adjusted)"))

  # ----
  # Calculate predict column
  dataPred <- calcPred(data = dataLong, model = models$tertile[[3]], condition = paste0(marker, "_tertile"))

  # ----
  # Return a plot of the trajectories split by categorical variable
  # Fully adjusted model = models$tertile[[3]]
  plotTertiles <- plotSplitTraj(model = models$tertile[[3]],
                     data = dataPred,
                     condition = paste0(marker, "_tertile"),
                     plotColours = c(  "darkolivegreen3","dodgerblue2", "darkorchid3"),
                     legendTitle = marker,
                     legendLabels = c("Bottom Third", "Middle Third", "Top Third"))

  # ----
  # Return a table and plot for scores at ages
  # And differences between these scores at ages pair wise for each level

  scoresAtAges <- plotScoresAgesCat(model = models$tertile[[3]],
                                    data = dataPred,
                                    ageInputScore = c(10,13,16,19,22,25,28),
                                    condition = paste0(marker, "_tertile"),
                                    plotColours = c(  "darkolivegreen3","dodgerblue2", "darkorchid3"),
                                    legendTitle = marker,
                                    sex = FALSE,
                                    legendLabels = c("Bottom Third", "Middle Third", "Top Third"))


  # ----
  # Save files
  write.csv(resultsCat, paste0("Output/traj_inflam/table_all_estimates/resultsCat", marker ,".csv"), row.names = F)
  write.csv(resultsCatTownsend, paste0("Output/traj_inflam/table_all_estimates/resultsCatTownsend", marker ,".csv"), row.names = F)
  write.csv(resultsCatAnyMeds, paste0("Output/traj_inflam/table_all_estimates/resultsCatAnyMeds", marker ,".csv"), row.names = F)
  write.csv(resultsCatMinTwo, paste0("Output/traj_inflam/table_all_estimates/resultsCatMinTwo", marker ,".csv"), row.names = F)
  write.csv(resultsContLog, paste0("Output/traj_inflam/table_all_estimates/resultsCont_log", marker ,".csv"), row.names = F)
  write.csv(resultsContINT, paste0("Output/traj_inflam/table_all_estimates/resultsCont_INT", marker ,".csv"), row.names = F)

  png(paste0("Output/traj_inflam/plotSplitTraj/", marker, ".png"), width = 2300, height = 1000, units = "px", res = 300)
  print(plotTertiles)
  dev.off()

  png(paste0("Output/traj_inflam/plotScoresAtAges/", marker, ".png"), width = 2300, height = 1000, units = "px", res = 300)
  print(scoresAtAges$plot)
  dev.off()


  write.csv(scoresAtAges$scores, paste0("Output/traj_inflam/tableScoresAtAges/scores", marker ,".csv"), row.names = T)
  write.csv(scoresAtAges$difference, paste0("Output/traj_inflam/tableScoresAtAges/difference", marker ,".csv"), row.names = F)



