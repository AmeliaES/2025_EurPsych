# Rum model for depression trajectories with inflammatory markers added
# -------------------------
PHQ <- "PHQ2"

dataLong <- fread(paste0("/Volumes/GenScotDepression/users/amelia/UKB/UKB_dataLong_",PHQ,".csv"))  %>%
  mutate_at(c("Sex", "IL6_tertile", "inflam_condition",  "BMI_cat", "Batch", "Assessment_centre_baseline", "Assessment_centre_1", "Assessment_centre_2", "Assessment_centre_3"), as.factor) %>%
  mutate(Sex = factor(Sex, levels = c("Male", "Female"))) # change order of male/female levels so the same sex is used as reference in the model as in ALSPAC

dataLongBMI30rm <- fread(paste0("/Volumes/GenScotDepression/users/amelia/UKB/UKB_dataLong_",PHQ,"_BMI30rm.csv"))  %>%
  mutate_at(c("Sex", "IL6_tertile", "inflam_condition",  "BMI_cat","Batch", "Assessment_centre_baseline", "Assessment_centre_1", "Assessment_centre_2", "Assessment_centre_3"), as.factor)%>%
  mutate(Sex = factor(Sex, levels = c("Male", "Female")))

dataLongBMI40rm <- fread(paste0("/Volumes/GenScotDepression/users/amelia/UKB/UKB_dataLong_",PHQ,"_BMI40rm.csv"))  %>%
  mutate_at(c("Sex", "IL6_tertile", "inflam_condition",  "BMI_cat","Batch", "Assessment_centre_baseline", "Assessment_centre_1", "Assessment_centre_2", "Assessment_centre_3"), as.factor)%>%
  mutate(Sex = factor(Sex, levels = c("Male", "Female")))

dataLongBMI30Plus <- fread(paste0("/Volumes/GenScotDepression/users/amelia/UKB/UKB_dataLong_",PHQ,"_BMI30Plus.csv"))  %>%
  mutate_at(c("Sex", "IL6_tertile", "inflam_condition",  "BMI_cat","Batch", "Assessment_centre_baseline", "Assessment_centre_1", "Assessment_centre_2", "Assessment_centre_3"), as.factor)%>%
  mutate(Sex = factor(Sex, levels = c("Male", "Female")))

dataLongBMI40Plus <- fread(paste0("/Volumes/GenScotDepression/users/amelia/UKB/UKB_dataLong_",PHQ,"_BMI40Plus.csv"))  %>%
  mutate_at(c("Sex", "IL6_tertile", "inflam_condition",  "BMI_cat","Batch", "Assessment_centre_baseline", "Assessment_centre_1", "Assessment_centre_2", "Assessment_centre_3"), as.factor)%>%
  mutate(Sex = factor(Sex, levels = c("Male", "Female")))

dataLongAlive <- dataLong %>%
                  filter(Dead == FALSE)

dataLongMinTwo <- dataLong %>%
                    filter(NCompleted > 1)



# Run lmer (quartic models) for unadjusted, sex adjusted and fully adjusted (sex, maternal education and BMI )
marker <- "IL6"
models <- quadraticInflamModel(inflam =  marker,
                             data = dataLong,
                             type = list("tertile", "INT"))

modelsAlive <- quadraticInflamModel(inflam =  marker,
                                    data = dataLongAlive,
                                    type = list("tertile", "INT"))

modelsMinTwo <- quadraticInflamModel(inflam =  marker,
                                     data = dataLongMinTwo,
                                     type = list("tertile"))

# BMI sensitivities:
modelsBMI40rm <- quadraticInflamModel(inflam =  marker,
                              data = dataLongBMI40rm,
                              type = list("tertile"))

resultsCatBMISens <- lapply(list(dataLongBMI30rm,
                         dataLongBMI30Plus,
                         dataLongBMI40Plus), function(data){
                         model <- quadraticInflamModel(inflam =  marker,
                               data = data,
                               type = list("tertile"))

                         results <- resultsTable(modelList = model$tertile[1:3],
                                                       colNames = c(" (Unadjusted)", " (Sex Adjusted)", " (Fully Adjusted)"))
})
# ----
# Get a nice results table to put in supplement
# tertiles (categorical variable):
resultsCat <- resultsTable(modelList = models$tertile[1:3],
                           colNames = c(" (Unadjusted)", " (Sex Adjusted)", " (Fully Adjusted)"))

# BMI as a categorical variable
resultsCatBMI <- resultsTable(modelList = models$tertile[c(3,4)],
                                 colNames = c(" (Fully Adjusted - BMI continuous)"," (Fully Adjusted - BMI categorical)"))

# Sensitivity analysis of individuals that remained alive
resultsCatAlive <- resultsTable(modelList = modelsAlive$tertile[1:3],
                                     colNames = c(" (Unadjusted)", " (Sex Adjusted)", " (Fully Adjusted)"))

# Sensitivity analysis at least 2 dep questionnaires completed
resultsCatMinTwo <- resultsTable(modelList = modelsMinTwo$tertile[1:3],
                                colNames = c(" (Unadjusted)", " (Sex Adjusted)", " (Fully Adjusted)"))


# Sensitivity analysis with non log transformed BMI
resultsCatBMIraw  <- resultsTable(modelList = models$tertile[c(3,5)],
                                     colNames = c(" (Fully Adjusted - BMI log transformed)"," (BMI not log transformed)"))


# Sensitivity analysis removed people with inflammatory condition
dataLongInflam <- dataLong %>%
  filter(inflam_condition == FALSE)
modelsInflam <- quadraticInflamModel(inflam =  marker,
                                    data = dataLongInflam,
                                    type = list("tertile"))
resultsCatInflam <- resultsTable(modelList = modelsInflam$tertile[1:3],
                                 colNames = c(" (Unadjusted)", " (Sex Adjusted)", " (Fully Adjusted)"))


# Sensitivity analysis removed people taking anti-inflammatory meds
dataLongMeds <- dataLong %>%
  filter(inflam_meds == FALSE)
modelsMeds <- quadraticInflamModel(inflam =  marker,
                                   data = dataLongMeds,
                                   type = list("tertile"))
resultsCatMeds <- resultsTable(modelList = modelsInflam$tertile[1:3],
                                 colNames = c(" (Unadjusted)", " (Sex Adjusted)", " (Fully Adjusted)"))


# Continuous variable:
resultsCont <- resultsTable(modelList = models$INT[1:3],
                            colNames = c(" (Unadjusted)", " (Sex Adjusted)", " (Fully Adjusted)"))

# ----
# Calculate predict column
dataPred <- calcPredQuad(data = dataLong, model = models$tertile[[3]], condition = paste0(marker, "_tertile"))

dataPredInflam <- calcPredQuad(data = dataLongInflam, model = modelsInflam$tertile[[3]], condition = paste0(marker, "_tertile"))

dataPredMeds <- calcPredQuad(data = dataLongMeds, model = modelsMeds$tertile[[3]], condition = paste0(marker, "_tertile"))

dataPredAlive <- calcPredQuad(data = dataLongAlive, model = modelsAlive$tertile[[3]], condition = paste0(marker, "_tertile"))

dataPredMinTwo <- calcPredQuad(data = dataLongMinTwo, model = modelsMinTwo$tertile[[3]], condition = paste0(marker, "_tertile"))

dataPredBMI40rm <- calcPredQuad(data = dataLongBMI40rm, model = modelsBMI40rm$tertile[[3]], condition = paste0(marker, "_tertile"))

# ----
# Return a plot of the trajectories split by categorical variable
# Fully adjusted model = models$tertile[[3]]
plotTertiles <- plotSplitTrajQuad(model = models$tertile[[3]],
                              data = dataPred,
                              condition = paste0(marker, "_tertile"),
                              plotColours = c(  "darkolivegreen3","dodgerblue2", "darkorchid3"),
                              legendTitle = marker,
                              legendLabels = c("Bottom Third", "Middle Third", "Top Third"))

# ----
# Return a table and plot for scores at ages
# And differences between these scores at ages pair wise for each level

scoresAtAges <- plotScoresAgesCatQuad(model = models$tertile[[3]],
                                  data = dataPred,
                                  ageInputScore = c(40, 50, 60, 70, 80),
                                  condition = paste0(marker, "_tertile"),
                                  plotColours = c(  "darkolivegreen3","dodgerblue2", "darkorchid3"),
                                  legendTitle = marker,
                                  legendLabels =  c("Bottom Third", "Middle Third", "Top Third"))


# Inflammatory conditions removed sensitivity
scoresAtAgesInflamCond <- plotScoresAgesCatQuad(model = modelsInflam$tertile[[3]],
                                      data = dataPredInflam,
                                      ageInputScore = c(40, 50, 60, 70, 80),
                                      condition = paste0(marker, "_tertile"),
                                      plotColours = c(  "darkolivegreen3","dodgerblue2", "darkorchid3"),
                                      legendTitle = marker,
                                      legendLabels = c("Bottom Third", "Middle Third", "Top Third"))

# People taking anti-inflammatory meds removed sensitivity
scoresAtAgesmodelsMeds <- plotScoresAgesCatQuad(model = modelsMeds$tertile[[3]],
                                                data = dataPredMeds,
                                                ageInputScore = c(40, 50, 60, 70, 80),
                                                condition = paste0(marker, "_tertile"),
                                                plotColours = c(  "darkolivegreen3","dodgerblue2", "darkorchid3"),
                                                legendTitle = marker,
                                                legendLabels = c("Bottom Third", "Middle Third", "Top Third"))

# Sensitivity analysis of individuals that remained alive
scoresAtAgesmodelsAlive <- plotScoresAgesCatQuad(model = modelsAlive$tertile[[3]],
                                                data = dataPredAlive,
                                                ageInputScore = c(40, 50, 60, 70, 80),
                                                condition = paste0(marker, "_tertile"),
                                                plotColours = c(  "darkolivegreen3","dodgerblue2", "darkorchid3"),
                                                legendTitle = marker,
                                                legendLabels = c("Bottom Third", "Middle Third", "Top Third"))


scoresAtAgesmodelsMinTwo <- plotScoresAgesCatQuad(model = modelsMinTwo$tertile[[3]],
                                                 data = dataPredMinTwo,
                                                 ageInputScore = c(40, 50, 60, 70, 80),
                                                 condition = paste0(marker, "_tertile"),
                                                 plotColours = c(  "darkolivegreen3","dodgerblue2", "darkorchid3"),
                                                 legendTitle = marker,
                                                 legendLabels = c("Bottom Third", "Middle Third", "Top Third"))

# Remove people with BMI >= 40
scoresAtAgesmodelsBMI40rm <- plotScoresAgesCatQuad(model = modelsBMI40rm$tertile[[3]],
                                                data = dataPredBMI40rm,
                                                ageInputScore = c(40, 50, 60, 70, 80),
                                                condition = paste0(marker, "_tertile"),
                                                plotColours = c(  "darkolivegreen3","dodgerblue2", "darkorchid3"),
                                                legendTitle = marker,
                                                legendLabels = c("Bottom Third", "Middle Third", "Top Third"))


# ----
# Save files
write.csv(resultsCat, paste0("Output/traj_inflam/table_all_estimates/",PHQ,"_resultsCat", marker ,".csv"), row.names = F)
write.csv(resultsCatInflam , paste0("Output/traj_inflam/table_all_estimates/",PHQ,"_resultsCat_sensitivityInflamCond_", marker ,".csv"), row.names = F)
write.csv(resultsCatMeds , paste0("Output/traj_inflam/table_all_estimates/",PHQ,"_resultsCat_sensitivityMeds_", marker ,".csv"), row.names = F)
write.csv(resultsCatBMI, paste0("Output/traj_inflam/table_all_estimates/",PHQ,"_results_BMIranges", marker ,".csv"), row.names = F)
write.csv(resultsCatBMIraw, paste0("Output/traj_inflam/table_all_estimates/",PHQ,"_results_BMIlog_raw", marker ,".csv"), row.names = F)
write.csv(resultsCont, paste0("Output/traj_inflam/table_all_estimates/",PHQ,"_resultsCont", marker ,".csv"), row.names = F)


write.csv(resultsCatAlive, paste0("Output/traj_inflam/table_all_estimates/",PHQ,"_resultsCat_Alive", marker ,".csv"), row.names = F)
write.csv(resultsCatMinTwo, paste0("Output/traj_inflam/table_all_estimates/",PHQ,"_resultsCat_MinTwo", marker ,".csv"), row.names = F)



fileNamesBMI <- c("BMI30rm", "BMI30Plus", "BMI40Plus")
lapply(1:3, function(i){
  write.csv(resultsCatBMISens[[i]], paste0("Output/traj_inflam/table_all_estimates/",PHQ,"_results_", fileNamesBMI[i] ,"_", marker ,".csv"), row.names = F)
})


png(paste0("Output/traj_inflam/plotSplitTraj/",PHQ,"_", marker, ".png"), width = 2300, height = 1000, units = "px", res = 300)
print(plotTertiles)
dev.off()

png(paste0("Output/traj_inflam/plotScoresAtAges/",PHQ,"_", marker, ".png"), width = 2300, height = 1000, units = "px", res = 300)
print(scoresAtAges$plot)
dev.off()

png(paste0("Output/traj_inflam/plotScoresAtAges/",PHQ,"_", marker, "_inflamCondSens.png"), width = 2300, height = 1000, units = "px", res = 300)
print(scoresAtAgesInflamCond$plot)
dev.off()

png(paste0("Output/traj_inflam/plotScoresAtAges/",PHQ,"_", marker, "_MinTwoSens.png"), width = 2300, height = 1000, units = "px", res = 300)
print(scoresAtAgesmodelsMinTwo$plot)
dev.off()


write.csv(scoresAtAges$scores, paste0("Output/traj_inflam/tableScoresAtAges/",PHQ,"scores", marker ,".csv"), row.names = T)
write.csv(scoresAtAges$difference, paste0("Output/traj_inflam/tableScoresAtAges/",PHQ,"difference", marker ,".csv"), row.names = F)

write.csv(scoresAtAgesInflamCond$scores, paste0("Output/traj_inflam/tableScoresAtAges/",PHQ,"_scores_inflamCondSens", marker ,".csv"), row.names = T)
write.csv(scoresAtAgesInflamCond$difference, paste0("Output/traj_inflam/tableScoresAtAges/",PHQ,"_difference_inflamCondSens", marker ,".csv"), row.names = F)

write.csv(scoresAtAgesmodelsMinTwo$scores, paste0("Output/traj_inflam/tableScoresAtAges/",PHQ,"_scores_MinTwoSens", marker ,".csv"), row.names = T)
write.csv(scoresAtAgesmodelsMinTwo$difference, paste0("Output/traj_inflam/tableScoresAtAges/",PHQ,"_difference_MinTwoSens", marker ,".csv"), row.names = F)

