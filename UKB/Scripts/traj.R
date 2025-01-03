# -------------------------
# Make depression trajectory for linear, quadratic,
# Compare model fit
# -------------------------
PHQ <- "PHQ2"

dataLong <- fread(paste0("/Volumes/GenScotDepression/users/amelia/UKB/UKB_dataLong_",PHQ,".csv"))  %>%
  mutate_at(c("Sex", "IL6_tertile"), as.factor)

# Compare model results between linear, quadratic
modLin <- lmer(dep ~ age + (age|f.eid), REML=FALSE , data = dataLong)
modQuad <- lmer(dep ~ age  + I(age^2) + (age|f.eid), REML=F , data = dataLong)
# modQuadRand <- lmer(dep ~ age  + I(age^2) + (age + I(age^2) |f.eid), REML=F , data = dataLong)
# Error: number of observations (=110789) <= number of random effects (=130707) for term (age + I(age^2) | f.eid); the random-effects parameters and the residual variance (or scale parameter) are probably unidentifiable

# Make a results table similar to Supplementary Table 3 in Alex's JYA 2019 paper
comparePoly <- resultsTable(modelList = list(modLin, modQuad),
                            colNames = c(" (Linear)", " (Quadratic)"))

write.csv(comparePoly, paste0("Output/traj/",PHQ,"modelComparison_poly.csv"), row.names = F)

# Likelihood ratio test between these models
tidy(anova(modLin, modQuad)) %>%
  write.csv(., paste0("Output/traj/",PHQ,"modelComparison_poly_LRT.csv"), row.names = F)

# Plot of population trajectories
plotList <- list(
  plotTraj(modLin, "Linear") +
    ggtitle("Linear"),
  plotTraj(modQuad, "Quadratic") +
    ggtitle("Quadratic"))

png(paste0("Output/traj/",PHQ,"models.png"), width = 3000, height = 1500, res = 300)
p <- plot_grid(plotlist = plotList,
          nrow = 1,
          ncol = 2)
print(p)
dev.off()

# -------------------------
# Compare unadjusted, sex adjusted and fully adjusted models for the model we take forward
# Compare model results between unadjusted, partially adjusted and fully adjusted models (for the model we are taking forward)
modUnadjusted <- lmer(as.formula("dep ~ age + I(age^2)   + (age |f.eid)"), REML=F , data = dataLong)

modSex <- lmer(as.formula("dep ~ age + I(age^2) + Sex + (age |f.eid)"), REML=F , data = dataLong)

modFull <- lmer(as.formula("dep ~ age + I(age^2)  + Sex + Townsend + BMI + (age |f.eid)"), REML=F , data = dataLong)

compareCovars <- resultsTable(modelList = list(modUnadjusted, modSex, modFull),
                              colNames = c(" (Unadjusted)", " (Sex Adjusted)", " (Fully Adjusted)"))

write.csv(compareCovars, paste0("Output/traj/",PHQ,"modelComparison_covars.csv"), row.names = F)

