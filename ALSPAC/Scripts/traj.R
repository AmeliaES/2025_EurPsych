# -------------------------
# Make depression trajectory for linear, quadratic, cubic and quartic.
# Compare model fit between those 4, choose the best model to take forward.
# -------------------------
# Compare model results between linear, quadratic, cubic and quartic
modLin <- lmer(dep ~ age + (age|Subject), REML=F , data = dataLong)
modQuad <- lmer(dep ~ age  + I(age^2) + (age + I(age^2)|Subject), REML=F , data = dataLong)
modCubic <- lmer(dep ~ age + I(age^2) + I(age^3) + (age + I(age^2)|Subject), REML=F , data = dataLong)
modQuartic <- lmer(dep ~ age + I(age^2) + I(age^3) + I(age^4) + (age + I(age^2)|Subject), REML=F , data = dataLong)

# Make a results table similar to Supplementary Table 3 in Alex's JYA 2019 paper
comparePoly <- resultsTable(modelList = list(modLin, modQuad, modCubic, modQuartic),
             colNames = c(" (Linear)", " (Quadratic)", " (Cubic)", " (Quartic)"))

write.csv(comparePoly, "Output/traj/modelComparison_poly.csv", row.names = F)


# Likelihood ratio test between these models
tidy(anova(modLin, modQuad)) %>%
  write.csv(., "Output/traj/modelComparison_poly_LRTLinQuad.csv", row.names = F)
tidy(anova(modQuad, modCubic)) %>%
  write.csv(., "Output/traj/modelComparison_poly_LRTQuadCubic.csv", row.names = F)
tidy(anova(modCubic, modQuartic)) %>%
  write.csv(., "Output/traj/modelComparison_poly_LRTCubicQuart.csv", row.names = F)


# Plot of population trajectories
png(paste0("Output/traj/plotTraj.png"), width = 2900, height = 2600, units = "px", res = 300)
plotList <- list(
  plotTraj(modLin, "Linear") +
    ggtitle("Linear"),
  plotTraj(modQuad, "Quadratic") +
    ggtitle("Quadratic"),
  plotTraj(modCubic, "Cubic") +
    ggtitle("Cubic"),
  plotTraj(modQuartic, "Quartic") +
    ggtitle("Quartic"))

print(plot_grid(plotlist = plotList,
          nrow = 2,
          ncol = 2))

dev.off()

# -------------------------
# Compare unadjusted, sex adjusted and fully adjusted models for the model we take forward
# Compare model results between unadjusted, partially adjusted and fully adjusted models (for the model we are taking forward)
modUnadjusted <- lmer(as.formula("dep ~ age + I(age^2) + I(age^3) + I(age^4)  + (age + I(age^2)|Subject)"), REML=F , data = dataLong)

modSex <- lmer(as.formula("dep ~ age + I(age^2) + I(age^3) + I(age^4) + Sex + (age + I(age^2)|Subject)"), REML=F , data = dataLong)

modFull <- lmer(as.formula("dep ~ age + I(age^2) + I(age^3) + I(age^4) + Sex + Maternal.education.at.birth + BMI_age9 + (age + I(age^2)|Subject)"), REML=F , data = dataLong)

compareCovars <- resultsTable(modelList = list(modUnadjusted, modSex, modFull),
             colNames = c(" (Unadjusted)", " (Sex Adjusted)", " (Fully Adjusted)"))

write.csv(compareCovars, "Output/traj/modelComparison_covars.csv", row.names = F)

# -------------------------
# Plot for unadjusted, sex adjusted and fully adjusted quartic models

plotTraj(model = modUnadjusted,
         modelType = "Quartic",
         desc = TRUE)

plotTraj(model = modSex,
         modelType = "Quartic",
         desc = TRUE)

plotTraj(model = modFull,
         modelType = "Quartic",
         desc = TRUE)

# -------------------------
