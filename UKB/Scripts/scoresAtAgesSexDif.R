# Calculate scores at ages sex difference and plot these differences

marker <- "IL6"
model = models$tertile[[2]]
data = dataPred
ageInputScore = c(40, 50, 60, 70, 80)
condition = paste0(marker, "_sex_tertile")
sex = TRUE

# ----------------------------
levelsScoresList <- combn(rev(levels(pull(data, !!sym(condition)))), 2, simplify = FALSE)

difTabList <- lapply(levelsScoresList, function(levelsScores){

  ageOrig <- data %>% pull(age_original)
  ageOrig <- ageOrig[!is.na(ageOrig)]

  differenceScores <- lapply(as.numeric(ageInputScore), function(x){
    ageInput <- round(x - 39, 3)
    ageInput2 <- ageInput^2

    coef <- summary(model)$coefficients

    rowIndex <- which(str_detect(string = row.names(coef),
                                 pattern = condition) &
                        str_detect(string = row.names(coef),
                                   pattern = ":", negate = T))

    rowNames <- rownames(coef) %>%
      str_remove_all("\\(|I\\(|\\^|\\)|\\:")

    levelNames <- paste0(condition,levelsScores) %>%
      str_remove_all("\\(|I\\(|\\^|\\)|\\:")

    if( sum(str_detect(rowNames, levelNames[1])) == sum(str_detect(rowNames, levelNames[2])) ){
      levelNames1 <- rowNames[str_detect(rowNames, levelNames[1])]
      levelNames2 <- rowNames[str_detect(rowNames, levelNames[2])]

      res <- deltaMethod(model, c(paste0(
        "(", rowNames[1], " + ", rowNames[2], "*", ageInput, " + ", rowNames[3], "*", ageInput2, " + ", levelNames1[1], " + ", levelNames1[2], "*", ageInput, " + ", levelNames1[3], "*", ageInput2, ") - (", rowNames[1], " + ", rowNames[2], "*", ageInput, " + ", rowNames[3], "*", ageInput2, " + ", levelNames2[1], " + ", levelNames2[2], "*", ageInput, " + ", levelNames2[3], "*", ageInput2,  ")"
      )), parameterNames = rowNames )

    }else{
      levelNames1 <- rowNames[str_detect(rowNames, paste0(levelNames, collapse = "|"))]

      res <- deltaMethod(model, c(paste0(
        "(", rowNames[1], " + ", rowNames[2], "*", ageInput, " + ", rowNames[3], "*", ageInput2," + ", levelNames1[1], " + ", levelNames1[2], "*", ageInput, " + ", levelNames1[3], "*", ageInput2,  ") - (", rowNames[1], " + ", rowNames[2], "*", ageInput, " + ", rowNames[3], "*", ageInput2,  ")"
      )), parameterNames = rowNames )

    }

    # Calculate p-values: https://www.bmj.com/content/343/bmj.d2304
    # 1. Calculate SE
    # 2. Calculate test statistic
    # 3. Calculate the p-value
    p <- 2 * (1 - pnorm(abs(res$Estimate/((res$`97.5 %` - res$`2.5 %`)/(2*1.96)) )))

    FDRp <- p.adjust(p, method = "fdr", n = length(ageInputScore))

    difTab <- data.frame("Difference" = round(res$Estimate, 3),
                         "Lower_CI" = round(res$`2.5 %`,3),
                         "Upper_CI" = round(res$`97.5 %`,3),
                         "uncorrected_p" = ifelse(p < 0.0001, "p<0.0001", paste0(round(p, 4))),
                         "FDR_p" = ifelse(FDRp < 0.0001, "p<0.0001", paste0(round(FDRp, 4))))
    colnames(difTab)[1] <- paste0("Difference (age ", x, ")")

    # dif <- paste0( round(res$Estimate, 3), " (", round(res$`2.5 %`,3), ", ", round(res$`97.5 %`,3), ") ",  p)
    # res <- data.frame(age = dif)
    return(difTab)
  })

  difTab <- do.call(cbind, differenceScores)
  difTab <- cbind(data.frame("Marker" = paste0("IL6 tertile: ", levelsScores[1]," vs ", levelsScores[2])),
                  difTab)
  return(difTab)
})

difTab <- do.call(rbind, difTabList)
difTab





