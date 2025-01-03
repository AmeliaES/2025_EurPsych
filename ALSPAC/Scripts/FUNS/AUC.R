# Calculate AUC
# ------------
AUC <- function(model, ageInputScore, condition, data, plotColours, legendTitle, legendLabels){

  coef <- summary(model)$coefficients

  ageOrig <- data %>%
    pull(age_original)
  ageOrig <- ageOrig[!is.na(ageOrig)]
  age1 <- ageInputScore[1] - mean(ageOrig)
  age2 <- ageInputScore[2] - mean(ageOrig)

  rowNames <- rownames(coef) %>%
    str_remove_all("\\(|I\\(|\\^|\\)|\\:")

  AUC <- deltaMethod(model, c( paste0("(((", age2, ")*(", rowNames[1], ")) + ((", rowNames[2], ")*(", age2, ")^2/2) + ((", rowNames[3], ")*(", age2, ")^3/3) + ((", rowNames[4], ")*(", age2, ")^4/4) + ((", rowNames[5], ")*(", age2, ")^5/5)) - (((", age1,")*(", rowNames[1], ")) + ((", rowNames[2], ")*(", age1, ")^2/2) + ((", rowNames[3], ")*(", age1, ")^3/3) + ((", rowNames[4], ")*(", age1, ")^4/4) + ((", rowNames[5], ")*(", age1, ")^5/5))") ), parameterNames = rowNames )

  AUC <- data.frame("AUC" = round(AUC$Estimate, 3),
                    "Lower_CI" = round(AUC$`2.5 %`,3),
                    "Upper_CI" = round(AUC$`97.5 %`,3))


  rowIndex <- which(str_detect(string = row.names(summary(model)$coefficients),
                               pattern = condition) &
                      str_detect(string = row.names(summary(model)$coefficients),
                                 pattern = ":", negate = T))

  rowIndexInteract1 <- which(str_detect(string = row.names(summary(model)$coefficients),
                                        pattern = condition) &
                               str_detect(string = row.names(summary(model)$coefficients),
                                          pattern = ":") &
                               str_detect(string = row.names(summary(model)$coefficients),
                                          pattern = "\\^", negate = T))

  rowIndexInteract2 <- which(str_detect(string = row.names(summary(model)$coefficients),
                                        pattern = condition) &
                               str_detect(string = row.names(summary(model)$coefficients),
                                          pattern = ":") &
                               str_detect(string = row.names(summary(model)$coefficients),
                                          pattern = "\\^2"))

  rowIndexInteract3 <- which(str_detect(string = row.names(summary(model)$coefficients),
                                        pattern = condition) &
                               str_detect(string = row.names(summary(model)$coefficients),
                                          pattern = ":") &
                               str_detect(string = row.names(summary(model)$coefficients),
                                          pattern = "\\^3"))

  rowIndexInteract4 <- which(str_detect(string = row.names(summary(model)$coefficients),
                                        pattern = condition) &
                               str_detect(string = row.names(summary(model)$coefficients),
                                          pattern = ":") &
                               str_detect(string = row.names(summary(model)$coefficients),
                                          pattern = "\\^4"))

  n <- length(unique(pull(data, !!sym(condition))))

  AUCCovs <- lapply(1:(n-1), function(i){
    AUC <- deltaMethod(model,
                       c( paste0(
                         "(((",age2,")*(",rowNames[1],")) + ((",rowNames[2],")*(",age2,")^2/2) + ((",rowNames[3],")*(",age2,")^3/3) + ((",rowNames[4],")*(",age2,")^4/4) + ((",rowNames[5],")*(",age2,")^5/5) + ((",rowNames[rowIndex[i]],")*(",age2,")) + ((",rowNames[rowIndexInteract1[i]],")*(",age2,")^2/2) + ((",rowNames[rowIndexInteract2[i]], ")*(",age2,")^3/3) + ((",rowNames[rowIndexInteract3[i]], ")*(", age2, ")^4/4)  + ((",rowNames[rowIndexInteract4[i]], ")*(", age2, ")^5/5)) -
                                 (((",age1,")*(",rowNames[1],")) + ((",rowNames[2],")*(",age1,")^2/2) + ((",rowNames[3],")*(",age1,")^3/3) + ((",rowNames[4],")*(",age1,")^4/4) + ((",rowNames[5],")*(",age1,")^5/5) + ((",rowNames[rowIndex[i]],")*(",age1,")) + ((",rowNames[rowIndexInteract1[i]],")*(",age1,")^2/2) + ((",rowNames[rowIndexInteract2[i]], ")*(",age1,")^3/3) + ((",rowNames[rowIndexInteract3[i]], ")*(", age1, ")^4/4) + ((",rowNames[rowIndexInteract4[i]], ")*(", age1, ")^5/5) )"
                       ) )
                       , parameterNames = rowNames )

    AUC <- data.frame("AUC" = round(AUC$Estimate, 3),
                 "Lower_CI" = round(AUC$`2.5 %`,3),
                 "Upper_CI" = round(AUC$`97.5 %`,3))
  })


  # ----------------------
  # Plot
  plot <- ggplot(data = data) +
    geom_ribbon(data = data,
                aes(x = age_original, ymax = pred, ymin = 0, fill = !!sym(condition)),
                alpha = 0.1, show.legend = FALSE) +
    coord_cartesian(xlim = c(ageInputScore[1], ageInputScore[2])) +
    geom_line(aes(x = age_original, y = pred, color = !!sym(condition)), na.rm = TRUE) +
    theme(legend.text = element_text(color = "black")) +
    ylab(paste0("SMFQ Score")) +
    xlab("Age (years)") +
    scale_x_continuous(breaks = seq(round(min(data$age_original, na.rm =T)), round(max(data$age_original, na.rm =T)), by = 1),
                       expand = c(0, 0))+
    scale_y_continuous(expand = c(0, 0))


  # ----------------------
  # Table
  df <-
    rbind(
      AUC,
      do.call(rbind, AUCCovs)
    )
  levelNamesAUC <- as.character(levels(as.factor(pull(data, condition))))
  rownames(df) <- c(paste0("AUC (SMFQ Score) [", condition, ", level = ", levelNamesAUC, " ]") )

  # ----------------------
  # Differences in AUC between factor levels

  levelsAUCList <- combn(rev(as.numeric(levels(pull(data, !!sym(condition))))), 2, simplify = FALSE)

  difTabList <- lapply(levelsAUCList, function(levelsAUC){

  levelNames <- paste0(condition,levelsAUC) %>%
    str_remove_all("\\(|I\\(|\\^|\\)|\\:")

  if( sum(str_detect(rowNames, levelNames[1])) == sum(str_detect(rowNames, levelNames[2])) ){

    levelNames1 <- rowNames[str_detect(rowNames, levelNames[1])]
    levelNames2 <- rowNames[str_detect(rowNames, levelNames[2])]
    res <- deltaMethod(model, c( paste0(" (( ((",age2,")*(",rowNames[1],")) + ((",rowNames[2],")*(",age2,")^2/2)  + ((",rowNames[3],")*(",age2,")^3/3) + ((",rowNames[4],")*(",age2,")^4/4) + ((",rowNames[5],")*(",age2,")^5/5)   + ( (",levelNames1[1],")*(",age2,") ) + ((",levelNames1[2],")*(",age2,")^2/2) + ((",levelNames1[3],")*(",age2,")^3/3) + ((",levelNames1[4],")*(",age2,")^4/4)  + ((",levelNames1[5],")*(",age2,")^5/5) ) - ( ((",age1,")*(",rowNames[1],")) + ((",rowNames[2],")*(",age1,")^2/2) + ((",rowNames[3],")*(",age1,")^3/3)  + ((",rowNames[4],")*(",age1,")^4/4) + ((",rowNames[5],")*(",age1,")^5/5)  + ( (",levelNames1[1],")*(",age1,") ) + ((",levelNames1[2],")*(",age1,")^2/2) + ((",levelNames1[3],")*(",age1,")^3/3) + ((",levelNames1[4],")*(",age1,")^4/4) + ((",levelNames1[5],")*(",age1,")^5/5) )) -  (( ((",age2,")*(",rowNames[1],")) + ((",rowNames[2],")*(",age2,")^2/2)   + ((",rowNames[3],")*(",age2,")^3/3)  + ((",rowNames[4],")*(",age2,")^4/4) + ((",rowNames[5],")*(",age2,")^5/5)  + ( (",levelNames2[1],")*(",age2,") ) + ((",levelNames2[2],")*(",age2,")^2/2) + ((",levelNames2[3],")*(",age2,")^3/3)  + ((",levelNames2[4],")*(",age2,")^4/4) + ((",levelNames2[5],")*(",age2,")^5/5) ) - ( ((",age1,")*(",rowNames[1],")) + ((",rowNames[2],")*(",age1,")^2/2) + ((",rowNames[3],")*(",age1,")^3/3)  + ((",rowNames[4],")*(",age1,")^4/4)  + ((",rowNames[5],")*(",age1,")^5/5)   + ( (",levelNames2[1],")*(",age1,") ) + ((",levelNames2[2],")*(",age1,")^2/2) + ((",levelNames2[3],")*(",age1,")^3/3) + ((",levelNames2[4],")*(",age1,")^4/4)  + ((",levelNames2[5],")*(",age1,")^5/5) )) ") ) ,parameterNames = rowNames)
  }else{
    levelNames1 <- rowNames[str_detect(rowNames, paste0(levelNames, collapse = "|"))]

    res <- deltaMethod(model, c( paste0(" (( ((",age2,")*(",rowNames[1],")) + ((",rowNames[2],")*(",age2,")^2/2)  + ((",rowNames[3],")*(",age2,")^3/3) + ((",rowNames[4],")*(",age2,")^4/4) + ((",rowNames[5],")*(",age2,")^5/5)   + ( (",levelNames1[1],")*(",age2,") ) + ((",levelNames1[2],")*(",age2,")^2/2) + ((",levelNames1[3],")*(",age2,")^3/3) + ((",levelNames1[4],")*(",age2,")^4/4)  + ((",levelNames1[5],")*(",age2,")^5/5) ) - ( ((",age1,")*(",rowNames[1],")) + ((",rowNames[2],")*(",age1,")^2/2) + ((",rowNames[3],")*(",age1,")^3/3)  + ((",rowNames[4],")*(",age1,")^4/4) + ((",rowNames[5],")*(",age1,")^5/5)  + ( (",levelNames1[1],")*(",age1,") ) + ((",levelNames1[2],")*(",age1,")^2/2) + ((",levelNames1[3],")*(",age1,")^3/3) + ((",levelNames1[4],")*(",age1,")^4/4) + ((",levelNames1[5],")*(",age1,")^5/5) )) -  (( ((",age2,")*(",rowNames[1],")) + ((",rowNames[2],")*(",age2,")^2/2)   + ((",rowNames[3],")*(",age2,")^3/3)  + ((",rowNames[4],")*(",age2,")^4/4) + ((",rowNames[5],")*(",age2,")^5/5)   ) - ( ((",age1,")*(",rowNames[1],")) + ((",rowNames[2],")*(",age1,")^2/2) + ((",rowNames[3],")*(",age1,")^3/3)  + ((",rowNames[4],")*(",age1,")^4/4)  + ((",rowNames[5],")*(",age1,")^5/5)    )) ") ) ,parameterNames = rowNames)

  }

  # Calculate p-values: https://www.bmj.com/content/343/bmj.d2304
  # 1. Calculate SE
  # 2. Calculate test statistic
  # 3. Calculate the p-value
  p <- 2 * (1 - pnorm(abs(res$Estimate/((res$`97.5 %` - res$`2.5 %`)/(2*1.96)) )))
  p <- ifelse(p < 0.0001, "p<0.0001", paste0(round(p, 4)))

  difTab <- data.frame("Marker" = paste0(legendTitle, " quartile: ", levelsAUC[1]," vs ", levelsAUC[2]),
                        "Difference" = round(res$Estimate, 3),
                       "Lower_CI" = round(res$`2.5 %`,3),
                       "Upper_CI" = round(res$`97.5 %`,3),
                       "p" = p)
  colnames(difTab)[2] <- paste0("AUC difference (age ", ageInputScore[1], "-", ageInputScore[2], ")")
  return(difTab)

  })

  difTab <- do.call(rbind, difTabList)

  returnList <- list(plot, df, difTab)
  names(returnList) <- c("plot", "tableAUC", "difference")

  return(returnList)


}
