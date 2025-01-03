# Function to calculate scores at ages for categorical variables

plotScoresAgesCat <- function(model, ageInputScore, condition, conditionPlot, data, plotColours, legendTitle, legendLabels, sex = FALSE){
  ageOrig <- data %>% pull(age_original)
  ageOrig <- ageOrig[!is.na(ageOrig)]

  score_glht <- lapply(as.numeric(ageInputScore), function(x){
    n <- length(levels(pull(data, !!sym(condition))))

    rowIndex <- which(str_detect(string = row.names(summary(model)$coefficients),
                                 pattern = condition) &
                        str_detect(string = row.names(summary(model)$coefficients),
                                   pattern = ":", negate = T))

    rowNames <- rownames(summary(model)$coefficients)

    ageInput <- round(x - 10, 3)
    ageInput2 <- ageInput^2
    ageInput3 <- ageInput^3
    ageInput4 <- ageInput^4

    equations <- sapply(1:(n-1), function(i){
      paste0(rowNames[1], " + ",
             rowNames[rowIndex[i]] , " + ",
             rowNames[2], "*", ageInput, " + \`",
             rowNames[3], "\`*", ageInput2, " + \`",
             rowNames[4], "\`*", ageInput3,  " + \`",
             rowNames[5], "\`*", ageInput4,  " + \`",
             rowNames[2], ":", rowNames[rowIndex[i]],"\`*",ageInput , " + \`",
             rowNames[3], ":", rowNames[rowIndex[i]],"\`*",ageInput2 , " + \`",
             rowNames[4], ":", rowNames[rowIndex[i]],"\`*",ageInput3 , " + \`",
             rowNames[5], ":", rowNames[rowIndex[i]],"\`*",ageInput4 )
    })

    res <- lapply(c( paste0(rowNames[1], " + ",
                            rowNames[2], "*", ageInput," + \`",
                            rowNames[3], "\`*", ageInput2,  " + \`",
                            rowNames[4], "\`*", ageInput3,  " + \`",
                            rowNames[5], "\`*", ageInput4),
                     equations),
                  function(X) deltaMethod(model,  X))%>%
      do.call(rbind,.)

      # Tidy results dataframe and rename rows/columns
      rowname <- paste0("SMFQ Score")

      res <- data.frame(estimate = res$Estimate,
                        conf.low = res$`2.5 %`,
                        conf.high = res$`97.5 %`)

      levelNames <- as.character(levels(as.factor(pull(data, !!sym(condition)))))

      res <-  res %>%
        mutate(condition = levelNames)

      return( res )
    })

  estimate <- lapply(score_glht, function(df) {
    df %>%
      dplyr::select(estimate)
  })  %>% do.call(cbind, .)
  colnames(estimate) <- ageInputScore

    estimate <- estimate %>%
      gather(age, score, 1:ncol(estimate)) %>%
      mutate(age = as.numeric(age))

    conf.low <- lapply(score_glht, function(df) {df %>% dplyr::select(conf.low)}) %>% do.call(cbind, .)
    colnames(conf.low) <- ageInputScore
    conf.low  <- conf.low %>%
      gather(age, conf.low, 1:ncol(conf.low)) %>%
      mutate(age = as.numeric(age))

    conf.high <- lapply(score_glht, function(df) {df %>% dplyr::select(conf.high)}) %>% do.call(cbind, .)
    colnames(conf.high) <- ageInputScore
    conf.high  <- conf.high %>%
      gather(age, conf.high, 1:ncol(conf.high)) %>%
      mutate(age = as.numeric(age))%>%
      dplyr::select(-age)

    conf <- cbind(conf.low, conf.high)
    conf$error_bar_colour <- as.character(rep(c("Bottom", "Middle", "Top"), nrow(conf)/3))

if(sex == FALSE){
    p <- ggplot() +
      geom_line(data = data, aes(x= age_original ,  y = pred, color = !!sym(condition) ) , linewidth = 1, na.rm=T) +
      theme(legend.text = element_text(color = "black")) +
      geom_errorbar(data = conf, aes(x = age, ymin = conf.low, ymax = conf.high, color = error_bar_colour ), width = 0.5, alpha = 0.5) +
      scale_color_manual(values = plotColours,
                         labels = legendLabels,
                         name = "IL-6 Tertile Group") +
      geom_point(data = estimate, aes(x = age, y = score), col = "black", size = 3, alpha = 0.5) +
      ylab(paste0("SMFQ Score")) +
      xlab("Age (years)") +
      labs(color = paste0(legendTitle))+
      scale_x_continuous(limits = c(10, 30))
}else{
  data <- data %>%
    mutate(Sex = ifelse(Sex == 0, "Male", "Female"))
  p <- ggplot() +
    geom_line(data = data, aes(x= age_original ,  y = pred, color = !!sym(conditionPlot), linetype = Sex ) , linewidth = 1, na.rm=T) +
    theme(legend.text = element_text(color = "black")) +
    geom_errorbar(data = conf, aes(x = age, ymin = conf.low, ymax = conf.high, color = error_bar_colour), width = 0.5, alpha = 0.5) +
    scale_color_manual(values = plotColours,
                       labels = legendLabels,
                       name = "IL-6 Tertile Group") +
    scale_linetype_manual(values=c("solid", "dotdash"))+
    geom_point(data = estimate, aes(x = age, y = score), col = "black", size = 3, alpha = 0.5) +
    ylab(paste0("SMFQ Score")) +
    xlab("Age (years)") +
    labs(color = paste0(legendTitle))+
    scale_x_continuous(limits = c(10, 30))

}


    # --------------------------------
    # Return a table of these scores
    estimateCI <- lapply(1:length(score_glht), function(i) {
      df <- score_glht[[i]] %>%
        mutate(Score = round(estimate,3)) %>%
        mutate(Lower_CI = round(conf.low,3)) %>%
        mutate(Upper_CI = round(conf.high,3)) %>%
        dplyr::select(c(Score, Lower_CI, Upper_CI))
      colnames(df)[1] <- paste0("Score (Age ", ageInputScore[i], ")")
      df
    })  %>% do.call(cbind, .)
    estimateCI

    # -------------------------------
    # Return a table of the differences in these scores
    levelsScoresList <- combn(rev(levels(pull(data, !!sym(condition)))), 2, simplify = FALSE)

    difTabList <- lapply(levelsScoresList, function(levelsScores){

    ageOrig <- data %>% pull(age_original)
    ageOrig <- ageOrig[!is.na(ageOrig)]

    differenceScores <- lapply(as.numeric(ageInputScore), function(x){
      ageInput <- round(x - 10, 3)
      ageInput2 <- ageInput^2
      ageInput3 <- ageInput^3
      ageInput4 <- ageInput^4

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
          "(", rowNames[1], " + ", rowNames[2], "*", ageInput, " + ", rowNames[3], "*", ageInput2, " + ", rowNames[4], "*", ageInput3, " + ", rowNames[5], "*", ageInput4, " + ", levelNames1[1], " + ", levelNames1[2], "*", ageInput, " + ", levelNames1[3], "*", ageInput2, " + ", levelNames1[4], "*", ageInput3, " + ", levelNames1[5], "*", ageInput4,  ") - (", rowNames[1], " + ", rowNames[2], "*", ageInput, " + ", rowNames[3], "*", ageInput2, " + ", rowNames[4], "*", ageInput3, " + ", rowNames[5], "*", ageInput4, " + ", levelNames2[1], " + ", levelNames2[2], "*", ageInput, " + ", levelNames2[3], "*", ageInput2, " + ", levelNames2[4], "*", ageInput3, " + ", levelNames2[5], "*", ageInput4,  ")"
        )), parameterNames = rowNames )

      }else{
        levelNames1 <- rowNames[str_detect(rowNames, paste0(levelNames, collapse = "|"))]

        res <- deltaMethod(model, c(paste0(
          "(", rowNames[1], " + ", rowNames[2], "*", ageInput, " + ", rowNames[3], "*", ageInput2, " + ", rowNames[4], "*", ageInput3, " + ", rowNames[5], "*", ageInput4, " + ", levelNames1[1], " + ", levelNames1[2], "*", ageInput, " + ", levelNames1[3], "*", ageInput2, " + ", levelNames1[4], "*", ageInput3, " + ", levelNames1[5], "*", ageInput4,  ") - (", rowNames[1], " + ", rowNames[2], "*", ageInput, " + ", rowNames[3], "*", ageInput2, " + ", rowNames[4], "*", ageInput3, " + ", rowNames[5], "*", ageInput4,   ")"
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
    difTab <- cbind(data.frame("Marker" = paste0(legendTitle, " tertile: ", levelsScores[1]," vs ", levelsScores[2])),
                    difTab)
    return(difTab)
    })

    difTab <- do.call(rbind, difTabList)

    returnList <- list(p, estimateCI, difTab)
    names(returnList) <- c("plot", "scores", "difference")

    return(returnList)

  }


