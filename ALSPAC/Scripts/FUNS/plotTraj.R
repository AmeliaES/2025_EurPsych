# Main plot of linear, quadratic, cubic and quartic models

# -------------------------
plotTraj <- function(model, modelType, desc = TRUE){

  if(modelType != "Quartic"){
    dataPred <- dataLong %>%
    mutate(pred = predict(model, ., re.form = NA))
  }else{
    age <- dataLong %>% pull(age)
    coef <- summary(model)$coefficients
    adjustedScore <- age * coef[2,1] + coef[1,1]  +
      age^2 * coef[3,1] +
      age^3 * coef[4,1] +
      age^4 * coef[5,1]
    dataPred <- dataLong %>%
      mutate(pred = adjustedScore)
  }

  ageOrig <- dataPred %>% pull(age_original)
  ageOrig <- ageOrig[!is.na(ageOrig)]

  ageCalcs <- c(min(ageOrig), seq(round(min(ageOrig), 1), round(max(ageOrig), 1), 0.1), max(ageOrig) )

  score_glht <- lapply(ageCalcs, function(x){

    rowNames <- rownames(summary(model)$coefficients)

    ageInput <- round(x - 39, 3) # age is centered to the start of the trajectory, age 39y
    ageInput2 <- ageInput^2
    ageInput3 <- ageInput^3
    ageInput4 <- ageInput^4
    # --------------------
    # Change for model type
    if(modelType == "Linear"){
      res <- deltaMethod(model, c( paste0(rowNames[1], " + ", rowNames[2], "*", ageInput)) )
    }else if(modelType == "Quadratic"){


      res <- deltaMethod(model, c( paste0(rowNames[1], " + ",
                                               rowNames[2], "*", ageInput," + \`",
                                               rowNames[3], "\`*", ageInput2)) )
    } else if(modelType == "Cubic"){


      res <- deltaMethod(model, c( paste0(rowNames[1], " + ",
                                               rowNames[2], "*", ageInput," + \`",
                                               rowNames[3], "\`*", ageInput2,  " + \`",
                                               rowNames[4], "\`*", ageInput3) ) )

    } else if(modelType == "Quartic"){


      res <- deltaMethod(model, c( paste0(rowNames[1], " + ",
                                               rowNames[2], "*", ageInput," + \`",
                                               rowNames[3], "\`*", ageInput2,  " + \`",
                                               rowNames[4], "\`*", ageInput3,  " + \`",
                                               rowNames[5], "\`*", ageInput4) ))
    }

    # --------------------

    res <- data.frame(estimate = res$Estimate,
                      conf.low = res$`2.5 %`,
                      conf.high = res$`97.5 %`) %>%
      mutate(across(where(is.numeric), round, 2))
    rownames(res) <- paste0("SMFQ Score")
    res

    return( res )
  })

  # ------------------------------------------
  # Get the mean and sd for depression scores at each time point
  df.plot <-
    dataPred %>%
      group_by(across( age_cat )) %>%
      summarise(Age = mean(age_original, na.rm = T),
                Phenotype = mean(dep, na.rm = T),
                SD = sd(dep, na.rm = T),
                n = sum(!is.na( dep ))
      ) %>%
      mutate(upper = Phenotype + ( qnorm(0.975)*SD/sqrt(n) ),
             lower = Phenotype - ( qnorm(0.975)*SD/sqrt(n) ))

# ------------------------------------------

  estimate <- do.call(rbind, score_glht) %>%
    mutate(age = ageCalcs)

  if(desc == TRUE){
  p <-  ggplot() +
      geom_line(data = dataPred, aes(x= age_original ,  y = pred), color = "#1D86C7", linewidth = 1.5, na.rm=T) +
      geom_ribbon(data = estimate, aes(x= age , ymin = conf.low, ymax = conf.high), fill = "#1D86C7", alpha = 0.2, na.rm = T) +
      geom_point(data = df.plot,aes(x=Age, y=Phenotype))+
      geom_line(data = df.plot,aes(x=Age, y=Phenotype)) +
      geom_errorbar(data = df.plot, aes(x=Age, y=Phenotype, ymin = lower, ymax = upper)) +
      ylab(paste0("SMFQ Score")) +
      xlab("Age (years)")
  }else{
    p <-  ggplot() +
      geom_line(data = dataPred, aes(x= age_original ,  y = pred), color = "#1D86C7", linewidth = 1.5, na.rm=T) +
      geom_ribbon(data = estimate, aes(x= age , ymin = conf.low, ymax = conf.high), fill = "#1D86C7", alpha = 0.2, na.rm = T) +
      ylab(paste0("SMFQ Score")) +
      xlab("Age (years)")
  }

 return(p)

}










