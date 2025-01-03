# Function to take a condtional model on a categorical variable and return a plot of trajectories
# also pass dataset, condition eg. IL6_quartile, and a vector of colours where the length is the same as the length of condition levels
# ------------------------------------

plotSplitTraj <- function(model, data, condition, conditionPlot, plotColours, legendTitle, legendLabels, sex = FALSE){

# -----------
# Calculate 95% CIs for these predictions
ageOrig <- data %>% pull(age_original)
ageOrig <- ageOrig[!is.na(ageOrig)]
ageCalcs <-  c(min(ageOrig), seq(round(min(ageOrig), 1), round(max(ageOrig), 1), 0.5), max(ageOrig) )

levelNames <- as.character(levels(as.factor(pull(data, !!sym(condition)))))

CI_glht <- lapply(ageCalcs, function(x){
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

estimate <- do.call(rbind, CI_glht) %>%
  dplyr::mutate(age = rep(ageCalcs, each = length(levelNames)) )

# ----------
# Plot split by the condition/marker variable
if(sex == FALSE){
p <- ggplot() +
  geom_line(data = data, aes(x= age_original ,  y = pred, color = !!sym(condition) ), linewidth = 1 , na.rm=T) +
  geom_ribbon(data = estimate  ,
              aes(x= age , ymin = conf.low, ymax = conf.high, fill = condition), alpha = 0.1, na.rm = T, show.legend = FALSE) +
 scale_color_manual(values = plotColours,
                    labels = legendLabels) +
 scale_fill_manual(values = plotColours, guide = "none") +
  theme(legend.text = element_text(color = "black")) +
  labs(color = paste0(legendTitle)) +
  ylab(paste0("SMFQ Score")) +
  xlab("Age (years)")
}else{
  data <- data %>%
    mutate(Sex = ifelse(Sex == 0, "Male", "Female"))
  p <- ggplot() +
    geom_line(data = data, aes(x= age_original ,  y = pred, color = !!sym(conditionPlot), linetype = Sex ), linewidth = 1 , na.rm=T) +
    geom_ribbon(data = estimate  ,
                aes(x= age , ymin = conf.low, ymax = conf.high, fill = condition), alpha = 0.1, na.rm = T, show.legend = FALSE) +
    scale_color_manual(values = plotColours,
                       labels = legendLabels) +
    scale_fill_manual(values = rep(plotColours,2) , guide = "none") +
    scale_linetype_manual(values=c("solid", "dotdash"))+
    theme(legend.text = element_text(color = "black")) +
    labs(color = paste0(legendTitle)) +
    ylab(paste0("SMFQ Score")) +
    xlab("Age (years)")
}


# ---------
# Return the plot
return(p)

}



