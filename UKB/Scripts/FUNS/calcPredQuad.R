# Get prediction for each categorical variable
# ------------------------------------

calcPredQuad <- function(data, model, condition){

  ageVec <- data %>% pull(age)

  coef <- summary(model)$coefficients

  # Mean trajectory when covariates set to zero
  zero  <- ageVec * coef[2,1] + coef[1,1]  +
    ageVec^2 * coef[3,1]

  rowIndex <- which(str_detect(string = row.names(coef),
                               pattern = condition) &
                      str_detect(string = row.names(coef),
                                 pattern = ":", negate = T))

  rowIndexInteract1 <- which(str_detect(string = row.names(coef),
                                        pattern = condition) &
                               str_detect(string = row.names(coef),
                                          pattern = ":") &
                               str_detect(string = row.names(coef),
                                          pattern = "\\^", negate = T))

  rowIndexInteract2 <- which(str_detect(string = row.names(coef),
                                        pattern = condition) &
                               str_detect(string = row.names(coef),
                                          pattern = ":") &
                               str_detect(string = row.names(coef),
                                          pattern = "\\^2"))


  num <- str_subset(row.names(coef), condition) %>%
    sub(paste0(".*", condition), "", .) %>%
    unique()

  n <- length(num)+1

  predCovs <- lapply(1:(n-1), function(i){
    coef[1,1] +
      ageVec * coef[2,1] +
      coef[rowIndex[i],1]  +
      ageVec^2 * coef[3,1] +
      (ageVec * coef[rowIndexInteract1[i],1]) +
      (ageVec^2 * coef[(rowIndexInteract2)[i],1])
  })



  names(predCovs) <- paste0(condition, "_", num)

  data <- cbind(data, do.call(cbind, predCovs)) %>%
    mutate(zero = zero) %>%
    mutate(pred =  eval(parse(text =
                                paste0(paste0("ifelse(", condition, " == '", num, "', ", condition, "_",num,",", collapse = " "), "zero",
                                       paste0(rep(")", length(num)), collapse = ""), collapse = "")
    )))


  return(data)

}
