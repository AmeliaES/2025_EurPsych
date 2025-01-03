# Function that takes a list of lmer models and returns a results table
# Also takes a vector of column names which are suffixes applied to the end of each column

# ------------------------------------
resultsTable <- function(modelList, colNames){

  # --- FIXED EFFECTS:
  # Tidying and modifying code for each model
  fixed_results <- lapply(modelList, function(model) {
    tidy(model, "fixed") %>%
      mutate(p = 2 * (1 - pnorm(abs(statistic)))) %>% # use normal distribution to approximate p-value
      dplyr::select(c(Parameter = term, Estimate = estimate, SE = std.error, p)) %>%
      mutate(across(where(is.numeric), ~ ifelse(. < 0.0001, "<0.0001", round(., 4)))) %>%
      mutate(Parameter = Parameter %>% str_replace("\\(Intercept\\)", "Intercept") %>%
               str_remove("\\)") %>%
               str_remove("I\\(") %>%
               str_replace("\\bage\\b(?!\\^)(?<!\\w)", "age (slope)") %>%
               str_replace("age\\^2", "age^2 (acceleration)") %>%
               str_replace("age\\^3", "age^3 (cubic change)") %>%
               str_replace("age\\^4", "age^4 (quartic change)")) %>%
      mutate(N = unname(summary(model)$ngrps))
  })



  # Combine the results into a single data frame
  fixed_results <- Reduce(function(df1, df2) full_join(df1, df2, by = "Parameter"), fixed_results) %>%
    mutate_all(~ifelse(is.na(.), "-", .))
  colnames(fixed_results)[-1] <- unlist(lapply(colNames, function(x) paste0(c("Estimate", "SE", "p-value", "N"), x)))
  fixed_results


  # --- RANDOM EFFECTS:
  random_results <- lapply(modelList, function(model) {
    as.data.frame(VarCorr(model)) %>%
      mutate(across(where(is.numeric), round, 4)) %>%
      unite(Parameter, var1, var2) %>%
      mutate(Parameter = ifelse(grp == "Residual", "Residual variance", Parameter)) %>%
      mutate(`p-value` = NA) %>%
      mutate(N = NA) %>%
      dplyr::select(c(Parameter, Estimate = vcov, SD = sdcor, `p-value`, N)) %>%
      mutate(Parameter = Parameter %>%
               str_replace(., "_NA$", " variance") %>%
               str_replace(., "(?<!variance)$", " covariance") %>%
               str_replace("\\(Intercept\\)", "Intercept") %>%
               str_remove("\\)") %>%
               str_remove("I\\(") %>%
               str_replace("(?<!.)age(?!\\^)", "Age (slope)") %>%
               str_replace("age\\^2", "Quadratic") %>%
               str_replace("age\\^3", "Cubic") %>%
               str_replace("age\\^4", "Quartic") %>%
               str_replace("\\_", "/"))
  })

  # Combine the results into a single data frame
  random_results <- Reduce(function(df1, df2) full_join(df1, df2, by = "Parameter"), random_results) %>%
    mutate_all(~ifelse(is.na(.), "-", .))
  colnames(random_results)[-1] <- unlist(lapply(colNames, function(x) paste0(c("Estimate", "SE", "p-value", "N"), x)))
  random_results

  # --- Combine fixed and random effects
  combined_results <- as.data.frame(rbind(fixed_results, random_results))

  # Move residual variance to last row
  combined_results <- combined_results %>%
    filter(!combined_results$Parameter == "Residual variance") %>%
    bind_rows(combined_results %>% filter(combined_results$Parameter == "Residual variance"))

  # Add rows on model fit
  model_fit <- lapply(modelList, function(model) {
    # Calculate deviance
    deviance <- -2 * logLik(model)
    aic <- AIC(model)
    bic <- BIC(model)
    results <- data.frame(Parameter = c("Deviance", "AIC", "BIC"),
                          Estimate = c(deviance,  aic, bic),
                          SD = "",
                          `p-value` = "",
                          N = "")
    return(results)
  })

  # Combine the results into a single data frame
  model_fit <- Reduce(function(df1, df2) full_join(df1, df2, by = "Parameter"), model_fit) %>%
    mutate(across(where(is.numeric), round, 4))
  colnames(model_fit)[-1] <- unlist(lapply(colNames, function(x) paste0(c("Estimate", "SE", "p-value", "N"), x)))
  model_fit

  combined_results <- rbind(combined_results, model_fit)

  return(combined_results)
}

# ------------------------------------
