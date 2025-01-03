# Function for total_episodes script
# Function to format results from negative binomial regression model

result_func <- function(model){
  marker <- rownames(summary(model)$coefficients)[2]
  # other_sig_covars <- rownames(filter(as.data.frame( summary(model)$coefficients)[-c(1:2),], `Pr(>|z|)` < 0.05))
  # Format results
  results <- as.data.frame(t(as.data.frame( summary(model)$coefficients[2,] ))) %>%
    dplyr::select(c(Standardised_Beta = Estimate, SD = `Std. Error`, P = `Pr(>|z|)` )) %>%
    mutate(Exposure = str_remove(marker, "_F9"),
           P_corrected = p.adjust(P, method = "fdr", n = length(ids)),
           CI_upper = confint(model, level = 0.95)[2,1],
           CI_lower = confint(model, level = 0.95)[2,2],
           Sample_Size = nrow(dataWide) - length(summary(model)$na.action),
           sig = ifelse(P < 0.05, "P (uncorrected) < 0.05",
                        ifelse(P_corrected < 0.05, "P (FDR) < 0.05", "P > 0.05"))#,
           # Other_sig_covars = other_sig_covars
    ) %>%
    relocate(Exposure) %>%
    mutate_if(is.numeric, ~ round(.,4))
  row.names(results) <- NULL
  return(results)
}
