# Functions to convert scores and differences in scores to long format

makeLongDif <- function(results){
  cbind(
    results %>% pivot_longer(
      cols = starts_with("Difference"),
      names_to = "Difference_rm",
      values_to = "Difference"
    ) %>% as.data.frame() %>%
      mutate(Tertile = paste0(Marker, " - ", str_replace_all(str_remove(Difference_rm, "Difference.."), "\\.", " ")) ) %>%
      dplyr::select(Tertile, Difference)
    ,
    results %>% pivot_longer(
      cols = starts_with("Lower"),
      names_to = "Lower_CI_rm",
      values_to = "Lower_CI"
    )%>%
      dplyr::select(Lower_CI)
    ,
    results %>% pivot_longer(
      cols = starts_with("Upper"),
      names_to = "Upper_CI_rm",
      values_to = "Upper_CI"
    ) %>%
      dplyr::select(Upper_CI)
    ,
    results %>%
      mutate_at(vars(starts_with("uncorrected")), ~ as.character(.)) %>%
      pivot_longer(
        cols = starts_with("uncorrected"),
        names_to = "uncor_p_rm",
        values_to = "uncorrected_p"
      ) %>%
      dplyr::select(uncorrected_p)
    ,
    results %>%
      mutate_at(vars(starts_with("FDR")), ~ as.character(.)) %>%
      pivot_longer(
        cols = starts_with("FDR"),
        names_to = "FDR_p_rm",
        values_to = "FDR_p"
      ) %>%
      dplyr::select(FDR_p)
  )
}



makeLongScores <- function(results){
  # Loop through the vector to modify column names
  for (i in seq_along( colnames(results))) {
    if (grepl("Score",  colnames(results)[i])) {
      # Extract the age from the column name
      age <- gsub("\\D", "",  colnames(results)[i])
      # Modify the Lower_CI and Upper_CI columns accordingly
      colnames(results)[i+1] <- paste0("Lower_CI_", age)
      colnames(results)[i+2] <- paste0("Upper_CI_", age)
    }
  }

  cbind(
  results %>%
    gather(age, estimate, starts_with("Score")) %>%
    mutate(age = str_extract(age, "\\d+")) %>%
    dplyr::select(X, age, estimate)
,
  cbind(
  results %>%
    gather(age, Lower_CI, starts_with("Lower_CI")) %>%
    dplyr::select(Lower_CI)
  ,
  results %>%
    gather(age, Upper_CI, starts_with("Upper_CI")) %>%
    dplyr::select(Upper_CI)) %>%
    unite(`95% CI`, Lower_CI, Upper_CI, sep = " - ")
) %>%
    # mutate(X = str_replace(X, "IL6_tertile, level", "IL-6 tertile")) %>%
    # mutate(X = str_remove(X, " \\(95\\% CIs\\)")) %>%
    mutate(X = case_when(X == 1 ~ "Score [IL-6 tertile = Bottom]",
                         X == 2 ~ "Score [IL-6 tertile = Middle]",
                         X == 3 ~ "Score [IL-6 tertile = Top]")) %>%
    rename(`IL-6 Tertile Group` = X)


}

makeLongScoresSex <- function(results){
  # Loop through the vector to modify column names
  for (i in seq_along( colnames(results))) {
    if (grepl("Score",  colnames(results)[i])) {
      # Extract the age from the column name
      age <- gsub("\\D", "",  colnames(results)[i])
      # Modify the Lower_CI and Upper_CI columns accordingly
      colnames(results)[i+1] <- paste0("Lower_CI_", age)
      colnames(results)[i+2] <- paste0("Upper_CI_", age)
    }
  }

  cbind(
    results %>%
      gather(age, estimate, starts_with("Score")) %>%
      mutate(age = str_extract(age, "\\d+")) %>%
      dplyr::select(X, age, estimate)
    ,
    cbind(
      results %>%
        gather(age, Lower_CI, starts_with("Lower_CI")) %>%
        dplyr::select(Lower_CI)
      ,
      results %>%
        gather(age, Upper_CI, starts_with("Upper_CI")) %>%
        dplyr::select(Upper_CI)) %>%
      unite(`95% CI`, Lower_CI, Upper_CI, sep = " - ")
  ) %>%
    # mutate(X = str_replace(X, "IL6_tertile, level", "IL-6 tertile")) %>%
    # mutate(X = str_remove(X, " \\(95\\% CIs\\)")) %>%
    mutate(X = case_when(X == 1 ~ "Score [IL-6 tertile = Male_Bottom]",
                         X == 2 ~ "Score [IL-6 tertile = Male_Middle]",
                         X == 3 ~ "Score [IL-6 tertile = Male_Top]",
                         X == 4 ~ "Score [IL-6 tertile = Female_Bottom]",
                         X == 5 ~ "Score [IL-6 tertile = Female_Middle]",
                         X == 6 ~ "Score [IL-6 tertile = Female_Top]")) %>%
    rename(`IL-6 Tertile Group` = X)


}

