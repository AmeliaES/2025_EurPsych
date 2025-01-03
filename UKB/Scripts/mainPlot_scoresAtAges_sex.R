# Main plot for results (split by sex)
# -----
# Read in files
files <- str_subset(list.files("Output/traj_inflam_sex/tableScoresAtAges", full.names = T), "difference")


longData <- lapply(files, function(file){

  # Start with 3 vs 1 tertiles
  marker <- str_extract(file, "(?<=difference_)(.*?)(?=\\.csv)")
  res <- read.csv(file) %>%
    filter(str_detect(Marker, "Male_Top vs Male_Bottom") | str_detect(Marker, "Female_Top vs Female_Bottom"))%>%
    mutate(across(starts_with("FDR_p"), as.character)) %>%
    mutate(across(starts_with("uncorrected_p"), as.character))

  if(which(str_extract(res$Marker, "Female") == "Female") == 1){
    res$Sex <- as.factor(c("Female", "Male"))
  }else if(which(str_extract(res$Marker, "Male") == "Male") == 1){
    res$Sex <- as.factor(c("Male", "Female"))
  }else{
    res$Sex <- NA
  }


  # Make a data frame with the following columns
  # Marker, Age (for scores at ages), estimate (for difference), CIs
  # In long format by age

  Ages <- colnames(res) %>%
    str_subset("Difference") %>%
    str_extract("\\d+") %>%
    as.numeric()

  Ages <- paste0("Age ", Ages)

  res <- lapply(c("Male", "Female"), function(Sex){
    res <- filter(res, Sex == sym(!!Sex))
    resLong <- lapply(c("Difference", "Lower_CI", "Upper_CI", "uncorrected_p", "FDR_p"), function(col){
      res %>%
        pivot_longer(cols = starts_with(col),
                     names_to = "Age",
                     values_to = col) %>%
        mutate(Age = Ages) %>%
        dplyr::select(c(Marker, Age, !!sym(col)))
    }) %>%
      Reduce(function(x, y) left_join(x, y, by = c("Marker", "Age")), .) %>%
      mutate(Tertile = str_split(Marker, ": ")[[1]][2]) %>%
      mutate(Marker = str_remove(Marker, " tertile.*")) %>%
      mutate(Sex = Sex)

    return(resLong)
  }) %>% do.call(rbind, .)
})%>% do.call(rbind, .)

# Add column for significance
longData <- longData %>%
  mutate(sig = as.factor(
    case_when(FDR_p == "p<0.001" | as.numeric(FDR_p) <= 0.05 ~ "pFDR \U2264 0.05",
              (FDR_p != "p<0.001" | as.numeric(FDR_p) > 0.05) & as.numeric(uncorrected_p) < 0.05 ~ "p (uncorrected) \U2264 0.05",
              .default = "Not Significant"
    )
  ))


# Make sure order of markers in x axis don't change order
longData$Marker <- factor(longData$Marker, levels = rev(unique(longData$Marker)))
longData$Sex <- factor(longData$Sex, levels = c("Female", "Male"))

p <- ggplot(longData, aes(y = Difference, x = Marker, group = Sex)) +
  geom_errorbar(aes(ymin=Lower_CI, ymax = Upper_CI),
                width = 0.1,
                position = position_dodge(width = 0.5),
                colour = "grey38") +
  geom_point(aes(color = sig, shape = Sex), size = 2, position = position_dodge(0.5)) +
  facet_wrap(~Age, nrow = 1) +
  labs(y = "Difference in PHQ-2 Scores\n(Top vs Bottom Third IL-6 Tertile Groups)",
       x = " ",
       color = "",
       shape = "") +
  scale_color_manual(values = c("black", "blue", "red")) +
  scale_shape_manual(breaks = c("Female", "Male"), values = c(16,17), labels = c("Female", "Male"))+
  theme(
    axis.text = element_text(size = 12),  # Adjust axis text size
    axis.title = element_text(size = 14),  # Adjust axis title size
    strip.text = element_text(size = 12),  # Adjust facet label size
    axis.text.x = element_blank()
  )

p

png(paste0("Output/traj_inflam_sex/mainPlot_scoresAtAges.png"), width = 2400, height = 1500, units = "px", res = 300)
print(p)
dev.off()

png(paste0("Output/traj_inflam_sex/mainPlot_panel.png"), width = 2600, height = 2800, units = "px", res = 300)
print(
  plot_grid(
    scoresAtAges$plot,
    p,
    nrow=2,
    labels = c("A", "B"))
)
dev.off()


