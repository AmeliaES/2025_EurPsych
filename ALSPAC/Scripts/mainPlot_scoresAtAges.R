# Main plot for results
# -----
# Read in files
files <- str_subset(list.files("Output/traj_inflam/tableScoresAtAges", full.names = T), "difference")

longData <- lapply(files, function(file){

  # Start with 4 vs 1 quartiles
  marker <- str_extract(file, "(?<=difference)(.*?)(?=\\.csv)")
  res <- read.csv(file) %>%
    filter(str_detect(Marker, "Top vs Bottom")) %>%
    mutate(Marker = str_remove(Marker, "_F9"))%>%
    mutate(across(starts_with("FDR_p"), as.character)) %>%
    mutate(across(starts_with("uncorrected_p"), as.character))
  res

  # Make a data frame with the following columns
  # Marker, Age (for scores at ages), estimate (for difference), CIs
  # In long format by age

  Ages <- colnames(res) %>%
    str_subset("Difference") %>%
    str_extract("\\d+") %>%
    as.numeric()

  Ages <- paste0("Age ", Ages)

  resLong <- lapply(c("Difference", "Lower_CI", "Upper_CI","uncorrected_p", "FDR_p"), function(col){
                res %>%
                  pivot_longer(cols = starts_with(col),
                               names_to = "Age",
                               values_to = col) %>%
                  mutate(Age = Ages) %>%
                  dplyr::select(c(Marker, Age, !!sym(col)))
                }) %>%
              Reduce(function(x, y) left_join(x, y, by = c("Marker", "Age")), .) %>%
                mutate(Quartiles = str_split(Marker, ": ")[[1]][2]) %>%
                mutate(Marker = str_remove(Marker, " tertile.*"))

  return(resLong)
  }) %>% do.call(rbind, .)

# Add column for significance
longData <- longData %>%
  mutate(sig = as.factor(
    case_when(FDR_p == "p<0.001" | as.numeric(FDR_p) <= 0.05 ~ "pFDR \U2264 0.05",
              (FDR_p != "p<0.001" | as.numeric(FDR_p) > 0.05) & as.numeric(uncorrected_p) < 0.05 ~ "p (uncorrected) \U2264 0.05",
              .default = "Not Significant"
    )
  ))


# ---- Make dummy data ------------
# dummy <- lapply(seq(1, 90, 2), function(x){
# y <- x +1
# longData %>%
#   mutate(Marker = c(rep(paste0("Dummy", x), 7), rep(paste0("Dummy", y), 7)))
# }) %>% do.call(rbind,.)
#
# longData <- rbind(longData, dummy)
# ----------------------------------

# Make sure order of markers in x axis don't change order
longData$Marker <- factor(longData$Marker, levels = rev(unique(longData$Marker)))

# Filter for ages we want to show in the plot:
plotData <- longData

p <- ggplot(plotData, aes(y = Difference, x = Marker)) +
  geom_errorbar(aes(ymin=Lower_CI, ymax = Upper_CI),
                width = 0.1,
                position = position_dodge(width = 0.5),
                colour = "grey38")+
  geom_point(aes(color = sig), size = 2) +
  facet_wrap(~Age, nrow = 1) +
  labs(x = "",
       y = "Difference in SMFQ scores\n(Top vs Bottom Third IL-6 Tertile Groups)",
       color = "",
       shape = "") +
  scale_color_manual(values = c("black", "blue", "red")) +
  theme(
    axis.text = element_text(size = 12),  # Adjust axis text size
    axis.title = element_text(size = 14),  # Adjust axis title size
    strip.text = element_text(size = 12),  # Adjust facet label size
    axis.text.x = element_blank()
  )


p

png(paste0("Output/traj_inflam/mainPlot_scoresAtAges.png"), width = 2600, height = 1500, units = "px", res = 300)
print(p)
dev.off()

png(paste0("Output/traj_inflam/mainPlot_panel.png"), width = 2600, height = 2800, units = "px", res = 300)
print(
  plot_grid(
    scoresAtAges$plot,
    p,
    nrow = 2,
    labels = c("A", "B"))
)
dev.off()
