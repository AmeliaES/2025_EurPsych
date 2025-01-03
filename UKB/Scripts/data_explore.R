# Explore Olink IL6 UKB data
# Remove UKB-PPP participants
# Scale and create categorical variables
# ---------------------------
# f.30900.0.0 = Number of proteins measured
# f.30901.0.0 = Plate used for sample run
# f.30902.0.0 = Well used for sample run
# f.30903.0.0 = UKB-PPP Consortium selected participant (6264 items have value 1 (Yes))
# ---------------------------

olinkIL6 <- fread("/Volumes/GenScotDepression/users/amelia/UKB/olink_data_IL6.csv")


# First compare the randomly selected participants with the PPP cohort and Covid repeat participants
# Number of people:
length(unique(olinkIL6$f.eid))


# Number in UKB PPP cohort
sum(olinkIL6$f.30903.0.0 == 1, na.rm = T)


# Number remaining:
sum(is.na(olinkIL6$f.30903.0.0))


# Change from NA to coded 0
olinkIL6 <- olinkIL6 %>%
  mutate(f.30903.0.0 = as.factor(ifelse(is.na(f.30903.0.0), 0, f.30903.0.0)))

# Check for overlap between f.30903.0.0 and repeat_olink
olinkIL6 %>%
  filter( f.30903.0.0 == 1 & repeat_olink == 1) %>%
  pull(f.eid) %>%
  unique() %>%
  length()

# Sun et al paper it says the overlap is 20 (https://www.nature.com/articles/s41586-023-06592-6/figures/1)

olinkIL6 %>%
  filter(f.30903.0.0 == 1 & repeat_olink == 0)%>%
  pull(f.eid) %>%
  unique() %>%
  length()


olinkIL6 %>%
  filter(repeat_olink == 1 & f.30903.0.0 == 0)%>%
  pull(f.eid) %>%
  unique() %>%
  length()


olinkIL6 %>%
  filter(repeat_olink == 0 & f.30903.0.0 == 0)%>%
  pull(f.eid) %>%
  unique() %>%
  length()


# check those numbers add up to total no of people:
length(unique(olinkIL6$f.eid))

png("Output/explore_data/UKB-PPP_and_repeat_IL6_boxplot.png", res = 300, width = 3000, height = 1500)
print(
plot_grid(
ggplot(olinkIL6, aes (y = result, x = f.30903.0.0))+
  geom_boxplot(),
ggplot(olinkIL6, aes (y = result, x = repeat_olink))+
  geom_boxplot()
)
)
dev.off()

# ---------------------------
# Subset to randomly selected sample
olinkIL6 <- olinkIL6 %>%
  filter(repeat_olink == 0 & f.30903.0.0 == 0)
nrow(olinkIL6)


# ---------------------------
# Need to determine if it has already been transformed before normalising? Or do we need to apply a log or INT?
# https://www.nature.com/articles/s41586-023-06563-x this paper used the UKB data and applied an INT
inormal <- function(x)
{
  qnorm((rank(x, na.last = "keep") - 0.5) / sum(!is.na(x)))
}

olinkIL6 <- olinkIL6 %>%
  mutate(result_INT = inormal(result))

# ---------------------------
# Plot histogram
png("Output/explore_data/hist.png", res = 300, width = 3000, height = 1500)
print(
plot_grid(
ggplot(olinkIL6)+
  geom_histogram(aes(x = result))
,
ggplot(olinkIL6)+
  geom_histogram(aes(x = result_INT))
)
)
dev.off()

# ---------------------------
# Create categorical variables for IL6 and scale the continuous measure
olinkIL6 <- olinkIL6 %>%
  mutate(IL6_tertile = factor(ntile(result, 3)),
         IL6_quartile = factor(ntile(result, 4)),
         IL6_raw = result,
         IL6 = scale(result),
         IL6_INT = scale(result_INT))

png("Output/explore_data/hist_scaled.png", res = 300, width = 3000, height = 1500)
print(
plot_grid(
  ggplot(olinkIL6)+
    geom_histogram(aes(x = IL6))
  ,
  ggplot(olinkIL6)+
    geom_histogram(aes(x = IL6_INT))
)
)
dev.off()

# ---------------------------
# Check final n for sample
nrow(olinkIL6) 

olinkIL6 %>%
  filter(!is.na(IL6_INT)) %>%
  nrow() 



# This sample size and the sample sizes determined from the UKB-PPP and covid repeat participants seems to be sensible with what is reported in Sun et al 

# ---------------------------
# Save data
write.csv(olinkIL6, "/Volumes/GenScotDepression/users/amelia/UKB/olink_data_IL6_clean.csv")
