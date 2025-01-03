# Read in ALSPAC data and prepare it
# -------------------------

dataWide <- read.csv("/Volumes/ALSPAC/users/amelia/Data/dataSubQCWide.csv")

# log transform, INT transform and factorise vars
inormal <- function(x)
{
  qnorm((rank(x, na.last = "keep") - 0.5) / sum(!is.na(x)))
}

dataWide <- dataWide %>%
  filter(!is.na(Sex)) %>%
  rename(CRP = CRP..age.9.) %>%
  filter(CRP < 10) %>%
  rename(IL6 = IL.6..age.9.) %>%
  filter(!is.na(IL6)) %>%
  mutate(CRP_quartile = ntile(CRP, 4)) %>% # Create quartiles for inflammatory markers
  mutate(IL6_quartile = ntile(IL6, 4)) %>%
  mutate(CRP_tertile = ntile(CRP, 3)) %>% # Create tertiles for inflammatory markers
  mutate(IL6_tertile = ntile(IL6, 3)) %>%
  mutate(CRP_log = log(CRP+1)) %>%
  mutate(IL6_log = log(IL6+1)) %>%
  mutate(IL6_INT = inormal(IL6)) %>%
  mutate(BMI_age9_original = BMI_age9) %>%
  mutate(BMI_age9_log = log(BMI_age9)) %>%
  mutate(BMI_age9_tertile = ntile(BMI_age9, 3)) %>%
  mutate(IL6_tertile = case_when(IL6_tertile== 1 ~ "Bottom",
                                 IL6_tertile==2 ~ "Middle",
                                 IL6_tertile==3 ~ "Top")) %>%
  mutate_at(c("Sex", "IL6_quartile", "CRP_quartile","IL6_tertile", "CRP_tertile", "Maternal.social.class.at.birth", "Maternal.education.at.birth"), as.factor) %>%
  mutate_at(c( "IL6_log", "CRP_log", "IL6_INT", "BMI_age9" ), scale)  # z-scale continuous variables

# Read in data to get new variables on Townsend deprivation index quintiles and medication
ALSPACdata <- read_dta("/Volumes/ALSPAC/data/B3421/B3421_Whalley_18Oct2023.dta")
# Get the labels for the col names
vars <- var_label(ALSPACdata)
# Get labels of values for each variable
vars[str_detect(vars, "Towns")]
vars[str_detect(vars, "edication")]
ALSPACdata$jTownsendq5
ALSPACdata$IL6_F24

# "Currently taking medication: samples: F9": 1 = yes, 2 = no
ALSPACdata <- ALSPACdata %>%
  unite("Subject", c(cidB3421, qlet)) %>%
  dplyr::select(Subject, kqTownsendq5, tcTownsendq5, jTownsendq5, f9sa015, IL6_F24) %>% # j = 4yrs, kq = 6y, tc = 16y
  mutate_at(vars(c(kqTownsendq5, tcTownsendq5, jTownsendq5)), ~ as.factor(ifelse(. < 0, NA, .))) %>%
  mutate(any_meds = as.factor(ifelse(f9sa015 < 0 , NA, ifelse(
    f9sa015 == 1, "TRUE", "FALSE"
  ))))

nrow(ALSPACdata) 
nrow(dataWide) 

dataWide <- merge(dataWide, ALSPACdata, by = "Subject")

nrow(dataWide)

# Check how many people have at least one depressive measure
dataWide %>%
  dplyr::select(starts_with("mfq")) %>%
  ncol() 

dataWide %>%
  mutate(total_na = rowSums(across(all_of(starts_with("mfq")), ~is.na(.)))) %>%
  filter(total_na < 11) %>%
  nrow() 

# FINAL SAMPLE:
dataWide <- dataWide %>%
  mutate(total_na = rowSums(across(all_of(starts_with("mfq")), ~is.na(.)))) %>%
  filter(total_na < 11) # because there are 11 time points

nrow(dataWide)

# How many people with IL-6 data at age 9y have IL-6 data at age 24y?
dataWide %>%
  dplyr::select(IL6, IL6_F24) %>%
  mutate(IL6_F24 = ifelse(IL6_F24 > 0, IL6_F24, NA)) %>%
  summarise_all(~ sum(!is.na(.)))

# Cut off values for IL-6 tertiles
IL6_cut_off <- dataWide %>%
  group_by(IL6_tertile) %>%
  summarise(min = min(IL6), max = max(IL6))

write.csv(IL6_cut_off, "Output/serum/IL6_cut_offs.csv", row.names = F)

# Stats on number of time points for each person
dataWide <- dataWide %>%
  mutate(nTimePoints = rowSums(across(all_of(starts_with("mfq")), ~!is.na(.))))

dataWide %>%
  summarise(mean = mean(nTimePoints),
            median = median(nTimePoints),
            mode = getmode(nTimePoints),
            sd = sd(nTimePoints),
            IQR = IQR(nTimePoints)) %>%
  mutate_if(is.numeric, round, 3) %>%
  write.csv(., "Output/serum/nTimePointsStats.csv", row.names = F)

# Counts of people for the number of time points they attended
dataWide %>%
  count(nTimePoints)%>%
  write.csv(., "Output/serum/nTimePointsCounts.csv", row.names = F)

# Plot those time points
png("Output/serum/nTimePointsCounts.png", res=300, height = 1200, width = 1500)
dataWide %>%
  ggplot(data = .) +
  geom_bar(aes(x = nTimePoints))+
  labs(fill = "Only attended\ninitial assessment",
       x = "Number of assessments attended",
       y = "Number of people")+
  scale_x_continuous(breaks = 1:11)
dev.off()

# --------------------
# add column for number of questionnaires answered
dataWide <- dataWide %>%
  mutate(CompletedQ = rowSums(!is.na(dplyr::select(., starts_with("mfq_t") ) )))

png(paste0("Output/serum/CompletedQ.png"), width = 2500, height = 1500, res = 300)
print(
ggplot(dataWide) +
  geom_bar(aes(CompletedQ, fill = IL6_tertile)) +
  scale_x_continuous(breaks = 1:11) +
  scale_fill_manual(values = c(  "darkolivegreen3","dodgerblue2", "darkorchid3")) +
  labs(fill = "IL-6 tertile",
       x = "Number of time points attended",
       y = "Number of people")
)
dev.off()

fit <- lm("CompletedQ ~ IL6_tertile", data = dataWide)
summary(fit)

CompletedQFit <- tidy(summary(fit)) %>%
  mutate(across(where(is.numeric), \(x) round(x,  3))) %>%
  mutate(p.value = ifelse(p.value < 0.0001, "p<0.0001", p.value)) %>%
  mutate(N = nrow(model.frame(fit)))

write.csv(CompletedQFit,
          "Output/serum/CompletedQFit.csv", row.names = F, quote = F)

# --------------------
# Convert table from wide format to long format by age and depression score
dataLong <- gather(dataWide, age_cat, age, paste0("age_t", c(paste0("0", 1:9), "10", "11")), factor_key=TRUE)
data_dep <- gather(dataWide, dep_cat, dep, paste0("mfq_t", c(paste0("0", 1:9), "10", "11")), factor_key=TRUE)

dataLong$dep_cat <- data_dep$dep_cat
dataLong$dep <- data_dep$dep

dataLong <- dataLong[order(dataLong$Subject, dataLong$age_cat),]
head(dataLong)

dataLong %>%
  filter(age_cat == "age_t01") %>%
  summarise(mean = mean(age, na.rm = T),
            min = min(age, na.rm = T))

# Center to mean age
dataLong <- dataLong %>%
  mutate(age_original = as.numeric(age) ) %>%
  mutate(age = as.numeric(age) - 10 ) %>% # reviewer asked to not mean center age
  mutate(age_mean = age_original - mean( age_original, na.rm = T ))
# Mean age =
mean(dataLong$age_original,na.rm = T)
min(dataLong$age_original,na.rm = T)