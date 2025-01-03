# Combined depression data with IL-6 data and convert to long format
# -----------------------------
# Read in depression data we derived on Eddie, (see scripts in "Eddie/")
dataDep <- fread("/Volumes/GenScotDepression/users/amelia/UKB/dataWideDerived.csv")
colnames(dataDep)

dataDep <- dataDep %>%
  mutate(NCompleted = rowSums(!is.na(dplyr::select(., !ends_with("_isNA") & starts_with("PHQ2") )))) %>%
  mutate(NCompletedExIm = rowSums(!is.na(dplyr::select(., !ends_with("_isNA") & !ends_with(c("1", "2")) & starts_with("PHQ2") ))))

# Read in Olink data:
olinkIL6 <- fread("/Volumes/GenScotDepression/users/amelia/UKB/olink_data_IL6_clean.csv") %>%
  mutate_at(c("IL6_tertile", "IL6_quartile"), as.factor)
nrow(olinkIL6)

# Merge together
dataWide <- merge(olinkIL6, dataDep, by = "f.eid")

head(dataWide)
nrow(dataWide) 

# -----------------------------
# Create log transformed BMI:
dataWide <- dataWide %>%
  mutate(BMI_raw = BMI) %>%
  mutate(BMI = log(BMI)) %>%
  mutate(smoking_status = factor( ifelse(f.20116.0.0 == "Prefer not to answer", NA, f.20116.0.0 ),
                                  levels = c("Previous", "Current", "Never", NA)))

# Change IL-6 tertile from 1,2,3 to bottom, middle and top third
dataWide <- dataWide %>%
  mutate(IL6_tertile = case_when(IL6_tertile== 1 ~ "Bottom",
                                 IL6_tertile==2 ~ "Middle",
                                 IL6_tertile==3 ~ "Top"))

# -----------------------------
# How many people with CRP and Olink IL-6 data?
dataDep %>%
  left_join(olinkIL6, by = "f.eid") %>%
  filter(!is.na(IL6)) %>%
  mutate(CRP_NA = ifelse(is.na(CRP), TRUE, FALSE)) %>%
  group_by(CRP_NA) %>%
  count()

# How many people with CRP and Olink IL-6 data, split by CRP >= 10 mg/L
dataDep %>%
  left_join(olinkIL6, by = "f.eid") %>%
  filter(!is.na(IL6)) %>%
  filter(!is.na(CRP)) %>%
  mutate(CRP_high = ifelse(CRP >= 10, TRUE, FALSE)) %>%
  group_by(CRP_high) %>%
  count()

dataWide <- dataWide %>%
  filter(!is.na(CRP)) %>%
  filter(CRP < 10)

# -----------------------------
# Mean age and SD for the sample of individuals with IL-6 data and CRP < 10mg/L
mean(dataWide$age_0) 
sd(dataWide$age_0) 
min(dataWide$age_0) 
boxplot(dataWide$age_0)

# -----------------------------
# How many people with at least one measure of depressive symptoms?
dataWide %>%
  mutate(total_na = rowSums(across(all_of(c("PHQ2_dep_0", "PHQ2_dep_1", "PHQ2_dep_2", "PHQ2_dep_3",
                                            "PHQ2_dep_mh", "PHQ2_dep_mwell", "PHQ2_dep_hwell", "PHQ2_dep_pain")), ~is.na(.)))) %>%
  filter(total_na < 8) %>%
  nrow()
# Seeing as this is the sample used in the minimum model we will subset our sample to this for the exploratory plots and tables

# FINAL SAMPLE:
dataWide <- dataWide %>%
  mutate(total_na = rowSums(across(all_of(c("PHQ2_dep_0", "PHQ2_dep_1", "PHQ2_dep_2", "PHQ2_dep_3",
                                            "PHQ2_dep_mh", "PHQ2_dep_mwell", "PHQ2_dep_hwell", "PHQ2_dep_pain")), ~is.na(.)))) %>%
  filter(total_na < 8)

# Stats on number of time points for each person
dataWide <- dataWide %>%
  mutate(nTimePoints = rowSums(across(all_of(c("PHQ2_dep_0", "PHQ2_dep_1", "PHQ2_dep_2", "PHQ2_dep_3",
                                               "PHQ2_dep_mh", "PHQ2_dep_mwell", "PHQ2_dep_hwell", "PHQ2_dep_pain")), ~!is.na(.))))

dataWide %>%
  summarise(mean = mean(nTimePoints),
            median = median(nTimePoints),
            mode = getmode(nTimePoints),
            sd = sd(nTimePoints),
            IQR = IQR(nTimePoints)) %>%
  mutate_if(is.numeric, round, 3) %>%
  write.csv(., "Output/data_merge_long/nTimePointsStats.csv", row.names = F)

# Cut off values for IL-6 tertiles
IL6_cut_off <- dataWide %>%
  group_by(IL6_tertile) %>%
  summarise(min = min(IL6_raw), max = max(IL6_raw))

write.csv(IL6_cut_off, "Output/data_merge_long/IL6_cut_offs.csv", row.names = F)

# Counts of people for the number of time points they attended
dataWide %>%
  count(nTimePoints)%>%
  write.csv(., "Output/data_merge_long/nTimePointsCounts.csv", row.names = F)

# Plot those time points
png("Output/data_merge_long/nTimePointsCounts.png", res=300, height = 1800, width = 2500)
dataWide %>%
  ggplot(data = .) +
  geom_bar(aes(x = nTimePoints, fill = IL6_tertile))+
  scale_fill_manual(values = c(  "darkolivegreen3","dodgerblue2", "darkorchid3"))+
  labs(fill = "IL-6 tertile",
       x = "Number of assessments attended",
       y = "Number of people")+
  scale_x_continuous(breaks = 1:8)
dev.off()

# For people who attended only one appointment, how many were the initial assessment?
dataWide %>%
  filter(nTimePoints == 1) %>%
  count(!is.na(PHQ2_dep_0))

dataWide %>%
  filter(is.na(f.53.0.0))

dataWide %>%
  summarise(min(f.53.0.0))

dataWide %>%
  summarise(max(f.53.0.0))

dataWide %>%
  dplyr::select(f.53.0.0)%>%
  summary()

# Colour the bar plot above to highlight those who only attended baseline assessment and this was the only measure used for their trajecory
png("Output/data_merge_long/nTimePointsCountsFill.png", res=300, height = 1200, width = 2500)
dataWide %>%
  ggplot(data = .) +
  geom_bar(aes(x = nTimePoints, fill = nTimePoints == 1 & !is.na(PHQ2_dep_0))) +
  labs(fill = "Only completed PHQ-2\nat initial assessment",
       x = "Number of assessments attended",
       y = "Number of people")+
  scale_x_continuous(breaks = 1:8)
dev.off()

# -----------------------------
# Explore protein batch and assessment centre variables
png("Output/data_merge_long/assessmentCentre_batch.png", res = 300, width = 2550, height = 1500)
dataWide %>%
  mutate_at(c("Batch", "Assessment_centre_baseline"), as.factor) %>%
  ggplot(data = .) +
  geom_bar(aes(x = Assessment_centre_baseline, fill = Batch)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
dev.off()

# Recode the assessment_centre_baseline from the codings to the place names
assessmentCentres <- fread("Supplementary/coding10.tsv") %>%
  mutate(meaning = str_remove_all(meaning, "\\(|\\)")) %>%
  mutate(meaning = str_replace_all(meaning, " ", "_"))

dataWide$Assessment_centre_baseline <- factor(assessmentCentres$meaning[match(dataWide$Assessment_centre_baseline, assessmentCentres$coding)])

# ----------------------------
# Create column for anyone taking medication and count how many people
codesMeds <- fread("Supplementary/coding4.tsv")

# Types of NSAIDs accoring to NHS website (https://www.nhs.uk/conditions/nsaids/)
drugs <- c("aspirin", "ibuprofen", "naproxen", "diclofenac", "celecoxib", "mefenamic acid",
           "etoricoxib", "indomethacin")

# List of drug codes
drug_codes <- sapply(drugs, function(x){
  codesMeds%>%
    filter(str_detect(meaning, x)) %>%
    pull(coding)
}) %>% unlist() %>% unname()

# Create new column for anyone with an anti-inflammatory drug code in any drug collumn to have TRUE or FALSE in inflam_meds
dataWide <- dataWide %>%
  mutate(inflam_meds = factor(if_any(starts_with("f.20003"), ~(. %in% drug_codes),na.rm = T)))

dataWide %>%
  count(inflam_meds)


# List of medications and number of people taking each one
total <- lapply(drug_codes, function(code){
  dataTmp <- dataWide %>%
    dplyr::select(starts_with("f.20003"))
  sapply(1:ncol(dataTmp), function(i){
    sum(dataTmp[[i]] %in% code)
  }) %>% sum()
})
total <- do.call(rbind.data.frame, total)

total <- total %>%
  mutate(Medication = str_to_title( codesMeds[codesMeds$coding %in% drug_codes,]$meaning) ) %>%
  dplyr::select(2:1) %>%
  rename(Count = 2)
total
write.csv(total, "Output/data_merge_long/anti-inflam_meds_count.csv", row.names = F, quote = F)

# ----------------------------
# Is the sample getting more healthy as the study progresses due to attrition?
# How many people died after baseline appointment? ie. they couldn't fill in the questionnaire because they were dead?

sum(is.na(dataWide$f.53.0.0)) # good, everyone has an initial appointment date
sum(!is.na(dataWide$DOD)) 

dataWide %>%
  filter(DOD > f.53.0.0) %>%
  nrow()
# Those people all died after the initial appointment date


# How many questionnaires did people fill in?
# Excluding imaging visits for which only a subset were invited

# Get names of appointment date columns:
dates <- read.csv("Supplementary/fields_to_extract.csv") %>%
  filter(Type == "For_age_date") %>%
  filter(!Field_ID %in% c("53.2.0", "53.3.0")) # remove imaging assessment dates

# Create a new column with number of assessment dates attended
dataWide <- dataWide %>%
  mutate(Dead = ifelse(DOD < f.53.0.0 | is.na(DOD), "FALSE", "TRUE"))

# Plot the number of assessments attended in a stacked barplot, coloured by if they died after baseline or not
# per IL-6 tertile

png("Output/data_merge_long/N_completed_questionnaires_dead_alive.png", res = 300, width = 2550, height = 1500)
print(
ggplot(data = filter(dataWide, NCompletedExIm > 0)) +
  geom_bar(aes(x = NCompletedExIm, fill = Dead), stat = "count")+
  facet_wrap(~ IL6_tertile)+
  scale_x_continuous(breaks = 1:6, labels = 1:6)+
  xlab("Number of completed questionnaires (excluding imaging appts)\n(shown for each IL-6 tertile)")
)
dev.off()

# Completed number of questionnaires vs IL-6 tertile, adjusted for by age at basline:
# so the same as above but adjusting for age as that affects both IL-6 and date of death potentially
dataWide %>%
  filter(Dead == FALSE) %>%
  filter(NCompletedExIm > 0) %>%
  ggplot(.) +
  geom_boxplot(aes(x = factor(NCompletedExIm), y = IL6_INT))

png(paste0("Output/data_merge_long/CompletedQ.png"), width = 2500, height = 1500, res = 300)
print(
dataWide%>%
  filter(Dead == FALSE)%>%
  filter(NCompletedExIm > 0) %>%
ggplot(data =.) +
  geom_bar(aes(NCompletedExIm, fill = IL6_tertile)) +
  scale_x_continuous(breaks = 1:6) +
  scale_fill_manual(values = c(  "darkolivegreen3","dodgerblue2", "darkorchid3")) +
  labs(fill = "IL-6 tertile",
       x = "Number of time points attended",
       y = "Number of people")
)
dev.off()

fit <- lm("NCompleted ~ IL6_tertile", data = filter(dataWide, Dead == FALSE))
summary(fit)

CompletedQFit <- tidy(summary(fit)) %>%
  mutate(across(where(is.numeric), \(x) round(x,  3))) %>%
  mutate(p.value = ifelse(p.value < 0.0001, "p<0.0001", p.value)) %>%
  mutate(N = nrow(model.frame(fit)))

write.csv(CompletedQFit,
          "Output/data_merge_long/CompletedQFit.csv", row.names = F, quote = F)
# ----------------------------
# Amount of time between each appointment per person
# First check Health and Wellbeing and Mental well-being questions
# For each individiual what's the time difference between age_hwell and age_mwell
dateCols <- dates %>%
  filter(TimePoint %in% c("hwell", "mwell")) %>%
  pull(Field_ID) %>%
  paste0("original_col_f.",.)

# Date cols:
# "f.28755.0.0" "f.29198.0.0"
png("Output/data_merge_long/timeDif_weeks_between_hwell_mwell.png", width = 2600, height = 1500, res = 300)
dataWide %>%
  dplyr::select(f.eid, date_hwell = dateCols[1], date_mwell = dateCols[2]) %>%
  mutate(timeDif = abs(as.numeric(difftime(date_mwell, date_hwell, units = "weeks")))) %>%
ggplot(data = .) +
  geom_bar(aes(x = timeDif), stat = "count") +
  xlab("Number of weeks between Health & well-being and Mental well-being") +
  ylab("Number of people")
dev.off()

dataWide %>%
  dplyr::select(f.eid, date_hwell = dateCols[1], date_mwell = dateCols[2]) %>%
  mutate(timeDif = abs(as.numeric(difftime(date_mwell, date_hwell, units = "weeks")))) %>%
  reframe(mean = mean(timeDif, na.rm = T),
          SD = sd(timeDif, na.rm = T),
          min = min(timeDif, na.rm = T),
          max = max(timeDif, na.rm = T))

# What about other time points?
dateCols <- read.csv("Supplementary/fields_to_extract.csv") %>%
  filter(Type == "For_age_date") %>%
  pull(Field_ID) %>%
  paste0("f.",.)

dataWide %>%
  dplyr::select(f.eid, all_of(dateCols[1:4]), all_of(paste0("original_col_", dateCols[5:8])))


# -----------------------------
# How many people with inflammatory condition in original sample?
dataDep %>%
  count(inflam_condition)


# How many people with inflammatory condition in the Olink sample?
dataWide %>%
  count(inflam_condition)

# How many people with inflammatory condition in Olink sample grouped by IL6 tertile?
dataWide %>%
  group_by(IL6_tertile) %>%
  count(inflam_condition)


# -----------------------------
# How many people with each individual inflammatory condition:

dataDep %>%
  dplyr::select(starts_with("inflam_ind")) %>%
  ncol() 

dataDepLong <- dataDep %>%
  dplyr::select(starts_with("inflam_ind")) %>%
  rename_with(~str_replace_all(., "\\_\\.\\_", "/"), everything()) %>%
  rename_with(~str_replace_all(., "\\.", "/"), everything()) %>%
  rename_with(~str_replace_all(., "\\_\\/nos\\/", ""), everything()) %>%
  rename_with(~str_replace_all(., "\\_\\/tb\\/", ""), everything())%>%
  rename_with(~str_replace_all(., "\\/\\_pid", ""), everything()) %>%
  rename_with(~str_remove(., "inflam_ind_"), everything()) %>%
  gather(key = "Variable", value = "Value")

dataDepLong %>%
  group_by(Variable, Value) %>%
  count()

inflamCond <- dataWide %>%
  dplyr::select(starts_with("inflam_ind")) %>%
  rename_with(~str_replace_all(., "\\_\\.\\_", "/"), everything()) %>%
  rename_with(~str_replace_all(., "\\.", "/"), everything()) %>%
  rename_with(~str_replace_all(., "\\_\\/nos\\/", ""), everything()) %>%
  rename_with(~str_replace_all(., "\\_\\/tb\\/", ""), everything())%>%
  rename_with(~str_replace_all(., "\\/\\_pid", ""), everything()) %>%
  rename_with(~str_remove(., "inflam_ind_"), everything()) %>%
  rename_with(~str_replace_all(., "_", " "), everything()) %>%
  gather(key = "Variable", value = "Value") %>%
  group_by(Variable, Value) %>%
  count() %>%
  filter(Value == TRUE) %>%
  arrange(desc(n)) %>%
  dplyr::select(c(Condition = Variable, N = n))%>%
  as.data.frame() %>%
  dplyr::select(-Value) %>%
  mutate(Condition = str_to_title(Condition)) %>%
  mutate(Condition = str_replace_all(Condition, "Sjogren\\/S", "Sjogren's")) %>%
  mutate(Condition = str_replace_all(Condition, "Non\\/", "Non-")) %>%
  mutate(Condition = str_replace_all(Condition, "Hiv", "HIV")) %>%
  mutate(Condition = str_replace_all(Condition, "Aids", "AIDS"))

inflamCond

# Add a row for count of "None"
inflamCond <- rbind(inflamCond,
data.frame("Condition" = "None", "N" = sum(dataWide$inflam_condition == FALSE))
)


write.csv(
inflamCond ,
"Output/data_merge_long/inflam_conditions_N.csv", row.names = F, quote = F)


# -----------------------------
# Explore PHQ-2 vs PHQ-4

lapply(c("PHQ2", "PHQ4"), function(PHQ){

  # ---------
  # Plot the number of people and the depression score for each time point
  if(PHQ == "PHQ2"){
  dataTmpDep <- dataWide %>%
    dplyr::select(f.eid, (!ends_with("_isNA") & starts_with(PHQ))) %>%
    gather(key = "Variable", value = "Value", - f.eid) %>%
    mutate(Variable = factor(Variable, levels = c(paste0(PHQ, "_dep_", 0:3),
                                                  paste0(PHQ, "_dep_mh"),
                                                  paste0(PHQ, "_dep_pain"),
                                                  paste0(PHQ, "_dep_hwell"),
                                                  paste0(PHQ, "_dep_mwell")),
                             labels = c("Initial","Repeat","Imaging","Repeat Imaging","Mental health","Pain","Health & well-being","Mental well-being")
                            )) %>%
    mutate(Value = ifelse(is.na(Value), "Missing", Value))
  }else{
    dataTmpDep <- dataWide %>%
      dplyr::select(f.eid, (!ends_with("_isNA") & starts_with(PHQ))) %>%
      gather(key = "Variable", value = "Value", - f.eid) %>%
    mutate(Variable = factor(Variable, levels = c(paste0(PHQ, "_dep_", 0:3),
                                                  paste0(PHQ, "_dep_mh"),
                                                  paste0(PHQ, "_dep_mwell")),
                             labels = c("Initial","Repeat","Imaging","Repeat Imaging","Mental health","Mental well-being")
    ))%>%
      mutate(Value = ifelse(is.na(Value), "Missing", Value))
  }

  png(paste0("Output/data_merge_long/",PHQ,"NDepScore.png"), width = 3000, height = 1500, res = 300)
  print(
    ggplot(dataTmpDep, aes(x = Variable, fill = factor(Value))) +
      geom_bar(stat = "count") +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
      labs(x = "Time Point", y = "Count", fill = paste0(PHQ,  " Score"))+
      guides(fill = guide_legend(ncol = 2))+
      scale_fill_brewer(palette = "Dark2")
  )
  dev.off()

  # ---------
  # Table used to create above figure:
  write.csv(
  dataTmpDep %>%
    group_by(Variable) %>%
    count(Value) %>%
    dplyr::select("Time Point" = Variable, "PHQ Score" = Value, N = n),
  paste0("Output/data_merge_long/",PHQ,"NDepScore.csv"), row.names = F, quote = F)

  # ---------
  # How many participants did not answer questions at each time point:
  if(PHQ == "PHQ2"){
  dataTmp <- dataWide %>%
    dplyr::select(f.eid, (ends_with("_isNA") & starts_with(PHQ))) %>%
    gather(key = "Variable", value = "Value", - f.eid) %>%
    mutate(Variable = factor(Variable, levels = c(paste0(PHQ, "_dep_", 0:3, "_isNA"),
                                                  paste0(PHQ, "_dep_mh", "_isNA"),
                                                  paste0(PHQ, "_dep_pain", "_isNA"),
                                                  paste0(PHQ, "_dep_hwell", "_isNA"),
                                                  paste0(PHQ, "_dep_mwell", "_isNA")),
                             labels = c("Initial","Repeat","Imaging","Repeat Imaging","Mental health","Pain","Health & well-being","Mental well-being")
    ))
  }else{
    dataTmp <- dataWide %>%
      dplyr::select(f.eid, (ends_with("_isNA") & starts_with(PHQ))) %>%
      gather(key = "Variable", value = "Value", - f.eid) %>%
      mutate(Variable = factor(Variable, levels = c(paste0(PHQ, "_dep_", 0:3, "_isNA"),
                                                    paste0(PHQ, "_dep_mh", "_isNA"),
                                                    paste0(PHQ, "_dep_mwell", "_isNA")),
                               labels = c("Initial","Repeat","Imaging","Repeat Imaging","Mental health","Mental well-being")
      ))
  }

  png(paste0("Output/data_merge_long/",PHQ,"NMissing.png"), width = 3000, height = 1500, res = 300)
  print(
  ggplot(dataTmp, aes(x = Variable, fill = factor(Value))) +
    geom_bar(stat = "count") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    labs(x = "Time Point", y = "Count", fill = "Number of\nQuestions\nNot Answered")
  )
  dev.off()

  # ---------
  # Table used to create above figure:
  write.csv(
    dataTmp %>%
      group_by(Variable) %>%
      count(Value) %>%
      dplyr::select("Time Point" = Variable, "N Questions Not Answered" = Value, N = n),
    paste0("Output/data_merge_long/",PHQ,"NMissing.csv"), row.names = F, quote = F)

  # ---------
  # Number of people with depression data at at-least one time point
  # https://stats.stackexchange.com/questions/4441/how-many-data-points-do-we-need-for-mixed-effects-longitudinal-data
  # People with at least one time point will be used in the estimation of the intercept
  NTimePoints <- dataWide %>%
    dplyr::select((!ends_with("_isNA") & starts_with(PHQ))) %>%
    mutate(N_timepoints_responded = rowSums(!is.na(.))) %>%
    count(N_timepoints_responded)

  write.csv(NTimePoints, paste0("Output/data_merge_long/", PHQ ,"N_timepoints_responded.csv"), row.names = F, quote = F)

  # ---------

})

# -----------------------------
# Plot ages at each time point (box plot):
dataTmpAge <-  dataWide %>%
  dplyr::select(f.eid, (starts_with("age_") & !ends_with("education") )) %>%
  gather(key = "Variable", value = "Value", - f.eid) %>%
  mutate(Variable = factor(Variable, levels = c(paste0("age_", 0:3),
                                                paste0("age_mh"),
                                                paste0("age_pain"),
                                                paste0("age_hwell"),
                                                paste0("age_mwell")),
                           labels = c("Initial","Repeat","Imaging","Repeat Imaging","Mental health","Pain","Health & well-being","Mental well-being")
  ))

png(paste0("Output/data_merge_long/ages.png"), width = 3000, height = 1500, res = 300)
print(
ggplot(dataTmpAge) +
  geom_boxplot(aes(x = Variable, y = Value))+
  ylab("Age (years)") +
  xlab("Time Point") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
)
dev.off()

# ----------------------------
# Figure IL-6 vs age at initial appointment
# Do older people have higher IL-6?

png(paste0("Output/data_merge_long/age_vs_IL6.png"), width = 2500, height = 1500, res = 300)
print(
ggplot(data = dataWide) +
  geom_point(aes(x = age_0, y = IL6, color = IL6_tertile))+
  scale_color_manual(values = c(  "darkolivegreen3","dodgerblue2", "darkorchid3"))+
  labs(color = "IL-6\nTertile\nGroup",
       y = "IL-6 (Z-score)",
       x = "Age at Initial Time Point (years)")
)
dev.off()

write.csv(
dataWide %>%
  group_by(IL6_tertile) %>%
  reframe(baseline_age_mean = mean(age_0, na.rm = T),
          baseline_age_SD = sd(age_0, na.rm = T),
          baseline_age_min = min(age_0, na.rm = T),
          baseline_age_max = max(age_0, na.rm = T),
          baseline_age_IQR = IQR(age_0, na.rm = T)) %>%
  mutate(across(where(is.numeric), ~round(., 3))),
"Output/data_merge_long/age_vs_IL6_tertile.csv", quote = F, row.names = F)

# ----------------------------
# IL-6 histogram coloured by tertile
png(paste0("Output/data_merge_long/IL-6_histogram.png"), width = 2500, height = 1500, res = 300)
print(
ggplot(data = dataWide)+
  geom_histogram(aes(x = IL6, fill = IL6_tertile), position = "identity", binwidth = 0.1, alpha = 0.7)+
  scale_fill_manual(values = c(  "darkolivegreen3","dodgerblue2", "darkorchid3"))+
  labs(fill = "IL-6\nTertile\nGroup",
       y = "N People",
       x = "IL-6 (Z-score)")
)
dev.off()

# ----------------------------
# Range, mean, sd, etc for IL-6 in each tertile (to compare with ALSPAC)
# Note that IL6 is a z-score of the UKB normalised expression measures. UKB don't give the raw values of proteins from the Olink panel.
write.csv(
dataWide %>%
group_by(IL6_tertile) %>%
  reframe(IL6_mean = mean(IL6, na.rm = T),
          IL6_SD = sd(IL6, na.rm = T),
          IL6_min = min(IL6, na.rm = T),
          IL6_max = max(IL6, na.rm = T),
          IL6_IQR = IQR(IL6, na.rm = T))%>%
  mutate(across(where(is.numeric), ~round(., 3))),
"Output/data_merge_long/IL6_tertile_stats.csv", quote = F, row.names = F)


# -----------------------------
# Plots and summary statistics of covariate variables:
# Split by IL-6 tertile:
# Sex
write.csv(
dataWide %>%
  group_by(IL6_tertile) %>%
  count(Sex),
"Output/data_merge_long/covars_sex_count.csv", quote = F, row.names = F)

# BMI
write.csv(
dataWide %>%
  group_by(IL6_tertile) %>%
  reframe(BMI_mean = mean(BMI_raw, na.rm = T),
          BMI_SD = sd(BMI_raw, na.rm = T),
          BMI_min = min(BMI_raw, na.rm = T),
          BMI_max = max(BMI_raw, na.rm = T),
          BMI_IQR = IQR(BMI_raw, na.rm = T))%>%
  mutate(across(where(is.numeric), ~round(., 3))),
"Output/data_merge_long/covars_BMI_stats.csv", quote = F, row.names = F)

# log BMI
write.csv(
dataWide %>%
  group_by(IL6_tertile) %>%
  reframe(log_BMI_mean = mean(BMI, na.rm = T),
          log_BMI_SD = sd(BMI, na.rm = T),
          log_BMI_min = min(BMI, na.rm = T),
          log_BMI_max = max(BMI, na.rm = T),
          log_BMI_IQR = IQR(BMI, na.rm = T))%>%
  mutate(across(where(is.numeric), ~round(., 3))),
"Output/data_merge_long/covars_BMI_log_stats.csv", quote = F, row.names = F)

# Townsend deprivation index
write.csv(
dataWide %>%
  group_by(IL6_tertile) %>%
  reframe(Townsend_mean = mean(Townsend, na.rm = T),
          Townsend_SD = sd(Townsend, na.rm = T),
          Townsend_min = min(Townsend, na.rm = T),
          Townsend_max = max(Townsend, na.rm = T),
          Townsend_IQR = IQR(Townsend, na.rm = T))%>%
  mutate(across(where(is.numeric), ~round(., 3))),
"Output/data_merge_long/covars_Townsend_stats.csv", quote = F, row.names = F)

# Combined plots:
png("Output/data_merge_long/desc_covars.png", res = 300, width = 3650, height = 2500)
print(
  plot_grid(
ggplot(dataWide, aes(x = IL6_tertile, y = Townsend)) +
  geom_violin(aes(fill = IL6_tertile), width = 0.8, alpha = 0.3) +
  geom_boxplot(width = 0.2, color = "grey", alpha = 0.4) +
  xlab("IL-6 Tertile Group") +
  ylab("Townsend\nDeprivation Index") +
  theme(legend.position="none") +
  scale_fill_manual(values = c(  "darkolivegreen3","dodgerblue2", "darkorchid3"))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
,
ggplot(dataWide, aes(x = IL6_tertile, y = BMI_raw)) +
  geom_violin(aes(fill = IL6_tertile), width = 0.8, alpha = 0.3) +
  geom_boxplot(width = 0.2, color = "grey", alpha = 0.4) +
  xlab("IL-6 Tertile Group") +
  ylab("BMI") +
  theme(legend.position="none") +
  scale_fill_manual(values = c(  "darkolivegreen3","dodgerblue2", "darkorchid3"))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
,
ggplot(dataWide, aes(x = IL6_tertile, y = BMI)) +
  geom_violin(aes(fill = IL6_tertile), width = 0.8, alpha = 0.3) +
  geom_boxplot(width = 0.2, color = "grey", alpha = 0.4) +
  xlab("IL-6 Tertile Group") +
  ylab("log transformed\nBMI") +
  theme(legend.position="none") +
  scale_fill_manual(values = c(  "darkolivegreen3","dodgerblue2", "darkorchid3"))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
,
ggplot(dataWide) +
  geom_bar(stat = "count", aes(x = Sex, fill = IL6_tertile, alpha = Sex) ) +
  facet_wrap(~ IL6_tertile, strip.position = "bottom") +
  ylab("Number of\nparticipants") +
  xlab("Sex by IL-6 Tertile Group") +
  theme(legend.position="none") +
  scale_fill_manual(values = c(  "darkolivegreen3","dodgerblue2", "darkorchid3")) +
  scale_alpha_manual(values = c(0.8, 0.6))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
,
ggplot(dataWide) +
  geom_bar(stat = "count", aes(x = smoking_status, fill = IL6_tertile), alpha = 0.8 ) +
  facet_wrap(~ IL6_tertile, strip.position = "bottom") +
  ylab("Number of\nparticipants") +
  xlab("Smoking status by IL-6 Tertile Group") +
  theme(legend.position="none") +
  scale_fill_manual(values = c(  "darkolivegreen3","dodgerblue2", "darkorchid3")) +
  scale_alpha_manual(values = c(0.8, 0.6))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
,labels = c("A", "B", "C", "D", "E")))
dev.off()


ggplot(dataWide, aes(x = IL6_tertile, y = birth_weight)) +
  geom_violin(aes(fill = IL6_tertile), width = 0.8, alpha = 0.3) +
  geom_boxplot(width = 0.2, color = "grey", alpha = 0.4) +
  xlab("IL-6 Tertile Group") +
  ylab("Self-reported birth weight (kg?)") +
  theme(legend.position="none") +
  scale_fill_manual(values = c(  "darkolivegreen3","dodgerblue2", "darkorchid3"))

ggplot(dataWide, aes(x = BMI_raw, y = birth_weight)) +
  geom_point()

ggplot(dataWide, aes(x = IL6_INT, y = birth_weight)) +
  geom_point()

# -----------------------------
# Distribution of BMI and Townsend
# Plot histogram
png("Output/data_merge_long/hist_covars.png", res = 300, width = 3000, height = 1500)
print(
  plot_grid(
    ggplot(dataWide)+
      geom_histogram(aes(x = BMI))
    ,
    ggplot(dataWide)+
      geom_histogram(aes(x = Townsend))
    ,
    ggplot(dataWide)+
      geom_histogram(aes(x = log(BMI)))
    ,
    ggplot(dataWide)+
      geom_histogram(aes(x = log(Townsend)))
  )
)
dev.off()

# -----------------------------
# Lots of people in tertile 3 have higher BMI
# Sensitivity analysis removing people with high BMI?
# BMI range:
# https://www.nhs.uk/common-health-questions/lifestyle/what-is-the-body-mass-index-bmi/#:~:text=below%2018.5%20%E2%80%93%20you're%20in,re%20in%20the%20obese%20range
# BMI >= 40 = morbidly obese according to NHS/NICE https://digital.nhs.uk/data-and-information/publications/statistical/health-survey-england-additional-analyses/ethnicity-and-health-2011-2019-experimental-statistics/overweight-and-obesity#top
dataWide <- dataWide %>%
  mutate(BMI_raw = round(BMI_raw, 1)) %>%
  mutate(BMI_cat = factor(case_when(BMI_raw < 18.5 ~ "Underweight",
                             between(BMI_raw, 18.5, 25) ~ "Healthy",
                             between(BMI_raw, 25, 30) ~ "Overweight",
                             between(BMI_raw, 30, 40) ~ "Obese",
                             BMI_raw >= 40 ~ "Morbidly Obese"), levels = c("Underweight", "Healthy", "Overweight", "Obese", "Morbidly Obese")))

ggplot(dataWide) +
  geom_point(aes(x = BMI_raw, y = IL6_INT, color = BMI_cat), alpha = 0.4)

ggplot(dataWide) +
  geom_bar(stat = "count", aes(x = BMI_cat, fill = IL6_tertile) ) +
  facet_wrap(~ IL6_tertile, strip.position = "bottom") +
  ylab("Number of\nparticipants") +
  xlab("IL-6 Tertile Group") +
  theme(legend.position="none") +
  scale_fill_manual(values = c(  "darkolivegreen3","dodgerblue2", "darkorchid3"))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

dataWideBMI30rm <- dataWide %>%
  filter(BMI_raw < 30)
nrow(dataWideBMI30rm)


dataWideBMI40rm <- dataWide %>%
  filter(BMI_raw < 40)
nrow(dataWideBMI40rm)


dataWideBMI30Plus <- dataWide %>%
  filter(BMI_raw >= 30)
nrow(dataWideBMI30Plus)


dataWideBMI40Plus <- dataWide %>%
  filter(BMI_raw >= 40)
nrow(dataWideBMI40Plus)



# Is this sex specific?
png("Output/data_merge_long/sex_split_BMI_cat_IL6.png", res = 300, width = 2550, height = 1500)
print(
ggplot(dataWide) +
  geom_bar(stat = "count", aes(x = BMI_cat, fill = Sex) ) +
  facet_wrap(~ IL6_tertile, strip.position = "bottom") +
  ylab("Number of\nparticipants") +
  xlab("IL-6 Tertile Group") +
  scale_fill_manual(values = c( "darkorange1", "mediumpurple3" ))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
)
dev.off()


# -----------------------------
# Demographic table of final sample

dataSub <- dataWide %>%
  dplyr::select(
          `IL-6 Tertile` = IL6_tertile,
         Sex,
         `BMI (log transformed)` = BMI,
         `BMI (categorised)` = BMI_cat,
         `Inflammatory condition` = inflam_condition,
         `Taking inflammatory medication` = inflam_meds,
         `Smoking status` = smoking_status,
         `Townsend Deprivation Index` = Townsend,
         `IL-6 (Inverse Normal Transformed, Z-score)` = IL6_INT,
         `IL-6 (Olink normalised)` = IL6_raw,
         `PHQ-2 (Initial time point)` = PHQ2_dep_0,
         `PHQ-2 (Repeat time point)` = PHQ2_dep_1,
         `PHQ-2 (Imaging time point)` = PHQ2_dep_2,
         `PHQ-2 (Repeat Imaging time point)` = PHQ2_dep_3,
         `PHQ-2 (Mental health time point)` = PHQ2_dep_mh,
         `PHQ-2 (Pain time point)` = PHQ2_dep_pain,
         `PHQ-2 (Health & Well-being time point)` = PHQ2_dep_hwell,
         `PHQ-2 (Mental well-being time point)` = PHQ2_dep_mwell,
         `Age (Initial time point)` = age_0,
         `Age (Repeat time point)` = age_1,
         `Age (Imaging time point)` = age_2,
         `Age (Repeat Imaging time point)` = age_3,
         `Age (Mental health time point)` = age_mh,
         `Age (Pain time point)` = age_pain,
         `Age (Health & Well-being time point)` = age_hwell,
         `Age (Mental well-being time point)` = age_mwell
  ) %>%
  mutate_at(c("Sex", "IL-6 Tertile", "Inflammatory condition", "BMI (categorised)"), as.factor) %>%
  mutate_at(c("Sex", "IL-6 Tertile", "Inflammatory condition", "BMI (categorised)"), function(x) fct_explicit_na(x, na_level = "Missing"))

tabOne <- CreateTableOne(data = dataSub,
                         factorVars = c(  "Sex", "Inflammatory condition", "BMI (categorised)"),
                         strata = "IL-6 Tertile",
                         includeNA = T)

print(tabOne)[-c(2:5),1:3]

# Save as csv:
write.csv(print(tabOne)[-c(2:5),1:3], "Output/data_merge_long/demographicTable.csv")

# -----------------------------
# Z-score scale BMI and Townsend Deprivation Index
dataWide <- dataWide %>%
  mutate_at(c("Townsend", "BMI_raw", "BMI"), scale)

dataWideBMI <- list(dataWideBMI30rm,
                    dataWideBMI40rm,
                    dataWideBMI30Plus,
                    dataWideBMI40Plus)

fileName <- c("BMI30rm",
              "BMI40rm",
              "BMI30Plus",
              "BMI40Plus")

lapply(1:4, function(i){
  dataBMI <- dataWideBMI[[i]] %>%
    mutate_at(c("Townsend", "BMI_raw", "BMI"), scale)

  # Convert to long
  longData <- gather(dataBMI, age_cat, age, c(paste0("age_", c(0:3, "mh", "pain", "hwell", "mwell"))), factor_key=TRUE)
  longData_PHQ2 <- gather(dataBMI, dep_cat, dep, c(paste0("PHQ2_dep_", c(0:3, "mh", "pain", "hwell", "mwell" ))), factor_key=TRUE)
  longData$dep_cat <- longData_PHQ2$dep_cat
  longData$dep <- longData_PHQ2$dep

  longData <- longData %>%
    mutate(age_original = as.numeric(age) ) %>%
    mutate(age = age - 39 ) %>% # reviewer asked to center age at start of trajectory
    mutate(age_mean = age_original - mean( age, na.rm = T )) %>%
    mutate_at(c("Sex", "IL6_tertile", "inflam_condition"), as.factor)

  # Save file
   write.csv(longData, paste0("/Volumes/GenScotDepression/users/amelia/UKB/UKB_dataLong_PHQ2_", fileName[i] ,".csv"), row.names = F)

})

# -----------------------------
# Convert to long format
dataLong <- gather(dataWide, age_cat, age, c(paste0("age_", c(0:3, "mh", "pain", "hwell", "mwell"))), factor_key=TRUE)
data_PHQ2 <- gather(dataWide, dep_cat, dep, c(paste0("PHQ2_dep_", c(0:3, "mh", "pain", "hwell", "mwell" ))), factor_key=TRUE)
dataLong$dep_cat <- data_PHQ2$dep_cat
dataLong$dep <- data_PHQ2$dep

dataLongPHQ4 <- gather(dataWide, age_cat, age, c(paste0("age_", c(0:3, "mh", "mwell"))), factor_key=TRUE)
data_PHQ4 <- gather(dataWide, dep_cat, dep, c(paste0("PHQ4_dep_", c(0:3, "mh", "mwell" ))), factor_key=TRUE)
dataLongPHQ4$dep_cat <- data_PHQ4$dep_cat
dataLongPHQ4$dep <- data_PHQ4$dep


# Center to mean age

dataLong <- dataLong %>%
  mutate(age_original = as.numeric(age) ) %>%
  mutate(age = age - 39 ) %>% # reviewer asked to center age at start of trajecotry
  mutate(age_mean = age_original - mean( age, na.rm = T )) %>% # saved the mean centered variable in case analysis changes again
  mutate_at(c("Sex", "IL6_tertile", "inflam_condition"), as.factor)

mean(dataLong$age_original, na.rm = T) # 63 y

dataLongPHQ4 <- dataLongPHQ4 %>%
  mutate(age_original = as.numeric(age) ) %>%
  mutate(age = age - 39 ) %>% # reviewer asked to center age at start of trajecotry
  mutate(age_mean = age_original - mean( age, na.rm = T )) %>% # saved the mean centered variable in case analysis changes again
  mutate_at(c("Sex", "IL6_tertile", "inflam_condition"), as.factor)


# -----------------------------
# Save data:
write.csv(dataLong, "/Volumes/GenScotDepression/users/amelia/UKB/UKB_dataLong_PHQ2.csv", row.names = F)
write.csv(dataLongPHQ4, "/Volumes/GenScotDepression/users/amelia/UKB/UKB_dataLong_PHQ4.csv", row.names = F)
write.csv(dataWide, "/Volumes/GenScotDepression/users/amelia/UKB/UKB_dataWide.csv", row.names = F)

# ----------------------------
# Mean/SD for age and depression score at each time point
# Table
write.csv(
dataLong %>%
  mutate(dep_cat = factor(dep_cat, levels = c(paste0("PHQ2_dep_", 0:3),
                                                paste0("PHQ2_dep_mh"),
                                                paste0("PHQ2_dep_pain"),
                                                paste0("PHQ2_dep_hwell"),
                                                paste0("PHQ2_dep_mwell")),
                           labels = c("Initial","Repeat","Imaging","Repeat Imaging","Mental health","Pain","Health & well-being","Mental well-being")
  )) %>%
  group_by(dep_cat) %>%
  reframe(dep_mean = mean(dep, na.rm = T),
          dep_SD = sd(dep, na.rm = T),
          dep_min = min(dep, na.rm = T),
          dep_max = max(dep, na.rm = T),
          dep_IQR = IQR(dep, na.rm = T),
          dep_N = sum(!is.na(dep)),
          dep_N_missing = sum(is.na(dep)),
          age_mean = mean(age_original, na.rm = T),
          age_SD = sd(age_original, na.rm = T),
          age_min = min(age_original, na.rm = T),
          age_max = max(age_original, na.rm = T),
          age_IQR = IQR(age_original, na.rm = T),
          ) %>%
  mutate(across(where(is.numeric), ~round(., 3))) %>%
  rename(`Time Point` = dep_cat),
"Output/data_merge_long/summary_stats_age_dep.csv", row.names = F, quote=F
)


# Cell counts by age by time point
dataLong %>%
  mutate(age_cat = factor(case_when(age_cat == "age_0" ~ "Initial",
                             age_cat == "age_1" ~ "Repeat",
                             age_cat == "age_2" ~ "Imaging",
                             age_cat == "age_3" ~"Repeat Imaging",
                             age_cat == "age_mh" ~"Mental health",
                             age_cat == "age_pain"~ "Pain",
                             age_cat == "age_hwell"~ "Health & well-being",
                             age_cat == "age_mwell"~ "Mental well-being"), levels = c("Initial", "Repeat", "Imaging", "Repeat Imaging",
                                                                                      "Mental health", "Pain", "Health & well-being",
                                                                                      "Mental well-being"))) %>%
  group_by(age_cat, round(age_original)) %>%
  summarise(n()) %>%
  rename("Time point" = 1,"Age (years)" = 2, "N" = 3) %>%
  mutate(N = ifelse(N < 5, "N < 5", as.character(N))) %>%
  filter(!is.na(`Age (years)`)) %>%
  write.csv(., "Output/data_merge_long/N_by_age_by_time_point.csv", row.names = F)

# ---- Distribution of IL-6 tertiles for NCompleted == 1 and NCompleted > 1
png("Output/data_merge_long/MinTwo_IL6_tertiles.png", res=300, height = 1000, width = 2000)
dataWide %>%
  mutate(`Number of assessments attended` = factor(ifelse(NCompleted > 1, "At least two", "Only one" ),
                                                   levels = c("Only one", "At least two" ))) %>%
  group_by(`Number of assessments attended`) %>%
  count(IL6_tertile) %>%
  ggplot(data = .) +
  geom_col(aes(x = `Number of assessments attended`, y = n, fill = IL6_tertile))+
  scale_fill_manual(values = c(  "darkolivegreen3","dodgerblue2", "darkorchid3")) +
  labs(fill = "IL-6\nTertile\nGroup")
dev.off()
# Number of people with just one assessment:
sum(dataWide$NCompleted == 1) 
sum(dataWide$NCompleted > 1) 
# Number of people who died after initial assessment
dataWide %>%
  filter(DOD > f.53.0.0) %>%
  nrow() 

# Number of people who had not died, excluding ppl from imaging appointments
dataWide %>%
  filter(Dead == FALSE) %>%
  nrow()


