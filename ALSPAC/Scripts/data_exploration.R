# Plots of cleaned data

##################################################################
### Plot ages at each time point (box plot) ######################
##################################################################

dataTmpAge <-  dataWide %>%
  dplyr::select(Subject, (starts_with("age_t")  )) %>%
  gather(key = "Variable", value = "Value", - Subject) %>%
  mutate(Variable = factor(Variable, labels = 1:11))

png(paste0("Output/serum/ages.png"), width = 3000, height = 1500, res = 300)
print(
  ggplot(dataTmpAge) +
    geom_boxplot(aes(x = Variable, y = Value))+
    ylab("Age (years)") +
    xlab("Time Point")
)
dev.off()


##################################################################
### Table of ages and dep scores at each time point ##############
##################################################################

write.csv(
  dataLong %>%
    mutate(dep_cat = factor(dep_cat,  labels = 1:11)) %>%
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
  "Output/serum/summary_stats_age_dep.csv", row.names = F, quote=F
)

##################################################################
### Number of participants and their responses at each time point
##################################################################
dataTmpDep <- dataWide %>%
  dplyr::select(Subject, (starts_with("mfq"))) %>%
  gather(key = "Variable", value = "Value", - Subject) %>%
  mutate(Variable = factor(Variable, labels = 1:11)) %>%
  mutate(Value = factor(case_when(Value <= 5 ~ "0-5",
                           Value > 5 & Value <= 10~ "6-10",
                           Value > 10 & Value <= 15~ "11-15",
                           Value > 15 & Value <= 20~ "16-20",
                           Value > 20~ "21-26",
                           is.na(Value) ~ "Missing"
                           ), levels = c("0-5", "6-10", "11-15", "16-20", "21-26", "Missing")))

dataTmpDep %>%
  group_by(Variable) %>%
  mutate(Value = as.numeric(Value)) %>%
  count(Value)

png(paste0("Output/serum/NDepScore.png"), width = 3000, height = 1500, res = 300)
print(
  ggplot(dataTmpDep, aes(x = Variable, fill = factor(Value))) +
    geom_bar(stat = "count") +
    labs(x = "Time Point", y = "Count", fill = "SMFQ Score")+
    guides(fill = guide_legend(ncol = 1)) +
    scale_fill_brewer(palette = "Dark2")
)
dev.off()

##################################################################
### Histograms of distributions ##################################
##################################################################
# Plot the distributions of data for continuous variables

pHistClean <- lapply(c("IL6", "IL6_log", "BMI_age9","BMI_age9_log"), function(id){
  ggplot() +
    geom_histogram(data = dataWide, aes(x = !!sym(id)))
})

ggsave("Output/serum/histograms_cleaned_data.pdf",
       plot = marrangeGrob(pHistClean, nrow = 6, ncol = 2, layout_matrix = matrix(1:12, nrow = 6, byrow = TRUE)),
       width = 9, height = 15)


##################################################################
### Distribution of IL-6 by tertile ##############################
##################################################################
# IL-6 histogram coloured by tertile
png(paste0("Output/serum/IL-6_histogram.png"), width = 2500, height = 1500, res = 300)
ggplot(data = dataWide)+
  geom_histogram(aes(x = scale(IL6), fill = IL6_tertile), position = "identity", binwidth = 0.1, alpha = 0.7)+
  scale_fill_manual(values = c(  "darkolivegreen3","dodgerblue2", "darkorchid3"),
                    labels = c("Bottom Third", "Middle Third", "Top Third"))+
  labs(fill = "IL-6\nTertile\nGroup",
       y = "N People",
       x = "IL-6 (Z-score)")
dev.off()

# ----------------------------
# Range, mean, sd, etc for IL-6 in each tertile (to compare with UKB)
# Note that in UKB IL6 is a z-score of the UKB normalised expression measures. UKB don't give the raw values of proteins from the Olink panel but ALSPAC do.
write.csv(
  dataWide %>%
    mutate(IL6 = scale(IL6)) %>%
    group_by(IL6_tertile) %>%
    reframe(IL6_mean = mean(IL6, na.rm = T),
            IL6_SD = sd(IL6, na.rm = T),
            IL6_min = min(IL6, na.rm = T),
            IL6_max = max(IL6, na.rm = T),
            IL6_IQR = IQR(IL6, na.rm = T))%>%
    mutate(across(where(is.numeric), ~round(., 3))),
  "Output/serum/IL6_tertile_stats.csv", quote = F, row.names = F)


##################################################################
### Plots of total number of depression episodes #################
### Split by categorical variables (covariates) ##################
##################################################################

plots <- lapply(c("Sex", "Maternal.education.at.birth"), function(var){
  ggplot(dataWide) +
    geom_bar(aes(x = dep_episodes, fill = !!sym(var))) +
    labs(x = "Total Number of Depressive Episodes", y = "N people") +
    theme(legend.position = "top")+
    scale_x_continuous(breaks=c(0:10))
})

depTotEpPlot <- plot_grid(plotlist = list(plots[[1]] +
                                            theme(axis.title.x=element_blank(),
                                                  axis.text.x=element_blank(),
                                                  axis.ticks.x=element_blank()),
                                          plots[[2]]), nrow = 2,
                          rel_heights = c(0.45,0.55))

png("Output/serum/nTotalDepression_covariates.png", res = 300, height = 2000, width = 2000)
print(depTotEpPlot)
dev.off()


##################################################################
### # Plots and summary statistics of covariate variables:########
##################################################################

# Split by IL-6 tertile:
# Sex
write.csv(
  dataWide %>%
    group_by(IL6_tertile) %>%
    mutate(Sex = ifelse(Sex == 1, "Female", "Male")) %>%
    count(Sex),
  "Output/serum/covars_sex_count.csv", quote = F, row.names = F)

# 0 = CSE/Olevel/Vocational 1=Alevel/Degree
write.csv(
  dataWide %>%
    group_by(IL6_tertile) %>%
    mutate(Maternal.education.at.birth = ifelse(Maternal.education.at.birth == 1, "Alevel/Degree", "CSE/Olevel/Vocational")) %>%
    count(Maternal.education.at.birth),
  "Output/serum/covars_matEd_count.csv", quote = F, row.names = F)

# BMI
write.csv(
  dataWide %>%
    group_by(IL6_tertile) %>%
    reframe(BMI_mean = mean(BMI_age9_original, na.rm = T),
            BMI_SD = sd(BMI_age9_original, na.rm = T),
            BMI_min = min(BMI_age9_original, na.rm = T),
            BMI_max = max(BMI_age9_original, na.rm = T),
            BMI_IQR = IQR(BMI_age9_original, na.rm = T))%>%
    mutate(across(where(is.numeric), ~round(., 3))),
  "Output/serum/covars_BMI_stats.csv", quote = F, row.names = F)

# log BMI
write.csv(
  dataWide %>%
    group_by(IL6_tertile) %>%
    reframe(log_BMI_mean = mean(BMI_age9_log, na.rm = T),
            log_BMI_SD = sd(BMI_age9_log, na.rm = T),
            log_BMI_min = min(BMI_age9_log, na.rm = T),
            log_BMI_max = max(BMI_age9_log, na.rm = T),
            log_BMI_IQR = IQR(BMI_age9_log, na.rm = T))%>%
    mutate(across(where(is.numeric), ~round(., 3))),
  "Output/serum/covars_BMI_log_stats.csv", quote = F, row.names = F)


# Combined plots:

png("Output/serum/desc_covars.png", res = 300, width = 3550, height = 2000)
print(
  plot_grid(
    ggplot(dataWide%>%
             mutate(Sex = ifelse(Sex == 1, "Female", "Male")) ) +
      geom_bar(stat = "count", aes(x = Sex, fill = IL6_tertile, alpha = Sex) ) +
      facet_wrap(~ IL6_tertile, strip.position = "bottom") +
      ylab("Number of\nparticipants") +
      xlab("Sex by IL-6 Tertile Group") +
      theme(legend.position="none") +
      scale_fill_manual(values = c(  "darkolivegreen3","dodgerblue2", "darkorchid3")) +
      scale_alpha_manual(values = c(0.8, 0.6))
    ,
    ggplot(dataWide  %>%   mutate(Maternal.education.at.birth = ifelse(Maternal.education.at.birth == 1, "A/D", "C/O/V"))  ) +
      geom_bar(stat = "count", aes(x = Maternal.education.at.birth, fill = IL6_tertile) ) +
      facet_wrap(~ IL6_tertile, strip.position = "bottom") +
      ylab("Number of\nparticipants") +
      xlab("Maternal Education by IL-6 Tertile Group") +
      theme(legend.position="none") +
      scale_fill_manual(values = c(  "darkolivegreen3","dodgerblue2", "darkorchid3"))+
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
    ,
    ggplot(dataWide, aes(x = IL6_tertile, y = BMI_age9_original)) +
      geom_violin(aes(fill = IL6_tertile), width = 0.8, alpha = 0.3) +
      geom_boxplot(width = 0.2, color = "grey", alpha = 0.4) +
      xlab("IL-6 Tertile Group") +
      ylab("BMI") +
      theme(legend.position="none") +
      scale_fill_manual(values = c(  "darkolivegreen3","dodgerblue2", "darkorchid3"))+
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
    ,
    ggplot(dataWide, aes(x = IL6_tertile, y = BMI_age9_log)) +
      geom_violin(aes(fill = IL6_tertile), width = 0.8, alpha = 0.3) +
      geom_boxplot(width = 0.2, color = "grey", alpha = 0.4) +
      xlab("IL-6 Tertile Group") +
      ylab("log transformed\nBMI") +
      theme(legend.position="none") +
      scale_fill_manual(values = c(  "darkolivegreen3","dodgerblue2", "darkorchid3"))+
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
    , labels = c("A", "B", "C", "D")))
dev.off()

dataWide %>%
  filter(BMI_age9_original < 30) %>%
  nrow()
# 4,937

dataWide %>%
  filter(BMI_age9_original >= 30) %>%
  nrow()
# 8

##################################################################
### # Townsend sensitivity:########
##################################################################
# Overlap between different time points

png("Output/serum/Townsend_timePoints.png", res = 300, width = 2250, height = 1250)
dataWide %>%
  mutate_at(vars(c(kqTownsendq5, tcTownsendq5, jTownsendq5)), ~as.numeric(.)) %>%
  relocate(jTownsendq5, .before = kqTownsendq5) %>%
  group_by(kqTownsendq5, jTownsendq5, tcTownsendq5 ) %>%
  mutate(weight = n()) %>%
ggparcoord(. ,
            columns = which(colnames(dataWide) %in% c("kqTownsendq5", "jTownsendq5", "tcTownsendq5")),
           scale = "globalminmax",  # You can adjust the scale as needed
           alphaLines = 0.5,  # Adjust transparency if needed
           mapping = aes(color = weight, size = weight) ) +  # Use the weight variable for coloring
  scale_color_viridis_c(option = "magma", direction = -1) +  # You can change 'magma' to another viridis color palette
  theme_minimal() +  # Adjust the theme as needed
  labs(color = "N people",
       size = "",
       y =  "Townsend Deprivation Index Quintile\n(1 = least deprived, 5 = most deprived)",
       x = "Time Point")
dev.off()


# Alluvial plots:
# Include missing at any time point:
dataWide %>%
  mutate_at(vars(c(kqTownsendq5, tcTownsendq5, jTownsendq5)), ~fct_relevel(., as.character(5:1))) %>%
  mutate_at(vars(c(kqTownsendq5, tcTownsendq5, jTownsendq5)), ~fct_na_value_to_level(., "Missing")) %>%
  dplyr::select(c(kqTownsendq5, jTownsendq5, tcTownsendq5)) %>%
  group_by(kqTownsendq5, jTownsendq5, tcTownsendq5) %>%
  mutate(weight = n())  %>%
  distinct() %>%
  ggplot(., aes(y = weight, axis1 = jTownsendq5, axis2 = kqTownsendq5, axis3 = tcTownsendq5))+
  geom_alluvium(aes(fill = weight)) +
  geom_stratum() +
  geom_label(stat = "stratum", aes(label = after_stat(stratum)))+
  scale_x_discrete(limits = c("~ 4y", "~ 6y", "~ 16y"))

# remove missing at any time point
dataWide %>%
  mutate_at(vars(c(kqTownsendq5, tcTownsendq5, jTownsendq5)), ~fct_relevel(., as.character(5:1))) %>%
  filter(!is.na(kqTownsendq5)) %>%
  filter(!is.na(jTownsendq5)) %>%
  filter(!is.na(tcTownsendq5)) %>%
  dplyr::select(c(kqTownsendq5, jTownsendq5, tcTownsendq5)) %>%
  group_by(kqTownsendq5, jTownsendq5, tcTownsendq5) %>%
  mutate(weight = n())  %>%
  distinct() %>%
  ggplot(., aes(y = weight, axis1 = jTownsendq5, axis2 = kqTownsendq5, axis3 = tcTownsendq5))+
  geom_alluvium(aes(fill = weight)) +
  geom_stratum() +
  geom_label(stat = "stratum", aes(label = after_stat(stratum)))+
  scale_x_discrete(limits = c("~ 4y", "~ 6y", "~ 16y")) +
  xlab("Townsend Deprivation Quintile") +
  ylab("N people") +
  labs(fill = "N people")

png("Output/serum/Townsend_alluvial.png", res = 300, width = 3650, height = 2550)
# Colour by if people stay the same, move drastically down, move drastically up, or dont move much
print(
dataWide %>%
  mutate_at(vars(c(kqTownsendq5, tcTownsendq5, jTownsendq5)), ~as.numeric(.)) %>%
  mutate(dif = as.factor(case_when(kqTownsendq5 == jTownsendq5 & kqTownsendq5 == tcTownsendq5 ~ "Same",
                         ( kqTownsendq5 == jTownsendq5 & is.na(tcTownsendq5) ) |
                         ( kqTownsendq5 == tcTownsendq5 & is.na(jTownsendq5) ) |
                         ( jTownsendq5 == tcTownsendq5 & is.na(kqTownsendq5) )
                          ~ "Same at two time points\n& missing at one",
                         (jTownsendq5 - kqTownsendq5) > 2 | (kqTownsendq5 - tcTownsendq5) > 2 ~ "Move to less deprived by > 2 quintiles",
                         (kqTownsendq5 - jTownsendq5) > 2 | (tcTownsendq5 - kqTownsendq5) > 2 ~ "Move to more deprived by > 2 quintiles",
                         rowSums(is.na(dplyr::select(.,kqTownsendq5, tcTownsendq5, jTownsendq5))) > 1  ~"Missing at 2 or more time points",
                         .default = "Other Change"))) %>%
  mutate_at(vars(c(kqTownsendq5, tcTownsendq5, jTownsendq5)), ~as.factor(.)) %>%
  mutate_at(vars(c(kqTownsendq5, tcTownsendq5, jTownsendq5)), ~fct_na_value_to_level(., "Missing")) %>%
  mutate_at(vars(c(kqTownsendq5, tcTownsendq5, jTownsendq5)), ~fct_relevel(., c("Missing", as.character(5:1)))) %>%
  dplyr::select(c(kqTownsendq5, jTownsendq5, tcTownsendq5, dif))%>%
  group_by(kqTownsendq5, jTownsendq5, tcTownsendq5) %>%
  mutate(weight = n())  %>%
  distinct() %>%
  ggplot(., aes(y = weight, axis1 = jTownsendq5, axis2 = kqTownsendq5, axis3 = tcTownsendq5))+
  geom_alluvium(aes(fill = dif)) +
  geom_stratum() +
  geom_label(stat = "stratum", aes(label = after_stat(stratum)))+
  scale_x_discrete(limits = c("~ 4y", "~ 6y", "~ 16y"), expand = c(0.1,0.1)) +
  xlab("Time Points") +
  ylab("N people") +
  labs(fill = "Changes in Townsend\nquintile over time")+
  ggtitle("Changes in Townsend Deprivation Quintile (1 = least deprived, 5 = most deprived)\nat different time points")+
  scale_fill_manual(values = c( "darkolivegreen3","darkorange", "darkorchid2", "grey" ,"navy", "deepskyblue"))
)
dev.off()

# Colour by maternal education:
# 0 = CSE/Olevel/Vocational 1=Alevel/Degree
png("Output/serum/Townsend_alluvial_matEd.png", res = 300, width = 3650, height = 2550)
# Colour by if people stay the same, move drastically down, move drastically up, or dont move much
print(
  dataWide %>%
    mutate(Maternal.education.at.birth = ifelse(Maternal.education.at.birth == 1, "Alevel/Degree", "CSE/Olevel/Vocational")) %>%
    mutate_at(vars(c(kqTownsendq5, tcTownsendq5, jTownsendq5, Maternal.education.at.birth)), ~fct_na_value_to_level(., "Missing")) %>%
    mutate_at(vars(c(kqTownsendq5, tcTownsendq5, jTownsendq5)), ~fct_relevel(., c("Missing", as.character(5:1)))) %>%
    dplyr::select(c(kqTownsendq5, jTownsendq5, tcTownsendq5, Maternal.education.at.birth))%>%
    group_by(kqTownsendq5, jTownsendq5, tcTownsendq5) %>%
    mutate(weight = n())  %>%
    distinct() %>%
    ggplot(., aes(y = weight, axis1 = jTownsendq5, axis2 = kqTownsendq5, axis3 = tcTownsendq5))+
    geom_alluvium(aes(fill = Maternal.education.at.birth)) +
    geom_stratum() +
    geom_label(stat = "stratum", aes(label = after_stat(stratum)))+
    scale_x_discrete(limits = c("~ 4y", "~ 6y", "~ 16y"), expand = c(0.1,0.1)) +
    xlab("Time Points") +
    ylab("N people") +
    labs(fill = "Maternal Education")+
    ggtitle("Changes in Townsend Deprivation Quintile (1 = least deprived, 5 = most deprived)\nat different time points")+
    scale_fill_manual(values = c( "darkorange",  "deepskyblue", "navy"))
)
dev.off()

dataWide %>%
  mutate(Maternal.education.at.birth = ifelse(Maternal.education.at.birth == 1, "Alevel/Degree", "CSE/Olevel/Vocational")) %>%
  mutate_at(vars(c(kqTownsendq5, tcTownsendq5, jTownsendq5, Maternal.education.at.birth)), ~fct_na_value_to_level(., "Missing")) %>%
  mutate_at(vars(c(kqTownsendq5, tcTownsendq5, jTownsendq5)), ~fct_relevel(., c("Missing", as.character(5:1)))) %>%
  dplyr::select(c(kqTownsendq5, jTownsendq5, tcTownsendq5, Maternal.education.at.birth))%>%
  group_by(kqTownsendq5, Maternal.education.at.birth) %>%
  summarise(n())

dataWide %>%
  mutate(Maternal.education.at.birth = ifelse(Maternal.education.at.birth == 1, "Alevel/Degree", "CSE/Olevel/Vocational")) %>%
  mutate_at(vars(c(kqTownsendq5, tcTownsendq5, jTownsendq5, Maternal.education.at.birth)), ~fct_na_value_to_level(., "Missing")) %>%
  mutate_at(vars(c(kqTownsendq5, tcTownsendq5, jTownsendq5)), ~fct_relevel(., c("Missing", as.character(5:1)))) %>%
  dplyr::select(c(kqTownsendq5, jTownsendq5, tcTownsendq5, Maternal.education.at.birth))%>%
  ggplot(data = .) +
  geom_bar(aes(x = kqTownsendq5, fill = Maternal.education.at.birth))

##################################################################
####### Potential medication sensitivity ##########
##################################################################
dataWide %>%
  count(any_meds)
# 695 = N removed

##################################################################
####### Create table one for final sample, split by sex ##########
##################################################################

dataTab <- dataWide %>%
  mutate(Sex = fct_explicit_na(factor(ifelse(Sex == 0, "Males", "Females")), "NA")) %>%
  mutate(`Maternal Education At Birth` = fct_explicit_na(factor(ifelse(Maternal.education.at.birth == 0, "CSE/Olevel/Vocational", "Alevel/Degree")), "NA")) %>%
  mutate(IL6_INT = as.numeric(IL6_INT)) %>%
  dplyr::select(`IL-6 Tertile` = IL6_tertile,
                Sex,
                `BMI (log transformed)` = BMI_age9_log,
                `Maternal Education At Birth` ,
                `Currently taking any medication` = any_meds,
                `Townsend Deprivation Index quintile` = kqTownsendq5,
                `IL-6 (Inverse Normal Transformed, Z-score)` = IL6_INT,
                `IL-6 (raw value, pg/ml)` = IL6,
                `SMFQ (Time Point 1)` = mfq_t01,
                `SMFQ (Time Point 2)` = mfq_t02,
                `SMFQ (Time Point 3)` = mfq_t03,
                `SMFQ (Time Point 4)` = mfq_t04,
                `SMFQ (Time Point 5)` = mfq_t05,
                `SMFQ (Time Point 6)` = mfq_t06,
                `SMFQ (Time Point 7)` = mfq_t07,
                `SMFQ (Time Point 8)` = mfq_t08,
                `SMFQ (Time Point 9)` = mfq_t09,
                `SMFQ (Time Point 10)` = mfq_t10,
                `SMFQ (Time Point 11)` = mfq_t11,
                `Age (Time Point 1)` = age_t01,
                `Age (Time Point 2)` = age_t02,
                `Age (Time Point 3)` = age_t03,
                `Age (Time Point 4)` = age_t04,
                `Age (Time Point 5)` = age_t05,
                `Age (Time Point 6)` = age_t06,
                `Age (Time Point 7)` = age_t07,
                `Age (Time Point 8)` = age_t08,
                `Age (Time Point 9)` = age_t09,
                `Age (Time Point 10)` = age_t10,
                `Age (Time Point 11)` = age_t11
  )

tabOne <- CreateTableOne(data = dataTab,
                         factorVars = c("Sex", "Maternal Education At Birth", "Townsend Deprivation Index quintile"),
                         strata = "IL-6 Tertile",
                         includeNA = T)

print(tabOne)[-c(2:5),1:3]

write.csv(print(tabOne)[-c(2:5),1:3], "Output/serum/demographicTable.csv")
##################################################################
# Box plot of IL6 for each tertile
png("Output/serum/IL6_boxplot_tertiles.png")
ggplot(dataWide) +
  geom_boxplot(aes(x = IL6_tertile, y = IL6_log))
dev.off()

##################################################################
# Cell counts by age by time point
dataLong %>%
  group_by(age_cat, round(age_original)) %>%
  summarise(n()) %>%
  rename("Time point" = 1,"Age (years)" = 2, "N" = 3) %>%
  mutate(N = ifelse(N < 5, "N < 5", as.character(N))) %>%
  filter(!is.na(`Age (years)`)) %>%
  write.csv(., "Output/serum/N_by_age_by_time_point.csv", row.names = F)

##################################################################
# Distribution of IL-6 tertiles for NCompleted == 1 and NCompleted > 1
png("Output/serum/MinTwo_IL6_tertiles.png", res=300, height = 1000, width = 2000)
dataWide %>%
  mutate(`Number of assessments attended` = factor(ifelse(CompletedQ > 1, "At least two", "Only one" ),
                                                   levels = c("Only one", "At least two" ))) %>%
  group_by(`Number of assessments attended`) %>%
  count(IL6_tertile) %>%
  ggplot(data = .) +
  geom_col(aes(x = `Number of assessments attended`, y = n, fill = IL6_tertile))+
  scale_fill_manual(values = c(  "darkolivegreen3","dodgerblue2", "darkorchid3")) +
  labs(fill = "IL-6\nTertile\nGroup")
dev.off()
#
