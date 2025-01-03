# Function that takes a character string of inflammatory variable and dataset
# runs quartic model for unadjusted, sex adjusted and fully adjusted
# returns a list of these 3 models
# also takes type, which is the suffix of the colum name, eg. "log" or "quartic"

# ------------------------------------
quadraticInflamModel <- function(inflam, data, type){

  # Categorical and continuous forms of the marker:
  modelListTypes <- lapply(type, function(varType){
    inflam <- paste0(inflam, "_", varType)

    modUnadjusted <- lmer(as.formula(paste0("dep ~ age + I(age^2)  + Batch + Assessment_centre_baseline + ", inflam ,"+ ", inflam ,"*age + ", inflam ,"*I(age^2) + (age |f.eid)")), REML=F , data = data)

    modSex <- lmer(as.formula(paste0("dep ~ age + I(age^2)  + Batch + Assessment_centre_baseline  + Sex + ", inflam ,"+ ", inflam ,"*age + ", inflam ,"*I(age^2)+  (age |f.eid)")), REML=F , data = data)

    modFull <- lmer(as.formula(paste0("dep ~ age + I(age^2)  + Batch + Assessment_centre_baseline  + Sex + smoking_status + Townsend + BMI + ", inflam ,"+ ", inflam ,"*age + ", inflam ,"*I(age^2) + (age|f.eid)")), REML=F , data = data)

    modFullBMICat <- lmer(as.formula(paste0("dep ~ age + I(age^2) + Batch + Assessment_centre_baseline   + Sex + smoking_status+ Townsend + BMI_cat + ", inflam ,"+ ", inflam ,"*age + ", inflam ,"*I(age^2) + (age|f.eid)")), REML=F , data = data)

    modFullBMIraw <- lmer(as.formula(paste0("dep ~ age + I(age^2) + Batch + Assessment_centre_baseline   + Sex + smoking_status+ Townsend + BMI_raw + ", inflam ,"+ ", inflam ,"*age + ", inflam ,"*I(age^2) + (age|f.eid)")), REML=F , data = data)

    modelList <- list(modUnadjusted, modSex, modFull,  modFullBMICat,  modFullBMIraw )

    return(modelList)
  })

  names(modelListTypes) <- type

  return(modelListTypes)
}

# Split by sex:
quadraticInflamModelSex <- function(inflam, data, type){

  # Categorical and continuous forms of the marker:
  modelListTypes <- lapply(type, function(varType){
    inflam <- paste0(inflam, "_", varType)

    modUnadjusted <- lmer(as.formula(paste0("dep ~ age + I(age^2) + Batch + Assessment_centre_baseline + ", inflam ,"+ ", inflam ,"*age + ", inflam ,"*I(age^2) + (age |f.eid)")), REML=F , data = data)

    modFull <- lmer(as.formula(paste0("dep ~ age + I(age^2) + Batch + Assessment_centre_baseline + smoking_status  + Townsend + BMI + ", inflam ,"+ ", inflam ,"*age + ", inflam ,"*I(age^2) + (age|f.eid)")), REML=F , data = data)

    modelList <- list(modUnadjusted, modFull)

    return(modelList)
  })

  names(modelListTypes) <- type

  return(modelListTypes)
}

