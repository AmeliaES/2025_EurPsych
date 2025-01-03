# Function that takes a character string of inflammatory variable and dataset
# runs quartic model for unadjusted, sex adjusted and fully adjusted
# returns a list of these 3 models
# also takes type, which is the suffix of the colum name, eg. "log" or "quartic"

# ------------------------------------
quarticInflamModel <- function(inflam, data, type){

  # Categorical and continuous forms of the marker:
  modelListTypes <- lapply(type, function(varType){
    inflam <- paste0(inflam, "_", varType)

    modUnadjusted <- lmer(as.formula(paste0("dep ~ age + I(age^2) + I(age^3) + I(age^4) + ", inflam ,"+ ", inflam ,"*age + ", inflam ,"*I(age^2) + ", inflam ,"*I(age^3) + ", inflam ,"*I(age^4) + (age + I(age^2)|Subject)")), REML=F , data = data)

    modSex <- lmer(as.formula(paste0("dep ~ age + I(age^2) + I(age^3) + I(age^4) + Sex + ", inflam ,"+ ", inflam ,"*age + ", inflam ,"*I(age^2) + ", inflam ,"*I(age^3) + ", inflam ,"*I(age^4) + (age + I(age^2)|Subject)")), REML=F , data = data)

    modFull <- lmer(as.formula(paste0("dep ~ age + I(age^2)  + I(age^3) + I(age^4) + Sex + Maternal.education.at.birth + BMI_age9_log + ", inflam ,"+ ", inflam ,"*age + ", inflam ,"*I(age^2) + ", inflam ,"*I(age^3) + ", inflam ,"*I(age^4) + (age + I(age^2)|Subject)")), REML=F , data = data)

    modFullTownsend <- lmer(as.formula(paste0("dep ~ age + I(age^2)  + I(age^3) + I(age^4) + Sex + kqTownsendq5 + BMI_age9_log + ", inflam ,"+ ", inflam ,"*age + ", inflam ,"*I(age^2) + ", inflam ,"*I(age^3) + ", inflam ,"*I(age^4) + (age + I(age^2)|Subject)")), REML=F , data = data)

    modelList <- list(modUnadjusted, modSex, modFull, modFullTownsend)

    return(modelList)
    })

  names(modelListTypes) <- type

  return(modelListTypes)
}

# Split by sex:
quarticInflamModelSex <- function(inflam, data, type){

  # Categorical and continuous forms of the marker:
  modelListTypes <- lapply(type, function(varType){
    inflam <- paste0(inflam, "_", varType)

    modUnadjusted <- lmer(as.formula(paste0("dep ~ age + I(age^2) + I(age^3) + I(age^4) + ", inflam ,"+ ", inflam ,"*age + ", inflam ,"*I(age^2) + ", inflam ,"*I(age^3) + ", inflam ,"*I(age^4) + (age + I(age^2)|Subject)")), REML=F , data = data)

    modFull <- lmer(as.formula(paste0("dep ~ age + I(age^2)  + I(age^3) + I(age^4) + Maternal.education.at.birth + BMI_age9_log + ", inflam ,"+ ", inflam ,"*age + ", inflam ,"*I(age^2) + ", inflam ,"*I(age^3) + ", inflam ,"*I(age^4) + (age + I(age^2)|Subject)")), REML=F , data = data)

    modelList <- list(modUnadjusted, modFull)

    return(modelList)
  })

  names(modelListTypes) <- type

  return(modelListTypes)
}

