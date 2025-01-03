# Get data for UKB proteins
# Subset to IL-6 proteins and save in wide format

# ------------------------------------
# ---- Meta data:
# Recap what each column means
# https://biobank.ndph.ox.ac.uk/showcase/label.cgi?id=1839
# f.30900.0.0 = Number of proteins measured
# f.30901.0.0 = Plate used for sample run
# f.30902.0.0 = Well used for sample run
# f.30903.0.0 = UKB-PPP Consortium selected participant (6264 items have value 1 (Yes))

meta <- fread("/Volumes/GenScotDepression/data/ukb/phenotypes/fields/2023-12-olink-ukb676482/ukb676482.tsv")

# Check if any participants have data at instances 1,2 or 3
# These are likely the individuals who participated in the COVID-19 repeat-imaging study at multiple visits
# Create a new variable that codes this
meta %>%
  filter( (!is.na(f.30900.1.0))| (!is.na(f.30900.2.0)) | (!is.na(f.30900.3.0)) ) %>%
  nrow()
# 1254
# " 1,268 individuals who participated in the COVID-19 repeat-imaging study at multiple visits" - https://www.nature.com/articles/s41586-023-06592-6#Sec2

meta <- meta %>%
  mutate(repeat_olink = as.factor(ifelse((!is.na(f.30900.1.0))| (!is.na(f.30900.2.0)) | (!is.na(f.30900.3.0)),
                               1, 0)))

meta <- meta %>%
  dplyr::select(f.eid, starts_with(c("f.30900.0", "f.30901.0", "f.30902.0",  "f.30903.0", "repeat_olink"))) %>%
  filter(!if_all(ends_with("f.30900.0.0"), is.na))

# ---- Olink protein measures:
olink <- fread("/Volumes/GenScotDepression/data/ukb/phenotypes/fields/2023-12-olink-ukb676482/olink_data.txt")

# ---- Olink protein ids:
olink_linker <- fread("/Volumes/GenScotDepression/data/ukb/phenotypes/fields/2023-12-olink-ukb676482/olink_protein_ids.tsv")


# ---- Also some meta data in "docs_ukb"
files <- list.files("/Volumes/GenScotDepression/data/ukb/phenotypes/fields/2023-04-olink-ukb672610/docs_ukb", pattern = '\\.dat$')
docs_ukb <- lapply(paste0("/Volumes/GenScotDepression/data/ukb/phenotypes/fields/2023-04-olink-ukb672610/docs_ukb/", files),fread)
names(docs_ukb) <- files
lapply(docs_ukb, head)

# ------------------------------------
# Add a Assay ID specific column and a Protein_desc column
olink_linker <- olink_linker %>%
  separate(meaning, into = c("AssayID", "Protein_desc"), sep = ";", extra = "drop")

# Get data for IL6
IL6_id <- olink_linker[str_detect(olink_linker$AssayID, "\\bIL6\\b"),]$coding # 1418

olinkIL6 <- olink %>%
  filter(protein_id == IL6_id ) %>%
  mutate(AssayID = "IL6")

# How many people:
nrow(olinkIL6) # 53514

# Missing data:
olinkIL6 %>%
  filter(is.na(result)) %>%
  nrow()
# 0

# ------------------------------------
# Add meta data as extra columns
olinkIL6 <- merge(olinkIL6, meta, by.x = "eid", by.y = "f.eid")

nrow(olinkIL6) # 53439

# ------------------------------------
# Check limit of detection for IL-6
lod <- docs_ukb$olink_limit_of_detection.dat %>%
  filter(Assay == "IL6") %>%
  filter(Instance == 0) %>%
  dplyr::select(PlateID, LOD)

merge(olinkIL6, lod , by.x = "f.30901.0.0", by.y = "PlateID") %>%
  filter(result < LOD)
# XXXXXX ppl with protein measure less than LOD so we will exclude these

rmID <- merge(olinkIL6, lod , by.x = "f.30901.0.0", by.y = "PlateID") %>%
  filter(result < LOD) %>%
  pull(eid)

olinkIL6 <- olinkIL6 %>%
  filter(!eid %in% rmID)

nrow(olinkIL6) 

# ------------------------------------
# Add the batch column
olinkIL6 <- merge(olinkIL6, docs_ukb$olink_batch_number.dat, by.x = "f.30901.0.0", by.y = "PlateID")

nrow(olinkIL6) 

# ------------------------------------
# Check warnings: (all passed, these were filtered prior to data access)
sum(!docs_ukb$olink_assay_warning.dat$Assay_Warning == "PASS")

# ------------------------------------
# Rename eid to f.eid for consistency
olinkIL6 <- olinkIL6 %>%
  rename(f.eid = eid) %>%
  relocate(f.eid)

# Number of measures:
nrow(olinkIL6) 
# Number of people:
length(unique(olinkIL6$f.eid)) 

# ------------------------------------
# Save data:
write.csv(olinkIL6, "/Volumes/GenScotDepression/users/amelia/UKB/olink_data_IL6.csv", row.names = F)



