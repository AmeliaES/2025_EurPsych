# Associations between IL-6 and trajectories of depressive symptoms across the life course: Evidence from ALSPAC and UK Biobank cohorts

**Include citation here when article published**

## Scripts to run analysis

* **`ALPSAC/Scripts/master.R`** - This script runs the analysis in ALSPAC. Calling other scripts and functions in `ALPSAC/Scripts`.

* **`UKB/Scripts/master.R`** - This script runs the analysis in UKB. Calling other scripts and functions in `UKB/Scripts`, including data preparation of data stored in a duckDB data processed using the University of Edinburgh's high performance computer (see `UKB/Scripts/Eddie/README.md`). The file `UKB/Supplementary/fields_to_extract.csv` containing all the UK Biobank variables used in this analysis will be helpful if having to adapt this pipeline to a different environment.

* **`Scripts_both_cohorts/betas_to_Zscores.R`** - Create main results tables.
