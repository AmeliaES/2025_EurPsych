# Associations between IL-6 and trajectories of depressive symptoms across the life course: Evidence from ALSPAC and UK Biobank cohorts

Edmondson-Stait, A. J., Davyson, E., Shen, X., Adams, M. J., Khandaker, G. M., Miron, V. E., McIntosh, A. M., Lawrie, S. M., Kwong, A. S. F., & Whalley, H. C. (2025). Associations between IL-6 and trajectories of depressive symptoms across the life course: Evidence from ALSPAC and UK Biobank cohorts. European psychiatry : the journal of the Association of European Psychiatrists, 1–54. Advance online publication. https://doi.org/10.1192/j.eurpsy.2025.7

## Scripts to run analysis

* **`ALPSAC/Scripts/master.R`** - This script runs the analysis in ALSPAC. Calling other scripts and functions in `ALPSAC/Scripts`.

* **`UKB/Scripts/master.R`** - This script runs the analysis in UKB. Calling other scripts and functions in `UKB/Scripts`, including data preparation of data stored in a duckDB data processed using the University of Edinburgh's high performance computer (see `UKB/Scripts/Eddie/README.md`). The file `UKB/Supplementary/fields_to_extract.csv` containing all the UK Biobank variables used in this analysis will be helpful if having to adapt this pipeline to a different environment.

* **`Scripts_both_cohorts/betas_to_Zscores.R`** - Create main results tables.
