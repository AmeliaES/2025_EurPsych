# Readme file for longitudinal depression data in UKB

## How data was extracted and derived
Please see `master.sh`.
Briefly, extract.R was run to extract UKB data fields from duckDB and other tsv files copied from datastore.
Then, clean.R was run to derive depression variables. This is for PHQ2 and PHQ4 at 8 and 6 time points respectively. Age was also calculated at each of these time points, apart from assessment center ages which was already derived (field = 21003).

## Where data is located
On DataStore:
/exports/igmm/datastore/GenScotDepression/users/amelia/UKB/
*Also accessed via: smb://cmvm.datastore.ed.ac.uk/igmm/GenScotDepression*

File created in extract.R with UKB field IDs as columnms:
* dataWide.csv

File created in clean.R with UKB field IDs as well as depression derived variables:
* dataWideDerived.csv

## What the column names refer to in dataWideDerived.csv:
* Column names starting with "f." are followed by UKB field IDs
    - These include fields for calculating PHQ9
* PHQ2 variables start with "PHQ2_dep", followed by the time point the score refers to.
* PHQ4 variables start with "PHQ4_dep", followed by the time point the score refers to.
* Columns ending in "isNA" count how many questions the participant DID NOT ANSWER for each time point, for PHQ2 and PHQ4 separately.
* Columns starting with "age_" are followed by the time point. For 0 to 3 this is the age given by UK Biobank, for Online Assessments this was derived by using the date of assessment and year of birth variables.
* Covariates have been renamed for ease, these include: Sex, Townsend Deprivation Index, Qualifications, Age completed full time education and BMI. Not Qualifications has multiple instances and arrays.
* NCompleted is the number of PHQ2 questions both completed at each time point. Excluding the two imaging time points as these were only a subset of the initial 500,000 people invited.


