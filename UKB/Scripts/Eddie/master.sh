# Extract UKB data from duckDB and save to DataStore as wide format csv

# -------------
# Copy datastore data to scratch space
qlogin -q staging 

cp /exports/igmm/datastore/GenScotDepression/data/ukb/phenotypes/fields/2023-07-wellbeing-ukb673625/health-wellbeing.tsv /exports/eddie/scratch/$USER
cp /exports/igmm/datastore/GenScotDepression/data/ukb/phenotypes/fields/2023-10-mhq2-ukb675698/MentalWellBeing.tsv /exports/eddie/scratch/$USER

exit

# -------------
# Query duckDB
qlogin -l h_vmem=32G 
. /etc/profile.d/modules.sh
module load R/4.3.0 
R

# Run "extract.R" followed by "clean.R"
source("UKB/Scripts/Eddie/extract.R")
source("UKB/Scripts/Eddie/clean.R")


exit

######## # Some set up to get duckDB to work:
########  mkdir /exports/eddie3_homes_local/s1211670/.R
######## # paste the following line into /exports/eddie3_homes_local/s1211670/.R/Makevars
######## CXX17 = g++ -std=gnu++17 -fPIC

# -------------
# Copy data back to datastore
qlogin -q staging 
cp -r /exports/eddie/scratch/$USER/UKB /exports/igmm/datastore/GenScotDepression/users/amelia/
cp /exports/igmm/eddie/GenScotDepression/amelia/ALSPAC_trajectories/UKB/Scripts/Eddie/* /exports/igmm/datastore/GenScotDepression/users/amelia/UKB
exit

