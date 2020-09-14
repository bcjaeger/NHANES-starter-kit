
# Hello! Thank you for using this NHANES project for data cleaning.
# This script will show you how to create the final output of the project,
# a dataset with derived columns such as estimated glomerular filtration
# rate, diabetes status, 10-year predicted risk according to the Pooled
# Cohort risk equations, and others. All columns are named systematically,
# following an intuitive structure, and are labelled with descriptions
# as well as tags to the original variables in the NHANES data.

# Step 0 ----
# In case you need to update or install new packages, you should have pacman
# installed. If you do not have the pacman package, then running the code
# here should install the package for you.

if(!require(pacman)) install.packages('pacman')

# Step 1 ----
# Download and/or load the packages used by this project.

source("packages.R")

# Step 2 ----
# Use the drake package to run the project from start to finish.

drake::r_make()

# Step 3 ----
# Load and inspect the data created by running the project.
