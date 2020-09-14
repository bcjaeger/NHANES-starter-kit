
## library() calls go here
library(conflicted)  # manage the namespace
library(dotenv)
library(drake)
# data management
library(haven)       # reading sas data
library(janitor)     # cleaning names
library(magrittr)    # %>% and %<>%
library(labelled)    # add variable labels
library(nephro)      # compute egfr ckd-epi
# data analysis
library(tidyverse)
library(tidyselect)
# reporting
library(tblStrings)
library(officer)
library(glue)
library(flextable)
library(devEMF)
library(magick)
library(paletteer)

conflicted::conflict_prefer("roc",       "pROC")
conflicted::conflict_prefer("filter",    "dplyr")
conflicted::conflict_prefer("slice",     "dplyr")
conflicted::conflict_prefer('summarise', 'dplyr')
conflicted::conflict_prefer("gather",    "tidyr")
conflicted::conflict_prefer("set_names", "purrr")
