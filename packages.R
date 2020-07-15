## library() calls go here
library(conflicted)
library(dotenv)
library(drake)
# data management
library(haven)
library(janitor)
library(magrittr)
library(labelled)
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
