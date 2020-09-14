
## library() calls go here

pacman::p_load(

  # workspace managers ----
  conflicted,  # manage the namespace
  dotenv,      # manage environment variables
  drake,       # make the plan!

  # data management ----
  haven,       # reading sas data
  janitor,     # cleaning names
  magrittr,    # %>% and %<>%
  labelled,    # add variable labels
  nephro,      # compute egfr ckd-epi

  # data analysis ----
  tidyverse,
  tidyselect,

  # reporting
  glue
)

# manage namespace conflicts

conflicted::conflict_prefer("filter",    "dplyr")
conflicted::conflict_prefer('summarise', 'dplyr')
conflicted::conflict_prefer("gather",    "tidyr")
conflicted::conflict_prefer("set_names", "purrr")
