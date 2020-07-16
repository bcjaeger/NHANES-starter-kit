##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param exams
clean_qx_healthcare_utilization <- function(
  exams = c(1999, 2001, 2003, 2005, 2007, 2009, 2011, 2013, 2015, 2017),
  include_variable_labels = TRUE
) {

  fnames <- make_exam_files(exams, data_label = 'HUQ')

  var_guide <- tibble(
    term = c(
      'hc_usual_facility',
      'hc_visit_in_past_year'
    ),
    nhanes = c(
      'HUQ030',
      'HUQ050 and HUQ051'
    ),
    descr = c(
      'Do you have a usual facility to receive healthcare',
      'Have you had a healthcare visit in past year'
    )
  )

  data_in <- fnames %>%
    map_dfr(.f = read_xpt, .id = 'exam') %>%
    add_missing_cols(.names = c('HUQ030', 'HUQ050', 'HUQ051'))

  # Variable derivation ----

  # Creating `hc_usual_facility` requires recoding `HUQ030`, which is
  # measured consistently throughout all NHANES visits through 2017-2018.

  data_in %<>% mutate(
    hc_usual_facility = recode(
      HUQ030,
      "1"	= "yes",
      "2"	= "no",
      "3"	= "yes",
      "7"	= NA_character_,
      "9"	= NA_character_
    ),
    hc_visit_in_past_year = coalesce(HUQ050, HUQ051)
  )

  # recode the hc_visit_in_past_year variable to 'yes' or 'no'
  # - If number of visits in the past year is 0, then "no"
  # - If number of visit value is anything in the range of 1 to 8, "yes"
  # - Otherwise set as missing.
  # (why?) the coded values of HUQ050/HUQ051 represent the following:
  # 0   =  None
  # 1   =  1
  # 2   =  2 to 3
  # 3   =  4 to 5
  # 4   =  6 to 7
  # 5   =  8 to 9
  # 6   =  10 to 12
  # 7   =  13 to 15
  # 8   =  16 or more
  # 77  =  Refused
  # 99  =  Don't know

  data_out <- data_in %>%
    transmute(
      exam,
      seqn = SEQN,
      hc_usual_facility,
      hc_visit_in_past_year = case_when(
        hc_visit_in_past_year == 0 ~ "no",
        hc_visit_in_past_year %in% c(1:8) ~ "yes",
        TRUE ~ NA_character_
      )
    )

  if(include_variable_labels){
    add_labels(data_out, var_guide)
  } else {
    data_out
  }

}
