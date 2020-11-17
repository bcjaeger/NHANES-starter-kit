##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param exams
clean_mort <- function(exams, include_variable_labels = TRUE) {


  if(any(exams > 2013)) {
    warning('Mortality data only available through 2013-2014 exam.')
    exams <- exams[exams <= 2013]
  }

  exams_string <- glue("{exams}_{exams+1}")

  file_locations <- file.path(
    'ftp://ftp.cdc.gov',
    'pub',
    'Health_Statistics',
    'NCHS',
    'datalinkage',
    'linked_mortality',
    glue('NHANES_{exams_string}_MORT_2015_PUBLIC.dat')
  ) %>%
    set_names(exams_string)

  data_in <- map_dfr(
    .x = file_locations,
    .id = 'exam',
    .f = ~ read_fwf(
      file = .x,
      col_types = "ciiiiiiiddii",
      na = '.',
      fwf_cols(publicid = c(1,14),
               eligstat = c(15,15),
               mortstat = c(16,16),
               ucod_leading = c(17,19),
               diabetes = c(20,20),
               hyperten = c(21,21),
               dodqtr = c(22,22),
               dodyear = c(23,26),
               wgt_new = c(27,34),
               sa_wgt_new = c(35,42),
               permth_int = c(43,45),
               permth_exm = c(46,48)
      )
    )
  )

  var_guide <- tibble(
    term = c(
      'mortality_eligible_status',
      "mortality_time_interview",
      "mortality_time_exam",
      'mortality_status',
      'cod_leading',
      'cod_multiple_diabetes',
      'cod_multiple_hyperten'
    ),
    nhanes = c(
      'eligstat',
      'permth_int',
      'permth_exm',
      'mortstat',
      'ucod_leading',
      'diabetes',
      'hyperten'
    ),
    descr = c(
      'Eligibility status for mortality follow-up',
      'Number of person months of follow-up from interview date',
      'Number of person months of follow-up from mobile exam date',
      'Final mortality status',
      'Underlying leading cause of death',
      'Diabetes flag from multiple cause of death',
      'Hypertension flag from multiple cause of death'
    )
  )

  data_out <- data_in %>%
    transmute(
      seqn = as.numeric(substr(publicid, 1, 5)),
      exam = str_replace(exam, '_', '-'),
      mortality_eligible_status = recode(
        eligstat,
        "1" = "eligible",
        "2" = "under_18_not available_for_public_release",
        "3" = "ineligible"
      ),
      mortality_status = recode(
        mortstat,
        "0" = "alive",
        "1" = "deceased"
      ),
      mortality_status = if_else(
        is.na(mortstat),
        true = "ineligible_or_under_18",
        false = mortality_status
      ),
      mortality_time_interview = permth_int,
      mortality_time_exam = permth_exm,
      cod_leading = recode(
        ucod_leading,
        "1" = "cardiovascular",
        "2" = "malignant_neoplasms",
        "3" = "chronic_lower_respiratory",
        "4" = "accidents",
        "5" = "cerebrovascular",
        "6" = "alzheimers",
        "7" = "diabetes",
        "8" = "flu_or_pneumonia",
        "9" = "nephro",
        "10" = "other"
      ),
      across(
        .cols = c(diabetes, hyperten),
        .fns = list(cod_multiple = ~recode(.x, '0' = 'No', '1' = 'Yes')),
        .names = "{.fn}_{.col}"
      )
    )

  if(include_variable_labels){
    add_labels(data_out, var_guide)
  } else {
    data_out
  }

}
