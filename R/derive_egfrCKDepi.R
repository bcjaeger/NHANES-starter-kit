##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param data
derive_egfrCKDepi <- function(data,
                              egfr_cutpoint = 60,
                              include_variable_labels = TRUE){

  .variables <- c(
    'clean_demo.R' = 'age',
    'clean_demo.R' = 'sex',
    'clean_demo.R' = 'race_ethnicity',
    'clean_labs_biopro.R'  = 'creat_mgdl'
  )

  check_required_variables(data, .variables)

  data_out <- data %>%
    mutate(
      egfr_ckdepi = CKDEpi.creat(
        age = age,
        creatinine = creat_mgdl,
        sex = as.numeric(sex == 'Male'),
        ethnicity = as.numeric(race_ethnicity == 'Non-Hispanic Black')
      ),
      egfr_low = if_else(egfr_ckdepi < egfr_cutpoint, "yes", "no")
    )

  if(include_variable_labels){

    egfr_ckdepi_label <- "estimated glomerular filtration rate"
    egfr_low_label <- glue("estimated glomerular filtration rate ",
                           "<{egfr_cutpoint} ml/min/1.73m\u00b2")

    var_guide <- tibble(
      term = c('egfr_ckdepi', 'egfr_low'),
      nhanes = NA_character_,
      descr = c(egfr_ckdepi_label, egfr_low_label)
    )

    return(add_labels(data_out, var_guide, ref_nhanes = FALSE))

  }

  data_out

}
