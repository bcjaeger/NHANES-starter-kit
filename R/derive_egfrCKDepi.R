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
      'clean_labs_biopro.R'  = 'creat_mgdl'
  )

  check_required_variables(data, .variables)

  data_out <- data %>%
    mutate(
      # abbreviate the name so equations fit on one line
      cre = creat_mgdl,
      egfr_ckdepi = case_when(
        sex=='Female' & cre<=0.7 ~ 166*((cre/0.7)^(-0.329))*(0.993^(age)),
        sex=='Female' & cre >0.7 ~ 166*((cre/0.7)^(-1.209))*(0.993^(age)),
        sex=='Male'   & cre<=0.9 ~ 163*((cre/0.9)^(-0.411))*(0.993^(age)),
        sex=='Male'   & cre >0.9 ~ 163*((cre/0.9)^(-1.209))*(0.993^(age))
      ),
      egfr_low = if_else(egfr_ckdepi < egfr_cutpoint, "yes", "no"
      )
    ) %>%
    # drop the cre column since it was just made for convenience
    select(-cre)

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
