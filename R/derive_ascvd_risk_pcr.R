##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param data
derive_ascvd_risk_pcr <- function(data,
                                  set_miss_to_no = NULL,
                                  include_variable_labels = TRUE) {

  .variables <- c(
    'clean_demo.R' = 'age',
    'clean_demo.R' = 'sex',
    'clean_demo.R' = 'race_ethnicity',
    'clean_exam_bp.R' = 'bp_sys_mmhg',
    'clean_labs_tchol.R' = 'chol_total_mgdl',
    'clean_labs_hdl.R' = 'chol_hdl_mgdl',
    'clean_qx_high_bp.R' = 'meds_bp',
    'clean_qx_smoking_status.R' = 'smk_current',
    'derive_diabetes.R' = 'diabetes'
  )

  check_required_variables(data, .variables)

  incorrect_vars <- setdiff(set_miss_to_no,
                            c('meds_bp', 'smk_current', 'diabetes'))

  if(length(incorrect_vars) > 0){
    vars_to_list <- glue_collapse(incorrect_vars,
                                  sep = ', ',
                                  last = ', and ')
    msg <- glue("incorrect variable selected in `set_miss_to_no:` ",
                "{vars_to_list}. Correct variables are meds_bp, ",
                "smk_current, and diabetes")
    stop(msg, call. = FALSE)
  }

  if(!is.null(set_miss_to_no))
    data %<>% mutate(
      across(
        all_of(set_miss_to_no),
        ~replace(.x, is.na(.x), 'no')
      )
    )

  data_out <- data %>%
    mutate(
      # baseline survival probability varies by race/sex
      base_surv = case_when(
        sex == 'Female' & race_ethnicity == 'Non-Hispanic Black' ~ 0.9533,
        sex == 'Female' & race_ethnicity != 'Non-Hispanic Black' ~ 0.9665,
        sex == 'Male'   & race_ethnicity == 'Non-Hispanic Black' ~ 0.8954,
        sex == 'Male'   & race_ethnicity != 'Non-Hispanic Black' ~ 0.9144,
        TRUE ~ NA_real_
      ),
      # mean log-hazard varies by race/sex
      mean_coef = case_when(
        sex == 'Female' & race_ethnicity == 'Non-Hispanic Black' ~ 86.61,
        sex == 'Female' & race_ethnicity != 'Non-Hispanic Black' ~ -29.18,
        sex == 'Male'   & race_ethnicity == 'Non-Hispanic Black' ~ 19.54,
        sex == 'Male'   & race_ethnicity != 'Non-Hispanic Black' ~ 61.18,
        TRUE ~ NA_real_
      ),
      # individual sums taken from Table A of
      # 2013 ACC/AHA Guideline on the Assessment of
      # Cardiovascular Risk
      ind_sum = case_when(
        sex == 'Female' & race_ethnicity == 'Non-Hispanic Black' ~
          17.114 * log(age) +
          0.940 * log(chol_total_mgdl) +
          (-18.92) * log(chol_hdl_mgdl) +
          4.475 * log(age) * log(chol_hdl_mgdl) +
          29.291 * log(bp_sys_mmhg) * (meds_bp == 'yes') +
          (-6.432) * log(age) * log(bp_sys_mmhg) * (meds_bp == 'yes') +
          27.820 * log(bp_sys_mmhg) * (meds_bp == 'no') +
          (-6.087) * log(age) * log(bp_sys_mmhg) * (meds_bp == 'no') +
          0.691 * (smk_current == 'yes') +
          0.874 * (diabetes == 'yes'),
        sex == 'Female' & race_ethnicity != 'Non-Hispanic Black' ~
          (-29.799) * log(age) +
          4.884 * log(age)^2 +
          13.540 * log(chol_total_mgdl) +
          (-3.114) * log(age) * log(chol_total_mgdl) +
          (-13.578) * log(chol_hdl_mgdl) +
          3.149 * log(age) * log(chol_hdl_mgdl) +
          2.019 * log(bp_sys_mmhg) * (meds_bp == 'yes') +
          1.957 * log(bp_sys_mmhg) * (meds_bp == 'no') +
          7.574 * (smk_current == 'yes') +
          (-1.665) * log(age) * (smk_current == 'yes') +
          0.661 * (diabetes == 'yes'),
        sex == 'Male' & race_ethnicity == 'Non-Hispanic Black' ~
          2.469 * log(age) +
          0.302 * log(chol_total_mgdl) +
          (-0.307) * log(chol_hdl_mgdl) +
          1.916 * log(bp_sys_mmhg) * (meds_bp == 'yes') +
          1.809 * log(bp_sys_mmhg) * (meds_bp == 'no') +
          0.549 * (smk_current == 'yes') +
          0.645 * (diabetes == 'yes'),
        sex == 'Male' & race_ethnicity != 'Non-Hispanic Black' ~
          12.344 * log(age) +
          11.853 * log(chol_total_mgdl) +
          (-2.664) * log(age) * log(chol_total_mgdl) +
          (-7.990) * log(chol_hdl_mgdl) +
          1.769 * log(age) * log(chol_hdl_mgdl) +
          1.797 * log(bp_sys_mmhg) * (meds_bp == 'yes') +
          1.764 * log(bp_sys_mmhg) * (meds_bp == 'no') +
          7.837 * (smk_current == 'yes') +
          (-1.795) * log(age) * (smk_current == 'yes') +
          0.658 * (diabetes == 'yes')
      ),
      ascvd_risk_pcr = 1 - base_surv^exp(ind_sum - mean_coef)
    ) %>%
    select(-base_surv, -mean_coef, -ind_sum)

  if(include_variable_labels){

    var_guide <- tibble(
      term = c('ascvd_risk_pcr'),
      nhanes = NA_character_,
      descr = c("10-year predicted risk for ASCVD, pooled cohort equations")
    )

    return(add_labels(data_out, var_guide, ref_nhanes = FALSE))

  }

  data_out

}

