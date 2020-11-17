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

  data_out <- suppressWarnings(
    mutate(
      data,
      ascvd_risk_pcr = predict_10yr_ascvd_risk(
        age_years = age,
        race = race_ethnicity,
        sex = sex,
        smoke_current = smk_current,
        chol_total_mgdl = chol_total_mgdl,
        chol_hdl_mgdl = chol_hdl_mgdl,
        bp_sys_mmhg = bp_sys_mmhg,
        bp_meds = meds_bp,
        diabetes = diabetes,
        equation_version = "Goff_2013",
        override_boundary_errors = TRUE,
        race_levels = list(
          white = c(
            "Non-Hispanic White",
            "Hispanic",
            "Non-Hispanic Asian",
            "Mexican American",
            "Other Hispanic",
            "Other Race - Including Multi-Racial"
          ),
          black = "Non-Hispanic Black"
        ),
        sex_levels = list(
          male = 'Male',
          female = 'Female'
        )
      ),
      ascvd_risk_pcr_yadlowsky = predict_10yr_ascvd_risk(
        age_years = age,
        race = race_ethnicity,
        sex = sex,
        smoke_current = smk_current,
        chol_total_mgdl = chol_total_mgdl,
        chol_hdl_mgdl = chol_hdl_mgdl,
        bp_sys_mmhg = bp_sys_mmhg,
        bp_meds = meds_bp,
        diabetes = diabetes,
        equation_version = 'Yadlowsky_2018',
        override_boundary_errors = TRUE,
        race_levels = list(
          white = c(
            "Non-Hispanic White",
            "Hispanic",
            "Non-Hispanic Asian",
            "Mexican American",
            "Other Hispanic",
            "Other Race - Including Multi-Racial"
          ),
          black = "Non-Hispanic Black"
        ),
        sex_levels = list(
          male = 'Male',
          female = 'Female'
        )
      )
    )
  )


  if(include_variable_labels){

    var_guide <- tibble(
      term = c('ascvd_risk_pcr', 'ascvd_risk_pcr_yadlowsky'),
      nhanes = NA_character_,
      descr = c("10-year predicted risk for ASCVD, pooled cohort equations",
                "10-year predicted risk for ASCVD, updated pooled cohort equations")
    )

    return(add_labels(data_out, var_guide, ref_nhanes = FALSE))

  }

  data_out

}

