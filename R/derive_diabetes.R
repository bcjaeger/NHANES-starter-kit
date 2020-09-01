##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##'
##' @param data
##' @param fasted_hrs_lower
##' @param fasted_hrs_upper
##' @param gluc_cutpoint_fasted
##' @param gluc_cutpoint_fed
##' @param hba1c_cutpoint
##' @param include_variable_labels

derive_diabetes <- function(data,
                            fasted_hrs_lower = 8,
                            fasted_hrs_upper = 24,
                            gluc_cutpoint_fasted = 126,
                            gluc_cutpoint_fed = 200,
                            hba1c_cutpoint = 6.5,
                            include_variable_labels = TRUE) {

  .variables <-
    c(
      'clean_labs_fasting.R' = 'fasted_hours',
      'clean_labs_biopro.R'  = 'gluc_mgdl',
      'clean_labs_ghb.R'     = 'hba1c_perc',
      'clean_qx_diabetes.R'  = 'diab_ever',
      'clean_qx_diabetes.R'  = 'meds_insulin' ,
      'clean_qx_diabetes.R'  = 'meds_glucose'
    )

  check_required_variables(data, .variables)

  data_out <- data %>%
    mutate(
      fasted = if_else(
        condition =
          fasted_hours >= fasted_hrs_lower &
          fasted_hours <= fasted_hrs_upper,
        true = 'yes',
        false = 'no'
      ),
      # un-comment this definition to...
      # assume diabetes is 'no' unless otherwise indicated.
      # note: this is the default b/c it is commonly done,
      # but I do not agree with it.
      diabetes = 'no',
      diabetes = replace(
        diabetes, list =
          (gluc_mgdl >= gluc_cutpoint_fasted & fasted == 'yes') | # OR
          (gluc_mgdl >= gluc_cutpoint_fed & fasted == 'no')     | # OR
          (hba1c_perc >= hba1c_cutpoint)                        | # OR
          (diab_ever == 'yes' & meds_insulin == 'yes')          | # OR
          (diab_ever == 'yes' & meds_glucose == 'yes'),
        values = 'yes'
      )
      # un-comment this definition to ...
      # refrain from making  any assumptions about diabetes status
      # in cases with missing data.
      # diabetes = if_else(
      #   condition =
      #     (gluc_mgdl >= gluc_cutpoint_fasted & fasted == 'yes') | # OR
      #     (gluc_mgdl >= gluc_cutpoint_fed & fasted == 'no')     | # OR
      #     (hba1c_perc >= hba1c_cutpoint)                        | # OR
      #     (diab_ever == 'yes' & meds_insulin == 'yes')          | # OR
      #     (diab_ever == 'yes' & meds_glucose == 'yes'),
      #   true = 'yes',
      #   false = 'no'
      # )
    )

  if(include_variable_labels){

    diab_label <- glue("Diabetes was defined by fasting serum glucose ",
                       "\u2265{gluc_cutpoint_fasted} mg/dL, ",
                       "non-fasting glucose \u2265{gluc_cutpoint_fed} mg/dL, ",
                       "HbA1c \u2265{hba1c_cutpoint}%, or self-reported use ",
                       "of insulin or oral glucose lowering medication")

    fasted_label <- glue("Fasted for \u2265{fasted_hrs_lower} ",
                         "and \u2264{fasted_hrs_upper} hours")

    var_guide <- tibble(
      term = c('fasted', 'diabetes'),
      nhanes = NA_character_,
      descr = c(fasted_label, diab_label)
    )

    return(add_labels(data_out, var_guide, ref_nhanes = FALSE))

  }

  data_out

}
