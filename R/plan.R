

the_plan <- drake_plan(

  exams = seq(1999, 2017, by = 2),

  # Demographics ----
  demo = clean_demo(exams),

  # Exams ----
  exam_bp = clean_exam_bp(exams),

  # Labs ----
  labs_acr     = clean_labs_acr(exams),
  labs_biopro  = clean_labs_biopro(exams),
  labs_fasting = clean_labs_fasting(exams),
  labs_ghb     = clean_labs_ghb(exams),
  labs_hdl     = clean_labs_hdl(exams),
  labs_trig    = clean_labs_trig(exams),
  labs_tchol   = clean_labs_tchol(exams),

  # Questionnaires ----
  qx_medical_status         = clean_qx_medical_status(exams),
  qx_smoking_status         = clean_qx_smoking_status(exams),
  qx_health_insurance       = clean_qx_health_insurance(exams),
  qx_diabetes               = clean_qx_diabetes(exams),
  qx_high_blood_pressure    = clean_qx_high_bp(exams),
  qx_healthcare_utilization = clean_qx_healthcare_utilization(exams),

  data_pooled = reduce(
    .x = list(
      demo,
      exam_bp,
      labs_acr,
      labs_biopro,
      labs_fasting,
      labs_ghb,
      labs_hdl,
      labs_trig,
      labs_tchol,
      qx_medical_status,
      qx_smoking_status,
      qx_health_insurance,
      qx_diabetes,
      qx_high_blood_pressure,
      qx_healthcare_utilization
    ),
    .f = left_join,
    by = c('exam', 'seqn')
  ) %>%
    relocate(starts_with('wts'), .before = exam_status),

  data_derived = data_pooled %>%
    derive_diabetes() %>%
    derive_egfrCKDepi() %>%
    derive_ascvd_risk_pcr() %>%
    derive_sampson_ldl(),

  write_csv(data_derived, 'NHANES_derived.csv')

)
