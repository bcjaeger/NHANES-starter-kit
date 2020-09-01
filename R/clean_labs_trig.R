##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param exams
clean_labs_trig <- function(
  exams = c(1999, 2001, 2003, 2005, 2007, 2009, 2011, 2013, 2015, 2017),
  include_variable_labels = TRUE
) {

  fnames <- make_exam_files(exams, data_label = 'TRIGLY')

  if(1999 %in% exams)
    fnames['1999-2000'] %<>% str_replace('TRIGLY', 'LAB13AM')
  if(2001 %in% exams)
    fnames['2001-2002'] %<>% str_replace('TRIGLY', 'L13AM')
  if(2003 %in% exams)
    fnames['2003-2004'] %<>% str_replace('TRIGLY', 'L13AM')

  # 2017-2018 are not available yet
  if(2017 %in% exams)
    fnames <- fnames[-which(names(fnames) == '2017-2018')]

  var_guide <- tibble(
    term = c('chol_ldl_mgdl', 'triglycerides_mgdl'),
    nhanes = c("LBDLDL", "LBXTR"),
    descr = c("LDL-cholesterol, md/dL", "Triglyceride, mg/dL")
  )

  data_out <- map_dfr(
    .x = fnames,
    .f = read_xpt,
    .id = 'exam'
  ) %>%
    select(exam,
           seqn = SEQN,
           wts_af_2yr = WTSAF2YR,
           chol_ldl_mgdl = LBDLDL,
           triglycerides_mgdl = LBXTR)

  if(include_variable_labels){
    add_labels(data_out, var_guide)
  } else {
    data_out
  }

}
