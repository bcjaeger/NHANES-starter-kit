##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param exams
clean_labs_tchol <- function(
  exams = c(1999, 2001, 2003, 2005, 2007, 2009, 2011, 2013, 2015, 2017),
  include_variable_labels = TRUE
) {

  fnames <- make_exam_files(exams, data_label = 'TCHOL')

  if(1999 %in% exams) fnames['1999-2000'] %<>% str_replace('TCHOL', 'LAB13')
  if(2001 %in% exams) fnames['2001-2002'] %<>% str_replace('TCHOL', 'L13')
  if(2003 %in% exams) fnames['2003-2004'] %<>% str_replace('TCHOL', 'L13')

  var_guide <- tibble(
    term = c('chol_total_mgdl'),
    nhanes = c('LBXTC'),
    descr = c('Total cholesterol, mg/dL')
  )

  data_out <- map_dfr(
    .x = fnames,
    .f = read_xpt,
    .id = 'exam'
  ) %>%
    select(exam,
           seqn = SEQN,
           chol_total_mgdl = LBXTC)

  if(include_variable_labels){
    add_labels(data_out, var_guide)
  } else {
    data_out
  }

}
