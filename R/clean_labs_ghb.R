##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param exams
clean_labs_ghb <- function(
  exams = c(1999, 2001, 2003, 2005, 2007, 2009, 2011, 2013, 2015, 2017),
  include_variable_labels = TRUE
) {

  fnames <- make_exam_files(exams, data_label = 'GHB')

  if(1999 %in% exams) fnames['1999-2000'] %<>% str_replace('GHB', 'LAB10')
  if(2001 %in% exams) fnames['2001-2002'] %<>% str_replace('GHB', 'L10')
  if(2003 %in% exams) fnames['2003-2004'] %<>% str_replace('GHB', 'L10')

  var_guide <- tibble(
    term = c('hba1c_perc'),
    nhanes = c("LBXGH"),
    descr = c("Glycohemoglobin, %")
  )

  data_out <- map_dfr(
    .x = fnames,
    .f = read_xpt,
    .id = 'exam'
  ) %>%
    select(exam,
           seqn = SEQN,
           hba1c_perc = LBXGH)

  if(include_variable_labels){
    add_labels(data_out, var_guide)
  } else {
    data_out
  }

}
