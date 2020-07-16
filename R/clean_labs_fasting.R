##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param exams
clean_labs_fasting <- function(
  exams = c(1999, 2001, 2003, 2005, 2007, 2009, 2011, 2013, 2015, 2017),
  include_variable_labels = TRUE
  ) {

  fnames <- make_exam_files(exams, data_label = 'FASTQX')

  if(1999 %in% exams) fnames['1999-2000'] %<>% str_replace('FASTQX', 'PH')
  if(2001 %in% exams) fnames['2001-2002'] %<>% str_replace('FASTQX', 'PH')
  if(2003 %in% exams) fnames['2003-2004'] %<>% str_replace('FASTQX', 'PH')

  var_guide <- tibble(
    term = c('fasted_hours', 'fasted_minutes'),
    nhanes = c("PHAFSTHR", "PHAFSTMN"),
    descr = c(
      "Total length of 'food fast', hours",
      "Total length of 'food fast', minutes"
    )
  )

  data_out <- map_dfr(
    .x = fnames,
    .f = read_xpt,
    .id = 'exam'
  ) %>%
    select(exam,
           seqn = SEQN,
           fasted_hours   = PHAFSTHR,
           fasted_minutes = PHAFSTMN)

  if(include_variable_labels){
    add_labels(data_out, var_guide)
  } else {
    data_out
  }

}
