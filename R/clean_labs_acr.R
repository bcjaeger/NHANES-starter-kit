##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param exams
##' @param include_variable_labels
##'
clean_labs_acr <- function(
  exams = c(1999, 2001, 2003, 2005, 2007, 2009, 2011, 2013, 2015, 2017),
  include_variable_labels = TRUE
) {

  # retrieve data ----

  fnames <- make_exam_files(exams, data_label = 'ALB_CR')

  if(1999 %in% exams) fnames['1999-2000'] %<>% str_replace('ALB_CR', 'LAB16')
  if(2001 %in% exams) fnames['2001-2002'] %<>% str_replace('ALB_CR', 'L16')
  if(2003 %in% exams) fnames['2003-2004'] %<>% str_replace('ALB_CR', 'L16')

  data_out <- map_dfr(
    .x = fnames,
    .f = read_xpt,
    .id = 'exam'
  ) %>%
    mutate(acr_mgg = 100 * URXUMA / URXUCR) %>%
    select(exam,
           seqn = SEQN,
           acr_mgg)

  var_guide <- tibble(
    term = c('acr_mgg'),
    nhanes = c('URXUMA and URXUCR'),
    descr = c('Albumin-to-creatinine ratio, mg/g')
  )

  if(include_variable_labels){
    add_labels(data_out, var_guide)
  } else {
    data_out
  }

}


