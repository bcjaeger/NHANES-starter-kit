##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param exams
clean_labs_hdl <- function(
  exams = c(1999, 2001, 2003, 2005, 2007, 2009, 2011, 2013, 2015, 2017),
  include_variable_labels = TRUE
) {

  fnames <- make_exam_files(exams, data_label = 'HDL')

  if(1999 %in% exams) fnames['1999-2000'] %<>% str_replace('HDL', 'LAB13')
  if(2001 %in% exams) fnames['2001-2002'] %<>% str_replace('HDL', 'L13')
  if(2003 %in% exams) fnames['2003-2004'] %<>% str_replace('HDL', 'L13')

  var_guide <- tibble(
    term = c('chol_hdl_mgdl'),
    nhanes = c('LBDHDL [99, 01], LBXHDD [03], LBDHDD [05-17]'),
    descr = c('HDL cholesterol, mg/dL')
  )

  # LBDHDL was used to measure HDL in 1999-2000 and 2001-2002
  # LBXHDD was used to measure HDL in 2003-2004
  # LBDHDD was used to measure HDL in 2005-2006, ..., 2017-2018

  data_in <- fnames %>%
    map_dfr(.f = read_xpt, .id = 'exam') %>%
    add_missing_cols(.names = c("LBDHDL", "LBXHDD", "LBDHDD")) %>%
    mutate(chol_hdl_mgdl = coalesce(LBDHDL, LBXHDD, LBDHDD))

  # widdle columns down to the three we want
  data_out <- data_in %>%
    select(exam, seqn = SEQN, chol_hdl_mgdl)

  if(include_variable_labels){
    add_labels(data_out, var_guide)
  } else {
    data_out
  }

}
