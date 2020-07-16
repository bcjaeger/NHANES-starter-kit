##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param exams
clean_labs_biopro <- function(
  exams = c(1999, 2001, 2003, 2005, 2007, 2009, 2011, 2013, 2015, 2017),
  include_variable_labels = TRUE
) {

  fnames <- make_exam_files(exams, data_label = 'BIOPRO')

  if(1999 %in% exams) fnames['1999-2000'] %<>% str_replace('BIOPRO', 'LAB18')
  if(2001 %in% exams) fnames['2001-2002'] %<>% str_replace('BIOPRO', 'L40')
  if(2003 %in% exams) fnames['2003-2004'] %<>% str_replace('BIOPRO', 'L40')

  var_guide <- tibble(
    term = c(
      'bili_total_mgdl',
      'blood_urea_nitro_mgdl',
      'calcium_total_mgdl',
      'creat_mgdl',
      'gluc_mgdl',
      'iron_ugdl',
      'phosphorus_mgdl'
    ),
    nhanes = c(
      "LBXSTB",
      "LBXSBU",
      "LBXSCA",
      "LBXSCR",
      "LBXSGL",
      "LBXSIR",
      "LBXSPH"
    ),
    descr = c(
      "Total bilirubin, mg/dL",
      "Blood urea nitrogen, mg/dL",
      "Total calcium, mg/dL",
      "Creatinine, mg/dL",
      "Glucose, refrigerated serum, mg/dL",
      "Iron, ug/dL",
      "Phosphorus, mg/dL"
    )
  )

  .names <- c("LBXSTB",
              "LBDSTB",
              "LBXSBU",
              "LBXSCA",
              "LBXSCR",
              "LBDSCR",
              "LBXSGL",
              "LBXSIR",
              "LBXSPH",
              "LBDSPH")

  data_in <- fnames %>%
    map_dfr(.f = read_xpt, .id = 'exam') %>%
    add_missing_cols(.names) %>%
    mutate(
      # serum creatinine (mg/dl) was LBDSCR in 2001-2002 and LBXSCR otherwise.
      LBXSCR = coalesce(LBXSCR, LBDSCR),
      # phosphorus (mg/dl) was LBDSPH in 2001-2002 and LBXSPH otherwise.
      LBXSPH = coalesce(LBXSPH, LBDSPH),
      # total bilirubin (mg/dl) was LBDSTB in 2001-2002 and LBXSTB otherwise.
      LBXSTB = coalesce(LBXSTB, LBDSTB),
    )

  data_out <- data_in %>%
    select(
      exam,
      seqn                  = SEQN,
      bili_total_mgdl       = LBXSTB,
      blood_urea_nitro_mgdl = LBXSBU,
      calcium_total_mgdl    = LBXSCA,
      creat_mgdl            = LBXSCR,
      gluc_mgdl             = LBXSGL,
      iron_ugdl             = LBXSIR,
      phosphorus_mgdl       = LBXSPH
    )

  if(include_variable_labels){
    add_labels(data_out, var_guide)
  } else {
    data_out
  }

}
