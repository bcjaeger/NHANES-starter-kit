##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param exams
clean_qx_medical_status <- function(
  exams = c(1999, 2001, 2003, 2005, 2007, 2009, 2011, 2013, 2015, 2017),
  include_variable_labels = TRUE
) {

  fnames <- make_exam_files(exams, data_label = 'MCQ')

  var_guide <- tibble(
    term = c(
      "ever_had_hf",
      "ever_had_chd",
      "ever_had_mi",
      "ever_had_stroke",
      "ever_had_ascvd"
    ),
    nhanes = c(
      "MCQ160B",
      "MCQ160C",
      "MCQ160E",
      "MCQ160F",
      "MCQ160B, MCQ160C, MCQ160E, and MCQ160F"
    ),
    descr = c(
      "Ever told you had congestive heart failure",
      "Ever told you had coronary heart disease",
      "Ever told you had heart attack",
      "Ever told you had a stroke",
      "Ever told you had a stroke, heart attack, coronary heart disease, or congestive heart failure"
    )
  )

  data_in <- map_dfr(
    .x = fnames,
    .f = read_xpt,
    .id = 'exam'
  )


  data_out <- data_in %>%
    select(
      exam,
      seqn = SEQN,
      ever_had_hf = MCQ160B,
      ever_had_chd = MCQ160C,
      ever_had_mi = MCQ160E,
      ever_had_stroke = MCQ160F
    ) %>%
    mutate(
      across(
        .cols = starts_with('ever_had_'),
        .fns = recode,
        "1" =	"yes",
        "2" =	"no",
        "7" =	NA_character_, # "Refused",
        "9" =	NA_character_  # "Dont_know"
      ),
      ever_had_ascvd = if_else(
        condition =
          ever_had_hf  == 'yes' |
          ever_had_chd == 'yes' |
          ever_had_mi  == 'yes' |
          ever_had_stroke == 'yes',
        true = 'yes',
        false = 'no'
      )
    )


  if(include_variable_labels){
    add_labels(data_out, var_guide)
  } else {
    data_out
  }

}
