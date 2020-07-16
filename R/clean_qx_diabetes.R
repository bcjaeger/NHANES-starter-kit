##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param exams

clean_qx_diabetes <- function(
  exams = c(1999, 2001, 2003, 2005, 2007, 2009, 2011, 2013, 2015, 2017),
  include_variable_labels = TRUE
) {

  fnames <- make_exam_files(exams, data_label = 'DIQ')

  var_guide <- tibble(
    term   = c('diab_ever',
               'meds_insulin',
               'meds_glucose'),
    nhanes = c('DIQ010',
               'DIQ050',
               'DIQ070 and DID070'),
    descr  = c('Doctor told you have diabetes',
               'Taking insulin now',
               'Take diabetic pills to lower blood sugar')
  )

  .names <- c('DIQ010',
              'DIQ050',
              'DIQ070',
              'DID070')

  data_in <- fnames %>%
    map_dfr(.f = read_xpt, .id = 'exam') %>%
    add_missing_cols(.names)

  # The naming convention for meds_glucose was different in exams
  # 2005-2006 and 2007-2008 compared to all other exams.
  # The names are synchronized using coalesce().

  data_out <- data_in %>%
    transmute(
      exam,
      seqn = SEQN,
      diab_ever = DIQ010,
      meds_insulin = DIQ050,
      meds_glucose = coalesce(DIQ070, DID070)
    ) %>%
    mutate(
      diab_ever = recode(diab_ever,
                         "1" =	"yes",
                         "2" =	"no",
                         "3" =	"borderline",
                         "7" =	NA_character_, # "Refused",
                         "9" =	NA_character_  # "Don't know"
      ),
      meds_insulin = recode(meds_insulin,
                            "1" =	"yes",
                            "2" =	"no",
                            "7" =	NA_character_, # "Refused",
                            "9" =	NA_character_  # "Don't know"
      ),
      meds_glucose = recode(meds_glucose,
                            "1" =	"yes",
                            "2" =	"no",
                            "7" =	NA_character_, # "Refused",
                            "9" =	NA_character_  # "Don't know"
      ),
      meds_insulin = replace(
        meds_insulin,
        list = is.na(meds_insulin) & diab_ever == 'no' & meds_glucose == 'no',
        values = 'no'
      )
    )

  # the last bit of code in the mutate() above is included in order to
  # fix a skip pattern. If both diab_ever and meds_glucose are no,
  # the meds_glucose question was not asked because it was assumed to be no.
  # this code replaces the missing values resulting from the skip with 'no'.

  if(include_variable_labels){
    add_labels(data_out, var_guide)
  } else {
    data_out
  }

}
