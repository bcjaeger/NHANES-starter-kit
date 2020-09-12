##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param exams

clean_qx_smoking_status <- function(
  exams = c(1999, 2001, 2003, 2005, 2007, 2009, 2011, 2013, 2015, 2017),
  include_variable_labels = FALSE
) {

  fnames <- make_exam_files(exams, data_label = 'SMQ')

  var_guide <- tibble(
    term = c(
      'smk_current',
      'smk_status',
      'smk_quit_howlong'
    ),
    nhanes = c(
      "SMQ020 and SMQ040",
      "SMQ020 and SMQ040",
      "SMQ050Q and SMQ050U"
    ),
    descr = c(
      "Smoked at least 100 cigarettes and currently smoking",
      "Smoking status",
      "If ever smoked, how many days since quitting"
    )
  )

  data_in <- map_dfr(
    .x = fnames,
    .f = read_xpt,
    .id = 'exam'
  )

  data_out <- data_in %>%
    select(exam,
           seqn = SEQN,
           smk_100 = SMQ020,
           smk_now = SMQ040,
           smk_quit_howlong = SMQ050Q,
           smk_quit_units = SMQ050U) %>%
    mutate(
      smk_100 = recode(smk_100,
                       "1" =	"yes",
                       "2" =	"no",
                       "7" =	NA_character_, # "Refused",
                       "9" =	NA_character_  # "Dont_know"
      ),
      smk_now = recode(smk_now,
                       "1"	= "yes",        # "Every_day",
                       "2"	= "yes",        # "Some_days",
                       "3"	= "no",         # "not_at_all",
                       "7"	= NA_character_,# "Refused",
                       "9"	= NA_character_ # "Dont_know"
      ),
      smk_quit_howlong = case_when(
        smk_quit_howlong == 66666 ~ 50 * 365.25,          # 50 years or more
        smk_quit_howlong == 77777 ~ NA_real_,             # refused
        smk_quit_howlong == 99999 ~ NA_real_,             # dont know
        smk_quit_units == 1 ~ smk_quit_howlong,           # days
        smk_quit_units == 2 ~ smk_quit_howlong * 7,       # weeks
        smk_quit_units == 3 ~ smk_quit_howlong * 30.4167, # months
        smk_quit_units == 4 ~ smk_quit_howlong * 365.25,  # years
        TRUE ~ NA_real_
      ),
      smk_status = case_when(
        smk_now == "yes" & smk_100 == "yes" ~ "current",
        smk_now == "no" & smk_quit_howlong > 7 ~ "former",
        smk_now == "no" | smk_100 == 'no' ~ "never"
      ),
      smk_current = if_else(
        smk_now == "yes" & smk_100 == "yes",
        true = "yes",
        false = "no"
      )
    ) %>%
    select(exam,
           seqn,
           smk_current,
           smk_status,
           smk_quit_howlong)

  if(include_variable_labels){
    add_labels(data_out, var_guide)
  } else {
    data_out
  }

}

