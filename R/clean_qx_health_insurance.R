##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param exams
##'
clean_qx_health_insurance <- function(
  exams = c(1999, 2001, 2003, 2005, 2007, 2009, 2011, 2013, 2015, 2017),
  include_variable_labels = TRUE
) {

  fnames <- make_exam_files(exams, data_label = 'HIQ')

  var_guide <- tibble(
    term = c('health_insurance'),
    nhanes = c('HID010, HID030A, HID030B, HID030C, HID030D for 1999-2003; HIQ011, HIQ031A, HIQ031B, HIQ031D, HIQ031F, HIQ031G, HIQ031H, HIQ031I for 2005-2017'),
    descr = c('health insurance status')
  )

  .names <- c('HID010' ,
              'HID030A',
              'HID030B',
              'HID030C',
              'HID030D',
              'HIQ011' ,
              'HIQ031A',
              'HIQ031B',
              'HIQ031C',
              'HIQ031D',
              'HIQ031E',
              'HIQ031F',
              'HIQ031G',
              'HIQ031H',
              'HIQ031I')

  data_in <- fnames %>%
    map_dfr(.f = read_xpt, .id = 'exam') %>%
    add_missing_cols(.names = .names)

  # Variable derivation
  # NHANES uses multiple data collection designs.
  # This makes it a little tedious to stack our data.
  design1 <- c('1999-2000',
               '2001-2002',
               '2003-2004')
  design2 <- c('2005-2006',
               '2007-2008',
               '2009-2010',
               '2011-2012',
               '2013-2014',
               '2015-2016',
               '2017-2018')

  # 1999, 2001, and 2003 ----

  # The design used in 1999, 2001, and 2003 was based on 5 primary questions:
  #
  #   - `HID010`: Covered by health insurance?
  #   - `HID030A`: Covered by private insurance?
  #   - `HID030B`: Covered by Medicare?
  #   - `HID030C`: Covered by Medicaid/CHIP?
  #   - `HID030D`: Covered by other government insurance?

  # We collapse responses into three categories of insurance:
  # `'None'`, `'Private'`, `'Government'`. Notably, there are some
  # participants with private insurance __and__ government insurance.
  # We classify these participants as `'Private'` instead of
  # creating a fourth category. Code to do this is given in 3 steps.

  # 1. designate the years and columns we need to access.


  # 2. stack the data from these years together, and recode columns.

  hiq1 <- data_in %>%
    select(
      exam = exam,
      seqn = SEQN,
      hins_any = HID010,
      hins_priv = HID030A, # HIQ031A, HIQ031C in 2005+
      hins_medicare = HID030B, # HIQ031B in 2005+
      hins_medicaid_chip = HID030C, # HIQ031E, HIQ031D in 2005+
      hins_other_govt = HID030D # HIQ031F HIQ031G, HIQ031H, HIQ031I in 2005+
    ) %>%
    filter(exam %in% design1) %>%
    mutate(
      across(starts_with("hins"),
             ~ recode(.x,
                      '1' = 'yes',
                      '2' = 'no',
                      '7' = NA_character_,
                      '9' = NA_character_
             )
      )
    )

  # 3. Collapse the separate health insurance indicator columns
  # into one column (`health_insurance`) with the categories we want.

  hiq1 %<>%
    mutate(
      # all the indicators other than hins_any were 'no'
      all_no =
        hins_priv          == 'no' &
        hins_medicare      == 'no' &
        hins_medicaid_chip == 'no' &
        hins_other_govt    == 'no',
      # all the indicators other than hins_any were NA
      all_na =
        is.na(hins_priv) &
        is.na(hins_medicare) &
        is.na(hins_medicaid_chip) &
        is.na(hins_other_govt),
      # collapse categories
      health_insurance = case_when(
        hins_any == 'no' ~ "None",
        all_no ~ NA_character_,
        all_na ~ NA_character_,
        hins_priv == 'yes' ~ "Private",
        hins_medicare == 'yes' ~ "Medicare",
        hins_medicaid_chip == 'yes' ~ "Medicaid",
        hins_any == 'yes' ~ 'Government'
      )
    ) %>%
    select(seqn, exam, health_insurance)

  # 4. Check things out:
  # count(hiq1, hins_any, hins_priv, hins_medicare,
  #       hins_medicaid_chip, hins_other_govt, health_insurance)


  # 2005 through 2017 ----

  # The Health Insurance questionnaire was completely redesigned for 2005
  # and exams after 2005. In addition, it used to be administered at the
  # family level where one reference person in the family responded for each
  # individual participant in the family. During and after 2005, each
  # participant responds for themselves, except in situations where a
  # proxy is needed. We use the same 3 step protocol:

  hiq2 <- data_in %>%
    filter(exam %in% design2) %>%
    select(
      seqn = SEQN,
      exam,
      hins_any        = HIQ011,
      hins_priv       = HIQ031A,
      hins_medicare   = HIQ031B,
      hins_medigap    = HIQ031C, # medigap goes to other
      hins_medicaid   = HIQ031D,
      hins_schip      = HIQ031E, # schip goes to other
      hins_military   = HIQ031F, # goes to other
      hins_indian     = HIQ031G, # goes to other
      hins_state      = HIQ031H, # goes to other
      hins_other_govt = HIQ031I
    ) %>%
    mutate(
      hins_any = recode(hins_any,
                        '1' = 'yes',
                        '2' = 'no',
                        '7' = NA_character_,
                        '9' = NA_character_
      ),
      hins_priv = recode(hins_priv,
                         "14" = "yes",
                         "77" = NA_character_,
                         "99" = NA_character_
      ),
      hins_priv = if_else(is.na(hins_priv), 'no', hins_priv)
    ) %>%
    mutate_at(
      .vars = vars(
        hins_medicare,
        hins_medigap,
        hins_medicaid,
        hins_schip,
        hins_military,
        hins_indian,
        hins_state,
        hins_other_govt
      ),
      .funs = ~if_else(is.na(.x), "no", "yes")
    )

  hiq2 %<>%
    mutate(
      all_no = hins_priv == 'no' &
        hins_medicare == 'no' &
        hins_medigap == 'no' &
        hins_medicaid == 'no' &
        hins_schip == 'no' &
        hins_military == 'no' &
        hins_indian == 'no' &
        hins_state == 'no' &
        hins_other_govt == 'no',
      # all the indicators other than hins_any were NA
      all_na = is.na(hins_priv) &
        is.na(hins_medicare) &
        is.na(hins_medigap) &
        is.na(hins_medicaid) &
        is.na(hins_schip) &
        is.na(hins_military) &
        is.na(hins_indian) &
        is.na(hins_state) &
        is.na(hins_other_govt),
      # collapse categories
      health_insurance = case_when(
        hins_any == 'no' ~ "None",
        all_no ~ NA_character_,
        all_na ~ NA_character_,
        hins_priv == 'yes' ~ "Private",
        hins_medicare == 'yes' ~ "Medicare",
        hins_medicaid == 'yes' | hins_schip == 'yes' ~ "Medicaid",
        hins_any == 'yes' ~ 'Government'
      )
    )

  # Stacking ----

  data_out <- bind_rows(hiq1, hiq2) %>%
    select(exam, seqn, health_insurance)

  if(include_variable_labels){
    add_labels(data_out, var_guide)
  } else {
    data_out
  }

}
