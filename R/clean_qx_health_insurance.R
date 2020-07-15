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

  data_in <- map_dfr(
    .x = fnames,
    .f = read_xpt,
    .id = 'exam'
  )

  # Variable derivation
  # NHANES uses multiple data collection designs.
  # This makes it a little tedious to stack our data.

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

  design1 <- c('1999-2000', '2001-2002', '2003-2004')

  cols <- c(
    exam = 'exam',
    seqn = 'SEQN',
    hins_any = 'HID010',
    hins_priv = 'HID030A',
    # HIQ031A and HIQ031C in 2005+
    hins_medicare = 'HID030B',
    # HIQ031B in 2005+
    hins_medicaid_chip = 'HID030C',
    # HIQ031E and HIQ031D in 2005+
    hins_other_govt = 'HID030D'
    # HIQ031F HIQ031G, HIQ031H, and HIQ031I in 2005+
  )

  # 2. stack the data from these years together, and recode columns.

  hiq1 <- data_in %>%
    select(!!!cols) %>%
    filter(exam %in% design1) %>%
    mutate(
      across(starts_with("hins"),
             ~ recode(.x,
                      '1' = 'Yes',
                      '2' = 'No',
                      '7' = NA_character_,
                      '9' = NA_character_
             )
      )
    )

  # 3. Collapse the separate health insurance indicator columns
  # into one column (`health_insurance`) with the categories we want.

  hiq1 %<>%
    mutate(
      # all the indicators other than hins_any were 'No'
      all_no = hins_priv == 'No' &
        hins_medicare == 'No' &
        hins_medicaid_chip == 'No' &
        hins_other_govt == 'No',
      # all the indicators other than hins_any were NA
      all_na = is.na(hins_priv) &
        is.na(hins_medicare) &
        is.na(hins_medicaid_chip) &
        is.na(hins_other_govt),
      # collapse categories
      health_insurance = case_when(
        hins_any == 'No' ~ "None",
        all_no ~ NA_character_,
        all_na ~ NA_character_,
        hins_priv == 'Yes' ~ "Private",
        hins_medicare == 'Yes' ~ "Medicare",
        hins_medicaid_chip == 'Yes' ~ "Medicaid",
        hins_any == 'Yes' ~ 'Government'
      )
    )

  hiq1

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

  # 1. designate the years and columns we need to access.

  design2 <- setdiff(unique(data_in$exam), design1)


  # 2. stack the data from these years together, and recode the columns

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
                        '1' = 'Yes',
                        '2' = 'No',
                        '7' = NA_character_,
                        '9' = NA_character_
      ),
      hins_priv = recode(hins_priv,
                         "14" = "Yes",
                         "77" = NA_character_,
                         "99" = NA_character_
      ),
      hins_priv = if_else(is.na(hins_priv), 'No', hins_priv)
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
      .funs = ~if_else(is.na(.x), "No", "Yes")
    )

  # 3. Collapse the separate health insurance indicator columns into one
  # column (`health_insurance`) with the categories we want.

  hiq2 %<>%
    mutate(
      all_no = hins_priv == 'No' &
        hins_medicare == 'No' &
        hins_medigap == 'No' &
        hins_medicaid == 'No' &
        hins_schip == 'No' &
        hins_military == 'No' &
        hins_indian == 'No' &
        hins_state == 'No' &
        hins_other_govt == 'No',
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
        hins_any == 'No' ~ "None",
        all_no ~ NA_character_,
        all_na ~ NA_character_,
        hins_priv == 'Yes' ~ "Private",
        hins_medicare == 'Yes' ~ "Medicare",
        hins_medicaid == 'Yes' | hins_schip == 'Yes' ~ "Medicaid",
        hins_any == 'Yes' ~ 'Government'
      )
    )


  # 4. Check the table below to see how we assigned categories
  # count(hiq2, hins_any, hins_priv, hins_medicare, hins_medigap,
  #       hins_medicaid, hins_schip, hins_military, hins_indian,
  #       hins_state, hins_other_govt, health_insurance)

  # Stacking ----

  # The data from each different survey design need to be stacked,
  # using only the relevant columns.

  data_out <- bind_rows(
    select(hiq1, seqn, exam, health_insurance),
    select(hiq2, seqn, exam, health_insurance)
  )


  if(include_variable_labels){
    add_labels(data_out, var_guide)
  } else {
    data_out
  }

}
