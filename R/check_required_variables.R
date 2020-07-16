##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param data
##' @param required_variables
##' @param relevant_funs
check_required_variables <- function(data, .variables) {

  variables_missing <- setdiff(.variables, names(data))

  n_missing <- length(variables_missing)

  if(n_missing > 0){

    variable_string <- glue_collapse(variables_missing,
                                     sep = ', ',
                                     last = ', and ')

    f_string <- unique(names(.variables)[.variables %in% variables_missing])

    fun_string <- glue_collapse(f_string,
                                sep = ', ',
                                last = ', and ')

    msg <- glue("`data` are missing variables: {variable_string}.",
                "\nSee {fun_string} to create these variables.")

    stop(msg, call. = FALSE)

  }

}
