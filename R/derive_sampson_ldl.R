##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title

derive_sampson_ldl <- function(data) {

  data %>%
    mutate(
      chol_ldl_mgdl_sampson = (chol_total_mgdl / 0.948) -
        (chol_hdl_mgdl / 0.971) -
        ((triglycerides_mgdl / 8.56) +
           ((triglycerides_mgdl * (chol_total_mgdl - chol_hdl_mgdl)) / 2140) -
           (triglycerides_mgdl ^ 2) / 16100) - 9.44
    )

}
