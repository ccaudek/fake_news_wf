
#' @title Variable selection.
#' @description Predictive projection variable selection
#' @return A cv_varsel object.
#' @export
#' @param fit Character, brm object.
#' VARSEL_NAME:
#' varsel_td_pol, varsel_td_covid, varsel_ob_pol, varsel_ob_covid
get_cv_varsel_obj <- function(fit, VARSEL_NAME) {
  # get the reference model structure
  refmodel <- projpred::get_refmodel(fit)
  # The function cv_varsel runs a cross-validated variable selection, and returns
  # an object that contains the relevant information about the procedure, such as
  # the ordering of the variables. The search heuristic can be specified by the
  # keyword method. For a simple linear model, the fastest method is to use
  # L1–search, that is the default option. For a more accurate result we recommend
  # running forward search.
  if (!file.exists(here("workflow", "results", "r_objects", VARSEL_NAME))) {
    varsel <- projpred::cv_varsel(
      refmodel,
      method = "forward",
      cores = parallel::detectCores()
    )
    saveRDS(varsel, here::here("workflow", "results", "r_objects", VARSEL_NAME))
  } else {
    varsel <- readRDS(here::here("workflow", "results", "r_objects", VARSEL_NAME))
  }
  varsel
}
