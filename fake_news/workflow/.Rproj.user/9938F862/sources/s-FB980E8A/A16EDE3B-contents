#' @title Fit reference model for truth discernment.
#' @description predicts truth discernment from all 14 predictors
#' @return A brm object.
#' @export
#' @param file Character, data file path.
get_td_reference_fit <- function(df, NAME_SAVED_MODEL) {

  f_ref <- bf(
    discernment ~ age + sex + educ + poli + spir +
      agsu + conv + miis + rfsp + rfsn + para + cosp +
      crit + conf
  )

  reference_fit <- brms::brm(
    f_ref,
    data = df,
    prior = c(
      prior(normal(0, 1), class = "b"),
      prior(normal(0, 1), class = "Intercept"),
      prior(student_t(4, 0, 1), class = "sigma")
    ),
    iter = 2000L,
    inits = 0.01,
    chains = 4L,
    cores = parallel::detectCores(),
    backend = "cmdstan",
    file = NAME_SAVED_MODEL,
    seed = 123456
  )
  reference_fit <- brms::add_criterion(reference_fit, "loo")

  reference_fit
}


#' @title Fit projection model for truth discernment.
#' @description predicts truth discernment from 8 predictors
#' @return A brm object.
#' @export
#' @param file Character, data file path.
get_td_proj_fit <- function(df, varsel_obj, NAME_SAVED_MODEL) {

  predictor_variables <-
    projpred::solution_terms(varsel_obj)[1:projpred::suggest_size(varsel_obj)]
  # generate the model's formula by using the number
  # of predictors suggested by projpred.
  brm_formula <- formula(
    paste("discernment ~ ", paste(predictor_variables, collapse = " + "))
  )

  fit <- brms::brm(
    brm_formula,
    data = df, 
    prior = c(
      prior(normal(0, 1), class = "b"),
      prior(normal(0, 1), class = "Intercept"),
      prior(student_t(4, 0, 1), class = "sigma")
    ),
    iter = 20000L,
    inits = 0.01,
    chains = 4L,
    cores = parallel::detectCores(),
    backend = "cmdstan",
    file = here::here("code", "scripts", "models", NAME_SAVED_MODEL),
    seed = 123456
  )
  fit <- brms::add_criterion(fit, "loo")

  fit
}
