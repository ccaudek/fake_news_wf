log <- file(snakemake@log[[1]], open="wt")
sink(log)
sink(log, type="message")


# 1.0 LIBRARIES -----------------------------------------------------------

suppressPackageStartupMessages({
  # Data Manipulation:
  library("tidyr") # pivot_longer
  library("tidyverse")
  library("purrr") # *map*
  library("performance") # outliers
  library("here")
  # Modeling:
  library("cmdstanr")
  library("posterior") # as_draws_df
  library("brms")
  library("broom.mixed")
  library("tidybayes") # spread_draws
})
print("Loaded packages")

source(here("scripts", "util_functions", "funs_for_fitting_models.R"))
print("Sourced util functions.")


# 2.0 GET DATA ------------------------------------------------------------

# For debugging:
# pol_dat <-
#   readRDS("/Users/corrado/_repositories/fake_news_wf/fake_news/results/processed_data/pol_dat.rds")

varsel_td_pol <- readRDS(snakemake@input[["fitted_model"]])
print("Got fitted reference model.")

summary(varsel_td_pol)

projpred::solution_terms(varsel_td_pol) # selection order of the variables
# "cosp" "poli" "sex"  "educ" "crit" "age"  "conv" "miis" "spir"
# "conf" "para" "rfsn" "agsu" "rfsp"
projpred::suggest_size(varsel_td_pol)
# [1] 8
projpred::solution_terms(varsel_td_pol)[
  1:projpred::suggest_size(varsel_td_pol)
]

# Extract draws of the linear predictor from the predictive
# distribution of the projected submodel or submodels.
pol_dat <- readRDS(snakemake@input[["pol_data"]])
preds_reference_14 <- 
  projpred::proj_linpred(
    varsel_td_pol,
    newdata = pol_dat,
    nterms = 14,
    integrated = TRUE
  )
attributes(preds_reference_14)
# $names
# [1] "pred" "lpd"
length(preds_reference_14$pred)
# [1] 460
cor(yhat_reference_14, preds_reference_14$pred)
# [1] 0.99998

# Perform projection onto submodels of selected sizes or a specified feature
# combination.
proj1 <- projpred::project(
  varsel_td_pol,
  nterms = projpred::suggest_size(varsel_td_pol),
  seed = 123,
  ndraws = 2000
)
attributes(proj1)
# $names
# [1] "dis"                "kl"                 "weights"
# [4] "solution_terms"     "sub_fit"            "family"
# [7] "p_type"             "intercept"          "extract_model_data"
# [10] "refmodel"
#
# $class
# [1] "projection"

# predvsreal_reference_fit <- tibble(
#   predicted = preds_reference_14$pred,
#   real = pol_dat$discernment
# ) %>%
#   mutate(
#     error = predicted - real
#   )

# predvsreal_reference_fit <- tibble(
#   predicted = predict(reference_fit)[, 1],
#   real = pol_dat$discernment
#   ) %>%
#   mutate(
#     error = predicted - real
#   )

# Equivalent:
# predvsreal_reference_fit <- tibble(
#   predicted = preds_reference_14$pred,
#   real = pol_dat$discernment
# ) %>%
#   mutate(
#     error = predicted - real
#   )

# Compute the mean of the predictive distribution and evaluates the
# log density at the training points using the 8 most relevant variables.
pred_train_subset <- projpred::proj_linpred(
  varsel1,
  newdata = pol_dat,
  nterms = projpred::suggest_size(varsel1),
  integrated = TRUE
)

predvsreal_subset_fit <- tibble(
  predicted = pred_train_subset$pred,
  real = pol_dat$discernment
) %>%
  mutate(
    error = predicted - real
  )

# RMSE subset
round(sqrt(mean(predvsreal_subset_fit$error^2)), 3)
# [1] 0.805

# Correlation between the submodel's predicted values and the y data.
cor(pred_train_subset$pred, pol_dat$discernment)
# [1] 0.5765503

# R2 of the submodel
cor(pred_train_subset$pred, pol_dat$discernment)^2
# [1] 0.3324102


# * 4.3 Subset fit with optimal coefficients ----

td_proj_pol_fit <- get_td_proj_fit(
  pol_dat,
  varsel_td_pol,
  "td_proj_pol_fit"
)

plot(loo(td_proj_pol_fit))

# The projected submodel's R2 is very similar (but not identical) when
# computed with projpred and with bayes_R2().
bayes_R2(td_proj_pol_fit, cores = 4) %>%
  round(3)
#    Estimate Est.Error  Q2.5 Q97.5
# R2    0.335     0.029 0.275  0.39

print(summary(td_proj_pol_fit), 3)
#       Estimate Est.Error l-95% CI u-95% CI  Rhat Bulk_ESS Tail_ESS
# sigma    0.815     0.027    0.764    0.870 1.000    73356    31807

# The same result is obtained when using projpred::proj_linpred() or
# fitting again the data with a subset of predictors.
cor(pred_train_subset$pred, predict(td_proj_pol_fit)[, 1])
# [1] 0.99997
# So, projpred::proj_linpred() does not seem to be using the same coefficients
# as in the complete model.

# Correlation between the predicted values of the submodel and the
# predicted values of the full model.
cor(pred_train_subset$pred, predict(td_proj_pol_fit)[, 1])
# [1] 0.97615

# Plot of predicted values from the subset model and predicted values from
# the complete model.
ggplot() +
  geom_point(aes(x = pred_train_subset$pred, predict(reference_fit)[, 1])) +
  geom_abline(slope = 1, color = "red") +
  labs(x = "yhat submodel", y = "yhat complete model")

# Plot of predicted values from the complete model and the observed data.
ggplot() +
  geom_point(aes(x = predict(reference_fit)[, 1], pol_dat$discernment)) +
  geom_abline(slope = 1, color = "red") +
  labs(x = "yhat complete model", y = "y")

# Plot of predicted values from the subset model and the observed data.
ggplot() +
  geom_point(aes(x = pred_train_subset$pred, pol_dat$discernment)) +
  geom_abline(slope = 1, color = "red") +
  labs(x = "yhat submodel", y = "y")

# CONCLUSION: When using 8 instead of 14 predictors, R2 decreases from
#  0.353 to 0.332.


