log <- file(snakemake@log[[1]], open="wt")
sink(log)
sink(log, type="message")

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
  library("projpred")
  # Graphics:
  library("bayesplot")
  color_scheme_set("brightblue")
  library("patchwork")
})


d <- readRDS(snakemake@input[["brm_data"]])

f_ref <- bf(
  flipper_length_mm ~ body_mass_g
)

print("Read data")

print("Beginning brm fit")
reference_fit <- brms::brm(
    f_ref,
    data = d,
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
    # file = here::here("code", "scripts", "models", NAME_SAVED_MODEL),
    seed = 123456
)

#reference_fit <- brms::add_criterion(reference_fit, "loo")
print("Save brm object")
saveRDS(reference_fit, snakemake@output[["fitted_model"]])
