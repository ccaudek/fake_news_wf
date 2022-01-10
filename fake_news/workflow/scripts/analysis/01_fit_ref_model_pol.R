log <- file(snakemake@log[[1]], open="wt")
sink(log)
sink(log, type="message")


# 1.0 LIBRARIES -----------------------------------------------------------

print("get here")
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

source(here("scripts", "util_functions", "funs_for_fitting_models.R"))
print("Sourced util functions.")


# 2.0 GET DATA ------------------------------------------------------------

# For debugging:
# pol_dat <-
#   readRDS("/Users/corrado/_repositories/fake_news_wf/fake_news/results/processed_data/pol_dat.rds")
print("Got tidy data.")
pol_dat <- readRDS(snakemake@input[["pol_data"]])
names(pol_dat)


# 4.0 POLITICAL NEWS DATA -------------------------------------------------

# * Standardize all numeric  variables 

scale_this <- function(x) as.vector(scale(x))
pol_data_scaled <- pol_dat %>%
  as_tibble() %>%
  mutate(across(where(is.numeric), scale_this))

# Code sex as -0.5, +0.5.
pol_data_scaled$sex <- if_else(pol_data_scaled$sex < 0, -0.5, 0.5)
glimpse(pol_data_scaled)

f_ref <- bf(
  discernment ~ age + sex + educ + poli + spir +
    agsu + conv + mis + rfsp + rfsn + para + cosp +
    crit + conf
)

reference_fit <- brms::brm(
  f_ref,
  data = pol_data_scaled,
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
  # file = NAME_SAVED_MODEL,
  seed = 123456
)

reference_fit <- brms::add_criterion(reference_fit, "loo")

saveRDS(
  reference_fit,
  snakemake@output[["saved_model_fit"]]
)

print("Summary of posterior estimates")
print(summary(reference_fit), digits = 3)

print("Bayesian R2")
brms::bayes_R2(reference_fit)

pp_check_plot <- pp_check(reference_fit, ndraws = 50, alpha = 0.5) +
  labs(x = "Political news truth discernment")
pp_check_plot
ggsave(snakemake@output[["pp_check"]], width=8, height = 5)

print("LOO")
loo(reference_fit)



## End of file (eof). ----------
