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
  # Graphics:
  library("bayesplot")
  color_scheme_set("brightblue")
  library("patchwork")
})
print("Loaded packages")


# 2.0 GET DATA ------------------------------------------------------------

# For debugging:
# td_ref_pol_fit <-
#   readRDS("/Users/corrado/_repositories/fake_news_wf/fake_news/results/models/td_ref_pol_fit.rds")

td_ref_pol_fit <- readRDS(snakemake@input[["fitted_model"]])
print("Got fitted reference model.")

# For debugging purposes.
summary(td_ref_pol_fit)


# 3.0 predictive projection variable selection ----------------------------

# Here, I perform predictive projection variable selection using the projpred
# package. The goal of predictive projection is to find the smallest possible
# submodel producing similar predictions as those of the reference model.

refmodel <- projpred::get_refmodel(td_ref_pol_fit)
if (!file.exists(snakemake@output[["varsel_td_pol"]])) {
  varsel_td_pol <- projpred::cv_varsel(
    refmodel,
    method = "forward",
    cores = parallel::detectCores()
  )
  saveRDS(varsel_td_pol, snakemake@output[["varsel_td_pol"]])
} 

print("Completed R script 02_td_varsel_ref_pol_model.R")


## End of file (eof). ----------
