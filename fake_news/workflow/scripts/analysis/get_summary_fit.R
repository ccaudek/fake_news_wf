log <- file(snakemake@log[[1]], open="wt")
sink(log)
sink(log, type="message")

suppressPackageStartupMessages({
  # Data Manipulation:
  library("tidyr") # pivot_longer
  library("tidyverse")
  library("here")
  library("magrittr")
  # Modeling:
  library("cmdstanr")
  library("posterior") # as_draws_df
  library("brms")
  library("broom.mixed")
  library("tidybayes") # spread_draws
})

# bmod1 <- readRDS("../results/brms/fit_01.rds")

bmod1 <- readRDS(snakemake@input[["fitted_model"]])

a <- summary(bmod1)
summary_mod1 <- rbind(data.frame(a$fixed), data.frame(a$spec_pars) )
rownames(summary_mod1) <- c("$\\alpha$", "$\\beta$", "$\\sigma_{e}$")
colnames(summary_mod1) <- c("mean","SE", "lower bound", "upper bound", "Rhat", "Bulk ESS", "Tail ESS")

summary_mod1 %<>%
    select(-c("Bulk ESS", "Tail ESS")) %>% # removing ESS
    rownames_to_column(var = "parameter")

write.csv(summary_mod1, file = snakemake@output[["table_1"]], row.names = FALSE)
