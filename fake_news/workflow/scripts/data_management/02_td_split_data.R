# Script name: 02_td_split_data.R
# Project: fake news
# Script purpose: create data dictionary
# Date Created: Mon Jan 10 11:52:17 2022
# Last Modified Date: Mon Jan 10 11:52:17 2022
#
# 👉 The dataframe `d_raw` contain the following variables:

log <- file(snakemake@log[[1]], open="wt")
sink(log)
sink(log, type="message")


# 1.0 PRELIMS -------------------------------------------------------------

suppressPackageStartupMessages({
  # Data Manipulation:
  library("here")
  library("tidyverse")
})
print("Loaded packages.")


# 2.0 GET DATA ------------------------------------------------------------

# For debugging:
# d <-
#   readRDS("/Users/corrado/_repositories/fake_news_wf/fake_news/results/processed_data/fake_news_tidy_data.rds")
# print("Got tidy data.")

d <- readRDS(snakemake@input[["tidy_data"]])
print("Loaded tidy data")


# 3.0 DATA SPLIT ----------------------------------------------------------

# Training data: political news data.
pol_dat <- d %>%
  dplyr::filter(news == "political") %>%
  mutate(
    sex = if_else(sex == "Female", -0.5, 0.5)
  )
nrow(pol_dat)
# [1] 460
saveRDS(pol_dat, snakemake@output[["pol_split_data"]])

# Test data: COVID-19 news data.
covid_dat <- d %>%
  dplyr::filter(news == "covid") %>%
  mutate(
    sex = if_else(sex == "Female", -0.5, 0.5)
  )
nrow(covid_dat)
# [1] 653
saveRDS(covid_dat, snakemake@output[["covid_split_data"]])

print("Saved split data.")


## End of file (eof). ----------
