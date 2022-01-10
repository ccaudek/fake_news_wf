# Script name: 01_data_dictionary.R
# Project: fake news
# Script purpose: create data dictionary
# Date Created: Mon Jan 10 09:26:02 2022
# Last Modified Date: Mon Jan 10 09:26:02 2022
#
# 👉 The dataframe `d_raw` contain the following variables:
#
# names(d_raw)
# [1] "sex"                   "marital_status"        "sons"
# [4] "spir"                  "religion"              "educ"
# [7] "age"                   "occ"                   "poli"
# [10] "newsp"                 "rad"                   "tel"
# [13] "online_read"           "podcast"               "online_watch"
# [16] "cfriends"              "share"                 "crt_score"
# [19] "agsu"                  "conv"                  "miss"
# [22] "rfsp"                  "rfsn"                  "discernment"
# [25] "ov_belief"             "fs_paranormal_beliefs" "fs_crit_news"
# [28] "fs_conspiracy_beliefs" "fs_luck"               "fs_simplicity"
# [31] "fs_confirmation"       "fs_resentment"         "para"
# [34] "crit"                  "cosp"                  "supe"
# [37] "simp"                  "conf"                  "rese"
# [40] "news"

# "Durante la settimana, quanto spesso svolgi ciascuna delle seguenti attività? [Leggere notizie sui quotidiani cartacei]",
# "Durante la settimana, quanto spesso svolgi ciascuna delle seguenti attività? [Ascoltare notizie o approfondimenti alla radio]",
# "Durante la settimana, quanto spesso svolgi ciascuna delle seguenti attività? [Guardare telegiornali o programmi di attualità in tv]",
# "Durante la settimana, quanto spesso svolgi ciascuna delle seguenti attività? [Leggere notizie sui quotidiani online]",
# "Durante la settimana, quanto spesso svolgi ciascuna delle seguenti attività? [Ascoltare podcast su notizie o approfondimenti]",
# "Durante la settimana, quanto spesso svolgi ciascuna delle seguenti attività? [Guardare programmi di informazione o di approfondimento online]",
# "Durante la settimana, quanto spesso svolgi ciascuna delle seguenti attività? [Commentare con amici e conoscenti notizie e fatti di attualità]",
# "Durante la settimana, quanto spesso svolgi ciascuna delle seguenti attività? [Utilizzo di social media (es., Facebook, Instagram, WhatsApp, ecc.) per condividere o commentare notizie e fatti di attualità]"
# )

# TODO:
# - clarify agsu (is also negative)

log <- file(snakemake@log[[1]], open="wt")
sink(log)
sink(log, type="message")


# 1.0 PRELIMS -------------------------------------------------------------

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
  library("ggcorrplot")
  library("labelled")
})

print("Loaded packages.")

source(here("scripts", "util_functions", "funs_for_wrangling_data.R"))
print("Sourced util functions.")


# 2.0 GET DATA ------------------------------------------------------------

alldata <- readRDS(
  here("scripts", "data", "fake_news_raw_data.rds")
)

print("Got raw data.")

#' Locates and remove influential observations (i.e., "outliers") via several
#' distance and/or clustering methods. Given that several methods were selected,
#' we computed a composite outlier score, made of the average of the binary (0
#' or 1) results of each method. Such composite score represents the probability
#' of each observation of being classified as an outlier by at least one method.
#' The decision rule was to classify as outliers observations which composite
#' outlier score was superior or equal to 0.5 (i.e., that were classified as
#' outliers by at least half of the methods). The methods used were:
#' - Cook's Distance: Cook's distance estimates the variations in regression
#'   coefficients after removing each observation, one by one (Cook, 1977).
#' - Pareto: The reliability and approximate convergence of Bayesian models can
#'   be assessed using the estimates for the shape parameter k of the
#'   generalized Pareto distribution. If the estimated tail shape parameter k
#'   exceeds 0.7, the observation was flagged.
#' - IQR: Following Tukey, this method considers as outliers any data point that
#'   falls outside of either 1.5 times the IQR below the first or above the
#'   third quartile.
#' - Z-scores:This univariate method describes a data point as deviance from a
#'   central value, in terms of the Median Absolute Deviation (MAD) from the
#'   median. The threshold to classify outliers is 1.959, corresponding to the
#'   2.5 most extreme observations assuming the data is normally distributed.
#' - CI: Another univariate method computing, for each variable, a sort of
#'   "confidence" interval and considering as outliers values lying beyond the
#'   edges of that interval. We considered, as interval, the Bias Corrected and
#'   Accelerated Interval. The threshold was set to 0.95.
#' - Minimum Covariance Determinant: A robust version of Mahalanobis distance.
#'   Leys et al. (2018) argue that Mahalanobis Distance is not a robust way to
#'   determine outliers, as it uses the means and covariances of all the data –
#'   including the outliers – to determine individual difference scores. Minimum
#'   Covariance Determinant calculates the mean and covariance matrix based on
#'   the most central subset (66%) of the data.
#' - Invariant Coordinate Selection: With PCA, three steps have to be performed:
#'   (i) select the components most useful for the detection, (ii) compute
#'   distances as outlierness measures for all observation, and (iii) label
#'   outliers using a cut-off value derived from simulations (corresponding to
#'   the 2.5% value for outliers classification).
#' - Ordering Points To Identify the Clustering Structure (OPTICS) algorithm:
#'   The OPTICS algorithm (Ankerst et al., 1999) is an unsupervised clustering
#'   technique used for outliers detection. Compared to the others techniques,
#'   that will always detect several outliers (as these are usually defined as
#'   a percentage of extreme values), this algorithm functions in a different
#'   manner and won't always detect outliers.
#' - Local Outlier Factor: Based on a K nearest neighbours algorithm, LOF
#'   compares the local density of an point to the local densities of its
#'   neighbors instead of computing a distance from the center (Breunig et al.,
#'   2000). Points that have a substantially lower density than their neighbors
#'   are considered outliers. A LOF score of approximately 1 indicates that
#'   density around the point is comparable to its neighbors. Scores
#'   significantly larger than 1 indicate outliers. The default threshold of
#'   0.025 that was used classified as outliers the observations located at
#'   qnorm(1 - 0.025) * SD) of the log-transformed LOF distance.

d_raw <- suppressWarnings(remove_outliers(alldata))

# Proportion of removed observations:
round((nrow(alldata) - nrow(d_raw)) / nrow(alldata), 3)
print("Removed outlierness.")

# 3.0 TIDY DATA -----------------------------------------------------------

# Keep only the variables used in the statistical analyses.
keep_vars <- c(
  "sex", "marital_status", "sons", "spir", "religion", "educ",
  "age", "occ", "poli", "newsp", "rad", "tel", "online_read",
  "podcast", "online_watch", "cfriends", "share", "agsu", "conv",
  "miss", "rfsp", "rfsn", "para", "crit", "cosp", "conf",
  "discernment", "ov_belief", "news"
)

mydat <- d_raw %>%
  dplyr::select(all_of(keep_vars))

# Rename variable type
mydat <- mydat %>%
  dplyr::rename(
    "children" = "sons",
    "mis" = "miss"
  )

# Change variable type
mydat$marital_status <- factor(mydat$marital_status)
mydat$religion <- factor(mydat$religion)
mydat$children <- as.integer(mydat$children)

# Translate variables levels
mydat$religion <- dplyr::recode(
  mydat$religion,
  `Agnostico` = 'Agnostic',
  `Ateo` = 'Atheist',
  `Credente cattolico` = 'Chatolic',
  `Credente in altra religione` = 'Believer in another religion',
  `Credente senza riferimenti religiosi` = 'Believer without religious references'
)

mydat$marital_status <- dplyr::recode(
  mydat$marital_status,
  `Coniugato` = 'Married',
  `Convivente` = 'Cohabitant',
  `Nubile/ celibe` = 'Single / celibate',
  `Separato o divorziato` = 'Separated or divorced',
  `Vedovo` = 'Widow or Widower'
)

# * Add variable label ----

labelled::var_label(mydat$sex) <- "Gender"
labelled::var_label(mydat$marital_status) <- "Marital status"
labelled::var_label(mydat$children) <- "Number of children"
labelled::var_label(mydat$spir) <- "Number of weekly hours of spiritual practices (meditation, attending gatherings of like-minded believers, volunteer work together with like-minded believers, and so on)"
labelled::var_label(mydat$religion) <- "Religion"
labelled::var_label(mydat$educ) <- "Number of years of education completed"
labelled::var_label(mydat$age) <- "Age (years)"
labelled::var_label(mydat$occ) <- "Occupation"
labelled::var_label(mydat$poli) <- "Political orientation (coded as 1 = extreme right-wing affinity, ..., 8 = extreme left-wing affinity)"

labelled::var_label(mydat$newsp) <- "During the week, how often (in hours) do you do each of the following activities? (Read news in print newspapers)"
labelled::var_label(mydat$rad) <- "During the week, how often (in hours) do you do each of the following activities? (Listen to news or news programs on the radio)"
labelled::var_label(mydat$tel) <- "During the week, how often (in hours) do you do each of the following activities? (Watch news or current affairs programs on TV)"
labelled::var_label(mydat$online_read) <- "During the week, how often (in hours) do you do each of the following activities? (Read news in online newspapers)"
labelled::var_label(mydat$podcast) <- "During the week, how often (in hours) do you do each of the following activities? (Listen to news podcasts)"
labelled::var_label(mydat$online_watch) <- "During the week, how often (in hours) do you do each of the following activities? (Watch information or in-depth programs online)"
labelled::var_label(mydat$cfriends) <- "During the week, how often (in hours) do you do each of the following activities? (Comment with friends and acquaintances news and current events)"
labelled::var_label(mydat$share) <- "During the week, how often (in hours) do you do each of the following activities? [Use of social media (eg, Facebook, Instagram, WhatsApp, etc.) to share or comment on news and current events]"

labelled::var_label(mydat$agsu) <- "RWA - Authoritarian aggression and submission"
labelled::var_label(mydat$conv) <- "RWA - Conservatism"

labelled::var_label(mydat$mis) <- "Magical Ideation Scale"

labelled::var_label(mydat$rfsp) <- "Religious Fundamentalism Scale - Belief "
labelled::var_label(mydat$rfsn) <- "Religious Fundamentalism Scale - Skepticism"

labelled::var_label(mydat$para) <- "Fake News Susceptibility Scale - Irrational beliefs"
labelled::var_label(mydat$crit) <- "Fake News Susceptibility Scale - Critical news consumption"
labelled::var_label(mydat$cosp) <- "Fake News Susceptibility Scale - Conspiratorial beliefs"
labelled::var_label(mydat$conf) <- "Fake News Susceptibility Scale - Self belief"

labelled::var_label(mydat$discernment) <- "Truth discernment: the degree of belief in true news minus the degree of belief in fake news"
labelled::var_label(mydat$ov_belief) <- "Truth discernment: the sum of the degrees of belief in true news and fake news, without distinguishing between them"

labelled::var_label(mydat$news) <- "Data set"

# Check labels
labelled::look_for(mydat, details = "full")

# To get the variable label, simply call var_label(). E.g.,
# var_label(mydat$poli)
# var_label(mydat$religion)
# var_label(mydat$newsp)
print("Tidied data.")

saveRDS(mydat, snakemake@output[["tidy_data"]])
print("Saved tidy data.")


## End of file (eof). ----------
