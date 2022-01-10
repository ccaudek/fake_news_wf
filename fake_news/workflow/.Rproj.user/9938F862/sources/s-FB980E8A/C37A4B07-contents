#' @description 
#' Get raw data, remove outliers, rename variables.
#' @import 
#' Raw data.
#' @return 
#' Dataframe with cleaned data.
get_and_wrangle_data <- function() {
  
  # Get raw data.
  alldata <- readRDS(
    here("data", "processed", "fakenews_data.rds")
  )
  
  # Select numeric variables used in the regression models.
  foo <- alldata %>%
    dplyr::select(
      discernment, ov_belief, aggression_submission, conventionalism,
      rfs_pos, rfs_neg, paran_beliefs, crit_news, conspiracy_beliefs,
      confirmation, miis_score, edu, pol, spirituality
    )
  
  foo <- as.data.frame(foo)
  
  # Find outliers
  outliers_list <- performance::check_outliers(
    foo,
    method = c(
      "cook",
      "pareto",
      "iqr",
      "zscore_robust",
      "bci",
      "mcd",
      "ics",
      "optics",
      "lof"
    )
  )
  
  outliers_list # Show the row index of the outliers
  # as.numeric(outliers_list) # The object is a binary vector...
  outliers_info <- as.data.frame(outliers_list)
  filtered_data <- alldata[outliers_info$Outlier < 0.55, ]
  # Number of removed rows.
  nrow(alldata) - nrow(filtered_data)
  
  # Standardize all numeric  variables 
  scale_this <- function(x) as.vector(scale(x))
  alldata_scaled <- filtered_data %>%
    as_tibble() %>%
    mutate(across(where(is.numeric), scale_this))
  
  # Data used in the analyses.
  d <- alldata_scaled
  
  # Rename variables.
  d <- d %>%
    dplyr::rename(
      agsu = aggression_submission,
      conv = conventionalism,
      miis = miis_score,
      rfsp = rfs_pos,
      rfsn = rfs_neg,
      educ = edu,
      poli = pol,
      spir = spirituality,
      para = paran_beliefs,
      crit = crit_news,
      cosp = conspiracy_beliefs,
      supe = supersticion,
      simp = simplicity,
      conf = confirmation,
      rese = resentment
    )
  
  # Rename levels of sex factor.
  d$sex <- forcats::fct_recode(
    d$sex,
    "Female" = "Femmina",
    "Male" = "Maschio"
  )
  
  # d <-
  #   d %>%
  #   # contrast coding
  #   mutate(sex = ifelse(d$sex == "Female", -0.5, 0.5))
  
  d
  
}


#' @description 
#' Get raw data, remove outliers, rename variables.
#' @import 
#' Raw data.
#' @return 
#' Dataframe with cleaned data.
get_complete_data <- function() {
  
  # Get raw data.
  alldata <- readRDS(
    here("data", "processed", "fakenews_data.rds")
  )
  
  # Standardize all numeric  variables 
  scale_this <- function(x) as.vector(scale(x))
  alldata_scaled <- alldata %>%
    as_tibble() %>%
    mutate(across(where(is.numeric), scale_this))
  
  # Data used in the analyses.
  d <- alldata_scaled
  
  # Rename variables.
  d <- d %>%
    dplyr::rename(
      agsu = aggression_submission,
      conv = conventionalism,
      miis = miis_score,
      rfsp = rfs_pos,
      rfsn = rfs_neg,
      educ = edu,
      poli = pol,
      spir = spirituality,
      para = paran_beliefs,
      crit = crit_news,
      cosp = conspiracy_beliefs,
      # supe = supersticion,
      # simp = simplicity,
      conf = confirmation,
      # rese = resentment
    )
  
  # Rename levels of sex factor.
  d$sex <- forcats::fct_recode(
    d$sex,
    "Female" = "Femmina",
    "Male" = "Maschio"
  )
  
  # d <-
  #   d %>%
  #   # contrast coding
  #   mutate(sex = ifelse(d$sex == "Female", -0.5, 0.5))
  
  d
  
}

#--------------------------------------------------------------------------

#' @description 
#' Get raw data, remove outliers, rename variables, no standardization.
#' @import 
#' Raw data.
#' @return 
#' Dataframe with cleaned data.
remove_outliers <- function(alldata) {
  
  # Get raw data.
  # alldata <- readRDS(here("scripts", "data", "fake_news_raw_data.rds"))
  
  # Select numeric variables used in the regression models.
  foo <- alldata %>%
    dplyr::select(
      discernment, ov_belief, aggression_submission, conventionalism,
      rfs_pos, rfs_neg, paran_beliefs, crit_news, conspiracy_beliefs,
      confirmation, miis_score, edu, pol, spirituality
    )
  
  foo <- as.data.frame(foo)
  
  # Find outliers
  outliers_list <- performance::check_outliers(
    foo,
    method = c(
      "cook",
      "pareto",
      "iqr",
      "zscore_robust",
      "bci",
      "mcd",
      "ics",
      "optics",
      "lof"
    )
  )
  
  outliers_list # Show the row index of the outliers
  # as.numeric(outliers_list) # The object is a binary vector...
  outliers_info <- as.data.frame(outliers_list)
  filtered_data <- alldata[outliers_info$Outlier < 0.55, ]
  # Number of removed rows.
  nrow(alldata) - nrow(filtered_data)

  # Data used in the analyses.
  d <- filtered_data
  
  # Rename variables.
  d <- d %>%
    dplyr::rename(
      agsu = aggression_submission,
      conv = conventionalism,
      miss = miis_score,
      rfsp = rfs_pos,
      rfsn = rfs_neg,
      educ = edu,
      poli = pol,
      spir = spirituality,
      para = paran_beliefs,
      crit = crit_news,
      cosp = conspiracy_beliefs,
      supe = supersticion,
      simp = simplicity,
      conf = confirmation,
      rese = resentment
    )
  
  # Rename levels of sex factor.
  d$sex <- forcats::fct_recode(
    d$sex,
    "Female" = "Femmina",
    "Male" = "Maschio"
  )
  d
  
}
