# fit_subset_pol <- reference_proj_fit
# fit_subset_covid <- test_subset_fit


#' @title Coefficients plot.
#' @description Area coefficient plot for both the political news and
#' the covid data. Only the coefficients indicated in the political news
#' data set are used.
#' @return A brm object.
#' @export
#' @param ggplot object. The plot is also saved in the figs directory.
plot_subset_coefs_pol_and_covid <- 
  function(fit_subset_pol, fit_subset_covid) {
  
  library("bayesplot")
  bayesplot_theme_set(theme_default(base_family = "sans"))
  color_scheme_set("gray")
  # political news
  # get_variables(fit_subset_pol)
  posterior_pol <- as.array(fit_subset_pol)
  # dim(posterior)
  coef_plot_pol <- mcmc_areas(
    posterior_pol, 
    # discernment ~ cosp + poli + sex + educ + crit + age + conv + miis
    pars = c("b_age", "b_sex", "b_educ", "b_poli", "b_cosp", "b_miis", 
             "b_crit", "b_conv"),
    prob = 0.8, 
    prob_outer = 0.95, 
    point_est = "mean") + 
    coord_cartesian(xlim = c(-0.45, 0.45)) +
    scale_y_discrete(
      labels = c(
        "b_age" = "Age", 
        "b_sex"  = "Sex",
        "b_educ" = "Education level",
        "b_poli" = "Liberal orientation",
        "b_miis" = "Magical Ideation Scale",
        "b_cosp" = "FNSS-Conspiracy beliefs", 
        "b_crit" = "FNSS-Critical news consumption",
        "b_conv" = "RWAS-Conventionalism"
      )
    ) +
    labs(x = 'Coefficient size (95% CI)',
         title = 'Political news') +
    theme(axis.title = element_text(size = 13)) +
    theme(axis.text.y = element_text(size = 10, vjust = 1)) # angle = 325, 
  # coef_plot_pol  
  # check the direction of the sex coefficient
  # plot(
  #   conditional_effects(reference2_fit, effects = "sex"),
  #   points = FALSE, rug = FALSE
  # )
  
  # covid-19 news
  # get_variables(test2_fit)
  posterior_covid <- as.array(fit_subset_covid)
  # dim(posterior2)
  coef_plot_covid <- mcmc_areas(
    posterior_covid, 
    pars = c("b_age", "b_sex", "b_educ", "b_poli", "b_cosp", "b_miis", 
             "b_crit", "b_conv"),
    prob = 0.8, 
    prob_outer = 0.95, 
    point_est = "mean") + 
    coord_cartesian(xlim = c(-0.45, 0.45)) +
    scale_y_discrete(
      labels = c(
        "b_age" = "Age", 
        "b_sex"  = "Sex",
        "b_educ" = "Education level",
        "b_poli" = "Liberal orientation",
        "b_miis" = "Magical Ideation Scale",
        "b_cosp" = "FNSS-Conspiracy beliefs", 
        "b_crit" = "FNSS-Critical news consumption",
        "b_conv" = "RWAS-Conventionalism"
      )
    ) +
    labs(x = 'Coefficient size (95% CI)',
         title = 'Covid-19 news') +
    theme(axis.title = element_text(size = 13)) +
    theme(axis.text.y = element_text(angle = 325, size = 10, vjust = 1)) +
    theme(axis.text.y = element_blank())
  # coef_plot_covid  
  # check the direction of the sex coefficient
  # plot(
  #   conditional_effects(test2_fit, effects = "sex"),
  #   points = FALSE, rug = FALSE
  # )
  
  fig_coefs <- coef_plot_pol + coef_plot_covid
  fig_coefs <- fig_coefs + 
    patchwork::plot_annotation(tag_levels = 'A') & 
    theme(plot.tag = element_text(size = 24))
  ggsave(
    here("manuscript", "figs", "coefs.pdf"), 
    width = 30, 
    height = 15, 
    units = "cm"
  )
  return("coefs.pdf")
}

#--------------------------------------------------------------------
# ref_fit <- td_ref_covid_fit
# proj_fit <- td_proj_covid_fit
# XLAB <- "Bayesian R-squared\nTruth discernment about COVID-19 news data"

plot_R2_distributions <- function(ref_fit, proj_fit, XLAB, PLOT_NAME) {
  
  library("bayesplot")
  bayesplot_theme_set(theme_default()) # base_family = "sans"
  color_scheme_set("gray")
  
  # Bayesian R2 of the reference model (13 predictors) vs of the projected 
  # submodel (7 predictors; both showing 95% CI) -- political news data-set
  
  # political news
  ref_fit_r2_sims <- bayes_R2(ref_fit, summary = FALSE) 
  proj_fit_r2_sims <- bayes_R2(proj_fit, summary = FALSE) 
  
  both_sims_pol <- tibble(
    Model = c(rep('reference', 40000), rep('projection', 40000)), 
    sims = c(ref_fit_r2_sims, proj_fit_r2_sims)
  )
  
  r2 <- both_sims_pol %>%
    mutate(model = ifelse(Model == 'reference', 'Reference', 'Projection')) %>% 
    ggplot(aes(sims, fill = Model)) +
    geom_density(alpha = 0.5, col = NA) +
    scale_fill_grey(start = 0.1, end = 0.4) +
    labs(
      x = XLAB,
      y = "Density"
    ) +
    #xlim(c(0.2, 0.5)) +
    bayesplot::bayesplot_theme_set() +
    theme(legend.position = "bottom") 
  
  plot_name <- paste(PLOT_NAME, ".pdf", sep = "")
  ggsave(
    here("manuscript", "figs",  plot_name), 
    width = 10, 
    height = 10, 
    units = "cm"
  )
  return(plot_name)
}

#--------------------------------------------------------------------
# vs_pol <- varsel_td_pol
# pol_data <- pol_dat
# fit_reference_pol <- td_ref_pol_fit

save_varsel_plot <- function(
  vs_pol,
  pol_data,
  fit_reference_pol
) {
  pred <- projpred::proj_linpred(
    vs_pol,
    newdata = pol_data,
    nterms = projpred::suggest_size(vs_pol),
    integrated = TRUE
  )

  set.seed(123456)
  preds_reference_fit <- predict(fit_reference_pol)[, 1]
  cor1 <- cor(preds_reference_fit, pred$pred)
  cor1

  reference_vs_submodel_predictions <- ggplot() +
    geom_point(aes(x = pred$pred, y = preds_reference_fit)) +
    geom_abline(slope = 1, color = "gray") +
    labs(x = "Submodel predictions",
         y = "Reference model predictions") +
    annotate(geom = 'text',
             y = 2,
             x = -0.75,
             label = paste('italic(r) ==', round(cor1, 3)),
             parse = TRUE,
             size = 7) +
    papaja::theme_apa() +
    theme(axis.title = element_text(size = 18))

  # Predictive projection feature selection trajectory, showing the increase
  # in ELPD/decrease in RMSE as more predictors are added.
  proj_fig <- plot(
    vs_pol,
    stats = c('elpd', 'rmse')
  ) +
    papaja::theme_apa() +
    theme(
      axis.title = element_text(size = 18),
      legend.position = "none"
    )

  fig_varsel <- proj_fig + reference_vs_submodel_predictions
  fig_varsel <- fig_varsel +
    patchwork::plot_annotation(tag_levels = 'A') & theme(plot.tag = element_text(size = 24))
  ggsave(here("manuscript", "figs", "varsel.pdf"), width = 30, height = 15, units = "cm")
  return("varsel.pdf")
}


#--------------------------------------------------------------------

save_best_subset_covid_plot <- function(ob_best_subset_covid_fit) {

  bayesplot::bayesplot_theme_update(text = element_text(size = 16, family = "sans"))
  me_conf <- conditional_effects(
    ob_best_subset_covid_fit, "conf"
  )
  p1 <- plot(me_conf, plot = FALSE)[[1]] +
    scale_color_grey() +
    scale_fill_grey() +
    labs(
      x = "FNSS Self-belief",
      y = "Overall belief"
    ) +
    bayesplot::bayesplot_theme_set() 

  bayesplot::bayesplot_theme_update(text = element_text(size = 16, family = "sans"))
  me_conv <- conditional_effects(
    ob_best_subset_covid_fit, "conv"
  )
  p2 <- plot(me_conv, plot = FALSE)[[1]] +
    scale_color_grey() +
    scale_fill_grey() +
    labs(
      x = "RWAS Conventionalism",
      y = "Overall belief"
    ) +
    bayesplot::bayesplot_theme_set() 

  bayesplot::bayesplot_theme_update(text = element_text(size = 16, family = "sans"))
  me_agsu <- conditional_effects(
    ob_best_subset_covid_fit, "agsu"
  )
  p3 <- plot(me_agsu, plot = FALSE)[[1]] +
    scale_color_grey() +
    scale_fill_grey() +
    labs(
      x = "RWAS Aggression-submission",
      y = "Overall belief"
    ) +
    bayesplot::bayesplot_theme_set() 

  patchwork <- (p1 | p2 | p3)
  patchwork + 
    plot_annotation(tag_levels = 'A') & 
    theme(plot.tag = element_text(size = 18))
  
  ggsave(
    here("manuscript", "figs", "ob-covid-optsub.pdf"), 
    width = 30, 
    height = 10, 
    units = "cm"
  )

  return("ob-covid-optsub.pdf")
}

