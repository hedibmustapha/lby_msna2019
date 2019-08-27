median_with_confints <- function(dependent.var,
                               independent.var = NULL,
                               design) {
  if (!is.null(independent.var)) {
    warning(
      "confidence intervals calculated without disaggregation, but received data for an independent variable."
    )
  }
  formula_string <- paste0("~", dependent.var)
  summary <- svyquantile(formula(formula_string), design, na.rm = T,quantiles=0.5,ci=T)
  #confints <- confint(summary, level = 0.95)
  results <- data.frame(
    dependent.var = dependent.var,
    independent.var = "NA",
    dependent.var.value = "NA",
    independent.var.value = "NA",
    numbers = summary$quantiles[1],
    se = "NA",
    min = summary$CIs[1],
    max = summary$CIs[2]
  )
  return(results)
}

median_with_confints_groups <- function(dependent.var,
                                      independent.var,
                                      design) {
  formula_string <- paste0("~", dependent.var)
  by <- paste0("~", independent.var)


  result_svy_format <-
    svyby(
      formula(formula_string),
      formula(by),
      design,
      svyquantile,
      na.rm = T,
      quantiles=0.5,
      ci=T
    )
  confints <- confint(result_svy_format, level = 0.95)
  unique.independent.var.values <-
    design$variables[[independent.var]] %>% unique
  results <- unique.independent.var.values %>%
    lapply(function(x) {
      dependent_value_x_stats <- result_svy_format[as.character(x), ]
      colnames(dependent_value_x_stats) <-
        c("independent.var.value", "numbers", "SE")
      data.frame(
        dependent.var = dependent.var,
        independent.var = independent.var,
        dependent.var.value = NA,
        independent.var.value = x,
        numbers = dependent_value_x_stats[2],
        se = dependent_value_x_stats[3],
        min = confints[1],
        max = confints[2]
      )
    }) %>% do.call(rbind, .)

  return(results)
}
