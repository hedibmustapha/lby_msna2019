from_analysisplan_map_to_medians_output <- function (data, analysisplan, weighting = NULL, cluster_variable_name = NULL, 
          questionnaire = NULL, labeled = FALSE, verbose = TRUE) 
{
  if (is.null(questionnaire)) {
    labeled <- FALSE
  }
  analysisplan <- analysisplan %>% dplyr::rename(repeat.var = repeat.for.variable, 
                                                 dependent.var = dependent.variable, independent.var = independent.variable, 
                                                 dependent.var.type = dependent.variable.type, independent.var.type = independent.variable.type)
  analysisplan <- analysisplan[!is.na(analysisplan$dependent.var), 
                               ]
  analysisplan <- hypegrammaR:::analysisplan_clean(analysisplan)
  analysisplan <- hypegrammaR:::analysisplan_expand_repeat(analysisplan, 
                                             data)
  analysisplan$percentcomplete <- paste0(floor(1:nrow(analysisplan)/nrow(analysisplan) * 
                                                 100), "%\n\n")
  results <- apply(analysisplan, 1, function(x) {
    if (!(x["repeat.var"]) %in% c(NULL, "", " ", 
                                  NA) & !is.na(x["repeat.var"])) {
      this_valid_data <- data[(data[, as.character(x["repeat.var"])] == 
                                 as.character(x["repeat.var.value"])), ]
    }
    else {
      this_valid_data <- data
    }
    if (!(hypegrammaR:::is_good_dataframe(this_valid_data))) {
      result <- list()
      return(result)
    }
    if (verbose) {
      hypegrammaR:::printparamlist(x, "calculating summary statistics and hypothesis tests")
    }
    if (is.na(x["independent.var"]) | is.null(x["independent.var"])) {
      indep.var <- NULL
    }
    else {
      indep.var <- as.character(x["independent.var"])
    }
    case <- x["case"]
    result <- map_to_median_result(data = this_valid_data, dependent.var = x["dependent.var"], 
                            independent.var = indep.var, case = case, cluster.variable.name = cluster_variable_name, 
                            weighting = weighting, questionnaire = questionnaire, strata.names = x["strata.names"],
                            population = x["population"])
    if (!is.null(x["repeat.var"]) & (!is.na(x["repeat.var"]))) {
      result$parameters$repeat.var <- x["repeat.var"]
      result$parameters$repeat.var.value <- x["repeat.var"]
    }
    else {
      result$parameters$repeat.var <- NA
      result$parameters$repeat.var.value <- NA
    }
    if (labeled) {
      result <- map_to_labeled(result, questionnaire)
    }
    if (!is.null(result$summary.statistic)) {
      if (nrow(result$summary.statistic) > 0) {
        result$summary.statistic$repeat.var <- x["repeat.var"]
        result$summary.statistic$repeat.var.value <- x["repeat.var.value"]
      }
      else {
        result$summary.statistic$repeat.var <- character(0)
        result$summary.statistic$repeat.var.value <- character(0)
      }
    }
    return(result)
  })
  all_results <- list(results = results, analysisplan = analysisplan)
  class(all_results) <- "hypegrammar_resultlist"
  return(all_results)
}