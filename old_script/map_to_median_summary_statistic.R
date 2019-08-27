map_to_median_summary_statistic <-
  function(design,
           dependent.var,
           independent.var,
           case,
           questionnaire = NULL) {
    # sanitise input:
    # check if dependent is a select_multiple:

    if (case == "CASE_group_difference_median_numerical_categorical") {
      summary_stat <-
        median_with_confints_groups(
          dependent.var = dependent.var,
          independent.var = independent.var,
          design = design
        )
    }

    if (case == "CASE_direct_reporting_median_numerical_") {
      summary_stat <- median_with_confints(
        dependent.var = dependent.var,
        design = design
      )
    }

    return(summary_stat)

  }
