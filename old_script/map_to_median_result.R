map_to_median_result<-function(data,
                        dependent.var,
                        independent.var = NULL,
                        case,
                        cluster.variable.name=NULL,
                        weighting=NULL,
                        questionnaire=NULL,
                        strata.names,
                        population){
  #options(survey.lonely.psu = "average")


  # put the relevant input parameters in a list so we can attach them to the output:
  parameters<-list(
    dependent.var = dependent.var,
    independent.var = independent.var,
    cluster.variable.name=cluster.variable.name,
    weighted=weighting,
    case=case
  )

  # map from input to analysis case:
  design <- map_to_design_medians(data = data, strata.names = strata.names,population = population)

  summary.result  <- map_to_median_summary_statistic(design = design,
                                              dependent.var = dependent.var,
                                              independent.var = independent.var,
                                              case = case,
                                              questionnaire = questionnaire)

  return(list(parameters=parameters,
              summary.statistic=summary.result,
              hypothesis.test="NA",
              message="success (or unidentified issue)"
  ))

}

