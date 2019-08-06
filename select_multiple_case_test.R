rm(list = ls())
data <- load_data(file = "./input/data.csv")
sampling_frame <- load_samplingframe(file = "./input/sampling_frame.csv")
questionnaire <- load_questionnaire(data = "./input/data.csv",
                                    questions = "./input/survey.csv",
                                    choices = "./input/choices.csv",
                                    choices.label.column.to.use = "label::English (en)")
weights <-map_to_weighting(sampling.frame = sampling_frame,
                           data.stratum.column = "strata.names",
                           sampling.frame.population.column = "population",
                           sampling.frame.stratum.column = "strata.names",
                           data = data)

case <- map_to_case(hypothesis.type = "direct_reporting",
                    dependent.var.type = "categorical")
result<-map_to_result(data = data,
                      dependent.var = "food_source",
                      case = case,
                      weighting = weights,
                      questionnaire = questionnaire)

result %>% map_to_labeled(questionnaire) -> result_labeled
map_to_file(result_labeled$summary.statistic,"./output/summary_statistics.csv")
