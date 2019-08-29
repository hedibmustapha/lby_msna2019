rm(list = ls())

library(hypegrammaR)
library(parallel)
library(dplyr)


questions <- read.csv("./input/survey.csv", stringsAsFactors = F)
choices <- read.csv("./input/choices.csv", stringsAsFactors = F)
data <- load_data(file = "./input/data.csv")
sampling_frame <- load_samplingframe(file = "./input/sampling_frame.csv")
questionnaire <- load_questionnaire(data = data,
                                    questions = questions,
                                    choices = choices,
                                    choices.label.column.to.use = "label::English (en)")
analysisplan <- load_analysisplan(file = "./input/analysisplan_medians.csv")
#analysisplan <- make_analysisplan_all_vars(df = data,questionnaire = questionnaire,repeat.for.variable = "mantika_label")

kobostandards::check_input(data = data, questions = questions, choices = choices ,samplingframe = sampling_frame,
                           analysisplan = analysisplan) %>% write.csv("./output/check_input.csv")

weights <-map_to_weighting(sampling.frame = sampling_frame,
                           data.stratum.column = "strata.names",
                           sampling.frame.population.column = "population",
                           sampling.frame.stratum.column = "strata.names",
                           data = data)

#weights(data) %>% write.csv("./output/weightes.csv")

results <- from_analysisplan_map_to_output(data = data,
                                           analysisplan = analysisplan,
                                           weighting = weights,
                                           questionnaire = questionnaire
                                           )

labeled_results <- lapply(results$results, map_to_labeled,questionnaire)

map_to_master_table(results_object = labeled_results, filename = "./output/medians_split.csv")

#hypegrammaR:::map_to_generic_hierarchical_html(resultlist = results,
                                               #render_result_with = hypegrammaR:::from_result_map_to_md_table,
                                               #by_analysisplan_columns = c("dependent.var", "repeat.var.value"),
                                               #by_prefix = c("indicator: ","Mantika:"),
                                               #level = 2,
                                               #questionnaire = questionnaire,
                                               #dir = "./output",
                                               #filename = "overall_Mantika_output.html")


group_diff_case <- map_to_case(hypothesis.type = "group_difference_median",
                    dependent.var.type = "numerical",
                    independent.var.type = "categorical")

case_result<-map_to_result(data = data,
                      dependent.var = "own_business_income",
                      independent.var = "displacement_status",
                      case = group_diff_case,
                      weighting = weights,
                      questionnaire = questionnaire)

median_direct_case <- map_to_case(hypothesis.type = "direct_reporting_median",
                    dependent.var.type = "numerical")

direct_case <- map_to_case(hypothesis.type = "direct_reporting",
                           dependent.var.type = "categorical")

case_result<- map_to_result(data = data,
                           dependent.var = "hoh",
                           case = direct_case,
                           questionnaire = questionnaire,
                           weighting = weights
                           )

#case_result %>% map_to_labeled(questionnaire) -> result_labeled
#map_to_file(result_labeled$summary.statistic,"./output/summary_statistics.csv")
