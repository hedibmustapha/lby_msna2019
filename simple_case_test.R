rm(list = ls())

questions <- read.csv("./input/survey.csv")
choices <- read.csv("./input/choices.csv")
data <- load_data(file = "./input/data.csv")
sampling_frame <- load_samplingframe(file = "./input/sampling_frame.csv")
weights <-map_to_weighting(sampling.frame = sampling_frame,
                           data.stratum.column = "strata.names",
                           sampling.frame.population.column = "population",
                           sampling.frame.stratum.column = "strata.names",
                           data = data)

q <- load_questionnaire(data = data,
                        questions = questions,
                        choices = choices,
                        choices.label.column.to.use = "label::English (en)")

q$question_type("food_source")
q$question_is_select_multiple("food_source")

case <- map_to_case(hypothesis.type = "direct_reporting",
                    dependent.var.type = "categorical")

result<-map_to_result(data = data,
                      dependent.var = "food_source",
                      case = case,
                      weighting = weights,
                      questionnaire = q)


result %>% map_to_labeled(q) -> result_labeled

map_to_file(result_labeled$summary.statistic,"./output/labeled_summary_statistics.csv")
map_to_file(result$summary.statistic,"./output/summary_statistics.csv")
