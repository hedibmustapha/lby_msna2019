rm(list = ls())

library(hypegrammaR)
library(parallel)

questions <- read.csv("./input/survey.csv", stringsAsFactors = F)
choices <- read.csv("./input/choices.csv", stringsAsFactors = F)
data <- load_data(file = "./input/data.csv")
sampling_frame <- load_samplingframe(file = "./input/sampling_frame.csv")
questionnaire <- load_questionnaire(data = data,
                                    questions = questions,
                                    choices = choices,
                                    choices.label.column.to.use = "label::English (en)")
analysisplan <- load_analysisplan(file = "./input/analysisplan.csv")


kobostandards::check_input(data = data, questions = questions, choices = choices ,samplingframe = sampling_frame,
                           analysisplan = analysisplan) %>% write.csv("check_input.csv")



weights <-map_to_weighting(sampling.frame = sampling_frame,
                           data.stratum.column = "strata.names",
                           sampling.frame.population.column = "population",
                           sampling.frame.stratum.column = "strata.names",
                           data = data)


weighted_results <- from_analysisplan_map_to_output(data = data,
                                                    analysisplan = analysisplan,
                                                    weighting = weights,
                                                    questionnaire = questionnaire)

weighted_results_labeled <- lapply(weighted_results$results, map_to_labeled,questionnaire)
map_to_master_table(results_object = weighted_results_labeled, filename = "./output/bio_food_shelter.csv")
