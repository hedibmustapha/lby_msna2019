rm(list = ls())

source("code/map_to_master_table.R")
Sys.setenv("R_REMOTES_NO_ERRORS_FROM_WARNINGS"=TRUE)
remove.packages("hypegrammaR", c(.libPaths(),"C:/Users/REACH/Documents/R/win-library/3.4"))
devtools::install_github("mabafaba/hypegrammaR", build_opts = c())
devtools::install_github("hedibmustapha/hypegrammaR", build_opts = c())
library(hypegrammaR)
library(parallel)
library(dplyr)


questions <- read.csv("./input/edited_survey_datamerge.csv", stringsAsFactors = F)
choices <- read.csv("./input/edited_choices_datamerge.csv", stringsAsFactors = F)
choices$label..English..en. <- gsub("^\\d+[.]\\s*","", choices$label..English..en.)
data <- load_data(file = "./input/data.csv")
data <- mutate_if(data, is.character, na_if, "")
sampling_frame <- load_samplingframe(file = "./input/sampling_frame.csv")

questionnaire <- load_questionnaire(data = data,
                                    questions = questions,
                                    choices = choices,
                                    choices.label.column.to.use = "label::English (en)")
analysisplan <- load_analysisplan(file = "./input/analysisplan.csv")
analysisplan_groups <- load_analysisplan(file = "./input/analysisplan_overall_groups.csv")
#analysisplan <- make_analysisplan_all_vars(df = data,questionnaire = questionnaire,repeat.for.variable = "mantika_label")

kobostandards::check_input(data = data, questions = questions, choices = choices ,samplingframe = sampling_frame,
                           analysisplan = analysisplan_groups) %>% write.csv("./output/check_input.csv")

weights <-map_to_weighting(sampling.frame = sampling_frame,
                           data.stratum.column = "strata.names",
                           sampling.frame.population.column = "population",
                           sampling.frame.stratum.column = "strata.names",
                           data = data)

#weights(data) %>% write.csv("./output/weightes.csv")

results_groups <- from_analysisplan_map_to_output(data = dm_data,
                                           analysisplan = analysisplan,
                                           weighting = weights,
                                           questionnaire = questionnaire 
                                           )
results_mantika <- from_analysisplan_map_to_output(data = data,
                                                  analysisplan = analysisplan_repeat,
                                                  weighting = weights,
                                                  questionnaire = questionnaire
                                            )


labeled_results_groups <- lapply(results_groups$results, map_to_labeled,questionnaire)
#labeled_results_mantika <- lapply(results_mantika$results, map_to_labeled,questionnaire)

map_to_master_table(results_object = results_groups$results, filename = "./output/allvars_groups_hypegrammaR_pvalues.csv",questionnaire = questionnaire)
map_to_master_table(results_object = results_groups$results, filename = "./output/medians.csv")
map_to_master_table(results_object = results_mantika$results, filename = "./output/allvars_mantika_hypegrammaR_pvalues.csv",questionnaire = questionnaire)

saveRDS(results_groups,"output/results_groups.rds")
saveRDS(results_mantika, "output/results_mantika.rds")
#hypegrammaR:::map_to_generic_hierarchical_html(resultlist = results,
                                               #render_result_with = hypegrammaR:::from_result_map_to_md_table,
                                               #by_analysisplan_columns = c("dependent.var", "repeat.var.value"),
                                               #by_prefix = c("indicator: ","Mantika:"),
                                               #level = 2,
                                               #questionnaire = questionnaire,
                                               #dir = "./output",
                                               #filename = "overall_Mantika_output.html")


group_diff_case <- map_to_case(hypothesis.type = "group_difference",
                    dependent.var.type = "numerical",
                    independent.var.type = "categorical")

case_result<-map_to_result(data = data,
                      dependent.var = "nb_displaced_hosted",
                      independent.var = "displacement_status",
                      case = group_diff_case,
                      weighting = weights,
                      questionnaire = questionnaire)

median_direct_case <- map_to_case(hypothesis.type = "direct_reporting_median",
                    dependent.var.type = "numerical")

direct_case <- map_to_case(hypothesis.type = "direct_reporting_sum",
                           dependent.var.type = "numerical")

case_result<- map_to_result(data = data,
                           dependent.var = "nb_displaced_hosted",
                           case = direct_case,
                           weighting = weights,
                           questionnaire = questionnaire
                           )

map_to_visualisation(case_result)

case_result %>% map_to_labeled(questionnaire) -> result_labeled
map_to_file(result_labeled$summary.statistic,"./output/movement_restrictions_reasons.csv")
