library(purrr)

map_to_result(lsg_three_four,dependent.var = "preexisting_vulnerability_score",
             case = map_to_case(hypothesis.type = "direct_reporting",dependent.var.type = "categorical"),
             weighting = weights
             
              )

analysisplan <- load_analysisplan(file = "./input/composite_indicators_analysisplan_21022020.csv") 



overalldataframe <- apply(lsg_three_four[,unique(analysisplan$dependent.variable)],2,as.character ) %>% as.data.frame()

overalldataframe <- overalldataframe %>% mutate(strata.names=lsg_three_four$strata.names)

results <- from_analysisplan_map_to_output(data = overalldataframe,
                                           analysisplan = analysisplan,
                                           weighting = weights,
                                           questionnaire = questionnaire
)


summary.stats.list <- results$results


## SUMMARY STATS LIST FORMATTED WITH p-VALUES ##s
summary.stats.list %>%
  lapply((map_to_labeled),questionnaire) %>%
  lapply(result_format_numbers) %>% 
  lapply(add_p_value_to_summary_table) %>% 
  resultlist_summary_statistics_as_one_table %>%
  
  write.csv("./newoutput/summary_stats_persectorOverAll(capacityGap4).csv") 


overalldataframe$preexisting_vulnerability_index1
