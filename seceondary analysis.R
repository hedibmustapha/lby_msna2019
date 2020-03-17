
secondary_analysis <- lsg_three_four %>% mutate(
  request_1 = ifelse(vul_atlease_one == "yes" & preexisting_vulnerability_score >=3,"yes","no"),
  request_2 = ifelse(vul_atlease_one == "yes" & sm_selected(x = humanitarian_assistance_barriers,
                                                            none = c("no_barriers_humanitarian_assistance","notaware_ofany_assistance","dk","dwta")),"yes","no"),
  request_3 = ifelse(vul_atlease_one == "yes" & sm_selected(x = humanitarian_assistance_barriers,
                                                            any = c("notaware_ofany_assistance")),"yes","no"),  
  request_4 = ifelse(vul_atlease_one == "yes" & sm_selected(x = humanitarian_assistance_barriers,
                                                            exactly = c("no_barriers_humanitarian_assistance")),"yes","no")
)


seceondary_analysis_plan = load_analysisplan("./input/analysisplan_secondary_analysis.csv")


results <- from_analysisplan_map_to_output(data = secondary_analysis,
                                           analysisplan = seceondary_analysis_plan,
                                           weighting = weights,
                                           questionnaire = questionnaire
)



map_to_master_table(results_object = results$results, filename = "./output/secondary_analysis_result.csv")

summary.stats.list <- results$results


## SUMMARY STATS LIST FORMATTED WITH p-VALUES ##s
summary.stats.list %>%
  lapply((map_to_labeled),questionnaire) %>%
  lapply(result_format_numbers) %>% 
  lapply(add_p_value_to_summary_table) %>% 
  resultlist_summary_statistics_as_one_table %>%
  
  write.csv("./output/secondary_analysis_result_v2.csv")

