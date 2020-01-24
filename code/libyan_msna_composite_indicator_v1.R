many_records <- read.csv("./input/libyan_msna_composite_indicator_v1.csv")

ci_data <- ci_prep_data %>% recode_batch(tos = many_records$to_values,
                                         wheres = many_records$conditions,
                                         targets = many_records$target_variables,
                                         questionnaire = questionnaire) %>%
  end_recoding()


composite_data <- ci_data %>% 
  mutate(preexisting_vulnerability_score = ifelse(preexisting_vulnerability_index1 >= 3 & !is.na(preexisting_vulnerability_index1) | 
                                                  preexisting_vulnerability_index6 >= 3 & !is.na(preexisting_vulnerability_index6) | 
                                                  preexisting_vulnerability_index7 >= 3 & !is.na(preexisting_vulnerability_index7), 
                                                  pmax(preexisting_vulnerability_index1,preexisting_vulnerability_index6,
                                                       preexisting_vulnerability_index7, na.rm = T),
                                                  rounded_mean(rowMeans(select(., preexisting_vulnerability_index1:preexisting_vulnerability_index7), na.rm = T))),
         
         impact_score = ifelse(impact_index5 >= 3 & !is.na(impact_index5)|
                               impact_index8 >= 3 & !is.na(impact_index8)|
                               impact_index1 >= 3 & !is.na(impact_index1), 
                               pmax(impact_index1,impact_index5,impact_index8,na.rm = T), 
                               rounded_mean(rowMeans(select(., impact_index1:impact_index9), na.rm = T))),
         
         protection_score = ifelse(protection_index2 >= 3 & !is.na(protection_index2) |
                                   protection_index5 >= 3 & !is.na(protection_index5), 
                                   pmax(protection_index2,protection_index5,na.rm = T), 
                                   rounded_mean(rowMeans(select(.,protection_index1:protection_index6), na.rm = T))),
         
         shelter_nfi_score = ifelse(shelter_nfi_index2 >= 3 & !is.na(shelter_nfi_index2)|
                                    shelter_nfi_index1 >= 3 & !is.na(shelter_nfi_index1), 
                                    pmax(shelter_nfi_index1,shelter_nfi_index2,na.rm = T),
                                    rounded_mean(rowMeans(select(., shelter_nfi_index1:shelter_nfi_index4), na.rm = T))),
         
         fs_score = ifelse(fcs_index1 >= 4 & !is.na(fcs_index1), 
                           fcs_index1, 
                           rounded_mean(rowMeans(select(., fcs_index1:fcs_index4), na.rm = T))),
         
         health_score = health_index1,
         
         wash_score = ifelse(wash_index1 >= 3 & !is.na(wash_index1) |
                             wash_index3 >= 3 & !is.na(wash_index3), 
                             pmax(wash_index1,wash_index3,na.rm = T), 
                             rounded_mean(rowMeans(select(., wash_index1:wash_index5), na.rm = T))),
         
         education_score = ifelse(education_index2 >= 4 & !is.na(education_index2) |
                                  education_index1 >= 3 & !is.na(education_index1),
                                  pmax(education_index1,education_index2,na.rm = T), 
                                  rounded_mean(rowMeans(select(., education_index1:education_index2), na.rm = T))),
         
         capacity_gap_score = capacitygap_index1
  )


lsg_three_four <- composite_data %>% mutate(
  protection = ifelse(protection_score >=3,1,0),
  shelter = ifelse(shelter_nfi_score >=3,1,0),
  fs = ifelse(fs_score >=3,1,0),
  health = ifelse(health_score>=3,1,0),
  wash = ifelse(health_score>=3,1,0),
  education = ifelse(education_score>=3,1,0),
  capacity_gap = ifelse(capacity_gap_score>=3,1,0),
  vul_atlease_one = ifelse(protection+shelter+fs+health+wash+education+capacity_gap>=1,"yes","no")
)

results <- from_analysisplan_map_to_output(data = lsg_three_four,
                                           analysisplan = analysisplan,
                                           weighting = weights,
                                           questionnaire = questionnaire
)

#map_to_master_table(results_object = results$results, filename = "./output/composite_indicators_results_24012020.csv")

summary.stats.list <- results$results


## SUMMARY STATS LIST FORMATTED WITH p-VALUES ##
summary.stats.list %>%
  #lapply((map_to_labeled),questionnaire) %>%
  lapply(result_format_numbers) %>% 
  lapply(add_p_value_to_summary_table) %>% 
  resultlist_summary_statistics_as_one_table %>%
  
  write.csv("./output/summary_stats_24012020.csv")
