#Have any children in your household ever faced any of these issues when attending school?
dm_data <- dm_data %>% new_recoding(facedd_atleast_one_issue_while_attending) %>%
  recode_to(to = "yes",
            where= sm_selected(issues_faced_attending_school, none = c("no_issue_faced_attending_school")) & 
              !is.na(issues_faced_attending_school)) %>%
  recode_to(to = "no",
            where =sm_selected(issues_faced_attending_school, exactly = c("no_issue_faced_attending_school")) & 
              !is.na(issues_faced_attending_school)) %>% 
  end_recoding()


dm_data <- dm_data %>% new_recoding(lack_doc_disrupt_access_service) %>%
  recode_to(to = "yes",
            where = sm_selected(losing_legal_docs_impact, none = c("losing_docs_no_impact")) & !is.na(losing_legal_docs_impact))%>%
  recode_to(to = "no",
            where=  is.na(losing_legal_docs_impact)) %>%
  recode_to(to = "no",
            where=sm_selected(losing_legal_docs_impact, exactly = c("losing_docs_no_impact"))) %>%  end_recoding()

 dm_data <- dm_data %>% new_recoding(clean_water_source_less500m) %>%
   recode_to(to ="no",
             where = !(primary_drinkingwater_source %in% c("public_network","water_trucking","bottled_water","public_tap","protected_well","rainwater")) |
               distance_drinkingwater == c("more_500m")) %>%
  recode_to(to = "yes",
            where = primary_drinkingwater_source %in% c("public_network","water_trucking")) %>%
  recode_to(to = "yes",
            where = primary_drinkingwater_source %in% c("bottled_water","public_tap","protected_well","rainwater") & 
              distance_drinkingwater != c("more_500m")) %>% 
   end_recoding()


 dm_data <- dm_data %>% new_recoding(clean_water_source_less500m_sufficient) %>%
   recode_to(to ="no",
             where = !(primary_drinkingwater_source %in% c("public_network","water_trucking","bottled_water","public_tap","protected_well","rainwater")) |
               distance_drinkingwater == c("more_500m") | unsufficient_quantity_water %in%  c("yes", "dk","dwta")) %>%
   recode_to(to = "yes",
             where = primary_drinkingwater_source %in% c("public_network","water_trucking")& unsufficient_quantity_water == "no") %>%
   recode_to(to = "yes",
              where = primary_drinkingwater_source %in% c("bottled_water","public_tap","protected_well","rainwater") & 
                distance_drinkingwater != c("more_500m")& unsufficient_quantity_water == "no") %>%  end_recoding()

 dm_data <- dm_data %>% new_recoding(target = pour_flush_toilet, source = toilet_type) %>%
   recode_to(to = "yes",
             where = sm_selected(toilet_type, any = c("flush_toilet","pour_toilet"))) %>% 
   recode_to(to = "no",
             where = sm_selected(toilet_type, none  = c("flush_toilet","pour_toilet"))) %>% end_recoding()
 
dm_data %>% summarize(percent_response(clean_water_source_less500m,.,"yes"))
dm_data %>% summarize(percent_response(clean_water_source_less500m_sufficient,.,"yes"))
dm_data %>% summarize(percent_response(pour_flush_toilet,.,"yes"))
dm_data %>% summarize(percent_response(unsufficient_quantity_water,.,"yes"))

 dm_data %>% group_by(strata.names) %>%
  summarize(losing_legal_docs_impact_pct= percent_response(lack_doc_disrupt_access_service,.,"yes",group = !!get_group(.))) %>%
  write.csv("output/losing_legal_docs_impact_pct.csv")

dm_data %>%
  summarize(
    fcs_poor= percent_response(fcs_category, ., "poor"),
            fcs_borderline = percent_response(fcs_category, ., "borderline"),
            fcs_acceptable= percent_response(fcs_category, ., "acceptable"))

dm_data %>% group_by(strata.names) %>% summarize(total_minor = weighted_sum(nb_under18,.,group = !!get_group(.)),
                      working_minor_pct= round(100* weighted_sum(working_minors,., group = !!get_group(.))/total_minor,2))%>%select(-2)%>%
  write.csv("output/working_minor_pct.csv")

dm_data %>% summarize(no_issue_attending_school = percent_response(issues_faced_attending_school,.,"no_issue_faced_attending_school"))
dm_data %>% summarize(no_issue_attending_school = percent_response(faced_atleast_one_issue_while_attending,.,"yes"))

dm_data %>% group_by(strata.names) %>% summarize(faced_atleast_one_issue_while_attending = percent_response(faced_atleast_one_issue_while_attending,.,"yes",group = !!get_group(.))) %>% 
  arrange(desc(faced_atleast_one_issue_while_attending)) %>%
  write.csv("faced_atleast_one_issue_while_attendingschool.csv")


dm_data %>% group_by(strata.names) %>%  summarize(livelihood_coping_atleast_one_perc = 
                                                          percent_response(livelihood_coping_atleast_one,.,"yes",group = !!get_group(.))) %>% 
  write.csv("output/atleast_one_coping_mechanism_strata.csv")

dm_data %>% group_by(mantika_label) %>% summarize(seperated_children=round(weighted_sum(seperated_children_calc,.,group = !!get_group(.)),0)) %>% 
  arrange(desc(seperated_children))%>%
  write.csv("output/seperated_children_mantika.csv")

dm_data %>% summarize(livelihood_coping_atleast_one_perc = 
                                                           percent_response(livelihood_coping_atleast_one,.,"yes"))

dm_data %>%  summarize(
  livelihood_coping_atleast_one_perc_idp = percent_response(filter(., displacement_status =="idp")[["livelihood_coping_atleast_one"]],
                                                           filter(., displacement_status =="idp"),
                                                            "yes",
                                                            x_name = "livelihood_coping_atleast_one"))



svyby(~total_working_minors, ~mantika_label, svytotal, design = strata_design, keep.var = F,na.rm=T)[2] / svyby(~nb_under18, ~mantika_label, svytotal, design = strata_design, na.rm=T)[2] *100


dm_data %>% summarize(fcs_borderline_or_poor= percent_response(fcs_category,.,"borderline")+
                        percent_response(fcs_category,.,"poor"))
