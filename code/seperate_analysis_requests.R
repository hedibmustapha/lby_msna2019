many_records <- read.csv("input/many_records.csv")
 
seperate_request_data <- data %>% recode_batch(tos = many_records$to_values,
                              wheres = many_records$conditions,
                              targets = many_records$target_variables,
                              questionnaire = questionnaire) %>%
   end_recoding()

dm_data <- merge(unique(region), dm_data, by='mantika_label')

dm_data <- dm_data %>% mutate( 
  pregannt_woman = ifelse(nb_pregnant_women ==0 | is.na(nb_pregnant_women), "no", "yes"))
dm_data <- dm_data %>% mutate( 
  children_bottled_milk = ifelse(fed_children_bottled_milk ==0 | is.na(fed_children_bottled_milk), "no", "yes"),
  children_bottled_milk_subset =ifelse(fed_children_bottled_milk ==0 , "no", "yes"))
dm_data <- dm_data %>% mutate(
  nb_under18_health = ifelse(nb_under18 ==0, NA, nb_under18)
)

## Wash section
dm_data %>% summarize(percent_response(clean_water_source_less500m,.,"yes"))
dm_data %>% summarize(percent_response(clean_water_source_less500m_sufficient,.,"yes"))
dm_data %>% summarize(percent_response(pour_flush_toilet,.,"yes"))



## Protection section
 dm_data %>% group_by(strata.names) %>%
  summarize(losing_legal_docs_impact_pct= percent_response(lack_doc_disrupt_access_service,.,"yes",group = !!get_group(.)))
 
 
 
## Food section
 dm_data %>%
  summarize(fcs_poor= percent_response(fcs_category, ., "poor"),
            fcs_borderline = percent_response(fcs_category, ., "borderline"),
            fcs_acceptable= percent_response(fcs_category, ., "acceptable"),
            rcsi_low= percent_response(rcsi_category,.,"low"),
            rcsi_medium= percent_response(rcsi_category,.,"medium"),
            rcsi_high= percent_response(rcsi_category,.,"high")) %>% 
   write.csv("../final_analysis/analysis_requests/overall_food_security.csv")
 
 dm_data %>% group_by(strata.names) %>%
   summarize(fcs_poor= percent_response(fcs_category, ., "poor", group = !!get_group(.)),
             fcs_borderline = percent_response(fcs_category, ., "borderline", group = !!get_group(.)),
             fcs_acceptable= percent_response(fcs_category, ., "acceptable", group = !!get_group(.)),
             rcsi_low= percent_response(rcsi_category,.,"low", group = !!get_group(.)),
             rcsi_medium= percent_response(rcsi_category,.,"medium", group = !!get_group(.)),
             rcsi_high= percent_response(rcsi_category,.,"high", group = !!get_group(.))) %>% 
   write.csv("../final_analysis/analysis_requests/strata_food_security.csv")
 
 dm_data %>% summarize(fcs_borderline_or_poor= percent_response(fcs_category,.,"borderline")+
                         percent_response(fcs_category,.,"poor"))

 
 
 ## Cash&Market
dm_data %>% group_by(strata.names) %>% 
  summarize(total_minor = weighted_sum(nb_under18,.,group = !!get_group(.)),
            working_minor_pct= round(100* weighted_sum(working_minors,., group = !!get_group(.))/total_minor,2))

dm_data %>% 
  summarize(total_minor = weighted_sum(nb_under18,.),
            working_minor_pct= weighted_sum(working_minors,.)/total_minor * 100)

dm_data %>% 
  summarize(total_working_minor= weighted_sum(working_minors,.),
            working_minor_male_pct= weighted_sum(working_minors_male,.)/total_working_minor * 100,
            working_minor_female_pct= weighted_sum(working_minors_female,.)/total_working_minor * 100) %>% select(-total_working_minor)%>%
  write.csv("../final_analysis/analysis_requests/working_minor_bygender.csv")

dm_data %>% group_by(strata.names) %>%  
  summarize(livelihood_coping_atleast_one_perc = 
              percent_response(livelihood_coping_atleast_one,.,"yes",group = !!get_group(.)))

dm_data %>% summarize(livelihood_coping_atleast_one_perc =percent_response(livelihood_coping_atleast_one,.,"yes"))


dm_data %>%  summarize(
  livelihood_coping_atleast_one_perc_idp = 
    percent_response(filter(., displacement_status =="idp")[["livelihood_coping_atleast_one"]],
                     filter(., displacement_status =="idp"),
                     "yes",
                     x_name = "livelihood_coping_atleast_one"))

dm_data %>% summarize(
  total_expenditures_weighted = weighted_sum(total_expenditures,.),
  health_expenditure_share = weighted_sum(health_related_expenditure,.) / total_expenditures_weighted *100) %>% 
  select(-total_expenditures_weighted) %>% write.csv("output/overall_health_expenditure_share.csv")

dm_data %>% group_by(mantika_label) %>% summarize(
  total_expenditures_weighted = weighted_sum(total_expenditures,., group = !!get_group(.)),
  health_expenditure_share = weighted_sum(health_related_expenditure,.,group = !!get_group(.)) / total_expenditures_weighted *100
) %>% select(-total_expenditures_weighted) %>% write.csv("output/health_expenditure_share.csv")

dm_data %>% group_by(displacement_status) %>% summarize(
  total_adult = weighted_sum(nb_over18,.,group = !!get_group(.)),
  total_permanent_job_adult = weighted_sum(permanent_job_adult,., group = !!get_group(.)) / total_adult * 100,
  total_temporary_job_adult = weighted_sum(temporary_job_adult,., group = !!get_group(.)) / total_adult * 100,
  total_daily_labour_job_adult = weighted_sum(daily_labour_job_adult,., group = !!get_group(.)) / total_adult * 100,
  total_gvt_payroll_job_adult = weighted_sum(gvt_payroll_job_adult,., group = !!get_group(.)) / total_adult * 100,
  total_other_job_adult_adult = weighted_sum(other_job_adult_adult,., group = !!get_group(.)) / total_adult * 100,
  
  total_gvt_public_sector_adult = weighted_sum(gvt_public_sector_adult,., group = !!get_group(.)) / total_adult * 100,
  total_libyan_owned_business_adult = weighted_sum(libyan_owned_business_adult,., group = !!get_group(.)) / total_adult * 100,
  total_foreign_owned_business_adult = weighted_sum(foreign_owned_business_adult,., group = !!get_group(.)) / total_adult * 100,
  total_libyan_ngos_csos_adult = weighted_sum(libyan_ngos_csos_adult,., group = !!get_group(.)) / total_adult * 100,
  total_international_ngos_adult = weighted_sum(international_ngos_adult,., group = !!get_group(.)) / total_adult * 100,
  total_own_family_business_adult = weighted_sum(own_family_business_adult,., group = !!get_group(.)) / total_adult * 100,
  total_informal_irregular_labour_adult = weighted_sum(informal_irregular_labour_adult,., group = !!get_group(.)) / total_adult * 100,
  total_other_institution_adult = weighted_sum(other_institution_adult,., group = !!get_group(.)) / total_adult * 100
) %>% select(-total_adult) %>% write.csv("../final_analysis/analysis_requests/type_jobs_institutions_groups.csv")

dm_data %>% summarize(
  total_adult = weighted_sum(nb_over18,.),
  total_permanent_job_adult = weighted_sum(permanent_job_adult,.) / total_adult * 100,
  total_temporary_job_adult = weighted_sum(temporary_job_adult,.) / total_adult * 100,
  total_daily_labour_job_adult = weighted_sum(daily_labour_job_adult,.) / total_adult * 100,
  total_gvt_payroll_job_adult = weighted_sum(gvt_payroll_job_adult,.) / total_adult * 100,
  total_other_job_adult_adult = weighted_sum(other_job_adult_adult,.) / total_adult * 100,
  
  total_gvt_public_sector_adult = weighted_sum(gvt_public_sector_adult,.) / total_adult * 100,
  total_libyan_owned_business_adult = weighted_sum(libyan_owned_business_adult,.) / total_adult * 100,
  total_foreign_owned_business_adult = weighted_sum(foreign_owned_business_adult,.) / total_adult * 100,
  total_libyan_ngos_csos_adult = weighted_sum(libyan_ngos_csos_adult,.) / total_adult * 100,
  total_international_ngos_adult = weighted_sum(international_ngos_adult,.) / total_adult * 100,
  total_own_family_business_adult = weighted_sum(own_family_business_adult,.) / total_adult * 100,
  total_informal_irregular_labour_adult = weighted_sum(informal_irregular_labour_adult,.) / total_adult * 100,
  total_other_institution_adult = weighted_sum(other_institution_adult,.) / total_adult * 100
) %>% select(-total_adult) %>% write.csv("../final_analysis/analysis_requests/type_jobs_institutions_overall.csv")

dm_data %>% group_by(mantika_label) %>% summarize(
  total_adult = weighted_sum(nb_over18,.,group = !!get_group(.)),
  total_permanent_job_adult = weighted_sum(permanent_job_adult,., group = !!get_group(.)) / total_adult * 100,
  total_temporary_job_adult = weighted_sum(temporary_job_adult,., group = !!get_group(.)) / total_adult * 100,
  total_daily_labour_job_adult = weighted_sum(daily_labour_job_adult,., group = !!get_group(.)) / total_adult * 100,
  total_gvt_payroll_job_adult = weighted_sum(gvt_payroll_job_adult,., group = !!get_group(.)) / total_adult * 100,
  total_other_job_adult_adult = weighted_sum(other_job_adult_adult,., group = !!get_group(.)) / total_adult * 100,
  
  total_gvt_public_sector_adult = weighted_sum(gvt_public_sector_adult,., group = !!get_group(.)) / total_adult * 100,
  total_libyan_owned_business_adult = weighted_sum(libyan_owned_business_adult,., group = !!get_group(.)) / total_adult * 100,
  total_foreign_owned_business_adult = weighted_sum(foreign_owned_business_adult,., group = !!get_group(.)) / total_adult * 100,
  total_libyan_ngos_csos_adult = weighted_sum(libyan_ngos_csos_adult,., group = !!get_group(.)) / total_adult * 100,
  total_international_ngos_adult = weighted_sum(international_ngos_adult,., group = !!get_group(.)) / total_adult * 100,
  total_own_family_business_adult = weighted_sum(own_family_business_adult,., group = !!get_group(.)) / total_adult * 100,
  total_informal_irregular_labour_adult = weighted_sum(informal_irregular_labour_adult,., group = !!get_group(.)) / total_adult * 100,
  total_other_institution_adult = weighted_sum(other_institution_adult,., group = !!get_group(.)) / total_adult * 100
) %>% select(-total_adult) %>% write.csv("../final_analysis/analysis_requests/type_jobs_institutions_mantika.csv")

data %>% select(gvt_salary,gvt_social_benefits,non_gvt_salary,casual_labour,own_business_income,remittances,family_support,humanitarian_assistance,zakat,
                food_expenditure,
                rent_expenditure,
                shelter_maintenance_expenditure,
                water_expenditure,
                nfi_expenditure,
                utilities_expenditure,
                fuel_expenditure,
                health_related_expenditure,
                education_related_expenditure,
                transportation_expenditure,
                mobile_phone_credit_expenditure,
                productive_assets_expenditure,
                debt_repayment_expenditure,
                other_expenditure,
                weights,mantika_label) %>% group_by(mantika_label) %>% 
  summarise_all(funs(weightedMedian(.,weights,na.rm = TRUE))) %>% select(-weights) %>% write.csv("output/income_expenditures_medians_mantika.csv")

dm_data %>% mutate(weights=weights(dm_data)) %>% 
  select(food_expenditure,
                rent_expenditure,
                shelter_maintenance_expenditure,
                water_expenditure,
                nfi_expenditure,
                utilities_expenditure,
                fuel_expenditure,
                health_related_expenditure,
                education_related_expenditure,
                transportation_expenditure,
                mobile_phone_credit_expenditure,
                productive_assets_expenditure,
                debt_repayment_expenditure,
                other_expenditure,
                weights,region) %>% group_by(region) %>% 
  summarise_all(list(~weightedMedian(.,weights,na.rm = TRUE))) %>% select(-weights) %>%
  write.csv("../final_analysis/analysis_requests/median_expenditure_region.csv")

dm_data %>% mutate(weights=weights(dm_data)) %>% 
  filter(atleast_one_crisis_emergency_cash_coping =="yes")%>%
  select(total_income,weights) %>% summarise_all(list(~weightedMedian(.,weights,na.rm = T))) %>% select(-weights) %>%
  write.csv("../final_analysis/analysis_requests/median_income_overall_atleast_one_coping.csv")

dm_data %>% mutate(weights=weights(dm_data)) %>% 
  filter(atleast_one_crisis_emergency_cash_coping =="yes")%>%
  select(total_income,weights,region) %>% group_by(region) %>%
  summarise_all(list(~weightedMedian(.,weights,na.rm = T))) %>% select(-weights) %>%
  write.csv("../final_analysis/analysis_requests/median_income_region_atleast_one_coping.csv")

dm_data %>% mutate(strata_weights = weights(data)) %>%
  select(total_income_without_0, total_expenditure_without_0, displacement_status, strata_weights) %>% 
  group_by(displacement_status) %>%
  summarise_all(list(~weightedMedian(.,strata_weights, na.rm = T))) %>% select(-strata_weights) %>%
  write.csv("../final_analysis/analysis_requests/income_expense_without0_groups.csv")

dm_data %>% mutate(strata_weights = weights(data)) %>%
  select(total_income_without_0, total_expenditure_without_0, mantika_label, strata_weights) %>% 
  group_by(mantika_label) %>%
  summarise_all(list(~weightedMedian(.,strata_weights, na.rm = T))) %>% select(-strata_weights) %>%
  write.csv("../final_analysis/analysis_requests/income_expense_without0_mantika.csv")

dm_data %>% mutate(strata_weights = weights(data)) %>%
  select(total_income_without_0, total_expenditure_without_0, strata_weights) %>% 
  summarise_all(list(~weightedMedian(.,strata_weights, na.rm = T))) %>% select(-strata_weights) %>%
  write.csv("../final_analysis/analysis_requests/income_expense_without0_overall.csv")


dm_data %>% mutate(weights = weights(dm_data)) %>%
  select(gvt_salary_without_0,
         gvt_social_benefits_without_0,
         non_gvt_salary_without_0,
         casual_labour_without_0,
         own_business_income_without_0,
         remittances_without_0,
         family_support_without_0,
         humanitarian_assistance_without_0,
         zakat_without_0,
         income_other_without_0,
         total_income_without_0,
         food_expenditure_without_0,
         rent_expenditure_without_0,
         shelter_maintenance_expenditure_without_0,
         water_expenditure_without_0,
         nfi_expenditure_without_0,
         utilities_expenditure_without_0,
         fuel_expenditure_without_0,
         health_related_expenditure_without_0,
         education_related_expenditure_without_0,
         transportation_expenditure_without_0,
         mobile_phone_credit_expenditure_without_0,
         productive_assets_expenditure_without_0,
         debt_repayment_expenditure_without_0,
         other_expenditure_without_0,
         total_expenditure_without_0,
         rental_cost_without_0,
         hh_total_debt_without_0,
         bank_withdrawal_amount_without_0,
         weights, strata.names) %>% group_by(strata.names) %>%
  summarise_all(list(~weightedMedian(.,weights,na.rm = T))) %>% select(-weights) %>%
  write.csv("../final_analysis/analysis_requests/stratanames_income_expenditure_excluding_0.csv")

dm_data %>% mutate(weights = weights(dm_data)) %>%
  select(gvt_salary,
         gvt_social_benefits,
         non_gvt_salary,
         casual_labour,
         own_business_income,
         remittances,
         family_support,
         humanitarian_assistance,
         zakat,
         income_other,
         total_income,
         food_expenditure,
         rent_expenditure,
         shelter_maintenance_expenditure,
         water_expenditure,
         nfi_expenditure,
         utilities_expenditure,
         fuel_expenditure,
         health_related_expenditure,
         education_related_expenditure,
         transportation_expenditure,
         mobile_phone_credit_expenditure,
         productive_assets_expenditure,
         debt_repayment_expenditure,
         other_expenditure,
         total_expenditures,
         rental_cost,
         hh_total_debt,
         bank_withdrawal_amount,
         weights, strata.names) %>% group_by(strata.names) %>% 
  summarise_all(list(~weightedMedian(.,weights,na.rm = T))) %>% select(-weights) %>%
  write.csv("../final_analysis/analysis_requests/stratanames_income_expenditure.csv")


dm_data %>%
  summarize(cash_coping_emergency= percent_response(cash_coping, ., "emergency"),
            cash_coping_crisis = percent_response(cash_coping, ., "crisis"),
            cash_coping_stress= percent_response(cash_coping, ., "stress")) %>% 
  write.csv("../final_analysis/analysis_requests/overall_cash_coping.csv")

dm_data %>% group_by(strata.names) %>%
  summarize(cash_coping_emergency= percent_response(cash_coping, ., "emergency", group = !!get_group(.)),
            cash_coping_crisis = percent_response(cash_coping, ., "crisis", group = !!get_group(.)),
            cash_coping_stress= percent_response(cash_coping, ., "stress", group = !!get_group(.))) %>% 
  write.csv("../final_analysis/analysis_requests/strata_coping.csv")

dm_data %>% summarize(
  total_male_adults = weighted_sum(over18_male,.),
  permanent_job_adult_male = round(weighted_sum(permanent_job_adult_male,.) / total_male_adults *100,1),
  temporary_job_adult_male = round(weighted_sum(temporary_job_adult_male,.) / total_male_adults *100,1),
  daily_labour_adult_male = round(weighted_sum(daily_labour_adult_male,.) / total_male_adults *100,1),
  gvt_payroll_adult_male = round(weighted_sum(gvt_payroll_adult_male,.) / total_male_adults *100,1),
  other_job_adult_male = round(weighted_sum(other_job_adult_male,.) / total_male_adults *100,1),
  total_female_adults = weighted_sum(over18_female,.),
  permanent_job_adult_female = round(weighted_sum(permanent_job_adult_female,.) / total_female_adults *100,1),
  temporary_job_adult_female = round(weighted_sum(temporary_job_adult_female,.) / total_female_adults *100,1),
  daily_labour_adult_female = round(weighted_sum(daily_labour_adult_female,.) / total_female_adults *100,1),
  gvt_payroll_adult_female = round(weighted_sum(gvt_payroll_adult_female,.) / total_female_adults *100,1),
  other_job_adult_female = round(weighted_sum(other_job_adult_female,.) / total_female_adults *100,1),
  total_permanent_job_minors_male = weighted_sum(permanent_job_minor_male,.),
  total_temporary_job_minors_male = weighted_sum(temporary_job_minor_male,.),
  total_daily_job_minors_male = weighted_sum(daily_labour_minor_male,.),
  total_other_job_minors_male = weighted_sum(other_job_minor_male,.),
  total_permanent_job_minors_female = weighted_sum(permanent_job_minor_female,.),
  total_temporary_job_minors_female = weighted_sum(temporary_job_minor_female,.),
  total_daily_job_minors_female = weighted_sum(daily_labour_minor_female,.),
  total_other_job_minors_female = weighted_sum(other_job_minor_female,.),
  total_minors_male =  weighted_sum(under18_male,.),
  permanent_job_minors_male = round(weighted_sum(permanent_job_minor_male,.)/ total_minors_male *100,1),
  temporary_job_minors_male = round(weighted_sum(temporary_job_minor_male,.)/ total_minors_male *100,1),
  daily_job_minors_male = round(weighted_sum(daily_labour_minor_male,.)/ total_minors_male *100,1),
  other_job_minors_male = round(weighted_sum(other_job_minor_male,.)/ total_minors_male *100,1),
  total_minors_female = weighted_sum(under18_female,.),
  permanent_job_minors_female = round(weighted_sum(permanent_job_minor_female,.)/total_minors_female *100,1),
  temporary_job_minors_female = round(weighted_sum(temporary_job_minor_female,.)/total_minors_female *100,1),
  daily_job_minors_female = round(weighted_sum(daily_labour_minor_female,.)/total_minors_female *100,1),
  other_job_minors_female = round(weighted_sum(other_job_minor_female,.)/total_minors_female *100,1)
) %>% select(-total_male_adults,-total_female_adults,-total_minors_male,-total_minors_female) %>%
  write.csv("../final_analysis/analysis_requests/overall_type_jobs.csv")

dm_data %>% summarize(
  total_male_adults = weighted_sum(over18_male,.),
  total_female_adults = weighted_sum(over18_female,.),
  gvt_public_sector_male = round(weighted_sum(gvt_public_sector_male,.)/ total_male_adults *100,1),
  libyan_owned_business_male = round(weighted_sum(libyan_owned_business_male,.)/ total_male_adults *100,1),
  foreign_owned_business_male = round(weighted_sum(foreign_owned_business_male,.)/ total_male_adults *100,1),
  libyan_ngos_csos_male = round(weighted_sum(libyan_ngos_csos_male,.)/ total_male_adults *100,1),
  international_ngos_male = round(weighted_sum(international_ngos_male,.)/ total_male_adults *100,1),
  own_family_business_male = round(weighted_sum(own_family_business_male,.)/ total_male_adults *100,1),
  informal_irregular_labour_male = round(weighted_sum(informal_irregular_labour_male,.)/ total_male_adults *100,1),
  other_institution_male = round(weighted_sum(other_institution_male,.)/ total_male_adults *100,1),
  gvt_public_sector_female = round(weighted_sum(gvt_public_sector_female,.)/ total_female_adults *100,1),
  libyan_owned_business_female = round(weighted_sum(libyan_owned_business_female,.)/ total_female_adults *100,1),
  foreign_owned_business_female = round(weighted_sum(foreign_owned_business_female,.)/ total_female_adults *100,1),
  libyan_ngos_csos_female = round(weighted_sum(libyan_ngos_csos_female,.)/ total_female_adults *100,1),
  international_ngos_female = round(weighted_sum(international_ngos_female,.)/ total_female_adults *100,1),
  own_family_business_female = round(weighted_sum(own_family_business_female,.)/ total_female_adults *100,1),
  informal_irregular_labour_female = round(weighted_sum(informal_irregular_labour_female,.)/ total_female_adults *100,1),
  other_institution_female = round(weighted_sum(other_institution_female,.)/ total_female_adults *100,1)
  ) %>%
  select(-total_male_adults,-total_female_adults) %>% 
  write.csv("../final_analysis/analysis_requests/overall_type_institutions.csv")
  
dm_data %>% group_by(strata.names) %>% summarize(
  total_male_adults = weighted_sum(over18_male,.,group = !!get_group(.)),
  total_female_adults = weighted_sum(over18_female,.,group = !!get_group(.)),
  gvt_public_sector_male = round(weighted_sum(gvt_public_sector_male,.,group = !!get_group(.))/ total_male_adults *100,1),
  libyan_owned_business_male = round(weighted_sum(libyan_owned_business_male,.,group = !!get_group(.))/ total_male_adults *100,1),
  foreign_owned_business_male = round(weighted_sum(foreign_owned_business_male,.,group = !!get_group(.))/ total_male_adults *100,1),
  libyan_ngos_csos_male = round(weighted_sum(libyan_ngos_csos_male,.,group = !!get_group(.))/ total_male_adults *100,1),
  international_ngos_male = round(weighted_sum(international_ngos_male,.,group = !!get_group(.))/ total_male_adults *100,1),
  own_family_business_male = round(weighted_sum(own_family_business_male,.,group = !!get_group(.))/ total_male_adults *100,1),
  informal_irregular_labour_male = round(weighted_sum(informal_irregular_labour_male,.,group = !!get_group(.))/ total_male_adults *100,1),
  other_institution_male = round(weighted_sum(other_institution_male,.,group = !!get_group(.))/ total_male_adults *100,1),
  gvt_public_sector_female = round(weighted_sum(gvt_public_sector_female,.,group = !!get_group(.))/ total_female_adults *100,1),
  libyan_owned_business_female = round(weighted_sum(libyan_owned_business_female,.,group = !!get_group(.))/ total_female_adults *100,1),
  foreign_owned_business_female = round(weighted_sum(foreign_owned_business_female,.,group = !!get_group(.))/ total_female_adults *100,1),
  libyan_ngos_csos_female = round(weighted_sum(libyan_ngos_csos_female,.,group = !!get_group(.))/ total_female_adults *100,1),
  international_ngos_female = round(weighted_sum(international_ngos_female,.,group = !!get_group(.))/ total_female_adults *100,1),
  own_family_business_female = round(weighted_sum(own_family_business_female,.,group = !!get_group(.))/ total_female_adults *100,1),
  informal_irregular_labour_female = round(weighted_sum(informal_irregular_labour_female,.,group = !!get_group(.))/ total_female_adults *100,1),
  other_institution_female = round(weighted_sum(other_institution_female,.,group = !!get_group(.))/ total_female_adults *100,1)) %>%
  select(-total_male_adults,-total_female_adults) %>% 
  write.csv("../final_analysis/analysis_requests/strata_type_institutions.csv")
  
dm_data %>% group_by(strata.names) %>% summarize(
  total_male_adults = weighted_sum(over18_male,., group = !!get_group(.)),
  permanent_job_adult_male = round(weighted_sum(permanent_job_adult_male,., group = !!get_group(.)) / total_male_adults *100,1),
  temporary_job_adult_male = round(weighted_sum(temporary_job_adult_male,., group = !!get_group(.)) / total_male_adults *100,1),
  daily_labour_adult_male = round(weighted_sum(daily_labour_adult_male,., group = !!get_group(.)) / total_male_adults *100,1),
  gvt_payroll_adult_male = round(weighted_sum(gvt_payroll_adult_male,., group = !!get_group(.)) / total_male_adults *100,1),
  other_job_adult_male = round(weighted_sum(other_job_adult_male,., group = !!get_group(.)) / total_male_adults *100,1),
  total_female_adults = weighted_sum(over18_female,., group = !!get_group(.)),
  permanent_job_adult_female = round(weighted_sum(permanent_job_adult_female,., group = !!get_group(.)) / total_female_adults *100,1),
  temporary_job_adult_female = round(weighted_sum(temporary_job_adult_female,., group = !!get_group(.)) / total_female_adults *100,1),
  daily_labour_adult_female = round(weighted_sum(daily_labour_adult_female,., group = !!get_group(.)) / total_female_adults *100,1),
  gvt_payroll_adult_female = round(weighted_sum(gvt_payroll_adult_female,., group = !!get_group(.)) / total_female_adults *100,1),
  other_job_adult_female = round(weighted_sum(other_job_adult_female,., group = !!get_group(.)) / total_female_adults *100,1),
  total_permanent_job_minors_male = weighted_sum(permanent_job_minor_male,., group = !!get_group(.)),
  total_temporary_job_minors_male = weighted_sum(temporary_job_minor_male,., group = !!get_group(.)),
  total_daily_job_minors_male = weighted_sum(daily_labour_minor_male,., group = !!get_group(.)),
  total_other_job_minors_male = weighted_sum(other_job_minor_male,., group = !!get_group(.)),
  total_permanent_job_minors_female = weighted_sum(permanent_job_minor_female,., group = !!get_group(.)),
  total_temporary_job_minors_female = weighted_sum(temporary_job_minor_female,., group = !!get_group(.)),
  total_daily_job_minors_female = weighted_sum(daily_labour_minor_female,., group = !!get_group(.)),
  total_other_job_minors_female = weighted_sum(other_job_minor_female,., group = !!get_group(.)),
  total_minors_male =  weighted_sum(under18_male,., group = !!get_group(.)),
  permanent_job_minors_male = round(weighted_sum(permanent_job_minor_male,., group = !!get_group(.))/ total_minors_male *100,1),
  temporary_job_minors_male = round(weighted_sum(temporary_job_minor_male,., group = !!get_group(.))/ total_minors_male *100,1),
  daily_job_minors_male = round(weighted_sum(daily_labour_minor_male,., group = !!get_group(.))/ total_minors_male *100,1),
  other_job_minors_male = round(weighted_sum(other_job_minor_male,., group = !!get_group(.))/ total_minors_male *100,1),
  total_minors_female = weighted_sum(under18_female,., group = !!get_group(.)),
  permanent_job_minors_female = round(weighted_sum(permanent_job_minor_female,., group = !!get_group(.))/total_minors_female *100,1),
  temporary_job_minors_female = round(weighted_sum(temporary_job_minor_female,., group = !!get_group(.))/total_minors_female *100,1),
  daily_job_minors_female = round(weighted_sum(daily_labour_minor_female,., group = !!get_group(.))/total_minors_female *100,1),
  other_job_minors_female = round(weighted_sum(other_job_minor_female,., group = !!get_group(.))/total_minors_female *100,1)
) %>% select(-total_male_adults,-total_female_adults,-total_minors_male,-total_minors_female) %>%
  write.csv("../final_analysis/analysis_requests/strata_jobs.csv")

 ## Education
dm_data %>% summarize(no_issue_attending_school = percent_response(issues_faced_attending_school,.,"no_issue_faced_attending_school"))
dm_data %>% summarize(no_issue_attending_school = percent_response(faced_atleast_one_issue_while_attending,.,"no"))

dm_data %>% group_by(strata.names) %>% 
  summarize(faced_atleast_one_issue_while_attending = percent_response(faced_atleast_one_issue_while_attending,.,"yes",group = !!get_group(.))) %>% 
  arrange(desc(faced_atleast_one_issue_while_attending))

dm_data %>% group_by(strata.names) %>% 
  summarize(faced_atleast_one_issue_while_attending = percent_response(issues_faced_attending_school,.,"no_issue_faced_attending_school",group = !!get_group(.))) %>% 
  mutate(faced_atleast_one_issue_while_attending= 100 - faced_atleast_one_issue_while_attending) %>%
  arrange(desc(faced_atleast_one_issue_while_attending))


dm_data %>% summarize(total_school_age_male = weighted_sum(nb_male_edu,.),
                      total_school_age_female = weighted_sum(nb_female_edu,.),
                      enrolled_male= weighted_sum(enrolled_school_male_edu,.)/total_school_age_male *100,
                      enrolled_female= weighted_sum(enrolled_school_female_edu,.)/total_school_age_female *100,
                      attended_male= weighted_sum(attended_school_male,.)/total_school_age_male *100,
                      attended_female= weighted_sum(attended_school_female,.)/total_school_age_female *100) %>% mutate(
                        not_enrolled_male= 100 - enrolled_male,
                        not_enrolled_female= 100 - enrolled_female,
                        not_attended_male= 100 - attended_male,
                        not_attended_female= 100 - attended_female) %>% write.csv("output/enrolment_attendance_overall_gender.csv")

dm_data %>% group_by(mantika_label) %>% 
  summarize(total_school_age_male = weighted_sum(nb_male_edu,.,group = !!get_group(.)),
                      total_school_age_female = weighted_sum(nb_female_edu,.,group = !!get_group(.)),
                      enrolled_male= weighted_sum(enrolled_school_male_edu,.,group = !!get_group(.))/total_school_age_male *100,
                      enrolled_female= weighted_sum(enrolled_school_female_edu,.,group = !!get_group(.))/total_school_age_female *100,
                      attended_male= weighted_sum(attended_school_male,.,group = !!get_group(.))/total_school_age_male *100,
                      attended_female= weighted_sum(attended_school_female,.,group = !!get_group(.))/total_school_age_female *100) %>% mutate(
                        not_enrolled_male= 100 - enrolled_male,
                        not_enrolled_female= 100 - enrolled_female,
                        not_attended_male= 100 - attended_male,
                        not_attended_female= 100 - attended_female) %>% write.csv("output/enrolment_attendance_mantika_gender.csv")

dm_data %>% summarize(total_school_age = weighted_sum(school_age_children,.),
                      enrolled= weighted_sum(enrolled_school,.)/total_school_age *100,
                      attended= weighted_sum(attended_school,.)/total_school_age *100) %>% mutate(
                        not_enrolled= 100 - enrolled,
                        not_attended= 100 - attended) %>% write.csv("output/enrolment_attendance_school_overall.csv")


dm_data %>% group_by(mantika_label) %>% 
  summarize(total_school_age = weighted_sum(school_age_children,.,group = !!get_group(.)),
            enrolled= weighted_sum(enrolled_school,.,group = !!get_group(.))/total_school_age *100,
            attended= weighted_sum(attended_school,.,group = !!get_group(.))/total_school_age *100)%>% mutate(
              not_enrolled= 100 - enrolled,
              not_attended= 100 - attended) %>% write.csv("output/enrolment_attendance_school_mantika.csv")

dm_data %>% group_by(displacement_status) %>% 
  summarize(total_school_age = weighted_sum(school_age_children,.,group = !!get_group(.)),
            enrolled= weighted_sum(enrolled_school,.,group = !!get_group(.))/total_school_age *100,
            attended= weighted_sum(attended_school,.,group = !!get_group(.))/total_school_age *100)%>% mutate(
              not_enrolled= 100 - enrolled,
              not_attended= 100 - attended) %>% write.csv("output/enrolment_attendance_school_groups.csv")


## General
dm_data %>% group_by(mantika_label) %>% 
  summarize(seperated_children=round(weighted_sum(seperated_children_calc,.,group = !!get_group(.)),0)) %>% 
  arrange(desc(seperated_children))

dm_data %>% summarize(
  total_poeple= weighted_sum(size_hh,.),
  infant_male = round(weighted_sum(nb_infants_male,.) / total_poeple *100,1),
  children_male = round(weighted_sum(nb_children_male,.) / total_poeple *100,1),
  youth_male = round(weighted_sum(nb_youth_male,.) / total_poeple *100,1),
  adults_male = round(weighted_sum(nb_adults_male,.) / total_poeple *100,1),
  elderly_male = round(weighted_sum(nb_elderly_male,.) / total_poeple *100,1),
  infant_female = round(weighted_sum(nb_infants_female,.) / total_poeple *100,1),
  children_female = round(weighted_sum(nb_children_female,.) / total_poeple *100,1),
  youth_female = round(weighted_sum(nb_youth_female,.) / total_poeple *100,1),
  adults_female = round(weighted_sum(nb_adults_female,.) / total_poeple *100,1),
  elderly_female = round(weighted_sum(nb_elderly_female,.) / total_poeple *100,1),
  total_hosted_person = round(weighted_sum(nb_displaced_hosted,.),0),
  total_hosted_minor = round(weighted_sum(nb_minordisplaced_hosted,.),0),
  total_hosted_male_infant = round(weighted_sum(seperated_children_male_infants,.),0),
  total_hosted_male_children = round(weighted_sum(seperated_children_male_children,.),0),
  total_hosted_male_youth = round(weighted_sum(seperated_children_male_youths,.),0),
  total_hosted_female_infant = round(weighted_sum(seperated_children_female_infants,.),0),
  total_hosted_female_children = round(weighted_sum(seperated_children_female_children,.),0),
  total_hosted_female_youth = round(weighted_sum(seperated_children_female_youths,.),0),
  total_unaccompanied_male_infant = round(weighted_sum(unaccompanied_children_male_infants,.),0),
  total_unaccompanied_male_children = round(weighted_sum(unaccompanied_children_male_children,.),0),
  total_unaccompanied_male_youth = round(weighted_sum(unaccompanied_children_male_youths,.),0),
  total_unaccompanied_female_infant = round(weighted_sum(unaccompanied_children_female_infants,.),0),
  total_unaccompanied_female_children = round(weighted_sum(unaccompanied_children_female_children,.),0),
  total_unaccompanied_female_youth = round(weighted_sum(unaccompanied_children_female_youths,.),0)
) %>% select(-total_poeple) %>%
  write.csv("../final_analysis/analysis_requests/overall_biodata.csv")

dm_data %>% group_by(strata.names) %>% summarize(
  total_poeple= weighted_sum(size_hh,.,group = !!get_group(.)),
  infant_male = round(weighted_sum(nb_infants_male,.,group = !!get_group(.)) / total_poeple *100,1),
  children_male = round(weighted_sum(nb_children_male,.,group = !!get_group(.)) / total_poeple *100,1),
  youth_male = round(weighted_sum(nb_youth_male,.,group = !!get_group(.)) / total_poeple *100,1),
  adults_male = round(weighted_sum(nb_adults_male,.,group = !!get_group(.)) / total_poeple *100,1),
  elderly_male = round(weighted_sum(nb_elderly_male,.,group = !!get_group(.)) / total_poeple *100,1),
  infant_female = round(weighted_sum(nb_infants_female,.,group = !!get_group(.)) / total_poeple *100,1),
  children_female = round(weighted_sum(nb_children_female,.,group = !!get_group(.)) / total_poeple *100,1),
  youth_female = round(weighted_sum(nb_youth_female,.,group = !!get_group(.)) / total_poeple *100,1),
  adults_female = round(weighted_sum(nb_adults_female,.,group = !!get_group(.)) / total_poeple *100,1),
  elderly_female = round(weighted_sum(nb_elderly_female,.,group = !!get_group(.)) / total_poeple *100,1),
  total_hosted_person = round(weighted_sum(nb_displaced_hosted,.,group = !!get_group(.)),0),
  total_hosted_minor = round(weighted_sum(nb_minordisplaced_hosted,.,group = !!get_group(.)),0),
  total_hosted_male_infant = round(weighted_sum(seperated_children_male_infants,.,group = !!get_group(.)),0),
  total_hosted_male_children = round(weighted_sum(seperated_children_male_children,.,group = !!get_group(.)),0),
  total_hosted_male_youth = round(weighted_sum(seperated_children_male_youths,.,group = !!get_group(.)),0),
  total_hosted_female_infant = round(weighted_sum(seperated_children_female_infants,.,group = !!get_group(.)),0),
  total_hosted_female_children = round(weighted_sum(seperated_children_female_children,.,group = !!get_group(.)),0),
  total_hosted_female_youth = round(weighted_sum(seperated_children_female_youths,.,group = !!get_group(.)),0),
  total_unaccompanied_male_infant = round(weighted_sum(unaccompanied_children_male_infants,.,group = !!get_group(.)),0),
  total_unaccompanied_male_children = round(weighted_sum(unaccompanied_children_male_children,.,group = !!get_group(.)),0),
  total_unaccompanied_male_youth = round(weighted_sum(unaccompanied_children_male_youths,.,group = !!get_group(.)),0),
  total_unaccompanied_female_infant = round(weighted_sum(unaccompanied_children_female_infants,.,group = !!get_group(.)),0),
  total_unaccompanied_female_children = round(weighted_sum(unaccompanied_children_female_children,.,group = !!get_group(.)),0),
  total_unaccompanied_female_youth = round(weighted_sum(unaccompanied_children_female_youths,.,group = !!get_group(.)),0)
) %>% select(-total_poeple) %>%
  write.csv("../final_analysis/analysis_requests/strata_biodata.csv")

## Health 

dm_data %>% summarize( pregnant_woman_pct =percent_response(pregannt_woman,.,"yes"),
                       children_bottled_milk_pct =percent_response(children_bottled_milk,.,"yes"),
                       total_under18 = weighted_sum(nb_under18_health,.),
                      nb_chilren_vaccination_pct= weighted_sum(nb_children_vaccination_cards,.) /total_under18 * 100,
                      nb_chilren_notable_vaccination_pct=weighted_sum(nb_children_notable_get_vaccination,.) /total_under18 * 100) %>% select(-total_under18) %>%
  write.csv("output/health_overall.csv")

dm_data %>% group_by(displacement_status) %>%
  summarize( pregnant_woman_pct =percent_response(pregannt_woman,.,"yes",group = !!get_group(.)),
                       children_bottled_milk_pct =percent_response(children_bottled_milk,.,"yes",group = !!get_group(.)),
                       total_under18 = weighted_sum(nb_under18_health,.,group = !!get_group(.)),
                       nb_chilren_vaccination_pct= weighted_sum(nb_children_vaccination_cards,.,group = !!get_group(.)) /total_under18 * 100,
                       nb_chilren_notable_vaccination_pct=weighted_sum(nb_children_notable_get_vaccination,.,group = !!get_group(.)) /total_under18 * 100) %>% select(-total_under18) %>%
  write.csv("output/health_groups.csv")

dm_data %>% group_by(mantika_label) %>%
  summarize( pregnant_woman_pct =percent_response(pregannt_woman,.,"yes",group = !!get_group(.)),
             children_bottled_milk_pct =percent_response(children_bottled_milk,.,"yes",group = !!get_group(.)),
             total_under18 = weighted_sum(nb_under18_health,.,group = !!get_group(.)),
             nb_chilren_vaccination_pct= weighted_sum(nb_children_vaccination_cards,.,group = !!get_group(.)) /total_under18 * 100,
             nb_chilren_notable_vaccination_pct=weighted_sum(nb_children_notable_get_vaccination,.,group = !!get_group(.)) /total_under18 * 100) %>% select(-total_under18) %>%
  write.csv("output/health_mantika.csv")

dm_data %>% group_by(mantika_label) %>%
  summarize(children_bottled_milk_subset_pct =percent_response(children_bottled_milk_subset,.,"yes",group = !!get_group(.))) %>%
  write.csv("../final_analysis/analysis_requests/children_bottled_milk_subset_pct_mantika.csv")

dm_data %>% summarize(children_bottled_milk_subset_pct =percent_response(children_bottled_milk_subset,.,"yes")) %>%
  write.csv("../final_analysis/analysis_requests/children_bottled_milk_subset_pct_overall.csv")

dm_data %>% summarize(
  total_pregnant_woman = weighted_sum(nb_pregnant_women,.)
) %>% write.csv("../final_analysis/analysis_requests/overall_pregnant_woman.csv")

dm_data %>% group_by(strata.names) %>% summarize(
  total_pregnant_woman = weighted_sum(nb_pregnant_women,., group = !!get_group(.))
) %>% write.csv("../final_analysis/analysis_requests/strata_pregnant_woman.csv")

dm_data %>% summarize(
  total_infatns_0_5 = weighted_sum(nb_infants_female,.)+weighted_sum(nb_infants_male,.),
  vacciatino = weighted_sum(nb_children_vaccination_cards,.),
  infant_vaccination_card = round(weighted_sum(nb_children_vaccination_cards,.) / total_infatns_0_5 *100,1)
)

