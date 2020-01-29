library(composr)
library(stringr)
library(hypegrammaR)
library(dplyr)

source("code/data_merge_functions.R")
source("code/functions.R")

rounded_mean <- function(...){
  round(...,digits = 0)
}

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

weights <-map_to_weighting(sampling.frame = sampling_frame,
                           data.stratum.column = "strata.names",
                           sampling.frame.population.column = "population",
                           sampling.frame.stratum.column = "strata.names",
                           data = data)

analysisplan <- load_analysisplan(file = "./input/composite_indicators_analysisplan2.csv")

ci_prep_data <- data %>% mutate(
  problems_faced_returnees = str_concat("yes",
                                                      house_occupied,
                                                      inability_prove_legal_ownership,
                                                      hostility_from_local_community,
                                                      lack_security_area,
                                                      parts_house_property_destroyed,
                                                      missing_valuables_house_property,
                                                      inability_move_freely,
                                                      basic_services_hh_nolonger_working,
                                                      basic_services_community_nolonger_working,
                                                      mobilephone_network_nolonger_working,
                                                      other_problem_faced),
                                
 total_income = rowSums(select(.,gvt_salary,gvt_social_benefits,non_gvt_salary,casual_labour,
                                                              own_business_income,
                                                              remittances,
                                                              family_support,humanitarian_assistance,zakat,income_other) 
                                                       , na.rm=T),
 total_income_without_0 = ifelse(total_income ==0, NA, total_income),
 age_dependecny_ratio = (nb_infants_male+nb_children_male+nb_infants_female+nb_children_female+
                        nb_elderly_male+nb_elderly_female) / (nb_youth_male+nb_youth_male+nb_adults_male+nb_adults_female),
 working_minors_nb = rowSums(select(.,calcul_jobs_minors_male_nb,calcul_jobs_minors_female_nb),na.rm = T),
 
 enrolled_notattending = rowSums(select(.,enrolled_school_male_6_14:enrolled_school_female_15_17),na.rm = T)-
                rowSums(select(.,attended_school_male_6_14:attended_school_female_15_17),na.rm = T),
 not_enrolled = rowSums(select(.,school_aged_boys,school_aged_girls),na.rm = T)-
   rowSums(select(.,enrolled_school_male_6_14:enrolled_school_female_15_17),na.rm = T),
 fcs = (cereals * 2) + (legumes * 3) + veggies + fruits + (meat * 4) + (dairy * 4) + (fats * 0.5) + (sugar * 0.5),
 fcs_category = case_when(
   fcs <= 28 ~ "poor",
   fcs > 28 & fcs <= 42 ~ "borderline",
   fcs > 42 ~ "acceptable"
 ),
 rcsi = less_expensive_quality + (borrow_relatives * 2) + reduce_number_meals + (reduce_adult * 3) + shrink_meals,
 rcsi_category = case_when(
   rcsi <= 3 ~ "low",
   rcsi > 3 & rcsi <= 9 ~ "medium",
   rcsi > 9 ~ "high"
 ),
 
 cash_coping_stress = rowSums(select(., 
                                     sold_nonproductive_hh_assets,
                                     spent_savings,
                                     borrowed_purchased_oncredit_food,
                                     reduced_expenditures_essential_nfi) %>%
                                mutate_all(~ .x %in% c("already_exhausted_this_coping_strategy", "yes")),
                              na.rm = T),
 cash_coping_crisis = rowSums(select(., 
                                     sold_productive_hh_assets,
                                     borrowed_money,
                                     reduced_expenditures_health_education,
                                     took_additional_job) %>%
                                mutate_all(~ .x %in% c("already_exhausted_this_coping_strategy", "yes")),
                              na.rm = T),
 cash_coping_emergency = rowSums(select(., 
                                        begging,
                                        adult_accepting_degrading_illegal_work,
                                        minor_accepting_degrading_illegal_work,
                                        child_marriage) %>%
                                   mutate_all(~ .x %in% c("already_exhausted_this_coping_strategy", "yes")),
                                 na.rm = T),
 cash_coping = case_when(
   cash_coping_emergency > 0 ~ "emergency",
   cash_coping_crisis > 0 ~ "crisis",
   cash_coping_stress > 0 ~ "stress",
   cash_coping_emergency + cash_coping_crisis + cash_coping_stress == 0 ~ "none"
   
 ),
 
 have_docs_inprocess = rowSums(select(.,
                                      property_docs:other_docs) %>%
                                 mutate_all(~ .x %in% c("everyone_has_legal_doc","process_renewing_doc")),
                               na.rm = T),
 
 dont_have_cannot_obtain_docs = rowSums(select(.,
                                               property_docs:other_docs) %>%
                                          mutate_all(~ .x %in% c("not_everyone_has_legal_doc",
                                                                 "unable_obtain_doc",
                                                                 "lost_doc_conflict")),
                                        na.rm = T),
 
 minor_some_difficulties = rowSums(select(.,starts_with("minor_difficulties_carrying_activities"),
                                          starts_with("some_difficulties_carrying_activities")),
                                   na.rm = T),
 alot_cannot_difficulties = rowSums(select(.,starts_with("alot_difficulties_carrying_activities"),
                                           starts_with("cannot_carry_activities")),
                                    na.rm = T)
 )
