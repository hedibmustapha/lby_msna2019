rm(list = ls())

remove.packages("hypegrammaR", c(.libPaths(),"C:/Users/REACH/Documents/R/win-library/3.4"))
Sys.setenv("R_REMOTES_NO_ERRORS_FROM_WARNINGS"=TRUE)
devtools::install_github("mabafaba/hypegrammaR", build_opts = c())
devtools::install_github("hedibmustapha/hypegrammaR", build_opts = c())
library(hypegrammaR)
library(parallel)
library(dplyr)

# read questionnaire + choices
questions <- read.csv("./trend_analysis/inputs/survey.csv", stringsAsFactors = F)
choices <- read.csv("./trend_analysis/inputs/choices.csv", stringsAsFactors = F)
choices$label..English..en. <- gsub("^\\d+[.]\\s*","", choices$label..English..en.)
#read data
data <- load_data(file = "./trend_analysis/inputs/trend_analysis_msna.csv")
#add additional vars
data <- data %>%
  mutate(fcs = (cereals * 2) + (legumes * 3) + veggies + fruits + (meat * 4) + (dairy * 4) + (fats * 0.5) + (sugar * 0.5),
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
                                             reduce_expense_health,
                                             took_additional_job,
                                             child_dropped_school,
                                             delayed_skipped_rent) %>%
                                        mutate_all(~ .x %in% c("already_exhausted_this_coping_strategy", "yes")),
                                      na.rm = T),
         cash_coping_emergency = rowSums(select(., 
                                                begging,
                                                adult_accepting_degrading_illegal_work,
                                                minor_accepting_degrading_illegal_work,
                                                child_marriage,
                                                degrading_illegal_work) %>%
                                           mutate_all(~ .x %in% c("already_exhausted_this_coping_strategy", "yes")),
                                         na.rm = T),
         cash_coping = case_when(
           cash_coping_emergency > 0 ~ "emergency",
           cash_coping_crisis > 0 ~ "crisis",
           cash_coping_stress > 0 ~ "stress"
         ))
#filter to only account for msna2019 covered areas
data <- data %>% filter(mantika_label %in% c("Sebha",
                                             "Aljufra",
                                             "Tripoli",
                                             "Wadi Ashshati",
                                             "Aljfara",
                                             "Ejdabia",
                                             "Alkufra",
                                             "Sirt",
                                             "Ghat",
                                             "Al Jabal Al Gharbi",
                                             "Azzawya",
                                             "Zwara",
                                             "Murzuq",
                                             "Misrata",
                                             "Benghazi",
                                             "Ubari"))

# blank to NA
data <- mutate_if(data, is.character, na_if, "")
#create questionnaire object
questionnaire <- load_questionnaire(data = data,
                                    questions = questions,
                                    choices = choices,
                                    choices.label.column.to.use = "label::English (en)")

#read analysis plan for overall/split
analysisplan <- load_analysisplan(file = "./trend_analysis/inputs/analysisplan.csv")
# BE CAREFULL we have two WEIGHTS VARIABLES
weighting_function <- function(data){data$weights}

#Run and write results
results <- from_analysisplan_map_to_output(data = data,
                                           analysisplan = analysisplan,
                                           weighting = weighting_function,
                                           questionnaire = questionnaire)

map_to_master_table(results_object = results$results, filename = "./trend_analysis/outputs/results_split.csv", questionnaire = questionnaire)
