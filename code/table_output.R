rm(list = ls())
source("code/table_function.R")
# source("code/preliminary_weighted_analysis.R")
# Run the first 25 lines from the file above to get hte data, questionnaire, etc.
library(tidyverse)
library(hypegrammaR)
library(parallel)
library(dplyr)

questions <- read.csv("./input/survey.csv", stringsAsFactors = F)
choices <- read.csv("./input/choices.csv", stringsAsFactors = F)
data <- load_data(file = "./input/data.csv")
data <- mutate_if(data, is.character, na_if, "")
data <- data %>% select_if(~ !(all(is.na(.x)) | all(. == "")))
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
analysisplan <- load_analysisplan(file = "./input/bio_assistance_displacement.csv")
analysisplan %>% filter(analysisplan[["dependent.variable"]] %in% names(data)) -> analysisplan
# Correcting blank cells to NA
# filtering data for what we want to analyze
# leave select multiple text columns in

text <- filter(questions, str_detect(type, "(\\btext\\b)|(\\bnote\\b)"))$name
choices$label..English..en. <- gsub("^\\d+[.]\\s*","", choices$label..English..en.)

data_to_analyze <- data %>% 
  select(-one_of(text)) %>%
  select(-start, -end, -today, -mantika, -baladiya, -baladiya_label, -baladiya_other,
         -consent, -not_hoh_ability_toanswer, -`x_index`, -uuid, -`x_submission_time`, -`x_id`,-x__version__ ) %>%
  select_if(~ !(all(is.na(.x)) | all(. == "")))

#data_to_analyze <- data_to_analyze[,1:30]
#analysisplan <- analysisplan[2,]
strata_output <- table_maker(data_to_analyze, 
                            questionnaire,
                            questions, 
                            choices,
                            weights, 
                            #analysisplan,
                            labels = T, 
                            language = "english", 
                            "Libya", 
                            "displacement_status"
                            )

saveRDS(strata_output, "output/overall_displacement_status.RDS")
write_csv(strata_output, "output/overall_displacement_status.csv")
