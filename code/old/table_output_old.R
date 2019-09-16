source("code/table_function.R")
# source("code/preliminary_weighted_analysis.R")
# Run the first 25 lines from the file above to get hte data, questionnaire, etc.
library(tidyverse)

# Correcting blank cells to NA

data <- mutate_if(data, is.character, na_if, "")

# filtering data for what we want to analyze
# leave select multiple text columns in

text <- filter(questions, str_detect(type, "(\\btext\\b)|(\\bnote\\b)"))$name
choices$label..English..en. <- gsub("^\\d+[.]\\s*","", choices$label..English..en.)

data_to_analyze <- data %>%
  select(-one_of(text)) %>%
  select(-start, -end, -today, -mantika, -baladiya, -baladiya_label, -baladiya_other,
         -consent, -not_hoh_ability_toanswer, -x_index, -uuid, -x_submission_time, -x_id, -x__version__) %>%
  select_if(~ !(all(is.na(.x)) | all(. == "")))

#data_to_analyze<-data_to_analyze[,1:4]
strata_output <- table_maker(data_to_analyze, 
                            questions, 
                            choices,
                            weights, 
                            labels = T, 
                            language = "english",
                            "Libya",
                            "strata.names",
                            "mantika_label")

saveRDS(strata_output, "output/strata_names_mantika_table.RDS")
write_csv(strata_output, "output/strata_names_mantika_table.csv")
