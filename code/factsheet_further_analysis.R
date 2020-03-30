library(composr)


rounded_mean <- function(...){
  round(...,digits = 0)
}

many_records <- read.csv("./input/libyan_msna_composite_indicator_v1.csv")


datamerge_factsheet <- dm_data %>% recode_batch(tos = many_records$to_values,
                                                wheres = many_records$conditions,
                                                targets = many_records$target_variables,
                                                questionnaire = questionnaire) %>%
  end_recoding()


datamerge_factsheet <- datamerge_factsheet %>% 
  mutate(
    fs_score = ifelse(fcs_index1 >= 4 & !is.na(fcs_index1), 
                      fcs_index1, 
                      rounded_mean(rowMeans(select(., fcs_index1:fcs_index4), na.rm = T))
    ),
    fs = ifelse(fs_score >=3,1,0)
  )


datamerge_factsheet %>% group_by(mantika_label) %>%
  summarize(
    fine_todrink = percent_response(x = filter(., displacement_status == "non_displaced")[["drinkingwater_quality"]],
                                    df = filter(., displacement_status == "non_displaced"),
                                    "water_fine_todrink",
                                    x_name = "drinkingwater_quality",
                                    group = !! get_group(.))
  )

