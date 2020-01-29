library(Setviz)
library("UpSetR")
library(data.table)
library(purrr)

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

setnames(x = lsg_three_four,
         old = c("protection", "shelter", "fs", "health", "wash", "education", "capacity_gap"),
         new = c("Protection", "Shelter_NFI", "FSL", "Health", "WASH", "Education", "Capacity_gap"))

plot_set_percentages(data = filter(lsg_three_four,vul_atlease_one=="yes"),
                     varnames = c("Protection","Shelter_NFI","FSL","Health","WASH","Education","Capacity_gap"),
                     mutually_exclusive_sets = T,
                     exclude_unique = F,
                     round_to_1_percent = F,
                     nintersects = 5,
                     weighting_function = weights)


plots <- Setviz_data %>% filter(vul_atlease_one =="yes") %>%
              split(.$mantika_label) %>% purrr::map(plot_set_percentages,
              varnames = c("Protection","Shelter_NFI","FSL","Health","WASH","Education","Capacity_gap"),
              mutually_exclusive_sets = T,
              exclude_unique = F,
              round_to_1_percent = T,
              nintersects = 12)

filenames<- unique(Setviz_data$mantika_label)
filenames <- filenames[order(filenames)]
purrr::map2(plots,filenames,function(plot, fn){
  on.exit({dev.off()})
  pdf(paste0("./intersection_",fn,".pdf"))
  print(plot)
})


######J
library(extrafont)
font_import(pattern="ARIALN")
fonts()
fonttable()
loadfonts(device="win")

filenames <- str_c(filenames, ".png")

map2(filenames,plots, function(fn,plot){
  png(filename = fn, width = 784, height = 512, family = "arial", res = 100)
  print(plot)
  dev.off()
})
