library(survey)
library(srvyr)
data(api)

srs_design <- svydesign(id=~1, fpc = ~fpc, data=apisrs)
svytotal(~enroll, srs_design)
svymean(~enroll, srs_design)

nd_data <- filter(data, displacement_status=="non_displaced")

lookup <- unique(sampling_frame)
data <- merge(lookup, data, by='strata.names')
nd_data$weights <- weights(nd_data)
strata.weights <- weights(data)

strat_design_srvyr <- data_ %>%
  as_survey_design(1, strata = strata.names, fpc = population)
strat_design_srvyr %>%
  summarize(a = survey_mean(agricultural_activities_prior2011.livestock_activity, vartype = "ci",na.rm=T))

hype_design <- map_to_design(data = data, weighting_function = weights)

strata_design <- svydesign(ids = ~1, strata = ~strata.names, fpc = ~population, data = data, weights=~weights, nest = T)
strata_design <- svydesign(ids = ~1, strata = ~strata.names, fpc = ~population, data = data)
nd_design <- svydesign(ids = ~1,data = nd_data,weights=~weights)

perso_design <- map_to_design(data = data, strata.variable.name = "strata.names", population.variable.name = "population", weights.name = "weights")

svymean(~hoh, strata_design,na.rm=T)
svymean(~hoh, perso_design,na.rm=T)

svymean(~crop_production_challenges.insecurity, hype_design,na.rm=T)
confint(svymean(~pull_factors.conflict_ended, strat_design,na.rm=T), level = 0.95)

summary <-svymean(~size_hh, strat_design, se=T)

svytotal(~mantika_label, strat_design)
svymean(~hoh, strat_design)
svymean(~gvt_salary, strat_design, na.rm=T)
confint(svymean(~hoh, strat_design), level = 0.95)

summary <-svyquantile(~gvt_salary, design=strat_design, quantiles=0.5, na.rm=TRUE, ci=T)
number = summary[[quantiles]]
attr(x = summary,which = "SE")

confint(svyquantile(~gvt_salary, design=strat_design, quantiles=0.5, na.rm=TRUE),level = 0.95)
tab <-svymean(~interaction(hoh,displacement_status,drop = TRUE), strata_design, ci=T)
ftab <- ftable(tab, rownames = list(hoh=c("not hoh", "hoh"),
                                    displacement_status=c("idp", "non displaced", "returnees")))
round(ftab*100,1)

svyby(~non_gvt_salary, ~displacement_status, svymean, design = strat_design,keep.var = T)

svyby(~hoh, ~displacement_status, svymean, design = strata_design, keep.names = F,na.rm=T)
svyby(~hoh, ~displacement_status, svymean, design = hype_design, keep.names = F,na.rm=T)
summary <- summary[as.character("idp"),]
colnames(summary)<- c("independent.var.value", "numbers", "min", "max")
se = ((summary[2] - summary[3])/1.96) %>% colnames()<-"se"
colnames(se) <- "se"

Ejdabia <- subset(strat_design, mantika_label=="Ejdabia")
sub_design <- subset(strata_design, displacement_status=="non_displaced")

subset.by <- paste0("as.numeric(", "non_gvt_salary", ")")
formula(subset.by)
Ejdabia <- subset(x = Ejdabia, !Ejdabia$variables[,"non_gvt_salary"] %in% c(NA))

Ejdabia$variables <- Ejdabia$variables[,"non_gvt_salary"]

svymean(~hoh, sebha)
svyby(~age_hoh, ~mantika_label, svyquantile, design = strata_design, na.rm=T, quantiles=0.5, ci=T)
median_with_confints_groups(dependent.var ="non_gvt_salary",independent.var ="displacement_status",design = strat_design)
svyquantile(~non_gvt_salary, design=Ejdabia, quantiles=0.5, na.rm=TRUE)

confint(svyby(~gvt_salary, ~displacement_status, svyquantile, design = strat_design, na.rm=T, quantiles=0.5, ci=T))
confint(svyquantile(~gvt_salary, design=strat_design, quantiles=0.5, na.rm=TRUE, ci=T), level = 0.95)

svyquantile(~gvt_salary, design=strat_design, quantiles=0.5, na.rm=TRUE, ci=F, alpha=0.05)
svyquantile(~gvt_salary, design=strata_design, quantiles=0.5, na.rm=TRUE)

analysisplan <- read.csv("./input/analysisplan.csv", stringsAsFactors = F)
analysisplan2 <- analysisplan_expand_repeat(analysisplan = analysisplan,data = data, se=)

a <- a[as.character("idp"),]
names(a) <- c("min", "max")


median_with_confints_groups(dependent.var = "non_gvt_salary", independent.var = "displacement_status",design = Ejdabia)

unique(Ejdabia$variables$displacement_status[!is.na(Ejdabia$variables$non_gvt_salary)])

Ejdabia$variables["displacement_status"][!is.na(strat_design$variables["displacement_status"])]
svyby(~non_gvt_salary, ~displacement_status, svyquantile, design = strat_design, na.rm=T, quantiles=0.5, ci=T)

attr(x = svyquantile(~gvt_salary, design=strata_design, quantiles=0.5, na.rm=TRUE, ci=T),which = "SE")

dstrata <- data %>% as_survey_design(strata= strata.names, weights = weights)

dstrata %>% summarise(numbers= survey_median(gvt_salary , vartype =c("se", "ci"), na.rm = T))
dstrata %>% summarise(numbers= survey_median(gvt_social_benefits , vartype =c("se", "ci"), na.rm = T))
dstrata %>% summarise(numbers= survey_median(non_gvt_salary , vartype =c("se", "ci"), na.rm = T))
dstrata %>% summarise(numbers= survey_median(water_expenditure , vartype =c("se", "ci"), na.rm = T))
dstrata %>% summarise(numbers= survey_mean(movement_restrictions_reasons.activities_armed_groups , vartype =c("se", "ci"), na.rm = T))
dstrata %>% summarise(numbers= survey_mean(killed_injured_assistance_source.specialist_healthcare_source, vartype =c("se", "ci"), na.rm = T))
dstrata %>% summarise(numbers= survey_mean(food_source.own_production, vartype =c("se", "ci"), na.rm = T))

dstrata %>% group_by(displacement_status) %>% summarise(numbers= survey_median(gvt_salary , vartype =c("se", "ci"), na.rm = T))
dstrata %>% group_by(displacement_status) %>% summarise(numbers= survey_median(non_gvt_salary , vartype =c("se", "ci"), na.rm = T))
dstrata %>% group_by(hoh) %>% summarise(numbers= survey_mean(vartype =c("se", "ci"), na.rm = T))

dstrata %>%  subset(mantika_label=="Sebha") %>% group_by(displacement_status) %>% summarise(numbers= survey_median(gvt_salary , vartype =c("se", "ci"), level = 0.95, na.rm = T))
svymean(~movement_restrictions_reasons.checkpoints_movement_restriction, strata_design, na.rm=T)
svymean(~movement_restrictions_reasons.activities_armed_groups, hype_design, na.rm=T)
svymean(~movement_restrictions_reasons.checkpoints_movement_restriction, hype_design, na.rm=T)
confint(svymean(~movement_restrictions_reasons.activities_armed_groups, strata_design, na.rm=T))

strata_design <- as_survey_design(strata_design)

svymean(~nb_adults_male, strata_design, na.rm=T)
svymean(~killed_injured_assistance_source.specialist_healthcare_source, strata_design, na.rm=T)

dstrata %>% summarise(numbers= survey_mean(movement_restrictions_reasons.checkpoints_movement_restriction , vartype =c("se", "ci"), na.rm = T))
dstrata %>% summarise(numbers= survey_median(gvt_salary , vartype =c("se", "ci"), na.rm = T))
dstrata %>% summarise(numbers= survey_median(gvt_salary , vartype =c("se", "ci"), na.rm = T))
dstrata %>% summarise(numbers= survey_median(gvt_salary , vartype =c("se", "ci"), na.rm = T))
dstrata %>% summarise(numbers= survey_median(gvt_salary , vartype =c("se", "ci"), na.rm = T))
dstrata %>% summarise(numbers= survey_median(gvt_salary , vartype =c("se", "ci"), na.rm = T))

colnames(dstrata %>% group_by(displacement_status) %>% summarise(med=survey_median(own_business_income, vartype = c("se","ci"), na.rm=T, level=0.95)))
