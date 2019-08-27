library(survey)
data(api)

srs_design <- svydesign(id=~1, fpc = ~fpc, data=apisrs)
svytotal(~enroll, srs_design)
svymean(~enroll, srs_design)

lookup <- unique(sampling_frame)
data <- merge(lookup, data, by='strata.names')
data$weights <- weights(data)
strata.weights <- weights(data)
strat_design <- svydesign(id=~1, strata = names(strata.weights), weights = as.vector(strata.weights),
                          data = data, nest = T)

strat_design <- svydesign(ids=~1, strata = ~strata.names, fpc = ~population,
                          data = data, nest = T)
svymean(~pull_factors.conflict_ended, strat_design,na.rm=T)
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
tab <-svymean(~interaction(hoh,displacement_status,drop = TRUE), strat_design, ci=T)
ftab <- ftable(tab, rownames = list(hoh=c("not hoh", "hoh"),
                                    displacement_status=c("idp", "non displaced", "returnees")))
round(ftab*100,1)

svyby(~non_gvt_salary, ~displacement_status, svymean, design = strat_design,keep.var = T)

summary <-svyby(~own_business_income, ~displacement_status, svymean, design = strat_design, keep.names = T, vartype = "ci", na.rm=T)
summary <- summary[as.character("idp"),]
colnames(summary)<- c("independent.var.value", "numbers", "min", "max")
se = ((summary[2] - summary[3])/1.96) %>% colnames()<-"se"
colnames(se) <- "se"

Ejdabia <- subset(strat_design, mantika_label=="Ejdabia")

subset.by <- paste0("as.numeric(", "non_gvt_salary", ")")
formula(subset.by)
Ejdabia <- subset(x = Ejdabia, !Ejdabia$variables[,"non_gvt_salary"] %in% c(NA))

Ejdabia$variables <- Ejdabia$variables[,"non_gvt_salary"]

svymean(~hoh, sebha)
svyby(~non_gvt_salary, ~displacement_status, svyquantile, design = strat_design, na.rm=T, quantiles=0.5, ci=T)
median_with_confints_groups(dependent.var ="non_gvt_salary",independent.var ="displacement_status",design = strat_design)
svyquantile(~non_gvt_salary, design=Ejdabia, quantiles=0.5, na.rm=TRUE)

confint(svyby(~gvt_salary, ~displacement_status, svyquantile, design = strat_design, na.rm=T, quantiles=0.5, ci=T))
confint(svyquantile(~gvt_salary, design=strat_design, quantiles=0.5, na.rm=TRUE, ci=T), level = 0.95)

svyquantile(~gvt_salary, design=strat_design, quantiles=0.5, na.rm=TRUE, ci=F, alpha=0.05)
svyquantile(~gvt_salary, design=strat_design, quantiles=0.5, na.rm=TRUE, ci=T, alpha=0.15)

analysisplan <- read.csv("./input/analysisplan.csv", stringsAsFactors = F)
analysisplan2 <- analysisplan_expand_repeat(analysisplan = analysisplan,data = data)

a <- a[as.character("idp"),]
names(a) <- c("min", "max")


median_with_confints_groups(dependent.var = "non_gvt_salary", independent.var = "displacement_status",design = Ejdabia)

unique(Ejdabia$variables$displacement_status[!is.na(Ejdabia$variables$non_gvt_salary)])

Ejdabia$variables["displacement_status"][!is.na(strat_design$variables["displacement_status"])]
svyby(~non_gvt_salary, ~displacement_status, svyquantile, design = strat_design, na.rm=T, quantiles=0.5, ci=T)
