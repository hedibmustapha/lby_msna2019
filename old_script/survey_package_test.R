library(survey)
library(srvyr)
data(api)

lookup <- unique(sampling_frame)
data_survey <- merge(sampling_frame, data, by='strata.names')
data_survey$weights <- weights(data)

design_survey <- svydesign(ids = ~1, strata = ~strata.names, 
                           data = data_survey, weights = ~weights(data_survey)
                           )

design_survey <- svydesign(ids = ~1, 
                           strata = ~filter(data, displacement_status == "returnee")[["strata.names"]], 
                           data = filter(data, displacement_status == "returnee"), 
                           weights = ~weights(filter(data, displacement_status == "returnee"))
                           )

design_survey <- svydesign(ids = ~1, strata = ~strata.names, data = data_survey,fpc = ~population, nest = T)

svytotal(~nb_displaced_hosted, design_survey, na.rm=T)[1]
svyquantile(~gvt_salary, design=strat_design, quantiles=0.5, na.rm=TRUE, ci=T)
summary <-svymean(~size_hh, strat_design, se=T)
summary <-svyquantile(~gvt_salary, design=strat_design, quantiles=0.5, na.rm=TRUE, ci=T)
confint(svymean(~hoh, strat_design), level = 0.95)
confint(svyquantile(~gvt_salary, design=strat_design, quantiles=0.5, na.rm=TRUE),level = 0.95)
number = summary[[quantiles]]
attr(x = summary,which = "SE")

svyby(~non_gvt_salary, ~displacement_status, svymean, design = strat_design,keep.var = T)

Ejdabia <- subset(strat_design, mantika_label=="Ejdabia")

svyby(~age_hoh, ~mantika_label, svyquantile, design = strata_design, na.rm=T, quantiles=0.5, ci=T)
confint(svyby(~gvt_salary, ~displacement_status, svyquantile, design = strat_design, na.rm=T, quantiles=0.5, ci=T))

