map_to_design_medians <- function(data, strata.names, population) {
  strata.names.strings <- paste0("~",strata.names)
  population.strings <- paste0("~",population)
    survey.design <- svydesign(data = data, ids =~1, 
                               strata = formula(strata.names.strings), fpc = formula(population.strings), 
                               nest = T)
    return(survey.design)
}
