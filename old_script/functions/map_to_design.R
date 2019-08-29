#' Map to Design
#'
#' creates a `survey` design object from the data
#'
#' @param data the dataset as a sampling frame. Must match the sampling frame provided to create the `weighting_function` produced with `map_to_weighting()`
#' @param weighting_function if cluster sampling was used, what's the name of the column in `data` that identifies the cluster?
#' @param strata.variable.name strata names column 
#' @param population.variable.name population figures column
#' @param weights weights variable
#' @details create a `survey` package design object from the data and information on the sampling strategy
#' @return a `survey` package design object
#' @examples map_to_design(data,cluster_variable_name="cluster_id")
#' @export
map_to_design <- function(data,
                          strata.variable.name,
                          population.variable.name,
                          weights.name,
                          cluster_variable_name = NULL
                          ) {
  
  # if no weighting function / cluster variable provided, set defaults, otherwise use parameters:
  if(is.null(cluster_variable_name)){
    cluster.ids <- as.formula(c("~1"))}else{
      cluster.ids <- paste0("~",cluster_variable_name)
      if(any(is.na(data[cluster_variable_name]))){
        data <- data[!is.na(data[cluster_variable_name]),]
        warning("some records in the data has a missing cluster variable. These records have been removed")}}
    
  strata.names <- paste0("~",strata.variable.name)
    population.names <- paste0("~",population.variable.name)
    weights.name <- paste0("~",weights.name)
 
  survey.design <- svydesign(data = data,
                             ids = formula(cluster.ids),
                             strata = formula(strata.names),
                             fpc = formula(population.names),
                             weights = formula(weights.name)
                             )
  return(survey.design)}