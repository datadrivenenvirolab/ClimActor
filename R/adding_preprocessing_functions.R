# Data-Driven Lab
# Functions used for adding columns in the pre-processing of data for fuzzy matching

#' @title Renames the columns within the dataset
#' @description Checks the dataset for the correct names necessary for further data
#' @description processing. If the requisite data names are not in the dataset, then prompts
#' @description the user for the columns to be renamed to the requisite names.
#'
#' @param dataset Dataset to rename columns for
#' @return The dataset with the appropriately named columns
#' @example rename_col(df)
rename_col <- function(dataset) {
  if (!("name" %in% tolower(names(dataset)))){
    cat("Please input the column name that contains the actors' names. \nInput \"skip\" if column does not exist.")
    ans <- readline(prompt = "Input:")
    if (tolower(ans) != "skip"){
      names(dataset)[tolower(names(dataset)) == tolower(ans)] <- "name"
    }
  }
  if(!("country" %in% tolower(names(dataset)))){
    cat("Please input the column name that contains the countries. \nInput \"skip\" if column does not exist.")
    ans <- readline(prompt = "Input:")
    if (tolower(ans) != "skip"){
      names(dataset)[tolower(names(dataset)) == tolower(ans)] <- "country"
    }
  }
  if (!("entity_type" %in% tolower(names(dataset)))){
    cat("Please input the column name that contains the actors' entity types. \nInput \"skip\" if column does not exist.")
    ans <- readline(prompt = "Input:")
    if (tolower(ans) != "skip"){
      names(dataset)[tolower(names(dataset)) == tolower(ans)] <- "entity_type"
    }
  }
  if (all(c("name", "country", "entity_type") %in% names(dataset))) {
    cat("It seems like you have all the requisite column names in your dataset")
  }
  return(dataset)

}

#' Adds country column to the dataset
#'
#' @param dataset Dataset to add column to
#' @return The dataset with a new column named "country" added to dataset.
#' @example add_country(df)
add_country <- function(dataset){
  dataset$country <- NA
  return(dataset)
}

#' Adds the entity_type column to the dataset
#'
#' @param dataset Dataset to add column to
#' @param type Default entity type to set as value for the column
#' @return Dataset with new column consisting of "type" param added to dataset
#' @examples
#' add_entity_type(df, "city")
#' add_entity_type(df, NA)
add_entity_type <- function(dataset, type){
  # Function that takes in a dataset and returns a new column "entity.type", with
  # type input as the entry for the column
  dataset$entity.type <- type
  return(dataset)
}

#' Adds both the entity_type and country columns to the dataset
#'
#' @param dataset Dataset to add columns to
#' @param type Default value to set for both columns
#' @return Dataset with the 2 new columns
#' @examples
#' add_country_entity_type(df, NA)
#' add_country_entity_type(df, "")
add_country_entity_type <- function(dataset, type = NA) {
  if (!"country" %in% names(dataset)) {
    dataset$country <- NA
  }
  if (!"entity.type" %in% names(dataset)) {
    dataset$entity.type <- type
  } else if (!is.na(type)) {
    dataset$entity.type <- type
  }
  return(dataset)
}


