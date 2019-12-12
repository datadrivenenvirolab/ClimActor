# Data-Driven Lab
# Functions used for adding columns in the pre-processing of data for fuzzy matching

#' Renames the "name", "country", and "entity_type" columns to the correct column name
#'
#' @param dataset Dataset to rename columns for
#' @param raw_name_col Current column name for "name"
#' @param raw_country_col Current column name for "country"
#' @param raw_type_col Current column name for "entity_type"
#' @return The dataset with the appropriately named columns
#' @example rename_col(df, "actor", "Country", "type")
rename_col <- function(dataset, raw_name_col, raw_country_col, raw_type_col) {
  # Function that takes in the raw name, country, and entity.type columns and returns
  # standardized names for further analysis
  names(dataset)[names(dataset) == raw_name_col] <- "name"
  names(dataset)[names(dataset) == raw_country_col] <- "country"
  names(dataset)[names(dataset) == raw_type_col] <- "entity.type"
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


