# Data-Driven Lab
# Functions used for adding columns in the pre-processing of data for fuzzy matching

#' @export
#' @title Renames columns within the dataset
#' @description Checks the dataset for the correct names necessary for further data
#' processing. If the requisite data names are not in the dataset, then prompts
#' the user for the columns to be renamed to the requisite names.
#'
#' @param dataset Dataset to rename columns for
#' @return The dataset with the appropriately named columns
#'
#' @example \dontrun{rename_col(df)
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

#' @export
#' @title Adds country column to the dataset
#'
#' @param dataset Dataset to add column to
#' @return The dataset with a new column named "country" added to dataset.
#'
#' @example \dontrun{add_country(df)}
add_country <- function(dataset){
  if ("country" %in% tolower(names(dataset))){
    cat("A \"country\" column already exists in the dataset.")
    return(dataset)
  } else {
    dataset$country <- NA
    return(dataset)
  }
}

#' @export
#' @title Adds the entity_type column to the dataset
#'
#' @param dataset Dataset to add column to
#' @param type Default entity type to set as value for the column
#' @return Dataset with new column consisting of "type" param added to dataset
#'
#' @examples
#' \dontrun{add_entity_type(df, "City")}
#' \dontrun{add_entity_type(df, NA)}
add_entity_type <- function(dataset, type){
  # Function that takes in a dataset and returns a new column "entity_type", with
  # type input as the entry for the column
  if ("entity_type" %in% tolower(names(dataset))){
    cat("A \"entity_type\" column already exists in the dataset.")
    return(dataset)
  } else {
    dataset$entity_type <- type
    return(dataset)
  }

}

#' @export
#' @title Adds both the entity_type and country columns to the dataset
#'
#' @param dataset Dataset to add columns to
#' @param type Default value to set for the entity_type column
#' @return Dataset with the 2 new columns
#'
#'
#' @examples
#' \dontrun{add_country_entity_type(df, NA)}
#' \dontrun{add_country_entity_type(df, "City")}
add_country_entity_type <- function(dataset, type = NA) {
  if (!"country" %in% tolower(names(dataset))) {
    dataset$country <- NA
  } else {
    cat("A \"country\" already exists in the dataset.\n")
  }
  if (!"entity_type" %in% tolower(names(dataset))) {
    dataset$entity_type <- type
  } else {
    cat("A \"entity type\" already exists in the dataset.\n")
  }
  return(dataset)
}

