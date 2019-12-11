# Data-Driven Lab
# Functions used for pre-processing the data for fuzzy matching

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

#' Coerce location names to handle special characters
#'
#' @param locations Names of location (city, country, etc.)
#' @return A vector of locations names with the special character replaced with
#' cloest equivalent
#' @example coerce_locations_names(df$name)
coerce_location_names <- function(locations) {
  locations <- gsub("[ÀÁÂÃÄÅÆĀĂĄǍǞǠǢǺǼȀȂẤẦẨẶȦḀẠẢ]", "A", locations)
  locations <- gsub("[àáâãäåæāăąǎǟǡǣǻǽȁȃấầẩặȧḁạảẚ]", "a", locations)
  locations <- gsub("[ḂḄḆɃ]", "B", locations)
  locations <- gsub("[ḃḅḇƀᵬᶀɓ]", "b", locations)
  locations <- gsub("[ÇĆĈĊČ]", "C", locations)
  locations <- gsub("[çćĉċč]", "c", locations)
  locations <- gsub("[ÐĎĐ]", "D", locations)
  locations <- gsub("[ðďđ]", "d", locations)
  locations <- gsub("[ÈÉÊËĒĔĖĘĚȄȆ]", "E", locations)
  locations <- gsub("[èéêëēĕėęěȅȇ]", "e", locations)
  locations <- gsub("[Ƒ]", "F", locations)
  locations <- gsub("[ƒ]", "f", locations)
  locations <- gsub("[ĜĞĠĢǤǦ]", "G", locations)
  locations <- gsub("[ĝğġģǥǧ]", "g", locations)
  locations <- gsub("[ĤĦ]", "H", locations)
  locations <- gsub("[ĥħ]", "h", locations)
  locations <- gsub("[ÌÍÎÏĨĪĬĮİĲǏȈȊ]", "I", locations)
  locations <- gsub("[ìíîïĩīĭįıĳǐȉȋ]", "i", locations)
  locations <- gsub("[ĴɈ]", "J", locations)
  locations <- gsub("[ĵǰɉ]", "j", locations)
  locations <- gsub("[Ķ]", "K", locations)
  locations <- gsub("[ķĸ]", "k", locations)
  locations <- gsub("[ĹĻĽĿŁ]", "L", locations)
  locations <- gsub("[ĺļľŀłƚ]", "l", locations)
  locations <- gsub("[ÑŃŅŇŊǸ]", "N", locations)
  locations <- gsub("[ñńņňŉŋǹ]", "n", locations)
  locations <- gsub("[ÒÓÔÕÖØŌŎŐŒƟƠƢǑǪǬȌȎȪȬȮȰ]", "O", locations)
  locations <- gsub("[òóôõöøōŏőœơƣǒǫǭȍȏȫȭȯȱ]", "o", locations)
  locations <- gsub("[ŔŖŘȐȒɌ]", "R", locations)
  locations <- gsub("[ŕŗřȑȓɍ]", "r", locations)
  locations <- gsub("[ŚŜŞŠȘ]", "S", locations)
  locations <- gsub("[śŝşšș]", "s", locations)
  locations <- gsub("[ŢŤŦƬƮ]", "T", locations)
  locations <- gsub("[ţťŧƫƭ]", "t", locations)
  locations <- gsub("[ÙÚÛÜŨŪŬŮŰŲǓǕǗǙǛȔȖ]", "U", locations)
  locations <- gsub("[ùúûüũūŭůűųǔǖǘǚǜȕȗ]", "u", locations)
  locations <- gsub("[Ŵ]", "W", locations)
  locations <- gsub("[ŵ]", "w", locations)
  locations <- gsub("[ÝŶŸɎ]", "Y", locations)
  locations <- gsub("[ýÿŷɏ]", "y", locations)
  locations <- gsub("[ŹŻŽẒẔẐŹƵ]", "Z", locations)
  locations <- gsub("[źżžẓẕẑʐʑȥƶ]", "z", locations)

  return(locations)
}

