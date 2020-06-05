# Data-Driven Lab
# Functions used for cleaning and standardizing of values in the
# pre-processing of data for fuzzy matching

#' @export
#' @title Cleans the dataset's country names and adds iso.
#' @description Cleans the dataset actors' countries based on the package's country
#' dictionary and adds the corresponding iso to the dataset.
#'
#' @param dataset Dataset to clean the country names for
#' @param country.dict Country dictionary to clean the dataset against
#' @param iso Input either 2 or 3 to select for 2 or 3 letter ISO code. Defaults to ISO3
#' @param clean_enc Is the data read in with the correct encoding?
#' If unknown, set as FALSE. Defaults to TRUE
#' @return The original dataset with the country names cleaned
#'
#' @example \dontrun{clean_country_iso(df, country_dict, iso = 3, clean_enc = F)}
clean_country_iso <- function(dataset, country.dict, iso = 3, clean_enc = T) {
  # If not sure if data is clean, check and convert to try and repair the encoding
  if (!is.logical(clean_enc)){
    stop("clean_enc argument requires a logical (True/False) input.")
  }
  # Check for column naming using helper function
  dataset <- .col_check(dataset, "country", environment())
  if (exists("to.stop")){
    stop("Stopping function. Missing the \"country\" ",
         "column.")
  }
  # If not sure if data is clean, check and convert to try and repair the encoding

  if (!clean_enc & !all(is.na(dataset$country))){
    dataset$country <- .check_and_convert(dataset$country)
  }
  match_country <- which(dataset$country %in% .check_and_convert(country.dict$wrong))
  dataset$country[match_country] <- country.dict$right[match(dataset$country[match_country],
                                                             .check_and_convert(country.dict$wrong))]
  country_ind <<- which(!(dataset$country %in% country.dict$right))

  if (iso != 2 & iso != 3){
    stop("Please input either 2 or 3 for the \"iso\" argument.")
  }
  if (!("iso" %in% tolower(names(dataset)))){
    dataset$iso <- NA
  }
  if (iso == 2){
    dataset$iso <- country.dict$iso2[match(toupper(dataset$country),
                                           toupper(country.dict$right))]
  } else if (iso == 3){
    dataset$iso <- country.dict$iso[match(toupper(dataset$country),
                                          toupper(country.dict$right))]
  }
  if (length(country_ind) != 0){
    print(paste0("There are ", length(country_ind),
                 " entries with no exact matches in the country dictionary. The indices",
                 " for these names are recorded in the country_ind vector",
                 " Please use the fuzzify_country function to clean those names or input",
                 " them manually."))
  }

  # Helper function returns output that checks if the name is capitalized
  # Change back to capitalized version if the check is true
  if (exists("countryname")){
    names(dataset)[grepl("country", names(dataset))] <- countryname
  }
  return(dataset)
}


#' @export
#' @title Fills in the corresponding actor entity type for the dataset based on the actor name
#' @description Guesses the entity type for the actors within the dataset based on commonly
#' used words for the respective entity types.
#' @param dataset Dataset to fill in the entity type for
#' @return The original dataset with entity types filled for the actors
#'
#' @example \dontrun{fill_type(df)}
fill_type <- function(dataset) {
  # Check for column naming
  col <- "entity_type"
  dataset <- .col_check(dataset, col, environment())
  if (exists("to.stop")){
    stop(paste0("Stopping function. Please create or rename a \"", col, "\"",
                "column."))
  }
  # fill the corresponding entity type with "City" if it contains a U.S. state abbreviation
  dataset$entity_type[grep(", AL|, AK|, AZ|, AR|, CA|, CO|, CT|, DE|, FL|, GA|, HI|, ID|, IL|, IN|, IA|, KS|, KY|, LA|, ME|, MD|, MA|, MI|, MN|, MS|, MO|, MT|, NE|, NV|, NH|, NJ|, NM|, NY|, NC|, ND|, OH|, OK|, OR|, PA|, RI|, SC|, SD|, TN|, TX|, UT|, VT|, VA|, WA|, WV|, WI|, WY",
                           dataset$name, ignore.case = TRUE)] <- "City"

  # fill corresponding entity type with Company if it contains words that are usually associated with companies
  # this filling comes after the state abbreviations so that any company with "Inc"
  # (or similar company names that happen to have a ", ..") aren't coded as cities
  dataset$entity_type[grep(" Inc|servic|limited| Co\\.|inc\\.|util|constr|contract|plc|ltd|plc\\.|ltd\\.|P\\.L\\.C\\.|L\\.T\\.D\\.|produ|LLP|L\\.L\\.P\\.|L\\.L\\C\\.|LLC|group|hold|ltda|l\\.t\\.d\\.a\\.|craft |corp\\.| chapter|congregation|makers|method| stage|indust|organic|organiz|ingredi|transpo|glass|agricul|archite|hortic|logis|bevera|market|system|syst|corpo|packag|soluti|softwa|integra|perfo|desig|SRl|S\\.R\\.L\\.|chemic|cream|company|freight|metal|electr|intern|int'l|intl|aero|alcoh|contai|special|S\\.A\\.|SA\\.|sa de cv|sa de c\\.v\\.|s\\.a\\. de cv|s\\.a\\. de c\\.v\\.|C\\.V\\.|CV\\.|artform|corporat|co\\.|ltd\\.|print|maint|steel|rail|bank |banco |auto|build|special|plastic|health|medical|maintain|concie|office|hotel|food|center|charg|therap|pharma|device|harbor|harbour|mecan|mecha|ltda|L\\.T\\.D\\.A\\.|ltda\\.|styli|style|casting|investm|ventur|textil|knit|appare|merchan|sourc|soup|computer|labora|farm|greenh|outdoor|access|custom|produc|rubber|brewing| wood|lumber|bakery| baker| brand|dairy|confecti|interface|corporate|contract|electric|telecom|recycl|waste|energy|enviro|furnit|technolog|micro|surgic|manufac|interio|retail|holiday|worldwide|company|enterprise|propert|power",
                           dataset$name, ignore.case = TRUE)] <- "Company"

  # fill the corresponding entity type with City if it contains words associated
  # with "city" (as they're coded in the key dictionary)
  # this comes after the filling of company entity types because some city names
  # will include "corporation", "brand" or similar "company-esque" nwords in
  # their name (e.g. Pune Municipal Corporation is the civic body that governs Pune, a city in India)
  dataset$entity_type[grep("City|Muni|Town|County|Shire|District|Village|Assembly|Comm|Metro|Council|Ministry|Authority|Canton|Reg",
                           dataset$name, ignore.case = TRUE)] <- "City"

  # fill corresponding entity type with Region if it contains words associated
  # with "regions" (as coded in the key dictionary)
  # reasoning for having this entity type filled after companies is the same as
  # why the filling of cities came after companies
  dataset$entity_type[grep("Prov|Region|Government|State|Pref",
                           dataset$name, ignore.case = TRUE)] <- "Region"

  # alert the user to fill any NAs in the entity_type column, if they exist
  missing_ET <- sum(is.na(dataset$entity_type))
  if (missing_ET > 0) {
    print(paste0("You seem to have ", missing_ET, " missing values in the entity_type column. Please fill these before proceeding."))
  }
  cat("Warning: This function will be generally accurate for the entity type of most--but not all--entries. It is highly recommended you double check the entity types, fix any errors, and fill in any missing values. ")
  # Helper function returns output that checks if the name is slightly different
  # Change back to capitalized version if the check is true
  if (exists(paste0("entity_typename"))){
    names(dataset)[grepl("entity_type", names(dataset))] <- entity_typename
  }
  return(dataset)
}

# #' @export
# #' @title Standardizes existing entity types within the dataset
# #' @description Similar to function \code{fill_type}, guesses the entity type of actors based
# #' based on commonly used words. However, this function applies specifically
# #' to differentiate between different subnational actors (City vs Region)
# #' @param dataset Dataset to standardize and clean the entity type for
# #' @return Original dataset with the entity types cleaned
# #'
# #' @example \dontrun{standardize_type(df)}
# standardize_type <- function(dataset) {
#   # Check for column naming
#   col <- "entity_type"
#   .col_check(dataset, col)
#   if (exists("to.stop")){
#     stop(paste0("Stopping function. Please create or rename a \"", col, "\"",
#                 "column."))
#   }
#   dataset$entity_type[grep("City|Muni|Town|County|Shire|District|Village|Assembly|Comm|Metro|Council|Ministry|Authority|Canton|Reg",
#                            dataset$entity_type, ignore.case = TRUE)] <- "City"
#   dataset$entity_type[grep("Prov|Region|Government|State|Pref",
#                            dataset$entity_type, ignore.case = TRUE)] <- "Region"
#   # Helper function returns output that checks if the name is capitalized
#   # Change back to capitalized version if the check is true
#   if (exists(paste0("entity_typename"))){
#     names(dataset)[grepl("entity_type", names(dataset))] <- entity_typename
#   }
#   return(dataset)
# # }

#' @export
#' @title Remove extraneous words from actors' names
#' @description Removes extraneous words such as "council", "district", etc. from the
#' names of actors. See vignette for the full list of "extraneous" words
#'
#' @param dataset Dataset containing actors' names
#' @return Dataset with extraneous words removed from actors' names
remove_extra <- function(dataset){
  # cleaning names by getting rid of extraneous words
  # this list of words can be updated in the future
  # Check for column naming using helper function
  col <- "name"
  dataset <- .col_check(dataset, col, environment())
  if (exists("to.stop")){
    stop(paste0("Stopping function. Please create or rename a \"", col, "\"",
                "column."))
  }
  words <- c("council|adjuntament|corporation|government|urban|district|mayor|
           the|of|city|autonomous|state|province|provincial|county|municipality|
           municipalidad de|municipalidad|municipio|kommune|municipal|prefecture|
           prefectural|metropolitana|metropolis|m??tropole|metropolitan|metropole|town|
           community|communat|communat??|Ayuntamiento|Gemeente|Comune di|Comune|Kommune|
           Republic")
  dataset$name <- stringr::str_replace_all(dataset$name,
                                           stringr::regex(words, ignore_case = T),
                                           "")

  dataset$name <- trimws(dataset$name)
  # Helper function returns output that checks if the name is capitalized
  # Change back to capitalized version if the check is true
  if (exists(paste0("namename"))){
    names(dataset)[grepl("name", names(dataset))] <- namename
  }
  return(dataset)
}

#' @export
#' @title Resolve conflicting entity types
#' @description Resolve entity type conflicts between user's dataset and the key
#' dictionary. The function checks for entries that have the same name and are from the
#' same country between the user's dataset and the key dictionary but have different entity
#' types.
#'
#' @param dataset Dataset containing actors' names
#' @param key.dict Key dictionary
#' @param clean_enc Is the data read in with the correct encoding?
#' If unknown, set as FALSE. Defaults to TRUE.
#' @return Returns the dataset with entity types resolved (if applicable)
#' @return Also returns a dataset that contains the different conflicts (if applicable)
#'

resolve_entity_types <- function(dataset, key.dict, clean_enc = T){
  if (!is.logical(clean_enc)){
    stop("clean_enc argument requires a logical (True/False) input.")
  }
  # If not sure if data is clean, check and convert to try and repair the encoding
  if (!clean_enc){
    dataset$name <- .check_and_convert(dataset$name)
  }
  # Check for column naming using helper function
  dataset <- .col_check(dataset, "name", environment())
  dataset <- .col_check(dataset, "entity_type", environment())
  dataset <- .col_check(dataset, "iso", environment())
  if (exists("to.stop")){
    stop("Stopping function. Missing the \"name\", \"entity_type\", or \"iso\" columns.")
  }
  # Find actors that have the correct names and iso but not the correct entity type
  # Subset data for those that have correct names and iso first
  name_iso_right <- which(paste0(dataset$name, dataset$iso) %in% paste0(key.dict$right, key.dict$iso))
  name_iso_right_short <- which(unique(paste0(dataset$name, dataset$iso)) %in% paste0(key.dict$right, key.dict$iso))
  # Coerce NA entity types to random character
  dataset$entity_type[intersect(which(is.na(dataset$entity_type)), name_iso_right)] <- ","
  # Check for wrong entity types in those that have correct names and iso
  dict_ind <- na.omit(match(paste0(dataset$name[name_iso_right], dataset$iso[name_iso_right]),
                            paste0(key.dict$right, key.dict$iso)))
  ent_ind <- name_iso_right[dataset$entity_type[name_iso_right] != key.dict$entity_type[dict_ind]]
  dict_ent_ind <- dict_ind[dataset$entity_type[name_iso_right] != key.dict$entity_type[dict_ind]]
  # Let users decide which entity type conflicts they want to resolve
  if (length(ent_ind) != 0){
    # Print number of conflicts
    cat(paste0("We found ", length(ent_ind), " number of entries with the same actor name ",
               "and iso but conflicting entity types. Would you like to resolve all",
               " conflicts by accepting the key dictionary's entity type? (Y/N)"))
    ans <- readline(prompt = "Answer: ")
    # Make sure user enters valid response
    # Allow users to 1) accept all key dict's entity types or 2) not resolve and export
    # a dataset of conflicts
    while (substr(toupper(as.character(ans)), 1, 1) != "Y" & substr(toupper(as.character(ans)), 1, 1) != "N"){
      cat("Please enter a valid input (Y/N)")
      ans <- readline(prompt = "Answer: ")
    }
    if (substr(toupper(as.character(ans)), 1, 1) == "Y"){
      # Resolve conflicts by taking all key dict's entity types
      dataset$entity_type[ent_ind] <- key.dict$entity_type[dict_ent_ind]
      return(dataset)
    } else if (substr(toupper(as.character(ans)), 1, 1) == "N"){
      # Export a dataset consisting of conflicts
      cat(paste0("A dataframe of the actors with conflicting entity types will be ",
                 "created for closer inspection of the conflicts."))
      entity_conflicts <<- data.frame(name = dataset$name[ent_ind],
                                      iso = dataset$iso[ent_ind],
                                      user_entity = dataset$entity_type[ent_ind],
                                      keydict_entity = key.dict$entity_type[dict_ent_ind],
                                      user_index = ent_ind,
                                      keydict_index = dict_ent_ind)
      return(dataset)
    }
  } else {
    cat(paste0("There are no entries with conflicting entity types."))
  }
}
