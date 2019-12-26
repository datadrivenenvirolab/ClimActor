# Data-Driven Lab
# Functions used for cleaning and standardizing of values in the
# pre-processing of data for fuzzy matching


#' Coerce location names to handle special characters
#'
#' @param locations Column with names of location (city, country, etc.)
#' @return A vector of locations names with the special character replaced with
#' closest equivalent
#' @example coerce_locations_names(df$name)
.coerce_location_names <- function(locations) {
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

#' Helper function to help check and convert the encoding of the column specified
#' @param column Column to check and convert the encoding for
#' @return column with the encoding (hopefully) converted
.check_and_convert <- function(col){
  # Check if there exists a hidden environment to create hidden variables for
  # If not, then create one
  if (!exists(".pkgenv")){
    .pkgenv <- new.env(parent = emptyenv())
  }
  if (!exists("foreign", envir = .pkgenv)){
    assign("foreign", names(which(!unlist(l10n_info()[2:3]))),
           envir = .pkgenv)
    assign("native", names(which(unlist(l10n_info()[2:3]))),
           envir = .pkgenv)
  }
  if (.pkgenv$native == "UTF-8"){
    col <- iconv(col, from = "UTF-8", to = "latin1")
    col <- iconv(col, from = "latin1", to = "UTF-8")
  } else if (.pkgenv$native == "Latin-1"){
    col <- iconv(col, from = "latin1", to = "UTF-8")
    col <- iconv(col, from = "UTF-8", to = "UTF-8")
  }
}

#' Cleans the dataset's country names and adds iso.
#' @description Cleans the dataset actors' countries based on the package's country
#' @description dictionary and adds the corresponding iso to the dataset.
#'
#' @param dataset Dataset to clean the country names for
#' @param country.dict Country dictionary to clean the dataset against
#' @param iso Input either 2 or 3 to select for 2 or 3 letter ISO code. Defaults to ISO3
#' @param utf Is the data in UTF-8 encoding? If unknown, set as FALSE. Defaults to FALSE.
#' @return The original dataset with the country names cleaned
#' @example clean_country_iso(df, country_dict, iso = 3)
clean_country_iso <- function(dataset, country.dict, iso = 3, utf = F) {
  # If not sure if data is clean, check and convert to try to convert it to UTF-8
  if (!is.logical(utf)){
    stop("utf argument requires a logical (True/False) input.")
  }
  if (!utf){
    .check_and_convert(dataset$country)
  }
  dataset$country <- country.dict$right[match(toupper(dataset$country),
                                              toupper(country.dict$wrong))]
  if (iso != 2 & iso != 3){
    stop("Please input either 2 or 3 for the \"iso\" argument.")
  }
  if (!("iso" %in% names(dataset))){
    dataset$iso <- NA
  }
  if (iso == 2){
    dataset$iso <- country.dict$iso2[match(toupper(dataset$country),
                                           toupper(country.dict$right))]
  } else if (iso == 3){
    dataset$iso <- country.dict$iso[match(toupper(dataset$country),
                                          toupper(country.dict$right))]
  }
  return(dataset)
}


#' Fills in the corresponding actor entity type for the dataset based on the actor name
#'
#' @param dataset Dataset to fill in the entity type for
#' @return The original dataset with entity types filled for the actors
#' @example fill_type(df)
fill_type <- function(dataset) {

  # fill the corresponding entity type with "City" if it contains a U.S. state abbreviation
  dataset$entity.type[grep(", AL|, AK|, AZ|, AR|, CA|, CO|, CT|, DE|, FL|, GA|, HI|, ID|, IL|, IN|, IA|, KS|, KY|, LA|, ME|, MD|, MA|, MI|, MN|, MS|, MO|, MT|, NE|, NV|, NH|, NJ|, NM|, NY|, NC|, ND|, OH|, OK|, OR|, PA|, RI|, SC|, SD|, TN|, TX|, UT|, VT|, VA|, WA|, WV|, WI|, WY",
                           dataset$name, ignore.case = TRUE)] <- "City"

  # fill corresponding entity type with Company if it contains words that are usually associated with companies
  # this filling comes after the state abbreviations so that any company with "Inc"
  # (or similar company names that happen to have a ", ..") aren't coded as cities
  dataset$entity.type[grep(" Inc|servic|limited| Co[.]|inc[.]|util|constr|contract|plc|ltd|plc[.]|ltd[.]|P[.]L[.]C[.]|L[.]T[.]D[.]|produ|LLP|L[.]L[.]P[.]|L[.]L[.]C[.]|LLC|group|hold|ltda|l[.]t[.]d[.]a[.]|craft |corp[.]| chapter|congregation|makers|method| stage|indust|organic|organiz|ingredi|transpo|glass|agricul|archite|hortic|logis|bevera|market|system|syst|corpo|packag|soluti|softwa|integra|perfo|desig|SRl|S[.]R[.]L[.]|chemic|cream|company|freight|metal|electr|intern|int'l|intl|aero|alcoh|contai|special|S[.]A[.]|SA[.]|sa de cv|sa de c[.]v[.]|s[.]a[.] de cv|s[.]a[.] de c[.]v[.]|C[.]V[.]|CV[.]|artform|corporat|co[.]|ltd[.]|print|maint|steel|rail|bank |banco |auto|build|special|plastic|health|medical|maintain|concie|office|hotel|food|center|charg|therap|pharma|device|harbor|harbour|mecan|mecha|ltda|L[.]T[.]D[.]A[.]|ltda[.]|styli|style|casting|investm|ventur|textil|knit|appare|merchan|sourc|soup|computer|labora|farm|greenh|outdoor|access|custom|produc|rubber|brewing| wood|lumber|bakery| baker| brand|dairy|confecti|interface|corporate|contract|electric|telecom|recycl|waste|energy|enviro|furnit|technolog|micro|surgic|manufac|interio|retail|holiday|worldwide|company|enterprise|propert|power",
                           dataset$name, ignore.case = TRUE)] <- "Company"

  # fill the corresponding entity type with City if it contains words associated
  # with "city" (as they're coded in the key dictionary)
  # this comes after the filling of company entity types because some city names
  # will include "corporation", "brand" or similar "company-esque" nwords in
  # their name (e.g. Pune Municipal Corporation is the civic body that governs Pune, a city in India)
  dataset$entity.type[grep("City|Muni|Town|County|Shire|District|Village|Assembly|Comm|Metro|Council|Ministry|Authority|Canton|Reg",
                           dataset$name, ignore.case = TRUE)] <- "City"

  # fill corresponding entity type with Region if it contains words associated
  # with "regions" (as coded in the key dictionary)
  # reasoning for having this entity type filled after companies is the same as
  # why the filling of cities came after companies
  dataset$entity.type[grep("Prov|Region|Government|State|Pref",
                           dataset$name, ignore.case = TRUE)] <- "Region"

  # alert the user to fill any NAs in the entity.type column, if they exist
  missing_ET <- sum(is.na(dataset$entity.type))
  if (length(missing_ET) > 0) {
    print(paste0("You seem to have ", missing_ET, " missing values in the entity.type column. Please fill these before proceeding."))
  }
  cat("Warning: This function will be generally accurate for the entity type of most--but not all--entries. It is highly recommended you double check the entity types, fix any errors, and fill in any missing values. ")
  return(dataset)
}

#' Standardizes and cleans existing entity types within the dataset
#'
#' @param dataset Dataset to standardize and clean the entity type for
#' @return Original dataset with the entity types cleaned
#' @example standardize_type(df)
standardize_type <- function(dataset) {
  dataset$entity.type[grep("City|Muni|Town|County|Shire|District|Village|Assembly|Comm|Metro|Council|Ministry|Authority|Canton|Reg",
                           dataset$entity.type, ignore.case = TRUE)] <- "City"
  dataset$entity.type[grep("Prov|Region|Government|State|Pref",
                           dataset$entity.type, ignore.case = TRUE)] <- "Region"
  return(dataset)
}

#' Remove extraneous words from actors' names
#' @description Removes extraneous words such as "council", "district", etc. from the
#' @description names of actors. See vignette for the full list of "extraneous" words
#'
#' @param dataset Dataset containing actors' names
#' @return Dataset with extraneous words removed from actors' names
remove_extra <- function(dataset){
  # cleaning names by getting rid of extraneous words
  # this list of words can be updated in the future
  words <- c("council|adjuntament|corporation|government|urban|district|mayor|
           the|of|city|autonomous|state|province|provincial|county|municipality|
           municipalidad de|municipalidad|municipio|kommune|municipal|prefecture|
           prefectural|metropolitana|metropolis|m??tropole|metropolitan|metropole|town|
           community|communat|communat??|Ayuntamiento|Gemeente|Comune di|Comune|Kommune|
           Republic")
  dataset$name[indices] <- stringr::str_replace_all(dataset$name[indices],
                                                    stringr::regex(words, ignore_case = T), "")

  dataset$name[indices] <- trimws(dataset$name[indices])
  return(dataset)
}
