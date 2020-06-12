# Documentation for data

#' @title Country Dictionary for \code{climactor}
#' @description A list of commonly found country names and their standardized equivalent
#' for use in the cleaning of non-state actor names. Other information on these
#' countries provided include ISO2 and ISO3, land area, population, and region.
#' @format country_dict is a data frame with 456 commonly country names (rows) and 9 variables
#' (columns)
#' @format \describe{
#'     \item{\code{wrong}}{char Commonly found country names across different datasets.
#'     One row of each country consists of the standardized version of the name}
#'     \item{\code{right}}{char Standardized version of the country name}
#'     \item{\code{iso}}{char 3 letter ISO codes for the country}
#'     \item{\code{region}}{char}
#'     \item{\code{Landarea}}{double Land area of the country}
#'     \item{\code{iso2}}{char 2 letter ISO codes for the country}
#'     \item{\code{Population}}{double Population for the country}
#'     }
"country_dict"

#' @title Key Dictionary for non-state actors
#' @description The key dictionary contains the commonly found names for non-state climate
#' actors found across different climate databases for use in the cleaning
#' of actor names. The dictionary also includes the different phonetic codes
#' to be used in the phonetic string matching.
#' @format key_dict is a dataframe with 29215 commonly found climate actor names (rows) and
#' 22 variables (columns)
#' @format \describe{
#'     \item{\code{right}}{char Commonly found climate actor names across different datasets.
#'     One row of each actor consists of the standardized version of the name}
#'     \item{\code{wrong}}{char Standardized version of the actor name}
#'     \item{\code{iso}}{char 3 letter ISO codes for the country}
#'     \item{\code{entity_type}}{char The entity type of the actor (City, Business, etc.)}
#'     \item{\code{allcaps}}{char The all capital version of the standardized name}
#'     \item{\code{caverphone - statcan}}{char Different phonetic codes of the actor names
#'     based on different phonetic algorithms}
#'     }
"key_dict"

#' @title Contextuals Database for subnational actors
#' @description The contextuals database contains important contextual data for the
#' subnational climate actors found across the different climate databases. See below for
#' details on what data is included
#' @format contextuals is a dataframe with contextuals information for 10462 unique actors
#' (rows) and 15 variables (columns)
#' #' @format \describe{
#'     \item{\code{name}}{char Name of the subnational actor}
#'     \item{\code{iso}}{char 3 letter ISO codes for the country of actor}
#'     \item{\code{country}}{char Country in which actor resides in}
#'     \item{\code{entity_type}}{char The entity type of the actor (City, Region, etc.)}
#'     \item{\code{region}}{char The broader region which the country of the
#'     subnational actor belongs to}
#'     \item{\code{area}}{double Total unit area of the actor}
#'     \item{\code{area_units}}{char Units which the area of the actor are expressed
#'     in}
#'     \item{\code{initiatives_committed}}{char Climate initiatives to which the actor pledged
#'     commitments to}
#'     \item{\code{num_commit}}{int Number of initiatives to which actor pledged commitments
#'     to}
#'     \item{\code{lat}}{double Latitude of the actor}
#'     \item{\code{lng}}{double Longitude of the actor}
#'     \item{\code{population}}{double Total population of the actor}
#'     \item{\code{population_year}}{int Year of recorded population}
#'     \item{\code{state}}{char State in which the actor is situated in}
#'     }
"contextuals"
