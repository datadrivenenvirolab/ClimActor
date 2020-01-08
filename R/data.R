# Documentation for data

#' @title Country Dictionary for \code{climactor}
#' @description A list of commonly found country names and their standardized equivalent
#' @description for use in the cleaning of non-state actor names. Other information on these
#' @description countries provided include ISO2 and ISO3, land area, population, and region.
#' @format country_dict is a data frame with 456 commonly country names (rows) and 9 variables
#' @format (columns)
#' \describe{
#'     \item{\code{wrong}}{char Commonly found country names across different datasets.
#'     One row of each country consists of the standardized version of the name}
#'     \item{\code{right}}{char Standardized version of the country name}
#'     \item{\code{code}}{double}
#'     \item{\code{iso}}{char 3 letter ISO codes for the country}
#'     \item{\code{region}}{char}
#'     \item{\code{Landarea}}{double Land area of the country}
#'     \item{\code{iso2}}{char 2 letter ISO codes for the country}
#'     \item{\code{Population}}{double Population for the country}
#'     \item{\code{PopulationGroup}}{char Population group for the country}
#'     }
"country_dict"

#' @title Key Dictionary for non-state actors
#' @description The key dictionary contains the commonly found names for non-state climate
#' @description actors found across different climate databases for use in the cleaning
#' @description of actor names. The dictionary also includes the different phonetic codes
#' @description to be used in the phonetic string matching.
#' @format key_dict is a dataframe with 29215 commonly found climate actor names (rows) and
#' @format 22 variables (columns)
#' \describe{
#'     \item{\code{right}}{char Commonly found climate actor names across different datasets.
#'     One row of each actor consists of the standardized version of the name}
#'     \item{\code{wrong}}{char Standardized version of the actor name}
#'     \item{\code{iso}}{char 3 letter ISO codes for the country}
#'     \item{\code{entity.type}}{char The entity type of the actor (City, Business, etc.)}
#'     \item{\code{allcaps}}{char The all capital version of the standardized name}
#'     \item{\code{caverphone - statcan}}{char Different phonetic codes of the actor names
#'     based on different phonetic algorithms}
#'     }
"key_dict"
