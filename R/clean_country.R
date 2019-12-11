clean_country <- function(dataset, country.dict) {
  dataset$country <- country.dict$right[match(toupper(dataset$country),
                                              toupper(country.dict$wrong))]
  return(dataset)
}
