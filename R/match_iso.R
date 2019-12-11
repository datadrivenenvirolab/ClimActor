match_iso <- function(dataset, country.dict) {
  if ("iso" %in% names(dataset)){
    dataset$iso <- country.dict$iso[match(toupper(dataset$country), toupper(country.dict$right))]
  } else {
    dataset$iso <- NA
    dataset$iso <- country.dict$iso[match(toupper(dataset$country), toupper(country.dict$right))]
  }
  return(dataset)
}
