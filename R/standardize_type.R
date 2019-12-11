standardize_type <- function(dataset) {
  dataset$entity.type[grep("City|Muni|Town|County|Shire|District|Village|Assembly|Comm|Metro|Council|Ministry|Authority|Canton|Reg", 
                           dataset$entity.type, ignore.case = TRUE)] <- "City"
  dataset$entity.type[grep("Prov|Region|Government|State|Pref", 
                           dataset$entity.type, ignore.case = TRUE)] <- "Region"
  return(dataset)
}