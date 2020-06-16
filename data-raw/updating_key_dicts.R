## code to update key dictionaries goes here

# Export dictionaries to RDA
key_dict <- read.csv("key_dict_Apr2020.csv",
                     encoding = "UTF-8", stringsAsFactors = F)

save(key_dict, file = "../data/key_dict.rda")

# Country dict
country_dict <- read.csv("country_dict.csv", encoding = "UTF-8",
                         stringsAsFactors = F)
# Remove some columns for now
names(country_dict)
country_dict <- country_dict[, -grep("code", names(country_dict))]
country_dict <- country_dict[, -grep("PopulationGroup", names(country_dict))]
usethis::use_data(country_dict, overwrite = T)

# Export contextuals database to RDA
contextuals <- read.csv("subnational_contextuals_database_June2020.csv",
                        encoding = "UTF-8", stringsAsFactors = F)
names(contextuals)
contextuals <- contextuals[, -grep("gdp", names(contextuals))]
names(contextuals)
write.csv(contextuals, "subnational_contextuals_database_June2020.csv",
          row.names = F, fileEncoding = "UTF-8")
# save(contextuals, file = "../data/contextuals.rda")
usethis::use_data(contextuals, overwrite = T)
