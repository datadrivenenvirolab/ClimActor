## code to update key dictionaries goes here

# Export dictionaries to RDA
key_dict <- read.csv("key_dict_new_Apr2020_withphoneticcodes.csv",
                     encoding = "UTF-8", stringsAsFactors = F)
save(key_dict, file = "../data/key_dict.rda")

# Export contextuals database to RDA
contextuals <- read.csv("contextuals_database_10June20_zy.csv",
                        encoding = "UTF-8", stringsAsFactors = F)
# save(contextuals, file = "../data/contextuals.rda")
usethis::use_data(contextuals, overwrite = T)
