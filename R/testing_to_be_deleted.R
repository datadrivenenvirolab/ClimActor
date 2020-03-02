source("helper_functions.R", encoding = "UTF-8")

key.dict <- read.csv("../data-raw/key_dict_new_withphoneticcodes.csv",
                     stringsAsFactors = F)

stringi::stri_enc_mark(key.dict$wrong)
ind <- which(stringi::stri_enc_mark(key.dict$wrong) != "ASCII")

head(key.dict$wrong[stringi::stri_enc_mark(key.dict$wrong) != "ASCII"])

nonascii <- data.frame(string = key.dict$wrong[stringi::stri_enc_mark(col) != "ASCII"],
                       ind = which(stringi::stri_enc_mark(col) != "ASCII"))

kd2 <- .check_and_convert(key.dict$wrong)
head(kd2, 10)
tail(kd2, 10)
tail(key.dict$wrong, 10)
stringi::stri_enc_mark(kd2)

head(key.dict$wrong, 10)
kd2[ind]
key.dict$wrong[ind]
rvest::repair_encoding(key.dict$wrong[ind[1:200]])
