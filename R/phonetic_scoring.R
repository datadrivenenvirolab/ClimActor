#issues with phonetic algorithm:
#scoring is currently 1 to 0 but that has issues because 
#1. no nuance/distribution of scores like 0.5 that are somewhat close -- fixed, i think
#2. for other algorithms, it's not possible to have a 1 unless the word is an exact match. 
#for phonetic algs, it's possible to have a 1 as long as the generated character code (from the alg) is the same 
#3. only useful over the standard english alphabet A-Z, 
#bc most algs ignore special characters like ü, which are common in a lot of cities--
#i'll get a warning and the alg will return an NA if it encounters weird variables 
#which reduces its usefullness--
#the NA is a concern beacuse if the original name has an accent, 
#it'll return an NA and then any name in the key dictionary that has an accent 
#or umlaut will return an NA and match with the original name's "character coding" 
#as well--i can do na.rm = TRUE i guess?
#4. only one word can be used (spaces count as special characters)--
#fixed by deleting spaces, but that might mess things up

## editing methods: osa, lv, dl, lcs, hamming, qgram, cosine, jaccard, jw
## phonetic algs: "caverphone", "caverphone.modified", "cologne", 
## "lein",  "metaphone", "nysiis", "nysiis.modified", "onca", 
## "onca.modified", "onca.refined", "onca.modified.refined", "phonex", 
## "rogerroot", "soundex", "soundex.refined", "statcan"

#creating sample names to fuzzy match
library(phonics)
set.seed(230)
indices <- sample(1:nrow(x), 2, replace = FALSE)
rawnames <- x$name[indices]
names <- c("Thane", "Sudbury, Canada", "Delta", "Villavi, CO", "gangdong", 
           "Montreal",  "angnong,  republic korea", "panabi, country of india", 
           "osu, KR rep", "vancouver, CAN", "San Christ de las Casas",  "Lrenskog",  
           "Kwadukuza, S Africa", "bengaluru", "kuqing north,  malaysia", 
           "City of usan", "armenia city government", "village o f  johaness", 
           "lresn city", "district of quito or smth", "city of yaso", 
           "village of puerto l", "district or smth of betim", 
           "british columbia city of vancouver", "city of karl")

ds.phon.codes <- phonics(names, c("caverphone", "caverphone.modified", 
                                        "cologne", "lein",  "metaphone", 
                                        "nysiis", "nysiis.modified", "onca", 
                                        "onca.modified", "onca.refined", 
                                        "onca.modified.refined", "phonex", 
                                        "rogerroot", "soundex", 
                                        "soundex.refined", "statcan"), clean = FALSE)

name1 <- names[1] #Thane (Thane, India)--2nd option
name2 <- names[2] #"Sudbury, Canada" (Greater Sudbury, On,  Canada)--2nd option is Sudbury, ON
name3 <- names[3] #"Delta" (Delta, Nigeria) -- 30th option is Delta State, NGA; 
                  #first two options were Delta, BC and Delta, ZWE--
                  #when filtered by iso and entity type, delta state is the top option
name4 <- names[4] #"Villavi, CO" (Villavicencio, Colombia) #first option is villavicencio, COL
name5 <- names[5] #"gangdong" (gangdong-gu, republic korea) #first option
name6 <- names[6] #"Montreal" (montreal, canada) #top 3 choices are all it
name7 <- names[7] #"angnong,  republic korea" (gangneung, republic korea) #6 and 7
name8 <- names[8] #"panabi, country of india" (panaji, india) #1
name9 <- names[9] #osu, KR rep (Yeosu, Republic Korea) #2
name10 <- names[10] #"vancouver, CAN" (North Vancouver, Canada) #1 and 2
name11 <- names[11] #"San Christ de las Casas" (san crist/303/263bal de las casa, mexico) #1
name12 <- names[12] #"Lrenskog" (L/303/270renskog, norway) #1 and 2
name13 <- names[13] #"Kwadukuza, S Africa" (Kwadukuza Local, south africa) #1 and 2
name14 <- names[14] #"bengaluru" (bengaluru, india) #1
name15 <- names[15] #kuqing north,  malaysia" (Kuching North, Malaysia) #1, 2, 3
name16 <- names[16] #"City of usan" (osan, republickorea) -- #8
name17 <- names[17] #"armenia city government" (armenia, colombia) #1
name18 <- names[18] #"village o f  johaness" (johannesburg, south africa) #12
name19 <- names[19] #"lresn city" (L/303/270renskog, norway)  #1
name20 <- names[20] #"district of quito or smth" (quito, ecuador) #1
name21 <- names[21] # city of yaso (yasothon, thailand) #21
name22 <- names[22] # "village of puerto l" (puerto legu/303/255zamo, colombia) #29
name23 <- names[23] # "district or smth of betim", (betim, brazil) #8
name24 <- names[24] # "british columbia city of vancouver" (north vancouver, canada) #58
name25 <- names[25] # "city of karl" (karlstad, sweden) #85

#calculating scores using diff phonetic algorithms

#with as.character

key.dict <- read.csv("~/Documents/Summers/Summer 2019 (Data-Driven+WWF)/Data Driven Lab/Carbonn 2019/key_dict_new_withphoneticcodes.csv", as.is = TRUE)

for (i in 1:length(name1)) {
  
  kd.filtered <- key.dict %>% filter(iso == x$iso[indices[i]] & entity.type == x$entity.type[indices[i]])
  
  kd.filtered <- kd.filtered %>% mutate(score1 = stringsim(ds.phon.codes$caverphone[i], 
                                         kd.filtered$caverphone, method = "osa"),
                      score2 = stringsim(ds.phon.codes$caverphone[i], 
                                         kd.filtered$caverphone, method = "lv"),
                      score3 = stringsim(ds.phon.codes$caverphone[i], 
                                         kd.filtered$caverphone, method = "dl"),
                      score4 = stringsim(ds.phon.codes$caverphone[i], 
                                         kd.filtered$caverphone, method = "lcs"),
                      score5 = stringsim(ds.phon.codes$caverphone[i], 
                                         kd.filtered$caverphone, method = "hamming"),
                      score6 = stringsim(ds.phon.codes$caverphone[i], 
                                         kd.filtered$caverphone, method = "qgram"),
                      score7 = stringsim(ds.phon.codes$caverphone[i], 
                                         kd.filtered$caverphone, method = "cosine"),
                      score8 = stringsim(ds.phon.codes$caverphone[i], 
                                         kd.filtered$caverphone, method = "jaccard"),
                      score9 = stringsim(ds.phon.codes$caverphone[i], 
                                         kd.filtered$caverphone, method = "jw"))
  
  kd.filtered <- kd.filtered %>% mutate(caverphone.score = (score1 + score2 + score3 + 
                                            score4 + score5 + score6 + 
                                            score7 + score8 + score9)/9)
  
  kd.filtered <- kd.filtered %>% mutate(score1 = stringsim(ds.phon.codes$caverphone.modified[i], 
                                         kd.filtered$caverphone.modified, method = "osa"),
                      score2 = stringsim(ds.phon.codes$caverphone.modified[i], 
                                         kd.filtered$caverphone.modified, method = "lv"),
                      score3 = stringsim(ds.phon.codes$caverphone.modified[i], 
                                         kd.filtered$caverphone.modified, method = "dl"),
                      score4 = stringsim(ds.phon.codes$caverphone.modified[i], 
                                         kd.filtered$caverphone.modified, method = "lcs"),
                      score5 = stringsim(ds.phon.codes$caverphone.modified[i], 
                                         kd.filtered$caverphone.modified, method = "hamming"),
                      score6 = stringsim(ds.phon.codes$caverphone.modified[i], 
                                         kd.filtered$caverphone.modified, method = "qgram"),
                      score7 = stringsim(ds.phon.codes$caverphone.modified[i], 
                                         kd.filtered$caverphone.modified, method = "cosine"),
                      score8 = stringsim(ds.phon.codes$caverphone.modified[i], 
                                         kd.filtered$caverphone.modified, method = "jaccard"),
                      score9 = stringsim(ds.phon.codes$caverphone.modified[i], 
                                         kd.filtered$caverphone.modified, method = "jw"))
  
  kd.filtered <- kd.filtered %>% mutate(caverphone.modified.score = (score1 + score2 + score3 + 
                                                     score4 + score5 + score6 + 
                                                     score7 + score8 + score9)/9)
  
  kd.filtered <- kd.filtered %>% mutate(score1 = stringsim(as.character(ds.phon.codes$cologne[i]), 
                                         as.character(kd.filtered$cologne), method = "osa"),
                      score2 = stringsim(as.character(ds.phon.codes$cologne[i]), 
                                         as.character(kd.filtered$cologne), method = "lv"),
                      score3 = stringsim(as.character(ds.phon.codes$cologne[i]), 
                                         as.character(kd.filtered$cologne, method = "dl")),
                      score4 = stringsim(as.character(ds.phon.codes$cologne[i]), 
                                         as.character(kd.filtered$cologne, method = "lcs")),
                      score5 = stringsim(as.character(ds.phon.codes$cologne[i]), 
                                         as.character(kd.filtered$cologne, method = "hamming")),
                      score6 = stringsim(as.character(ds.phon.codes$cologne[i]), 
                                         as.character(kd.filtered$cologne, method = "qgram")),
                      score7 = stringsim(as.character(ds.phon.codes$cologne[i]), 
                                         as.character(kd.filtered$cologne, method = "cosine")),
                      score8 = stringsim(as.character(ds.phon.codes$cologne[i]), 
                                         as.character(kd.filtered$cologne, method = "jaccard")),
                      score9 = stringsim(as.character(ds.phon.codes$cologne[i]), 
                                         as.character(kd.filtered$cologne, method = "jw")))
  
  kd.filtered <- kd.filtered %>% mutate(cologne.score = (score1 + score2 + score3 + 
                                         score4 + score5 + score6 + 
                                         score7 + score8 + score9)/9)
  
  kd.filtered <- kd.filtered %>% mutate(score1 = stringsim(ds.phon.codes$lein[i], 
                                         kd.filtered$lein, method = "osa"),
                      score2 = stringsim(ds.phon.codes$lein[i], 
                                         kd.filtered$lein, method = "lv"),
                      score3 = stringsim(ds.phon.codes$lein[i], 
                                         kd.filtered$lein, method = "dl"),
                      score4 = stringsim(ds.phon.codes$lein[i], 
                                         kd.filtered$lein, method = "lcs"),
                      score5 = stringsim(ds.phon.codes$lein[i], 
                                         kd.filtered$lein, method = "hamming"),
                      score6 = stringsim(ds.phon.codes$lein[i], 
                                         kd.filtered$lein, method = "qgram"),
                      score7 = stringsim(ds.phon.codes$lein[i], 
                                         kd.filtered$lein, method = "cosine"),
                      score8 = stringsim(ds.phon.codes$lein[i], 
                                         kd.filtered$lein, method = "jaccard"),
                      score9 = stringsim(ds.phon.codes$lein[i], 
                                         kd.filtered$lein, method = "jw"))
  
  kd.filtered <- kd.filtered %>% mutate(lein.score = (score1 + score2 + score3 + 
                                      score4 + score5 + score6 + 
                                      score7 + score8 + score9)/9)
  
  kd.filtered <- kd.filtered %>% mutate(score1 = stringsim(ds.phon.codes$metaphone[i], 
                                         kd.filtered$metaphone, method = "osa"),
                      score2 = stringsim(ds.phon.codes$metaphone[i], 
                                         kd.filtered$metaphone, method = "lv"),
                      score3 = stringsim(ds.phon.codes$metaphone[i], 
                                         kd.filtered$metaphone, method = "dl"),
                      score4 = stringsim(ds.phon.codes$metaphone[i], 
                                         kd.filtered$metaphone, method = "lcs"),
                      score5 = stringsim(ds.phon.codes$metaphone[i], 
                                         kd.filtered$metaphone, method = "hamming"),
                      score6 = stringsim(ds.phon.codes$metaphone[i], 
                                         kd.filtered$metaphone, method = "qgram"),
                      score7 = stringsim(ds.phon.codes$metaphone[i], 
                                         kd.filtered$metaphone, method = "cosine"),
                      score8 = stringsim(ds.phon.codes$metaphone[i], 
                                         kd.filtered$metaphone, method = "jaccard"),
                      score9 = stringsim(ds.phon.codes$metaphone[i], 
                                         kd.filtered$metaphone, method = "jw"))
  
  kd.filtered <- kd.filtered %>% mutate(metaphone.score = (score1 + score2 + score3 + 
                                           score4 + score5 + score6 + 
                                           score7 + score8 + score9)/9)
  
  kd.filtered <- kd.filtered %>% mutate(score1 = stringsim(ds.phon.codes$nysiis[i], 
                                         kd.filtered$nysiis, method = "osa"),
                      score2 = stringsim(ds.phon.codes$nysiis[i], 
                                         kd.filtered$nysiis, method = "lv"),
                      score3 = stringsim(ds.phon.codes$nysiis[i], 
                                         kd.filtered$nysiis, method = "dl"),
                      score4 = stringsim(ds.phon.codes$nysiis[i], 
                                         kd.filtered$nysiis, method = "lcs"),
                      score5 = stringsim(ds.phon.codes$nysiis[i], 
                                         kd.filtered$nysiis, method = "hamming"),
                      score6 = stringsim(ds.phon.codes$nysiis[i], 
                                         kd.filtered$nysiis, method = "qgram"),
                      score7 = stringsim(ds.phon.codes$nysiis[i], 
                                         kd.filtered$nysiis, method = "cosine"),
                      score8 = stringsim(ds.phon.codes$nysiis[i], 
                                         kd.filtered$nysiis, method = "jaccard"),
                      score9 = stringsim(ds.phon.codes$nysiis[i], 
                                         kd.filtered$nysiis, method = "jw"))
  
  kd.filtered <- kd.filtered %>% mutate(nysiis.score = (score1 + score2 + score3 + 
                                        score4 + score5 + score6 + 
                                        score7 + score8 + score9)/9)
  
  kd.filtered <- kd.filtered %>% mutate(score1 = stringsim(ds.phon.codes$nysiis.modified[i], 
                                         kd.filtered$nysiis.modified, method = "osa"),
                      score2 = stringsim(ds.phon.codes$nysiis.modified[i], 
                                         kd.filtered$nysiis.modified, method = "lv"),
                      score3 = stringsim(ds.phon.codes$nysiis.modified[i], 
                                         kd.filtered$nysiis.modified, method = "dl"),
                      score4 = stringsim(ds.phon.codes$nysiis.modified[i], 
                                         kd.filtered$nysiis.modified, method = "lcs"),
                      score5 = stringsim(ds.phon.codes$nysiis.modified[i], 
                                         kd.filtered$nysiis.modified, method = "hamming"),
                      score6 = stringsim(ds.phon.codes$nysiis.modified[i], 
                                         kd.filtered$nysiis.modified, method = "qgram"),
                      score7 = stringsim(ds.phon.codes$nysiis.modified[i], 
                                         kd.filtered$nysiis.modified, method = "cosine"),
                      score8 = stringsim(ds.phon.codes$nysiis.modified[i], 
                                         kd.filtered$nysiis.modified, method = "jaccard"),
                      score9 = stringsim(ds.phon.codes$nysiis.modified[i], 
                                         kd.filtered$nysiis.modified, method = "jw"))
  
  kd.filtered <- kd.filtered %>% mutate(nysiis.modified.score = (score1 + score2 + score3 + 
                                                 score4 + score5 + score6 + 
                                                 score7 + score8 + score9)/9)
  
  kd.filtered <- kd.filtered %>% mutate(score1 = stringsim(ds.phon.codes$onca[i], 
                                         kd.filtered$onca, method = "osa"),
                      score2 = stringsim(ds.phon.codes$onca[i], 
                                         kd.filtered$onca, method = "lv"),
                      score3 = stringsim(ds.phon.codes$onca[i], 
                                         kd.filtered$onca, method = "dl"),
                      score4 = stringsim(ds.phon.codes$onca[i], 
                                         kd.filtered$onca, method = "lcs"),
                      score5 = stringsim(ds.phon.codes$onca[i], 
                                         kd.filtered$onca, method = "hamming"),
                      score6 = stringsim(ds.phon.codes$onca[i], 
                                         kd.filtered$onca, method = "qgram"),
                      score7 = stringsim(ds.phon.codes$onca[i], 
                                         kd.filtered$onca, method = "cosine"),
                      score8 = stringsim(ds.phon.codes$onca[i], 
                                         kd.filtered$onca, method = "jaccard"),
                      score9 = stringsim(ds.phon.codes$onca[i], 
                                         kd.filtered$onca, method = "jw"))
  
  kd.filtered <- kd.filtered %>% mutate(onca.score = (score1 + score2 + score3 + 
                                      score4 + score5 + score6 + 
                                      score7 + score8 + score9)/9)
  
  kd.filtered <- kd.filtered %>% mutate(score1 = stringsim(ds.phon.codes$onca.modified[i], 
                                         kd.filtered$onca.modified, method = "osa"),
                      score2 = stringsim(ds.phon.codes$onca.modified[i], 
                                         kd.filtered$onca.modified, method = "lv"),
                      score3 = stringsim(ds.phon.codes$onca.modified[i], 
                                         kd.filtered$onca.modified, method = "dl"),
                      score4 = stringsim(ds.phon.codes$onca.modified[i], 
                                         kd.filtered$onca.modified, method = "lcs"),
                      score5 = stringsim(ds.phon.codes$onca.modified[i], 
                                         kd.filtered$onca.modified, method = "hamming"),
                      score6 = stringsim(ds.phon.codes$onca.modified[i], 
                                         kd.filtered$onca.modified, method = "qgram"),
                      score7 = stringsim(ds.phon.codes$onca.modified[i], 
                                         kd.filtered$onca.modified, method = "cosine"),
                      score8 = stringsim(ds.phon.codes$onca.modified[i], 
                                         kd.filtered$onca.modified, method = "jaccard"),
                      score9 = stringsim(ds.phon.codes$onca.modified[i], 
                                         kd.filtered$onca.modified, method = "jw"))
  
  kd.filtered <- kd.filtered %>% mutate(onca.modified.score = (score1 + score2 + score3 + 
                                               score4 + score5 + score6 + 
                                               score7 + score8 + score9)/9)
  
  kd.filtered <- kd.filtered %>% mutate(score1 = stringsim(ds.phon.codes$onca.refined[i], 
                                         kd.filtered$onca.refined, method = "osa"),
                      score2 = stringsim(ds.phon.codes$onca.refined[i], 
                                         kd.filtered$onca.refined, method = "lv"),
                      score3 = stringsim(ds.phon.codes$onca.refined[i], 
                                         kd.filtered$onca.refined, method = "dl"),
                      score4 = stringsim(ds.phon.codes$onca.refined[i], 
                                         kd.filtered$onca.refined, method = "lcs"),
                      score5 = stringsim(ds.phon.codes$onca.refined[i], 
                                         kd.filtered$onca.refined, method = "hamming"),
                      score6 = stringsim(ds.phon.codes$onca.refined[i], 
                                         kd.filtered$onca.refined, method = "qgram"),
                      score7 = stringsim(ds.phon.codes$onca.refined[i], 
                                         kd.filtered$onca.refined, method = "cosine"),
                      score8 = stringsim(ds.phon.codes$onca.refined[i], 
                                         kd.filtered$onca.refined, method = "jaccard"),
                      score9 = stringsim(ds.phon.codes$onca.refined[i], 
                                         kd.filtered$onca.refined, method = "jw"))
  
  kd.filtered <- kd.filtered %>% mutate(onca.refined.score = (score1 + score2 + score3 + 
                                              score4 + score5 + score6 + 
                                              score7 + score8 + score9)/9)
  
  kd.filtered <- kd.filtered %>% mutate(score1 = stringsim(ds.phon.codes$onca.modified.refined[i], 
                                         kd.filtered$onca.modified.refined, method = "osa"),
                      score2 = stringsim(ds.phon.codes$onca.modified.refined[i], 
                                         kd.filtered$onca.modified.refined, method = "lv"),
                      score3 = stringsim(ds.phon.codes$onca.modified.refined[i], 
                                         kd.filtered$onca.modified.refined, method = "dl"),
                      score4 = stringsim(ds.phon.codes$onca.modified.refined[i], 
                                         kd.filtered$onca.modified.refined, method = "lcs"),
                      score5 = stringsim(ds.phon.codes$onca.modified.refined[i], 
                                         kd.filtered$onca.modified.refined, method = "hamming"),
                      score6 = stringsim(ds.phon.codes$onca.modified.refined[i], 
                                         kd.filtered$onca.modified.refined, method = "qgram"),
                      score7 = stringsim(ds.phon.codes$onca.modified.refined[i], 
                                         kd.filtered$onca.modified.refined, method = "cosine"),
                      score8 = stringsim(ds.phon.codes$onca.modified.refined[i], 
                                         kd.filtered$onca.modified.refined, method = "jaccard"),
                      score9 = stringsim(ds.phon.codes$onca.modified.refined[i], 
                                         kd.filtered$onca.modified.refined, method = "jw"))
  
  kd.filtered <- kd.filtered %>% mutate(onca.modified.refined.score = (score1 + score2 + score3 + 
                                                       score4 + score5 + score6 + 
                                                       score7 + score8 + score9)/9)
  
  kd.filtered <- kd.filtered %>% mutate(score1 = stringsim(ds.phon.codes$phonex[i], 
                                         kd.filtered$phonex, method = "osa"),
                      score2 = stringsim(ds.phon.codes$phonex[i], 
                                         kd.filtered$phonex, method = "lv"),
                      score3 = stringsim(ds.phon.codes$phonex[i], 
                                         kd.filtered$phonex, method = "dl"),
                      score4 = stringsim(ds.phon.codes$phonex[i], 
                                         kd.filtered$phonex, method = "lcs"),
                      score5 = stringsim(ds.phon.codes$phonex[i], 
                                         kd.filtered$phonex, method = "hamming"),
                      score6 = stringsim(ds.phon.codes$phonex[i], 
                                         kd.filtered$phonex, method = "qgram"),
                      score7 = stringsim(ds.phon.codes$phonex[i], 
                                         kd.filtered$phonex, method = "cosine"),
                      score8 = stringsim(ds.phon.codes$phonex[i], 
                                         kd.filtered$phonex, method = "jaccard"),
                      score9 = stringsim(ds.phon.codes$phonex[i], 
                                         kd.filtered$phonex, method = "jw"))
  
  kd.filtered <- kd.filtered %>% mutate(phonex.score = (score1 + score2 + score3 + 
                                        score4 + score5 + score6 + 
                                        score7 + score8 + score9)/9)
  
  kd.filtered <- kd.filtered %>% mutate(score1 = stringsim(as.character(ds.phon.codes$rogerroot[i]), 
                                         as.character(kd.filtered$rogerroot), method = "osa"),
                      score2 = stringsim(as.character(ds.phon.codes$rogerroot[i]), 
                                         as.character(kd.filtered$rogerroot, method = "lv")),
                      score3 = stringsim(as.character(ds.phon.codes$rogerroot[i]), 
                                         as.character(kd.filtered$rogerroot, method = "dl")),
                      score4 = stringsim(as.character(ds.phon.codes$rogerroot[i]), 
                                         as.character(kd.filtered$rogerroot, method = "lcs")),
                      score5 = stringsim(as.character(ds.phon.codes$rogerroot[i]), 
                                         as.character(kd.filtered$rogerroot, method = "hamming")),
                      score6 = stringsim(as.character(ds.phon.codes$rogerroot[i]), 
                                         as.character(kd.filtered$rogerroot, method = "qgram")),
                      score7 = stringsim(as.character(ds.phon.codes$rogerroot[i]), 
                                         as.character(kd.filtered$rogerroot, method = "cosine")),
                      score8 = stringsim(as.character(ds.phon.codes$rogerroot[i]), 
                                         as.character(kd.filtered$rogerroot, method = "jaccard")),
                      score9 = stringsim(as.character(ds.phon.codes$rogerroot[i]), 
                                         as.character(kd.filtered$rogerroot, method = "jw")))
  
  kd.filtered <- kd.filtered %>% mutate(rogerroot.score = (score1 + score2 + score3 + 
                                           score4 + score5 + score6 + 
                                           score7 + score8 + score9)/9)
  
  kd.filtered <- kd.filtered %>% mutate(score1 = stringsim(ds.phon.codes$soundex[i], 
                                         kd.filtered$soundex, method = "osa"),
                      score2 = stringsim(ds.phon.codes$soundex[i], 
                                         kd.filtered$soundex, method = "lv"),
                      score3 = stringsim(ds.phon.codes$soundex[i], 
                                         kd.filtered$soundex, method = "dl"),
                      score4 = stringsim(ds.phon.codes$soundex[i], 
                                         kd.filtered$soundex, method = "lcs"),
                      score5 = stringsim(ds.phon.codes$soundex[i], 
                                         kd.filtered$soundex, method = "hamming"),
                      score6 = stringsim(ds.phon.codes$soundex[i], 
                                         kd.filtered$soundex, method = "qgram"),
                      score7 = stringsim(ds.phon.codes$soundex[i], 
                                         kd.filtered$soundex, method = "cosine"),
                      score8 = stringsim(ds.phon.codes$soundex[i], 
                                         kd.filtered$soundex, method = "jaccard"),
                      score9 = stringsim(ds.phon.codes$soundex[i], 
                                         kd.filtered$soundex, method = "jw"))
  
  kd.filtered <- kd.filtered %>% mutate(soundex.score = (score1 + score2 + score3 + 
                                         score4 + score5 + score6 + 
                                         score7 + score8 + score9)/9)
  
  kd.filtered <- kd.filtered %>% mutate(score1 = stringsim(ds.phon.codes$soundex.refined[i], 
                                         kd.filtered$soundex.refined, method = "osa"),
                      score2 = stringsim(ds.phon.codes$soundex.refined[i], 
                                         kd.filtered$soundex.refined, method = "lv"),
                      score3 = stringsim(ds.phon.codes$soundex.refined[i], 
                                         kd.filtered$soundex.refined, method = "dl"),
                      score4 = stringsim(ds.phon.codes$soundex.refined[i], 
                                         kd.filtered$soundex.refined, method = "lcs"),
                      score5 = stringsim(ds.phon.codes$soundex.refined[i], 
                                         kd.filtered$soundex.refined, method = "hamming"),
                      score6 = stringsim(ds.phon.codes$soundex.refined[i], 
                                         kd.filtered$soundex.refined, method = "qgram"),
                      score7 = stringsim(ds.phon.codes$soundex.refined[i], 
                                         kd.filtered$soundex.refined, method = "cosine"),
                      score8 = stringsim(ds.phon.codes$soundex.refined[i], 
                                         kd.filtered$soundex.refined, method = "jaccard"),
                      score9 = stringsim(ds.phon.codes$soundex.refined[i], 
                                         kd.filtered$soundex.refined, method = "jw"))
  
  kd.filtered <- kd.filtered %>% mutate(soundex.refined.score = (score1 + score2 + score3 + 
                                                 score4 + score5 + score6 + 
                                                 score7 + score8 + score9)/9)
  
  kd.filtered <- kd.filtered %>% mutate(score1 = stringsim(ds.phon.codes$statcan[i], 
                                         kd.filtered$statcan, method = "osa"),
                      score2 = stringsim(ds.phon.codes$statcan[i], 
                                         kd.filtered$statcan, method = "lv"),
                      score3 = stringsim(ds.phon.codes$statcan[i], 
                                         kd.filtered$statcan, method = "dl"),
                      score4 = stringsim(ds.phon.codes$statcan[i], 
                                         kd.filtered$statcan, method = "lcs"),
                      score5 = stringsim(ds.phon.codes$statcan[i], 
                                         kd.filtered$statcan, method = "hamming"),
                      score6 = stringsim(ds.phon.codes$statcan[i], 
                                         kd.filtered$statcan, method = "qgram"),
                      score7 = stringsim(ds.phon.codes$statcan[i], 
                                         kd.filtered$statcan, method = "cosine"),
                      score8 = stringsim(ds.phon.codes$statcan[i], 
                                         kd.filtered$statcan, method = "jaccard"),
                      score9 = stringsim(ds.phon.codes$statcan[i], 
                                         kd.filtered$statcan, method = "jw"))
  
  kd.filtered <- kd.filtered %>% mutate(statcan.score = (score1 + score2 + score3 + 
                                         score4 + score5 + score6 + 
                                         score7 + score8 + score9)/9)
  
  kd.filtered <- kd.filtered %>% mutate(tot.score = caverphone.score + caverphone.modified.score + 
                        cologne.score + lein.score + metaphone.score + 
                        nysiis.score + nysiis.modified.score + onca.score + 
                        onca.modified.score + onca.refined.score + 
                        onca.modified.refined.score + phonex.score + rogerroot.score + 
                        soundex.score + soundex.refined.score + statcan.score)
  
  kd.filtered <- kd.filtered %>% arrange(desc(tot.score))
  
  return(kd.filtered)
}
