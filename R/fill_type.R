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