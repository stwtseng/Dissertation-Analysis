##########################
##### Clean Raw Data #####
##########################

##### Load Libraries #####

library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(tibble)
library(magrittr)
library(writexl)

##### Load Data #####

dat <- read_delim("./Dissertation Data/dissertationdataRAW.csv", delim = ",")

spec(dat)

##### Remove Incomplete Responses #####

sum(dat$Finished == 1) # 450 marked as finished, but Qualtrics marks attention fails as finished

sum(grepl("failed attention checks", dat$completion_fail)) # 49 attention fails

dat %<>%
  filter(Finished == 1) %>% # remove unfinished
  filter(!grepl("failed attention checks", completion_fail)) # remove attention fails

which(is.na(dat$check)) # 0 failed check after previous remove command

sum(grepl("failed attention checks", dat$completion_fail)) # 0 failed completion after remove

##### Check for duplicate IP addresses #####
dat %>%
  group_by(IPAddress) %>%
  filter(n() > 1) # no duplicate IPs

nrow(dat)
# Final dataset contains 401 rows

##### Assign ID numbers #####
dat %<>%
 rowid_to_column("id")

##### Remove unneeded Qualtrics metadata columns #####
dat %<>%
  select(-c(StartDate, EndDate, Status, IPAddress, Progress, `Duration (in seconds)`, Finished,
            RecordedDate, ResponseId, RecipientLastName, RecipientFirstName, RecipientEmail,
            ExternalReference, LocationLatitude, LocationLongitude, DistributionChannel,
            UserLanguage, check, bonus, SC0, completion_fail, score,
            attention_1, attention_2, attention_3, attention_4, attention_5))

colnames(dat)

##### Fix variable classes #####

# orgtenure
dat$orgtenure
dat$orgtenure[126]
dat$orgtenure[126] <- 1.33 # convert '1 year 4 months' to 1.33
dat$orgtenure[260]
dat$orgtenure[260] <- 0.5 # convert '1/2 year' to 0.5
dat$orgtenure[275]
dat$orgtenure[275] <- 1.5 # convert '1 1/2 years' to 1.5
dat$orgtenure[350]
dat$orgtenure[350] <- 2.92 # convert '2 years 11 months' to 2.92
dat$orgtenure[399]
dat$orgtenure[399] <- 0.5 # convert '0 years 6 months' to 0.5
dat$orgtenure <- str_remove(dat$orgtenure, "years")
dat$orgtenure <- str_remove(dat$orgtenure, "yrs")
dat$orgtenure
dat$orgtenure <- as.numeric(dat$orgtenure)
class(dat$orgtenure)

# postenure
dat$postenure
dat$postenure[67]
dat$postenure[67] <- 0.83 # convert '10 months' to 0.83
dat$postenure[126]
dat$postenure[126] <- 1.33 # convert '1 year 4 months' to 1.33
dat$postenure[260]
dat$postenure[260] <- 0.5 # convert '1/2 year' to 0.5
dat$postenure[275]
dat$postenure[275] <- 1.5 # convert '1 1/2 years' to 1.5
dat$postenure[350]
dat$postenure[350] <- 2.92 # convert '2 years 11 months' to 2.92
dat$postenure[399]
dat$postenure[399] <- 0.5 # convert '0 years 6 months' to 0.5
dat$postenure <- str_remove(dat$postenure, "years")
dat$postenure <- str_remove(dat$postenure, "yrs")
dat$postenure
dat$postenure <- as.numeric(dat$postenure)
class(dat$postenure)

# suptenure
dat$suptenure
dat$suptenure[67]
dat$suptenure[67] <- 0.25 # convert '3 months' to 0.25
dat$suptenure[126]
dat$suptenure[126] <- 1.33 # convert '1 year 4 months' to 1.33
dat$suptenure[260]
dat$suptenure[260] <- 0.5 # convert '1/2 year' to 0.5
dat$suptenure[275]
dat$suptenure[275] <- 1.5 # convert '1 1/2 years' to 1.5
dat$suptenure[326]
dat$suptenure[326] <- 0.83 # convert '10 months' to 0.83
dat$suptenure[350]
dat$suptenure[350] <- 2.92 # convert '2 years 11 months' to 2.92
dat$suptenure[399]
dat$suptenure[399] <- 0.5 # convert '0 years 6 months' to 0.5
dat$suptenure <- str_remove(dat$suptenure, "years")
dat$suptenure <- str_remove(dat$suptenure, "yrs")
dat$suptenure
dat$suptenure <- as.numeric(dat$suptenure)
class(dat$suptenure)

# weekhours
dat$weekhours
dat$weekhours[295]
dat$weekhours[295] <- 47.5 # convert '45-50' to midpoint, 47.5
dat$weekhours <- as.numeric(dat$weekhours)
class(dat$weekhours)

##### Recode demographic variables into factors #####
##### Must keep as numeric for Spearman correlations #####

dat %<>%
  mutate(orgtype = recode(orgtype, `1` = 1,
                          `2` = 0)) %>% # recode nonprofit = 0, forprofit = 1
  mutate(region = recode(region, `1` = "northeast",
                         `2` = "midwest",
                         `3` = "south",
                         `4` = "west")) %>%
  mutate(industry = recode(industry, `1` = "agriculture, forestry, fishing, hunting",
                           `2` = "mining, quarrying, oil and gas extraction",
                           `3` = "utilities",
                           `4` = "construction",
                           `5` = "manufacturing",
                           `6` = "wholesale trade",
                           `7` = "retail trade",
                           `8` = "transportation, warehousing",
                           `9` = "information",
                           `10` = "finance, insurance",
                           `11` = "real estate, rental, leasing",
                           `12` = "professional, scientific, technical services",
                           `13` = "management of companies and enterprises",
                           `14` = "administrative, support, waste management, remediation",
                           `15` = "educational services",
                           `16` = "health care, social assistance",
                           `17` = "arts, entertainment, recreation",
                           `18` = "accommodation, food services",
                           `19` = "public administration")) %>%
  mutate(status = recode(status, `1` = 0,
                         `2` = 1)) %>% # recode full-time = 0, part-time = 1
  mutate(gender = recode(gender, `1` = 0,
                         `2` = 1)) %>% # recode male = 0, female = 1
  mutate(racenum = recode(race, `1` = 0,
                        .default = 1)) %>% # recode white = 0, non-white = 1  
  mutate(racewb = recode(race, `1` = 0,
                         `2` = 1)) %>% # recode white = 0, black = 1
  mutate(race = recode(race, `1` = "white",
                       `2` = "black",
                       `3` = "hispanic, latino",
                       `4` = "asian",
                       `5` = "pacific islander, native hawaiian",
                       `6` = "american indian",
                       `7` = "alaskan native",
                       `8` = "middle eastern",
                       `9` = "other",
                       .default = "mixed")) %>%
  mutate(marital = recode(marital, `1` = "single",
                          `2` = "married",
                          `3` = "separated",
                          `4` = "divorced",
                          `5` = "widowed",
                          `6` = "domestic partnership"))

dat$orgcent[dat$orgcent == 5] <- NA # recode other = NA
dat$gender[dat$gender == 3] <- NA # recode other = NA
dat$racewb[dat$racewb > 1] <- NA # recode non-white non-black = NA

##### Below code to recode into factor labels #####
# dat %>%
#   mutate(orgtype = recode(orgtype, `1` = "forprofit",
#                           `2` = "nonprofit")) %>%
#   mutate(orgsize = recode_factor(orgsize, `1` = "less than 10",
#                           `2` = "10 to 49",
#                           `3` = "50 to 99",
#                           `4` = "100 to 499",
#                           `5` = "500 to 999",
#                           `6` = "1000 or more", .ordered = TRUE)) %>%
#   mutate(orglevels = recode_factor(orglevels, `1` = "three or fewer levels",
#                                    `2` = "four levels",
#                                    `3` = "five levels",
#                                    `4` = "six levels",
#                                    `5` = "seven levels or more", .ordered = TRUE)) %>%
#   mutate(orgcent = recode_factor(orgcent, `1` = "CEO, president, or managing director",
#                                  `2` = "corporate management",
#                                  `3` = "branch management",
#                                  `4` = "input from frontline employees",
#                                  `5` = "other"), .ordered = TRUE) %>%
#   mutate(region = recode(region, `1` = "northeast",
#                                 `2` = "midwest",
#                                 `3` = "south",
#                                 `4` = "west")) %>%
#   mutate(industry = recode(industry, `1` = "agriculture, forestry, fishing, hunting",
#                                   `2` = "mining, quarrying, oil and gas extraction",
#                                   `3` = "utilities",
#                                   `4` = "construction",
#                                   `5` = "manufacturing",
#                                   `6` = "wholesale trade",
#                                   `7` = "retail trade",
#                                   `8` = "transportation, warehousing",
#                                   `9` = "information",
#                                   `10` = "finance, insurance",
#                                   `11` = "real estate, rental, leasing",
#                                   `12` = "professional, scientific, technical services",
#                                   `13` = "management of companies and enterprises",
#                                   `14` = "administrative, support, waste management, remediation",
#                                   `15` = "educational services",
#                                   `16` = "health care, social assistance",
#                                   `17` = "arts, entertainment, recreation",
#                                   `18` = "accommodation, food services",
#                                   `19` = "public administration")) %>%
#   mutate(status = recode(status, `1` = "full-time",
#                                 `2` = "part-time")) %>%
#   mutate(dirreport = recode(dirreport, `1` = "supervises direct reports",
#                                    `0` = "does not supervisor direct reports")) %>%
#   mutate(salary = recode_factor(salary, `1` = "less than $10,000",
#                                 `2` = "$10,000 to $19,999",
#                                 `3` = "$20,000 to $29,999",
#                                 `4` = "$30,000 to $39,999",
#                                 `5` = "$40,000 to $49,999",
#                                 `6` = "$50,000 to $59,999",
#                                 `7` = "$60,000 to $69,999",
#                                 `8` = "$70,000 or more", .ordered = TRUE)) %>%
#   mutate(gender = recode(gender, `1` = "male",
#                                 `2` = "female",
#                                 `3` = "other")) %>%
#   mutate(age = recode_factor(age, `1` = "18-24",
#                              `2` = "25-30",
#                              `3` = "31-40",
#                              `4` = "41-50",
#                              `5` = "51-60",
#                              `6` = "over 60")) %>%
#   mutate(race = recode(race, `1` = "white",
#                               `2` = "black",
#                               `3` = "hispanic, latino",
#                               `4` = "asian",
#                               `5` = "pacific islander, native hawaiian",
#                               `6` = "american indian",
#                               `7` = "alaskan native",
#                               `8` = "middle eastern",
#                               `9` = "other",
#                               .default = "mixed")) %>%
#   mutate(english = recode(english, `1` = "english first language",
#                                  `0` = "english not first language")) %>%
#   mutate(marital = recode(marital, `1` = "single",
#                                  `2` = "married",
#                                  `3` = "separated",
#                                  `4` = "divorced",
#                                  `5` = "widowed",
#                                  `6` = "domestic partnership")) %>%
#   mutate(edu = recode_factor(edu, `1` = "high school, GED",
#                              `2` = "associates",
#                              `3` = "college",
#                              `4` = "masters",
#                              `5` = "doctoral", .ordered = TRUE))

##### Correct column names
# Note: Qualtrics BLM labelling incorrect (item 1 labelled as sblm_6)
# Note: Qualtrics POS labelling incorrect (goes from 9-16). 14 and 15 are reverse-coded
dat %<>%
  rename(sblm_1 = sblm_6) %>%
  rename(pos_1 = pos_9) %>%
  rename(pos_2 = pos_10) %>%
  rename(pos_3 = pos_11) %>%
  rename(pos_4 = pos_12) %>%
  rename(pos_5 = pos_13) %>%
  rename(pos_6 = pos_14) %>%
  rename(pos_7 = pos_15) %>%
  rename(pos_8 = pos_16)

##### Adjust reverse-coded items #####
# Note: Qualtrics POS labelling incorrect (goes from 9-16). 14 and 15 are reverse-coded
dat %<>%
  mutate(jobsat_2r = 8 - jobsat_2r) %>%
  mutate(acommit_3 = 8 - acommit_3) %>%
  mutate(acommit_4 = 8 - acommit_4) %>%
  mutate(acommit_5 = 8 - acommit_5) %>%
  mutate(diseng_1r = 8 - diseng_1r) %>%
  mutate(exhaust_3r = 8 - exhaust_3r) %>%
  mutate(diseng_4r = 8 - diseng_4r) %>%
  mutate(exhaust_5r = 8 - exhaust_5r) %>%
  mutate(diseng_7r = 8 - diseng_7r) %>%
  mutate(exhaust_7r = 8 - exhaust_7r) %>%
  mutate(diseng_8r = 8 - diseng_8r) %>%
  mutate(exhaust_8r = 8 - exhaust_8r) %>%
  mutate(pos_6 = 8 - pos_6) %>% 
  mutate(pos_7 = 8 - pos_7)


##### Write File #####
write_xlsx(dat, "./Dissertation Data/dissertationdataCLEAN.xlsx")

###############################################
##### Create Dataset with Construct Means #####
###############################################

##### Load Libraries #####

library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(tibble)
library(magrittr)
library(writexl)

##### Load Data #####
(dat <- read_xlsx("./Dissertation Data/dissertationdataCLEAN.xlsx"))

##### Calculate HR Practice Sums #####
dat %<>%
  mutate(selection = dplyr::select(., selection_1:selection_3) %>% rowSums) %>%
  mutate(training = dplyr::select(., training_1:training_3) %>% rowSums) %>%
  mutate(appraisal = dplyr::select(., appraisal_1:appraisal_3) %>% rowSums) %>%
  mutate(compensation = dplyr::select(., compens_1:compens_3) %>% rowSums) %>%
  mutate(hrpractices = dplyr::select(., selection:compensation) %>% rowSums)

##### Calculate Construct Means #####
dat %<>%
  mutate(exploitatt = dplyr::select(., exploit_1:exploit_3) %>% rowMeans) %>%
  mutate(costatt = dplyr::select(., cost_1:cost_3) %>% rowMeans) %>%
  mutate(perfatt = dplyr::select(., perfatt_1:perfatt_3) %>% rowMeans) %>%
  mutate(wbatt = dplyr::select(., wbatt_1:wbatt_3) %>% rowMeans) %>%
  mutate(sblm = dplyr::select(., sblm_1:sblm_4) %>% rowMeans) %>%
  mutate(soe = dplyr::select(., soe_1:soe_9) %>% rowMeans) %>%
  mutate(orgdehum = dplyr::select(., orgdehum_1:orgdehum_11) %>% rowMeans) %>%
  mutate(pos = dplyr::select(., pos_1:pos_8) %>% rowMeans) %>%
  mutate(jobsat = dplyr::select(., jobsat_1:jobsat_3) %>% rowMeans) %>%
  mutate(acommit = dplyr::select(., acommit_1:acommit_6) %>% rowMeans) %>%
  mutate(tointent = dplyr::select(., tointent_1:tointent_2) %>% rowMeans) %>%
  mutate(diseng = dplyr::select(., c(diseng_1r, diseng_2, diseng_3, diseng_4r, diseng_5,
                                     diseng_6, diseng_7r, diseng_8r)) %>% rowMeans) %>%
  mutate(exhaust = dplyr::select(., c(exhaust_1, exhaust_2, exhaust_3r, exhaust_4, exhaust_5r,
                                      exhaust_6, exhaust_7r, exhaust_8r)) %>% rowMeans) %>%
  mutate(burnout = dplyr::select(., diseng_1r:exhaust_8r) %>% rowMeans) %>%
  mutate(perf = dplyr::select(., perf_1:perf_5) %>% rowMeans) %>%
  mutate(ocb = dplyr::select(., ocb_1:ocb_10) %>% rowMeans) %>%
  mutate(cwb = dplyr::select(., cwb_1:cwb_10) %>% rowMeans)

##### Calculate Centered Variables #####
dat %<>%
  mutate(exploitattc = exploitatt - mean(exploitatt, na.rm = TRUE)) %>%
  mutate(costattc = costatt - mean(costatt, na.rm = TRUE)) %>%
  mutate(perfattc = perfatt - mean(perfatt, na.rm = TRUE)) %>%
  mutate(wbattc = wbatt - mean(wbatt, na.rm = TRUE)) %>%
  mutate(sblmc = sblm - mean(sblm, na.rm = TRUE)) %>%
  mutate(soec = soe - mean(soe, na.rm = TRUE)) %>%
  mutate(orgdehumc = orgdehum - mean(orgdehum, na.rm = TRUE)) %>%
  mutate(posc = pos - mean(pos, na.rm = TRUE))

##### Write File #####
write_xlsx(dat, "./Dissertation Data/dissertationdataCOMPLETE.xlsx")

##############################################################
##### Create Second Dataset for Further Data Exploration #####
##############################################################

library(readr)
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(tibble)
library(magrittr)
library(writexl)

rm(dat)

dat <- read_xlsx("./Dissertation Data/dissertationdataCOMPLETE.xlsx")

##### Various SOE scales
dat %<>%
  mutate(soe1 = dplyr::select(., soe_1:soe_4) %>% rowMeans) %>%
  mutate(soe1c = soe1 - mean(soe1, na.rm = TRUE)) %>%
  mutate(soe2 = dplyr::select(., soe_5:soe_9) %>% rowMeans) %>%
  mutate(soe2c = soe2 - mean(soe2, na.rm = TRUE)) %>%
  mutate(soe3 = dplyr::select(., c(soe_5, soe_6, soe_8, soe_9)) %>% rowMeans) %>%
  mutate(soe3c = soe3 - mean(soe3, na.rm = TRUE))

##### Write File #####
write_xlsx(dat, "./Dissertation Data/dissertationdataSUPPLEMENTAL.xlsx")
