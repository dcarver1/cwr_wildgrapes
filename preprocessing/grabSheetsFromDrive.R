# pull stuff to the server through 
library(googledrive)
library(googlesheets4)
library(readr)
library(dplyr)
gs4_deauth()

# USDA Plants Data
d1 <- googlesheets4::read_sheet(as_id("https://docs.google.com/spreadsheets/d/1BA7FQ8EU0ejooartCoNfx7moUN7wNEmEqU0VmCU6nPw/edit?usp=sharing"),
                                sheet = "Raw_data")
d1
write.csv(d1, file = "data/source_data/usda_plants/completeVitis.csv")

#synonym list 
d2 <- googlesheets4::read_sheet(as_id("https://docs.google.com/spreadsheets/d/19e6wNr4Luc53NfBQJgl4AWGovZMp24P9yyDqmW9-h3Y/edit#gid=137674278"))

d3 <- as.data.frame(d2)
write_csv(d3,file = "data/source_data/taxonomy20230628.csv")


# names list 
d4 <-googlesheets4::read_sheet(as_id("https://docs.google.com/spreadsheets/d/1BA7FQ8EU0ejooartCoNfx7moUN7wNEmEqU0VmCU6nPw/edit?usp=sharing"),
                               sheet = "summaryName")
write_csv(x = d4, file = "data/source_data/nameList.csv")


# Wiews Data 
## takes a long time to read in 
d5 <- googlesheets4::read_sheet(as_id("https://docs.google.com/spreadsheets/d/1QGY8witd4t8r6-ayNpLcx65z5UOTB8y4IBoevkuXrvs/edit?usp=sharing"))
                                #,sheet = "Wiews_Exsitu_1684365085723")
write_csv(x = d5, file = "data/source_data/wiews.csv")

#genesys data 
g1 <- read_sheet(as_id("https://docs.google.com/spreadsheets/d/1caDvF4nF7QA1-19A4n-sh2Gnc7Gl8xahCy5IwrTLaOY/edit?usp=sharing"))
g2 <-  read_sheet(as_id("https://docs.google.com/spreadsheets/d/1fHkrzRxUS1D6ZlI7bY7Omc4kFkwuOqzOe5QY9sAF2wQ/edit?usp=sharing"))
## this is a alternative name table. As we are not reference back to any orginal sources I'm skipping it. 
g3 <-  read_sheet(as_id("https://docs.google.com/spreadsheets/d/1wcO08FayynqV89Wa7ra9HOMy3fPGoE3rCXfuOOhhyAw/edit?usp=sharing"))
g5 <-  read_sheet(as_id("https://docs.google.com/spreadsheets/d/1vxRJiKzV_NNzEHb7_NSIKbrHKtA0fCq4SxQEUYc48sM/edit?usp=sharing"))

g_all <- g1 %>%
  left_join(g2, by ="genesysId")%>%
  # left_join(g3, by ="genesysId")
  left_join(g5, by ="genesysId")%>%
  distinct()

write_csv(x = g_all, file = "data/source_data/genesys.csv")

# botanical Data
bg1 <- read_sheet(as_id("https://docs.google.com/spreadsheets/d/16CUDkksq2cX4syHKdTeIPgmZ6CIxaFnbcwezbSv6eZ4/edit?usp=sharing"))
write_csv(x = bg1, file = "data/source_data/bg_survey.csv")



# synonym datasets 
syn1 <- read_sheet(as_id("https://docs.google.com/spreadsheets/d/19e6wNr4Luc53NfBQJgl4AWGovZMp24P9yyDqmW9-h3Y/edit#gid=137674278"))
write_csv(x = syn1, file = "data/vitis/synonymList.csv")


# uc davis 
davis <- read_sheet(as_id("https://docs.google.com/spreadsheets/d/1AYMp9SLFdpDwHar7nvxEELltopGxBZfXfgRu-YR5BIU/edit?usp=sharing"))%>%
  filter(!is.na(id))%>%
  mutate(lon = as.numeric(lon))
write_csv(x = davis, file = "data/source_data/ucDavis.csv")

## uc davis -- new dataset from Claire 
davis2 <- read_sheet(as_id("https://docs.google.com/spreadsheets/d/1Ozx8m8iEk5-6zKCKF1KykMHRbhG_5VabNJn4UQnL3sY/edit?usp=sharing"))%>%
  mutate(across(everything(), as.character),
         `Lat (DD)`= as.numeric(`Lat (DD)`),
         `Long (DD)`= as.numeric(`Long (DD)`))
write_csv(x = davis2, file = "data/source_data/ucDavis2.csv")

    
### 2020 PNAS paper 
pnas <- read_sheet(as_id("https://docs.google.com/spreadsheets/d/13D4SAcJwQDG_aVVQDJFuP8-vO0Lep8QoqbnKRaV00lw/edit?usp=sharing"))%>%
  mutate(across(everything(), as.character),
         latitude = as.numeric(latitude),
         longitude = as.numeric(longitude ))
write_csv(x = pnas, file = "data/source_data/pnas2020.csv")
