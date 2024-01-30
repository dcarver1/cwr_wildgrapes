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
d2 <- googlesheets4::read_sheet(as_id("https://docs.google.com/spreadsheets/d/1ZzKUr2GI8wZe42G-nnMZ3iyhUnnqn3OUCMK_0sFKniQ/edit?usp=sharing"))

d3 <- as.data.frame(d2) |>
  dplyr::select("taxon" = "Taxon Name",
                acceptedSynonym = "Names to include in this concept (Homotypic synonyms)",  
                countySpecies = "Include in county map analysis?",
                modelSpecies = "Include in gap analsyis? (Colin)",
                everything()) |>
  dplyr::select(c(-countySpecies, -modelSpecies, -`Reason for exclusion from county map`,-`Reason for exclusion from gap analysis (Colin)`,
                  ))

write_csv(d3,file = "data/source_data/taxonomy20231212.csv")



# names list 
d4 <-googlesheets4::read_sheet(as_id("https://docs.google.com/spreadsheets/d/1BA7FQ8EU0ejooartCoNfx7moUN7wNEmEqU0VmCU6nPw/edit?usp=sharing"),
                               sheet = "summaryName")
write_csv(x = d4, file = "data/source_data/nameList.csv")


# Wiews Data 
## takes a long time to read in 
d5 <- googlesheets4::read_sheet(as_id("https://docs.google.com/spreadsheets/d/1QGY8witd4t8r6-ayNpLcx65z5UOTB8y4IBoevkuXrvs/edit?usp=sharing"))
                                #,sheet = "Wiews_Exsitu_1684365085723")
write_csv(x = d5, file = "data/source_data/wiews.csv")



# SEINET 
d5a <- googlesheets4::read_sheet(as_id("https://docs.google.com/spreadsheets/d/1KSYUgtVBuQv0YTEtwm6_ZfElvFdV0m-aoxV_bdVnkZs/edit?usp=sharing"))
write_csv(d5a, file = "data/source_data/seinet.csv")


# flora of north america data 
d6 <- googlesheets4::read_sheet(as_id("https://docs.google.com/spreadsheets/d/19e6wNr4Luc53NfBQJgl4AWGovZMp24P9yyDqmW9-h3Y/edit?usp=sharing"),
                                  sheet = "FNA data")
### need to do a bunch of find and replace with these ugly state names 
uniqueNames <- d6$`States from FNA` |> 
  stringr::str_split(pattern = ",") |>
  unlist() |>
  unique()
# defined the correct names 
dfName <- data.frame(original = uniqueNames, replace = NA)
dfName$replace <- c(
  "Ontario",
  "Alabama",
  "Arkansas",
  "Connecticut",
  "Delaware",
  "D.C",
  "Florida",
  "Georgia",
  "Illinois",
  "Indiana",
  "Iowa",
  "Kansas",
  "Kentucky",
  "Louisiana",
  "Maine",
  "Maryland",
  "Massachusetts",
  "Michigan",
  "Minnesota",
  "Mississippi",
  "Missouri",
  "Nebraska",
  "New Hampshire",
  "New Jersey",
  "New York",
  "North Carolina",
  "Ohio",
  "Oklahoma",
  "Pennsylvania",
  "Rhode Island",
  "South Carolina",
  "Tennessee",
  "Texas",
  "Vermont",
  "Virginia",
  "West Virginia",
  "Wisconsin",
  NA,
  "Alabama",
  NA,
  "South Dakota",
  "Florida",
  "Texas",
  "Oklahoma",
  "California",
  "Oregon",
  "Colorado",
  "New Mexico",
  "Arizona",
  "Nevada",
  "Utah",
  "Chihuahua",
  "Coahuila",
  "Nuevo Leon",
  "Sinaloa",
  "Sonora",
  "Baja California",
  "Manitoba",
  "New Brunswick",
  "Nova Scotia",
  "Ontario",
  "Quebec",
  "Saskatchewan",
  "Colorado",
  "Montana",
  "North Dakota",
  "Washington",
  "Wyoming",
  "Arkansas",
  "British Columbia",
  "California",
  "Idaho",
  NA
)

newDF <- d6
#newDF$`States from FNA` <- list()
# remove the NA value 
dfName <- dfName[!is.na(dfName$original), ]

for(i in 1:nrow(d6)){
  vals <- d6[i,  "States from FNA"]
  vals2 <- stringr::str_split(vals, pattern = ",") |> unlist()
  vals2a <-c()
  for(j in seq_along(vals2)){
    print(j)
    feature <- vals2[j]
    vals2a[j] <- paste0(dfName[dfName$original == feature,"replace"][1], ",")
  }
  # need to push this 
  replacement <- paste(vals2a, collapse=" ",sep = ",") 

  newDF[i,  2] <- replacement
}

write_csv(x = newDF,file = "data/source_data/FNA_stateClassification.csv")


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


### bonap Data
bonap <- read_sheet(as_id("https://docs.google.com/spreadsheets/d/1agwJm4rpFfY13UlwN8A6Vtb5p5MEUsWo-LaSXiFnvqM/edit?usp=sharing"))%>%
  mutate(across(everything(), as.character))
write_csv(x = bonap, file = "data/source_data/bonap.csv")





