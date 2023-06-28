# pull stuff to the server through 

install.packages("googledrive")
install.packages("googlesheets4")
library(googledrive)
library(googlesheets4)
library(readr)

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
