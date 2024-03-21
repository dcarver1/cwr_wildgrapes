###
# script to generate county level maps 
# carverd@colostate.edu 
# 20230717
###
pacman::p_load(furrr, dplyr, readr, sf, terra, htmltools)
# set the parallel processing structure 
# plan(strategy = sequential)
plan(strategy = multisession, workers = 16) 

# multisessoin is in parallel works on windows -- sequential runs withour parallel
# multicore is faster because there is less overhead, but it can not be ran on windows or thourgh R studio
# 
speciesList <-c("Vitis acerifolia",
                "Vitis aestivalis",
                   "Vitis palmata",
                   "Vitis riparia",
                   "Vitis rotundifolia",
                   "Vitis rupestris",
                   "Vitis shuttleworthii",
                   "Vitis vulpina")
# or full species
fullSpecies <- read_csv("data/source_data/taxonomy20231212.csv")|>
  dplyr::filter(countySpecies  == "Y")|>
  select(taxon)|>
  pull()
fullSpeciesTrim <- c(
  # "Vitis acerifolia"                    
  #                    ,"Vitis aestivalis"                    
  #                    ,"Vitis aestivalis var. aestivalis"
  #                    ,"Vitis aestivalis var. bicolor"
  #                    ,"Vitis arizonica"                     
                     "Vitis baileyana" 
                     ,"Vitis berlandieri"                   
                     ,"Vitis californica"                   
                     ,"Vitis cinerea"                       
                    ,"Vitis girdiana"                      
                    ,"Vitis labrusca"                      
                    # ,"Vitis lincecumii"                    
                    ,"Vitis monticola"                     
                    ,"Vitis mustangensis"                  
                    ,"Vitis palmata"                       
                    ,"Vitis riparia"                       
                    ,"Vitis rotundifolia"                  
                    ,"Vitis rotundifolia var. munsoniana"  
                    ,"Vitis rotundifolia var. pygmaea"     
                    ,"Vitis rotundifolia var. rotundifolia"
                    ,"Vitis rufotomentosa"
                    ,"Vitis rupestris"                     
                    ,"Vitis shuttleworthii"                
                    ,"Vitis simpsonii"                     
                    ,"Vitis vulpina"                       
                    ,"Vitis x champinii"
                    ,"Vitis x doaniana"
                    ,"Vitis x novae-angliae"    )

erroredSpecies <- c(
  "Vitis aestivalis var. aestivalis"    
  ,"Vitis aestivalis var. bicolor"       
  ,"Vitis lincecumii"                    
  ,"Vitis rufotomentosa"                 
  ,"Vitis x champinii"                   
  ,"Vitis x doaniana"
  )

# input parameters --------------------------------------------------------
## taxonomic reference
speciesNames <- read_csv(file = "data/source_data/taxonomy20231212.csv")
namedFeatures <- read_csv(file = "data/source_data/nameList.csv")
## county level reference data
plantsData1 <- read_csv(file ="data/source_data/usda_plants/completeVitis.csv")
bonapData <- read_csv("data/source_data/bonap.csv")
natureSeverData <- read_csv("data/processed_occurrence/natureServe.csv")
# all data for the county maps
observationData <- read_csv("data/processed_occurrence/DataForCountyMaps_20230320.csv") |>
  dplyr::filter(!is.na(taxon))
# apply some additional filter to remove duplicated records 
duplicates <- duplicated(observationData, subset = c("taxon","recordID"))
observationData <- observationData[!duplicates, ]

### some Updates to the 


# fnaData
fnaData <- read_csv("data/source_data/FNA_stateClassification.csv")
# synonym dataset
synData <- read_csv("data/source_data/taxonomy20231212.csv")


# #spatial data
countySHP <- sf::st_read("data/geospatial_datasets/counties/ne_10m_admin_2_counties.gpkg")
stateSHP <- read_sf("data/geospatial_datasets/states/ne_10m_admin_1_states_provinces.gpkg")|>
  dplyr::filter(adm0_a3 == "USA")


### try to assign fips to all OCC data 
codes <- tigris::fips_codes %>%
  select(-state)

#grap only state records 
states <- codes %>%
  select(state_code, state_name)%>%
  distinct()


# bind the plants and bonap layers 
# b1 <- bonapData |> 
#   dplyr::select("State Abbveation" = Stateabb,
#                 "taxon"= "Scientific Name", 
#                 "countyFIPS" = "FIPS",  
#                 "county name" = "County")|>
#   dplyr::mutate("BONAP" =1)
# pl1 <- plantsData1 |>
#   dplyr::left_join(y = namedFeatures,
#                    by = c("plant_symbol" =  "Accepted Symbol"))|>
#   dplyr::mutate(countyFIPS = stringr::str_sub(geoid, start = 3))|>
#   dplyr::select(taxon = `Scientific Name`,
#                 state,
#                 countyFIPS,
#                 "county name" = county)|>
#   dplyr::mutate("USDA Plants"= 1)
# 
# pb <- dplyr::bind_rows(b1,pl1) |>
#   dplyr::select( "taxon","countyFIPS","county name", "State Abbveation", "state","BONAP","USDA Plants")
# write_csv(pb, file = "data/processed_occurrence/vitis_plants_bonap.csv" )

## map implementation 
generateOccurnaceRMD <- function(species1){
  print(species1)
    rmarkdown::render(input = "R2/summarize/countyEvaluation.Rmd",
                      output_format = "html_document",
                      output_dir = file.path("data/countyMaps"),
                      output_file = paste0(species1,"_Evaluation2.html"),
                      params = list(
                        speciesName = as.character(species1),
                        speciesNames = speciesNames,
                        namedFeatures = namedFeatures,
                        plantsData1 = plantsData1,
                        bonapData = bonapData,
                        natureSeverData = natureSeverData,
                        observationData = observationData,
                        countySHP = countySHP,
                        stateSHP = stateSHP,
                        fnaData = fnaData,
                        synData = synData
                        )
                      # envir = new.env(parent = globalenv()
    )
}
# # ## needs to be commented out unless running 
# fullSpecies |> purrr::map(generateOccurnaceRMD)
# speciesList |> purrr::map(generateOccurnaceRMD)
fullSpeciesTrim[1:length(fullSpeciesTrim)] |> purrr::map(generateOccurnaceRMD)
# erroredSpecies |> purrr::map(generateOccurnaceRMD)
# ### troubleshooting
# generateOccurnaceRMD(species ="Vitis rufotomentosa")


## erroring out at specific species need to troubleshoot that directly 
## "Vitis tiliifolia"
 # fullSpecies |> furrr::future_map(.f = generateOccurnaceRMD, .progress = TRUE,
 #                                  .options = furrr_options(seed=TRUE))



## for loop implementation 
# generateOccurnaceRMD <- function(speciesList){
#   for(i in speciesList){
#     print(i)
#     try(
#     rmarkdown::render(input = "R2/summarize/countyEvaluation.Rmd",
#                       output_format = "html_document",
#                       output_dir = file.path("data/countyMaps"),
#                       output_file = paste0(i,"_Evaluation.html"),
#                       params = list(
#                         speciesName = as.character(i))
#                       # envir = new.env(parent = globalenv()
#       )
#     )
#   }
# }


## needs to be commented out unless running 
#generateOccurnaceRMD(speciesList = fullSpecies)
# ### troubleshooting
### ISSUE with vitis x champinii
#generateOccurnaceRMD(speciesList ="Vitis palmata" )
