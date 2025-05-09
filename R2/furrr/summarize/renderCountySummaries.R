###
# script to generate county level maps 
# carverd@colostate.edu 
# 20230717
###
pacman::p_load(furrr, dplyr, readr)
# set the parallel processing structure 
# plan(strategy = sequential) 
plan(strategy = multisession, workers = 16) 

# multisessoin is in parallel works on windows -- sequential runs withour parallel
# multicore is faster because there is less overhead, but it can not be ran on windows or thourgh R studio
# 
speciesList <-c("Vitis aestivalis",
                   "Vitis palmata",
                   "Vitis riparia",
                   "Vitis rotundifolia",
                   "Vitis rupestris",
                   "Vitis shuttleworthii",
                   "Vitis vinifera",
                   "Vitis vulpina")

fullSpecies <- read_csv("~/Documents/cwr_wildgrapes/data/Vitis/synonymList.csv")%>%
  select(`Taxon Name`)%>%
  pull()


# input parameters --------------------------------------------------------
## taxonomic reference 
speciesNames <- read_csv(file = "~/Documents/cwr_wildgrapes/data/source_data/taxonomy20230628.csv")
namedFeatures <- read_csv(file = "~/Documents/cwr_wildgrapes/data/source_data/nameList.csv")
## county level reference data
plantsData1 <- read_csv(file ="~/Documents/cwr_wildgrapes/data/source_data/usda_plants/completeVitis.csv")
bonapData <- read_csv("~/Documents/cwr_wildgrapes/data/source_data/bonap.csv")
natureSeverData <- read_csv("~/Documents/cwr_wildgrapes/data/processed_occurrence/natureServe.csv")
# valid lat long datasets 
observationData <- read_csv("~/Documents/cwr_wildgrapes/data/processed_occurrence/tempDataForCountyMaps_20231018.csv") |> 
  filter(!is.na(taxon))

#spatial data 
countySHP <- read_sf("~/Documents/cwr_wildgrapes/data/geospatial_datasets/counties/ne_10m_admin_2_counties.gpkg")
stateSHP <- read_sf("~/Documents/cwr_wildgrapes/data/geospatial_datasets/states/ne_10m_admin_1_states_provinces.gpkg")|>
  dplyr::filter(adm0_a3 == "USA")




## map implementation 
generateOccurnaceRMD <- function(species){
  print(species)
    rmarkdown::render(input = "R2/summarize/occuranceDataEvaluation.Rmd",
                      output_format = "html_document",
                      output_dir = file.path("data/countyMaps"),
                      output_file = paste0(species,"_Evaluation.html"),
                      params = list(
                        speciesName = as.character(species),
                        speciesNames = speciesNames,
                        namedFeatures = namedFeatures,
                        plantsData1 = plantsData1,
                        bonapData = bonapData,
                        natureSeverData = natureSeverData,
                        observationData = observationData,
                        countySHP = countySHP,
                        stateSHP = stateSHP
                        )
                      # envir = new.env(parent = globalenv()
    )
}
# ## needs to be commented out unless running 
# fullSpecies |> purrr::map(generateOccurnaceRMD)
# ### troubleshooting
# generateOccurnaceRMD(species ="Vitis aestivalis" )


## erroring out at specific species need to troubleshoot that directly 
## "Vitis tiliifolia"
 # fullSpecies |> furrr::future_map(.f = generateOccurnaceRMD, .progress = TRUE,
 #                                  .options = furrr_options(seed=TRUE))


## for loop implementation 
generateOccurnaceRMD <- function(speciesList){
  for(i in speciesList){
    print(i)
    try(
    rmarkdown::render(input = "R2/summarize/occuranceDataEvaluation.Rmd",
                      output_format = "html_document",
                      output_dir = file.path("data/countyMaps"),
                      output_file = paste0(i,"_Evaluation.html"),
                      params = list(
                        speciesName = as.character(i))
                      # envir = new.env(parent = globalenv()
      )
    )
  }
}


## needs to be commented out unless running 
# generateOccurnaceRMD(speciesList = fullSpecies)
# ### troubleshooting
# generateOccurnaceRMD(speciesList ="Vitis aestivalis" )
