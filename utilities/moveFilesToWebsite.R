###
# script for transferring report files to the share site 
# carverd@colostate.edu 
# 20230713
###

pacman::p_load(dplyr,readr)





##################
# vitis specific moves ----------------------------------------------------
folder <- "~/trueNAS/work/vitis2/m"




# vitis SDMS --------------------------------------------------------------
genus <- "Vitis" 
modelRun <- "run08282025_1k"
# species 
splist <- read_csv("temp/allVitisData082025.csv")|>
  dplyr::filter(!is.na(taxon), genus == "Vitis")|>
  dplyr::select(taxon)|>
  distinct()



# files 
files <- list.files("data/Vitis/speciesSummaryHTML/run08282025_1k",full.names = TRUE)

# what species are missing 
for(i in splist$taxon){
  vals <- grepl(pattern = i, x = files)
  if(length(unique(vals)) != 2){
    print(i)
  }
}

# transfer the files 
for(i in files){
  print(i)
  file.copy(i, folder, overwrite = TRUE)
  
}


# move the run summary 
runsummary <- paste0("data/Vitis/",modelRun,"_Summary.html")
file.copy(runsummary, folder, overwrite = TRUE)
file.copy(paste0("data/Vitis/",modelRun,"_boxPlotSummary.html"), folder, overwrite = TRUE)
print("box plot summary doc copied")




# files <- list.files("data/countyMaps",pattern = "Evaluation2.html",full.names = TRUE)
# print(files)
# # Find the files 
# for(i in seq_along(files)){
#   if(length(files)>0){
#     file.copy(files[i], folder,overwrite = TRUE)
#     print(paste0(i, " moved"))
#   }
# }




# Daucus ---------------------------------------------------
## specific reference to file paths 
# genus <- "Daucus" 
# modelRun <- "run20240603"
# # folder to move too
# folder <- "~/Documents/Daucus"
# 
# 
# 
# #grab species list 
# splist <- read.csv("data/raw_occurances/daucusData_BioClimatic_2.5arc_modified.csv")|>
#   dplyr::select(taxon)|>
#   dplyr::distinct()|>
#   dplyr::pull()|>
#   sort()
# # or selected species 
# # splist <- c( "Daucus_aureus",
# #              "Daucus_carota_subsp._capillifolius",
# #              "Daucus_carota_subsp._gummifer",
# #              "Daucus_pusillus",
# #              "Daucus_sahariensis",
# #              "Daucus_syrticus")
# 
# # Find the files 
# for(i in splist){
#   path <- paste0("data/",genus,"/",i,"/",modelRun,"/results")
#   files <- list.files(path, pattern = ".html",full.names = TRUE)
#   if(length(files)>0){
#     file.copy(files[1], folder,overwrite = TRUE)
#     print(paste0(i, " moved"))
#   }
# }
# # run summary assessments 
# file.copy(paste0("data/Daucus/",modelRun,"_Summary.html"), folder, overwrite = TRUE)
# print("Summary doc copied")
# file.copy(paste0("data/Daucus/",modelRun,"_boxPlotSummary.html"), folder, overwrite = TRUE)
# print("box plot summary doc copied")

