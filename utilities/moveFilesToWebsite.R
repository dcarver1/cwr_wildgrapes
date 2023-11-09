###
# script for transferring report files to the share site 
# carverd@colostate.edu 
# 20230713
###





# Daucus ---------------------------------------------------
## specific reference to file paths 
genus <- "daucus" 
modelRun <- "test1"
# folder to move too
folder <- "~/Documents/GeospatialCentroid.github.io/Daucus"



#grab species list 
splist <- read.csv("data/raw_occurances/daucusData_BioClimatic_2.5arc_modified.csv")%>%
  dplyr::select(taxon)%>%
  dplyr::distinct()%>%
  dplyr::pull()%>%
  sort()
# or selected species 
# splist <- c( "Daucus_carota_subsp._capillifolius","Daucus_carota_subsp._fontanesii",
#              "Daucus_carota_subsp._gummifer","Daucus_sahariensis","Daucus_syrticus")

# Find the files 
for(i in splist){
  path <- paste0("data/",genus,"/",i,"/",modelRun,"/results")
  files <- list.files(path, pattern = ".html",full.names = TRUE)
  if(length(files)>0){
    file.copy(files[1], folder)
    print(paste0(i, " moved"))
  }
}
# run summary assessments 
file.copy("data/daucus/test1_Summary.html", folder, overwrite = TRUE)
print("Summary doc copied")


# vitis specific moves ----------------------------------------------------
folder <- "~/Documents/GeospatialCentroid.github.io/vitis"


# vitis SDMS --------------------------------------------------------------
genus <- "Vitis" 
modelRun <- "test1"
# species 
splist <- read.csv("data/processed_occurrence/draft_model_data.csv")%>%
  dplyr::select(taxon)%>%
  dplyr::distinct()%>%
  dplyr::pull()%>%
  sort()
# loop to grab the files 
for(i in splist){
  path <- paste0("data/",genus,"/",i,"/",modelRun,"/results")
  files <- list.files(path, pattern = ".html",full.names = TRUE)
  if(length(files)>0){
    file.copy(files[1], folder)
    print(paste0(i, " moved"))
  }
}

# move the run summary 
runsummary <- paste0("data/Vitis/",modelRun,"_Summary.html")
file.copy(runsummary, folder)

# vits county maps --------------------------------------------------------
files <- list.files("data/countyMaps",pattern = "Evaluation2.html",full.names = TRUE)
print(files)
# Find the files 
for(i in seq_along(files)){
  if(length(files)>0){
    file.copy(files[i], folder,overwrite = TRUE)
    print(paste0(i, " moved"))
  }
}
