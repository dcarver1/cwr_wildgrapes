###
# script for transferring report files to the share site 
# carverd@colostate.edu 
# 20230713
###


# adjustable parameters ---------------------------------------------------
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
splist <- c( "Daucus_carota_subsp._capillifolius","Daucus_carota_subsp._fontanesii",
             "Daucus_carota_subsp._gummifer","Daucus_sahariensis","Daucus_syrticus")

# Find the files 
for(i in splist){
  path <- paste0("data/",genus,"/",i,"/",modelRun,"/results")
  files <- list.files(path, pattern = ".html",full.names = TRUE)
  if(length(files)>0){
    file.copy(files[1], folder)
    print(paste0(i, " moved"))
  }
}
# move the files 

