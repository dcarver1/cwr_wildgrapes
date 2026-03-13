# species list 
spList <- c("Vitis acerifolia","Vitis aestivalis","Vitis aestivalis var. aestivalis"
            ,"Vitis aestivalis var. bicolor"    ,"Vitis arizonica"                  ,"Vitis californica"               
            ,"Vitis cinerea"                    ,"Vitis cinerea var. cinerea"       ,"Vitis girdiana"                  
            ,"Vitis labrusca"                   ,"Vitis monticola"                  ,"Vitis mustangensis"              
            ,"Vitis palmata"                    ,"Vitis riparia"                    ,"Vitis rotundifolia"              
            ,"Vitis rupestris"                  ,"Vitis shuttleworthii"             ,"Vitis tiliifolia"                
            ,"Vitis vulpina"                    ,"Vitis x novae-angliae"    )
# Setup paths
corePath <- "~/trueNAS/work/cwr_wildgrapes/data/Vitis"
exportFolder <- "~/trueNAS/work/cwrProtectedLands/data/2026_vitisData/distributions"
runVersion <- "run08282025_1k"

# Create export folder if it doesn't exist
if (!dir.exists(exportFolder)) {
  dir.create(exportFolder, recursive = TRUE)
}

# Loop over species
for (sp in spList) {
  # 1. Construct the path to the source TIFF
  # Following your example: corePath / Species Name / runVersion / results / prj_threshold.tif
  sourcePath <- file.path(corePath, sp, runVersion, "results/prj_threshold.tif")
  
  # 2. Construct a unique name for the export folder to avoid overwriting 
  # (e.g., Vitis_acerifolia_threshold.tif)
  cleanSpName <- gsub(" ", "_", sp)
  destPath <- file.path(exportFolder, paste0(cleanSpName, "_prj_threshold.tif"))
  
  # 3. Check if file exists and copy
  if (file.exists(sourcePath)) {
    message("Copying: ", sp)
    file.copy(from = sourcePath, to = destPath, overwrite = TRUE)
  } else {
    warning("File not found for: ", sp, "\nPath searched: ", sourcePath)
  }
}
