###
# Generate a set of targets to render the census tract data
# 20221109
# carverd@colostate.edu
###

environmentalSetup <- list(
  #grap species list
  tar_target(speciesList, command = unique(occuranceData$Taxon))
)

