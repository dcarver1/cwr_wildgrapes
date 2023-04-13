###
# Generate a set of targets to render the census tract data
# 20221109
# carverd@colostate.edu
###

dataProcessing <- list(
  # tar_target(
  #   speciesPoints, 
  #   subsetSpecies(occuranceData, speciesList),
  #   pattern = map(speciesList)
  # ),
  tar_map(
    values = speciesList,
    subsetSpecies(occuranceData, species = speciesList)
  )
)


