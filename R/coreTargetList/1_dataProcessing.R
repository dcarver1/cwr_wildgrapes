###
# Generate a set of targets to render the census tract data
# 20221109
# carverd@colostate.edu
###

dataProcessing <- list(
  tar_target(
    speciesPoints,
    subsetSpecies(occuranceData = occuranceData, species = speciesList),
    pattern = map(speciesList)
    ),
  
  tar_target(
    counts,
    generateCounts(speciesData = speciesPoints),
    pattern = map(speciesPoints)
  )
)


