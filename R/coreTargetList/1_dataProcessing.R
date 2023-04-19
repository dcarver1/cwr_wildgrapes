###
# Generate a set of targets to render the census tract data
# 20221109
# carverd@colostate.edu
###

dataProcessing <- list(
  # generate a dynamic branch for each species occurance data
  tar_target(
    speciesPoints,
    subsetSpecies(occuranceData = occuranceData, species = speciesList),
    pattern = map(speciesList)
    ),
  # generate a counts object for each species
  tar_target(
    counts,
    generateCounts(speciesData = speciesPoints),
    pattern = map(speciesPoints)
  ),
  #generate a sf point object for each species 
  tar_target(
    sp_points,
    createSF_Objects(speciesData = speciesPoints, species = speciesList),
    pattern = map(speciesList)
  ),
  # remove duplicated points observations. 
  tar_target(
    sp_points2, 
    removeDuplicates(sf_points = sp_points, species = speciesList),
    pattern = map(speciesList)
  )
)


