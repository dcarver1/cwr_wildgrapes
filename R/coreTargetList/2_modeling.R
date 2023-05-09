###
# target list for modeling process 
# carverd@colostate.edu 
# 20230419
###


runModels <- list(
  ## subsample to 2000 features based on geography 
  
  # generate a native area extent file 
  tar_target(
    natAreas,
    createNaturalAreas(sp_object = sp_points2,
                       species = speciesList,
                       ecoRegions = ecoregions),
    pattern = map(speciesList)
  )
  
  ## generate g50 buffer object 
  
  ## associate observations with bioclim data
  ## perform variable selection 
  ## perform maxent model 
  ## evaluate outputs of the modeling process
  ## generate a mess map 
  ## generate a kernal density map 
)