Tracking the code that is ran to support specific figures/data associated with the publication 


# 12-12 updated 
- add institution code for BG-survay anmd heurto datasets for better metrics calculations a G collection summaries 



# preprocessing 


# model runs 

- R2/summarize/gatherAUCMetrics.R
    - used to generate the set of AUC metrics that are evaluating the model 




# SI material 

The majority of this is gather and organized by data/datasetsForPublication/exsituInsitutions.R
noting other specific functions/workflows what that is not the case 

- sheet 1 : this is this full occurrence records datasets 
     - material is gathered from the data/datasetsForPublication/allSpeciesOccurrences.csv

-sheet 2 : summary of the bioclim layers 
 - "data/geospatial_datasets/bioclim_layers/variableNames_072025.csv"

- sheet 3: records of all the predictor values for specific species 
    - 

- sheet 4 : the variable importants for model species 

- shee 5 :  the updated =models testing results 

- sheet 6 : priority score metrics for all species 
 - this is generated during the creation of the run summary rmd documen 

- sheet 7 : summary of db source and instiution code for the G collections 

- sheet 8 : number of point locations within the protected areas database 
    - R2/summarize/richnessPerProtectedArea.R

- sheet 9 : predicted species within each protected area
    - R2/summarize/richnessPerProtectedArea.R

- sheet 10 : the locations required for select the min number of protected areas within all pro areas 
    - R2/summarize/optimalProtectedAreas.R

- sheet 11 : varaible buffer size method 
    - multipleBuffers.R

