# cwr_wildgrapes
update on the 2020 PNAS CWR of the USA workflow for new species

This is the base R workflow. See branch 'targets' for the targets based implementation of this modelling effort. 

As of 2023-08-16 the primary modeling methodology has been implemented and testing is occuring.  

## Goals 
1. revamp the existing CWR modeling methodology update libraries
2. replace absolute paths with a rproj file structure
3. provide more separation of the data cleaning, modeling, and gap analysis processes.
4. develop a more adaptive and flexible workflow that can be applied to additional species in the future

## Folder Structure

**Data**: holds inputs and outputs for the modeling effort and county level map developed. Individual species data for model runs will be store in here as well. 


**preprocessing** : code base and some content specific input datasets for prepping species observational data from multiple primary data sources

**R2** : functions and scripts to exicute the modeling process and generate the species level summary documents. 


## Additional notes 
