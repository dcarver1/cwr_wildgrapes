###
# taxonomic name fixes and synonym checking 
# 
#
### 


standardizeNames <- function(data){
  
  ### I'm not sure if I want the taxon to hold the full name or the species.... 
  
  d1 <- data %>% 
    ## species 
    # remove any genus reference in species column 
    mutate(species = str_remove(species, "Vitis "))%>% 
    # remove any cultivar references  
    mutate(species =str_replace(species, "(cv.).*", ""))%>%
    # remove leading and trailing white space 
    mutate(species =str_trim(species))%>%
    # make all lower case 
    mutate(species = str_to_lower(species))%>%
    # capitalize the gensus name 
    mutate(genus = str_to_title(genus))%>%
    # improve the taxon case 
    mutate(taxon = str_to_sentence(taxon))
  

  return(d1)

}
