###
# script to generate county level maps 
# carverd@colostate.edu 
# 20230717
###

# 
# speciesList <-c("Vitis aestivalis",
#                    "Vitis palmata",
#                    "Vitis riparia",
#                    "Vitis rotundifolia",
#                    "Vitis rupestris",
#                    "Vitis shuttleworthii",
#                    "Vitis vinifera", 
#                    "Vitis vulpina")

generateOccurnaceRMD <- function(speciesList){
  for(i in speciesList){
    print(i)
    rmarkdown::render(input = "R2/summarize/occuranceDataEvaluation.Rmd",
                      output_format = "html_document",
                      output_dir = file.path("data/countyMaps"),
                      output_file = paste0(i,"_Evaluation.html"),
                      params = list(
                        speciesName = as.character(i))
                      # envir = new.env(parent = globalenv()
    )
  }
}

