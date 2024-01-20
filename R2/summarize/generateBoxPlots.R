
# start generating the species box plots 

# library(plotly)
# library(dplyr)

# d1 <- read.csv("C:/Users/carverd/Downloads/daucusData_BioClimatic_2.5arc_modified_forTesting.csv")
# n1 <- read.csv("C:/Users/carverd/Downloads/variableNames2.csv")


generateBoxPlot <- function(data,names,parameter){
  # grab the title
  title <- names$Current.title[names$shortName == parameter]
  # assign the model param
  vals <- data |> dplyr::select(parameter) |> pull()
  data <- data |>
    dplyr::mutate(modelParam = vals)
  # select generate the plot 
  fig <- plot_ly(data,
                 x = ~modelParam,
                 color = ~Name, 
                 type = "box",
                 boxpoints = "outlier",
                 line = list(width = 6),
                 pointpos = 0) |> 
    layout(boxmode = "group",
           xaxis = list(title = title))|>
    hide_legend()
  
  #export the figure 
  return(fig)
}

# generate single sub plot and export individual elements  
generateBoxPlot_export <- function(data,names,variables,exportLocation){
  plots <- list()
  
  for(i in variables){
    # generate plot 
    p1 <- generateBoxPlot(data = data, names = names, parameter = i)
    plots <- append(plots,list(p1))
  }
  # this doesn't visualize well but it might be useful for a different genus 
  # subplot <- subplot(plots, nrows = 5,shareY = TRUE,titleX = TRUE)
}

# variables <- c("bio_01","bio_02","bio_03","bio_04","bio_05","bio_06",
#                "bio_07","bio_08","bio_09","bio_10","bio_11","bio_12",
#                "bio_13","bio_14","bio_15","bio_16","bio_17","bio_18",
#                "bio_19","bio_20","bio_21","bio_22","bio_23","bio_24",
#                "bio_25","bio_26")

generateBoxPlot_export <- 

