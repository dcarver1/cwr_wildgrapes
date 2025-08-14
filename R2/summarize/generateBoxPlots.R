
# start generating the species box plots 

# library(plotly)
# library(dplyr)

# d1 <- read.csv("C:/Users/carverd/Downloads/daucusData_BioClimatic_2.5arc_modified_forTesting.csv")
# n1 <- read.csv("C:/Users/carverd/Downloads/variableNames2.csv")


set.seed(8)
y <- rnorm(200)
group <- sample(LETTERS[1:3], size = 200,
                replace = TRUE)
df <- data.frame(y, group)




generateBoxPlot <- function(data,names,parameter){
  # grab the title
  title <- names$`Current title`[names$vitisModelNames  == parameter]
  # assign the model param
  data2 <- data |> 
    # remove the glochidiatus row with the extreme outline 
    # dplyr::filter(bio_01 > -30)|> 
    dplyr::select(taxon, type, feature = parameter) |>
    dplyr::mutate(taxon = str_replace_all(taxon, "_", " ") )
  # only G points for the jitters
  data3 <- data2 |>
    dplyr::filter(type == "G")
  
  # ggplot2 -----------------------------------------------------------------
  ## set the palette 
  ## 8 is the max 
  total <- length(unique(data2$taxon))
  bluePal <- c("#ffffcc","#ffeda0","#fed976","#feb24c","#fd8d3c","#fc4e2a","#e31a1c")
  setPalette <- rep(x = bluePal, times = ceiling(total/7))[1:total]
  # Box plot by group with jitter
  fig <- ggplot(data = data2, aes(x = taxon, y = feature, fill = taxon)) +
    geom_boxplot(outlier.shape = NA)+
    scale_fill_manual(values = setPalette  ) +  # Set manual colors for boxplots based on taxon levels
    # scale_fill_grey()+ # this works pretty well
    geom_jitter(data = data3, fill ="#6300f0", alpha = 0.5, shape = 21, size = 1, position = position_jitter(width = 0.3))+
    # Set manual colors for points based on type levels
    coord_flip() +
    labs(title = title)+
    theme(legend.position="none",
          axis.title.x = element_blank(),
          axis.title.y = element_blank())
  
  
  # plotly work  ------------------------------------------------------------
  # grab the title
  # title <- names$`Current title`[names$shortName == parameter]
  # # assign the model param
  # vals <- data |> 
  #   dplyr::select(parameter) |> 
  #   pull()
  # data2 <- data |>
  #   dplyr::mutate(modelParam = vals)
  # 
  # # select generate the plot 
  # fig <- plot_ly(data,
  #                x = ~modelParam,
  #                color = ~taxon, 
  #                type = "box",
  #                boxpoints = "outlier",
  #                line = list(width = 6),
  #                pointpos = 0) |> 
  #   layout(boxmode = "group",
  #          xaxis = list(title = title))|>
  #   hide_legend()
  
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

