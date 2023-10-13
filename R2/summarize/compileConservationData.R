


#' Generate Genus level conservation summaries 
#'
#' @param directory : top level directory contianing the output of the modeling process. 
#' @param runVersion : specific run reference used in constructing file paths 
#' @param figure : Return a figure or not 
#'
#' @return
compileConservationData <- function(directory, runVersion, genus, figure = FALSE){
  combined <- list.files( path = directory,
                          pattern =  "fcs_combined.csv",
                          full.names = TRUE,
                          recursive = TRUE)
  insitu <- list.files( path = directory,
                        pattern =  "fcs_in.csv",
                        full.names = TRUE,
                        recursive = TRUE)
    
  exsitu <- list.files( path = directory,
                        pattern =  "fcs_ex.csv",
                        full.names = TRUE,
                        recursive = TRUE)
  # subset 
  runCombined <- combined[grepl(pattern = runVersion, x = combined)] |>
    map(.f = read_csv) |>
    bind_rows()
  runInsitu <- insitu[grepl(pattern = runVersion, x = insitu)]|>
    map(.f = read_csv) |>
    bind_rows()%>%
    select( "ID",
            "SRSin" =SRS,
            "GRSin" = GRS,
            "ERSin" = ERS)
  runExsitu <- exsitu[grepl(pattern = runVersion, x = exsitu)]|>
    map(.f = read_csv) |>
    bind_rows()%>%
    select("ID",
           "SRSex" = SRS,
           "GRSex" = GRS,
           "ERSex" = ERS)
  
  
  
  # read and bind data to a single object 
  df <- runExsitu |> 
    left_join(y = runInsitu, by = "ID")%>%
    left_join(y = runCombined, by = "ID")
  
  
  # new attempt at plot for species only 
  tbl <- df |> 
    sort("ID") |>
    mutate(ID = str_replace_all(string = ID, pattern = "_", replacement = " "))
  
  if(figure==TRUE){
    
  p1 <- plot_ly(data = tbl, 
          x = ~ FCSc_mean,
          y = ~reorder(ID,FCSc_mean),
          type = "scatter",
          name = "FCS mean",
          marker = list(
            size = 16,
            color = "red",
            symbol = "diamond"
            )
          ) |>
    add_trace(x = ~FCSex,
              name = "FCSex",
              marker = list(
                size = 12,
                color = "black",
                symbol = "circle"
              )) |>
    add_trace(x = ~SRSex,
              name = "SRSex",
              marker = list(
                size = 8,
                color = "blue",
                symbol = "circle"
              )) |>
    add_trace(x = ~GRSex,
              name = "GRSex",
              marker = list(
                size = 8,
                color = "purple",
                symbol = "circle"
              )) |>
    add_trace(x = ~ERSex,
              name = "ERSex",
              marker = list(
                size = 8,
                color = "green",
                symbol = "circle"
              )) |>
    add_trace(x = ~FCSin,
              name = "FCSin",
              marker = list(
                size = 12,
                color ="black",
                symbol = "triangle-up"
              )) |>
    add_trace(x = ~SRSin,
              name = "SRSin",
              marker = list(
                size = 10,
                color = "blue",
                symbol = "triangle-up"
              )) |>
    add_trace(x = ~GRSin,
              name = "GRSin",
              marker = list(
                size = 10,
                color = "purple",
                symbol = "triangle-up"
              )) |>
    add_trace(x = ~ERSin,
              name = "ERSin",
              marker = list(
                size = 10,
                color = "green",
                symbol = "triangle-up"
              )) |>
    layout(shapes=list(list(type=rect, 
                            x0=0, 
                            x1=25, 
                            y0=-2, 
                            y1=length(unique(tbl$ID)), 
                            fillcolor='#ffb4b3', 
                            layer='below'),
                       list(type=rect, 
                            x0=25, 
                            x1=50, 
                            y0=-2,
                            y1=length(unique(tbl$ID)),
                            fillcolor='#ffd380', 
                            layer='below'),
                       list(type=rect, 
                            x0=50, 
                            x1=75, 
                            y0=-2,
                            y1=length(unique(tbl$ID)),
                            fillcolor='#ffff80', 
                            
                            layer='below'),
                      list(type=rect, 
                           x0=75, 
                           x1=100, 
                           y0=-2,
                           y1=length(unique(tbl$ID)),
                           fillcolor='#a8d2a8', 
                           layer='below')))%>%
    layout(title = paste0('Conservation Status of ',genus),
           xaxis = list(title = ''), 
           yaxis = list(title = ''), 
           legend = list(title=list(text='<b> Conservation Status </b>'),
                         orientation = 'h'))
  }else{
    p1 <- "No figure generated."
  }
  
  
  
  return(list(
    summaryData = df,
    figure = p1))
}
 