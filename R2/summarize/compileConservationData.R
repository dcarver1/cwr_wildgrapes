


#' Generate Genus level conservation summaries 
#'
#' @param directory : top level directory contianing the output of the modeling process. 
#' @param runVersion : specific run reference used in constructing file paths 
#' @param figure : Return a figure or not 
#'
#' @return
compileConservationData <- function(directory, runVersion, genus){
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
    mutate(ID = str_replace_all(string = ID, pattern = "_", replacement = " ")) |>
    sort("ID")
  

# figure 1 ----------------------------------------------------------------
  p1 <- plot_ly(data = tbl, 
          x = ~ FCSc_mean,
          y = ~ID,
          type = "scatter",
          name = "FCS mean",
          marker = list(
            size = 16,
            color = "red",
            symbol = "diamond"
            )
          ) |>
    add_trace(x = ~FCSex,
              name = "FCS ex situ",
              marker = list(
                size = 12,
                color = "black",
                symbol = "circle"
              )) |>
    add_trace(x = ~SRSex,
              name = "SRS ex situ",
              marker = list(
                size = 8,
                color = "blue",
                symbol = "circle"
              )) |>
    add_trace(x = ~GRSex,
              name = "GRS ex situ",
              marker = list(
                size = 8,
                color = "purple",
                symbol = "circle"
              )) |>
    add_trace(x = ~ERSex,
              name = "ERS ex situ",
              marker = list(
                size = 8,
                color = "green",
                symbol = "circle"
              )) |>
    add_trace(x = ~FCSin,
              name = "FCS in situ",
              marker = list(
                size = 12,
                color ="black",
                symbol = "triangle-up"
              )) |>
    add_trace(x = ~SRSin,
              name = "SRS in situ",
              marker = list(
                size = 10,
                color = "blue",
                symbol = "triangle-up"
              )) |>
    add_trace(x = ~GRSin,
              name = "GRS in situ",
              marker = list(
                size = 10,
                color = "purple",
                symbol = "triangle-up"
              )) |>
    add_trace(x = ~ERSin,
              name = "ERS in situ",
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
                            yref = "paper",
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
    layout(title = " ",
           xaxis = list(title = ''), 
           yaxis = list(title = '',
                        autorange="reversed"),
           legend = list(title=list(text='<b> Conservation Status </b>'),
                         orientation = 'h'),
           annotation = list(
             text = "Urgent Priority",
             xanchor = "left",
             yanchor = "top",
             layer='above'
           ))|>
    add_annotations( x = c(0.08, 0.37, 0.62,0.88),
                     y = 0.98,
                     yshift = 20,
                     text = c("<b>urgent priority</b>",
                              "<b>high priority</b>",
                              "<b>medium priority</b>",
                              "<b>low priority</b>"),
                     xref = "paper",
                     yref = "paper",
                     showarrow = FALSE
                     )



# figure 2 ----------------------------------------------------------------
  ### might need to add a group to the observations. 
  tbl$FCSc_mean <- as.numeric(tbl$FCSc_mean) 
  fcsmean <- mean(tbl$FCSc_mean)
  fcsexmean <- mean(tbl$FCSex)
  fcsinmean <- mean(tbl$FCSin)

  
  fig1 <- plot_ly(data = tbl,
                 x = ~jitter(FCSc_mean),
                 y = 'Final Conservation Score mean ',
                 type = "scatter",
                 orientation = 'h',
                 marker = list(color = 'rgb(0, 0, 0)'))|>
    add_trace(x = fcsmean,
              marker = list(color = 'red',
                            size = 20),
      showlegend = F
    ) |>
    layout(
      showgrid = FALSE,
      xaxis = list(
        range=c(0,100),
        showgrid = F,
        zeroline = F
      ),
      yaxis = list(zeroline = F,
                   showgrid = F),
      shapes=list(list(type=rect, 
                       x0=0, 
                       x1=25, 
                       y0=-2.2, 
                       y1=1, 
                       yref = "paper",
                       fillcolor='#ffb4b3',
                       line =list(width= 0),
                       layer='below'),
                  list(type=rect, 
                       x0=25, 
                       x1=50, 
                       y0=-2.2,
                       y1=1, 
                       yref = "paper",
                       fillcolor='#ffd380', 
                       line =list(width= 0),
                       layer='below'),
                  list(type=rect, 
                       x0=50, 
                       x1=75, 
                       y0=-2.2,
                       y1=1, 
                       yref = "paper",
                       fillcolor='#ffff80',
                       line =list(width= 0), 
                       layer='below'),
                  list(type=rect, 
                       x0=75, 
                       x1=100, 
                       y0=-2.2,
                       y1=1, 
                       yref = "paper",
                       fillcolor='#a8d2a8',
                       line =list(width= 0), 
                       layer='below'))
    )|>
    add_annotations( x = c(0.08, 0.37, 0.62,0.88),
                     y = 0.98,
                     yshift = 20,
                     text = c("<b>urgent priority</b>",
                              "<b>high priority</b>",
                              "<b>medium priority</b>",
                              "<b>low priority</b>"),
                     xref = "paper",
                     yref = "paper",
                     showarrow = FALSE
    )
  
    
  
  fig2 <- plot_ly(data = tbl,
                  x = ~jitter(FCSex),
                  y = 'Final Conservation Score ex situ ',
                  type = "scatter",
                  orientation = 'h',
                  marker = list(color = 'rgb(0, 0, 0)'))|>
    add_trace(x = fcsexmean,
            marker = list(color = 'red',
                          size = 20),
            showlegend = F
  )|>
    layout(
      showgrid = FALSE,
      xaxis = list(
        range=c(0,100),
        showgrid = F,
        zeroline = F
      ),
      yaxis = list(zeroline = F,
                   showgrid = F))
  
  fig3 <- plot_ly(data = tbl,
                  x = ~jitter(FCSin),
                  y = 'Final Conservation Score insitu ',
                  type = "scatter",
                  orientation = 'h',
                  marker = list(color = 'rgb(0, 0, 0)'))|>
    add_trace(x = fcsinmean,
              marker = list(color = 'red',
                            size = 20),
              showlegend = F
    ) |>
    layout(
      showgrid = FALSE,
      xaxis = list(
        range=c(0,100),
        showgrid = F,
        zeroline = F
      ),
      yaxis = list(zeroline = F,
                   showgrid = F))
  
  p2 <- subplot(fig1, fig2,fig3,
                nrows = 3,
                shareX = TRUE
                # margin = -0.01
                ) %>%
    layout(showlegend=FALSE,
           showgrid =FALSE,
           xaxis = list(title = '<b>Final Conservation Score</b>')) 


  

# return objects ----------------------------------------------------------
  return(list(
    summaryData = df,
    figure = p1,
    figure2 = p2))
}
 