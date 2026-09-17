# remove some errornous lat lon for specific speices 

# data <- speciesData
clearNewErrors <- function(data){
  # cinera - point new 0,0 
  c1 <- data |>
    dplyr::filter(taxon == "Vitis cinerea") 
  c2 <- c1 |>
    dplyr::filter(latitude == 0.00000)
  # remove all 0 lat points 
  c1 <- c1[!c1$index %in% c2$index, ]
  a1 <- data[data$taxon != "Vitis cinerea", ]
  # bind back together 
  data1 <- dplyr::bind_rows(c1,a1)
  # removed 13 points 
  
  
  # palmata 
  p1 <- data1 |>
    dplyr::filter(taxon == "Vitis palmata")   
  p2 <- p1 |>
    dplyr::filter(latitude == 26.73330 | latitude == 0.00000)
  # remove all 0 lat points 
  p1 <- p1[!p1$index %in% p2$index, ]
  a2 <- data[data$taxon != "Vitis palmata", ]
  # bind back together 
  data2 <- dplyr::bind_rows(p1,a2)
  # removed 5 points 
  
  
  # Vitis peninsularis
  pe1 <- data2 |>
    dplyr::filter(taxon == "Vitis peninsularis")   
  pe2 <- pe1 |>
    dplyr::filter(sourceUniqueID == "UCJEPS:UC:UC1191193") |>
    dplyr::mutate(latitude = -1 * latitude)
  # remove bind back together
  pe1 <- pe1[!pe1$index %in% pe2$index, ]
  pe3 <- bind_rows(pe1,pe2)
  a3 <- data2[data2$taxon != "Vitis peninsularis", ]
  # bind back together 
  data3 <- dplyr::bind_rows(pe3,a3)
  # no points removed 
  
  
  # Vitis riperia - remove very northern latitude value 
  v1 <- data3 |>
    dplyr::filter(taxon == "Vitis riparia")      
  # remove all 0 lat points 
  v2 <- v1 |>
    dplyr::filter(latitude == 0)
  # remove from v1 based on index 
  v1 <- v1[!v1$index %in% v2$index, ]
  
  a4 <- data3[data3$taxon != "Vitis riparia", ]
  # bind 
  data4 <- dplyr::bind_rows(v1,a4)
  # removed 6 points 
  
  
  # "Vitis shuttleworthii"
  s1 <- data4 |>
    dplyr::filter(taxon == "Vitis shuttleworthii")      
  s2 <- s1 |>
    dplyr::filter(longitude > -28)
  # remove all 0 lat points 
  s1 <- s1[!s1$index %in% s2$index, ]
  a5 <- data4[data4$taxon != "Vitis shuttleworthii", ]
  # bind 
  data5 <- dplyr::bind_rows(s1,a5)
  # removed 5 points
  
  # Vitis vulpina
  vu1 <- data5 |>
    dplyr::filter(taxon == "Vitis vulpina")      
  vu2 <- vu1 |>
    dplyr::filter(longitude ==0 )
  # remove all 0 lat points 
  vu1 <- vu1[!vu1$index %in% vu2$index, ]
  a6 <- data5[data5$taxon != "Vitis vulpina", ]
  # bind 
  data6 <- dplyr::bind_rows(vu1,a6)
  # 86 features removed 
  
  # Vitis Lambrusca 
  l1 <- data6 |>
    dplyr::filter(taxon == "Vitis labrusca")
  l2 <- l1 |>
    dplyr::filter(sourceUniqueID == "UNA00061932")
  # drop on index 
  l1 <- l1[!l1$index %in% l2$index,]
  
  a7 <- data6[data6$taxon != "Vitis labrusca", ]
  data7 <- dplyr::bind_rows(l1, a7)
  
  
  return(data7)
  
}
