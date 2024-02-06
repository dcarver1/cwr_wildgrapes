
# data <- occData

assignFIPS <- function(data){
  codes <- tigris::fips_codes %>%
    select(-state)
  
  #grap only state records 
  states <- codes %>%
    select(state_code, state_name)%>%
    distinct()
  
  
  # sort usa and others 
  d1 <- data[data$iso3 == "USA", ]
  d2 <- data[data$iso3 != "USA", ]
  
  # set state value 
  j1 <- left_join(d1, states, by = c("state" = "state_name"))%>%
    mutate(stateFIPS = state_code,
           county = case_when(
             grepl("County", county) ~ county, 
             is.na(county) ~ NA,
             TRUE ~ paste0(county," County")
             )
           )
  
  j2 <- left_join(j1, codes, by = c("stateFIPS" = "state_code" , 
                                     "county" = "county"))%>%
    mutate(countyFIPS= paste0(stateFIPS,county_code))
  
  dat <- j2 %>%
    select(names(d2))%>%
    bind_rows(d2)
  return(dat)
}
