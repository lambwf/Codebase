
## Global Carbon Project CO2 LUC (https://globalcarbonbudget.org/carbonbudget2023/)

load_gcb_countries_ffi <- function(sheet_ffi) {
  
  data_gcb_co2_ffi <- gather(sheet_ffi,country,value,-X1)
  data_gcb_co2_ffi <- data_gcb_co2_ffi %>% 
    rename(year=X1) %>% 
    mutate(iso=countrycode(country,"country.name","iso3c",warn=FALSE))
  
  data_gcb_co2_ffi$iso[grepl("Türkiye",data_gcb_co2_ffi$country)] <- "TUR"
  
  data_gcb_co2_ffi <- data_gcb_co2_ffi %>% 
    #mutate(iso=ifelse(country=="Netherlands Antilles","ANT",iso)) %>% 
    #mutate(iso=ifelse(country=="Türkiye","TUR",iso)) %>% 
    mutate(value=value/1000) %>% 
    mutate(value=value*(44/12)) %>%
    mutate(units="GtCO2") %>% 
    select(country,iso,units,year,value) 
  
  return(data_gcb_co2_ffi) 
}

# sheet_blue <- readxl::read_xlsx('sources/National_LandUseChange_Carbon_Emissions_2023v1.0.xlsx',range="A8:GT182",sheet=2)
# sheet_hn <- readxl::read_xlsx('sources/National_LandUseChange_Carbon_Emissions_2023v1.0.xlsx',range="A8:GT182",sheet=3)
# sheet_oscar <- readxl::read_xlsx('sources/National_LandUseChange_Carbon_Emissions_2023v1.0.xlsx',range="A8:GT182",sheet=4)
# library(tidyverse)
# library(countrycode)

load_gcb_countries_luc <- function(sheet_blue,sheet_hn,sheet_oscar,sheet_luce){
  
  
  colnames(sheet_blue)[1] <- "year"
  colnames(sheet_hn)[1] <- "year"
  colnames(sheet_oscar)[1] <- "year"
  colnames(sheet_luce)[1] <- "year"
  
  sheet_blue <- gather(sheet_blue,country,blue,-year)
  sheet_hn <- gather(sheet_hn,country,hn,-year)
  sheet_oscar <- gather(sheet_oscar,country,oscar,-year)
  sheet_luce <- gather(sheet_luce,country,luce,-year)
  
  data_gcb_luc <- left_join(sheet_blue,sheet_hn,by = join_by(year, country))
  data_gcb_luc <- data_gcb_luc %>% 
    left_join(.,sheet_oscar,by = join_by(year, country)) %>% 
    left_join(.,sheet_luce,by = join_by(year, country))
  
  data_gcb_luc <- data_gcb_luc %>% 
    filter(year!="QF") %>% 
    mutate(blue=as.numeric(blue)) %>% 
    mutate(hn=as.numeric(hn)) %>% 
    mutate(oscar=as.numeric(oscar)) %>% 
    mutate(luce=as.numeric(luce)) %>% 
    mutate(mean=(blue+hn+oscar+luce)/4) %>% 
    mutate(iso=countrycode(country,"country.name","iso3c")) 
  
  data_gcb_luc <- data_gcb_luc %>%  
    mutate(iso=ifelse(country=="Netherlands Antilles","ANT",iso)) %>% 
    mutate(iso=ifelse(country=="Türkiye","TUR",iso))
  
  data_gcb_luc <- gather(data_gcb_luc,key,value,blue,hn,oscar,luce,mean) %>% 
    mutate(value=value/1000) %>% 
    mutate(value=value*(44/12)) 
  
  data_gcb_luc <- spread(data_gcb_luc,key,value) %>%
    mutate(units="GtCO2") %>% 
    select(country,iso,units,year,blue,hn,oscar,luce,mean) 
  
  
  return(data_gcb_luc)
}