
load_edgar <- function(sheet_ffi,sheet_ch4,sheet_n2o,sheet_fgas) {
  
  data_edgar_co2_ffi <- gather(sheet_ffi,year,value,Y_1970:Y_2023)
  data_edgar_co2_ffi$year <- gsub("Y_","",data_edgar_co2_ffi$year)
  data_edgar_co2_ffi <- data_edgar_co2_ffi %>% 
    select(iso=Country_code_A3,country=Name,code=ipcc_code_1996_for_standard_report,code_description=ipcc_code_1996_for_standard_report_name,gas=Substance,fossil_bio,year,value)
  
  
  data_edgar_ch4 <- gather(sheet_ch4,year,value,Y_1970:Y_2023)
  data_edgar_ch4$year <- gsub("Y_","",data_edgar_ch4$year)
  data_edgar_ch4 <- data_edgar_ch4 %>% 
    select(iso=Country_code_A3,country=Name,code=ipcc_code_1996_for_standard_report,code_description=ipcc_code_1996_for_standard_report_name,gas=Substance,fossil_bio,year,value)
  
  
  data_edgar_n2o <- gather(sheet_n2o,year,value,Y_1970:Y_2023)
  data_edgar_n2o$year <- gsub("Y_","",data_edgar_n2o$year)
  data_edgar_n2o <- data_edgar_n2o %>% 
    select(iso=Country_code_A3,country=Name,code=ipcc_code_1996_for_standard_report,code_description=ipcc_code_1996_for_standard_report_name,gas=Substance,fossil_bio,year,value)
  
  
  data_edgar_fgas <- gather(sheet_fgas,year,value,Y_1990:Y_2023)
  data_edgar_fgas$year <- gsub("Y_","",data_edgar_fgas$year)
  data_edgar_fgas <- data_edgar_fgas %>% 
    select(iso=Country_code_A3,country=Name,code=ipcc_code_1996_for_standard_report,code_description=ipcc_code_1996_for_standard_report_name,gas=Substance,fossil_bio,year,value)
  
  data_edgar <- rbind(data_edgar_co2_ffi,data_edgar_ch4)
  data_edgar <- rbind(data_edgar,data_edgar_n2o)
  data_edgar <- rbind(data_edgar,data_edgar_fgas)
  data_edgar$year <- as.numeric(data_edgar$year)
  
  
  return(data_edgar) 
  
}