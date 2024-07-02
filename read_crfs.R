library(openxlsx)
library(tidyverse)
library(countrycode)

files_countries <- data.frame(files=list.files(path = "sources/CRFs/"))
files_countries <- files_countries %>% 
  mutate(country = str_extract(files, "^.{3}")) %>% 
  mutate(gwp = str_extract(files, ".{3}$")) %>% 
  mutate(year_submission = str_extract(files, "\\d{4}"))


### get total national emissions per country, per year, per gas

data_crfs <- data.frame(country=NA,year_submission=NA,gwp=NA,gas=NA,year=NA,category=NA,value=NA)

for (i in 1:length(files_countries$files)) {
  
  files_crfs <- data.frame(files=list.files(path = paste0("sources/CRFs/",files_countries$files[i])))
  files_crfs <- files_crfs %>% 
    mutate(year= str_extract(files, "(?<=_\\d{4}_)\\d{4}"))
  
  for (j in 1:length(files$files)) {
    
    sheet = read.xlsx(paste0("sources/CRFs/",files_countries$files[i],"/",files_crfs$files[j]),sheet="Summary1.As1",startRow = 5,cols = 1:9)
    names(sheet) <- c("category","CO2","CH4","N2O","HFCs","PFCs","HFCs/PFCs","SF6","NF3")
    sheet <- sheet %>% filter(category=="Total national emissions and removals")
    sheet <- gather(sheet,gas,value,-category)
    sheet <- sheet %>% 
      mutate(country = files_countries$country[i]) %>% 
      mutate(year_submission = files_countries$year_submission[i]) %>% 
      mutate(gwp = files_countries$gwp[i]) %>% 
      mutate(year = files_crfs$year[j]) %>% 
      select(country,year_submission,gwp,gas,year,category,value)
    
    data_crfs <- rbind(data_crfs,sheet)
    
    }
  
  
  
}

data_crfs <- data_crfs %>% 
  filter(!is.na(country)) %>% 
  mutate(iso=countrycode(country,"iso3c","iso3c")) %>% 
  mutate(iso=ifelse(country=="eua","EU27",iso)) %>% 
  mutate(country=countrycode(iso,"iso3c","country.name")) %>% 
  mutate(country=ifelse(iso=="EU27","European Union",country)) %>% 
  select(country,iso,everything())


save(data_crfs,file="data/data_crfs_2023.RData")
