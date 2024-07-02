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
    
    
    sheet_lulucf = read.xlsx(paste0("sources/CRFs/",files_countries$files[i],"/",files_crfs$files[j]),sheet="Summary1.As2",startRow = 5,cols = 1:9)
    names(sheet_lulucf) <- c("category","CO2","CH4","N2O","HFCs","PFCs","HFCs/PFCs","SF6","NF3")
    sheet_lulucf <- sheet_lulucf %>% filter(category=="4.  Land use, land-use change and forestry  (4)")
    sheet_lulucf <- gather(sheet_lulucf,gas,value,-category)
    sheet_lulucf <- sheet_lulucf %>% 
      filter(gas=="CO2") %>% 
      mutate(country = files_countries$country[i]) %>% 
      mutate(year_submission = files_countries$year_submission[i]) %>% 
      mutate(gwp = files_countries$gwp[i]) %>% 
      mutate(year = files_crfs$year[j]) %>% 
      select(country,year_submission,gwp,gas,year,category,value)
    
    data_crfs <- rbind(data_crfs,sheet)
    data_crfs <- rbind(data_crfs,sheet_lulucf)
    
    
    }
  
}

data_crfs <- data_crfs %>% 
  filter(!is.na(country)) %>% 
  mutate(iso=countrycode(country,"iso3c","iso3c")) %>% 
  mutate(iso=ifelse(country=="eua","EU27",iso)) %>% 
  mutate(country=countrycode(iso,"iso3c","country.name")) %>% 
  mutate(country=ifelse(iso=="EU27","European Union",country)) %>% 
  select(country,iso,everything())

## exclude LULUCF from totals

data_crfs <- data_crfs %>% 
  mutate(gas=ifelse(category=="4.  Land use, land-use change and forestry  (4)","CO2 LULUCF",gas)) %>% 
  select(-category) %>% 
  mutate(value=as.numeric(value))
data_crfs <- spread(data_crfs,gas,value)
data_crfs <- data_crfs %>% 
  mutate(CO2=CO2-`CO2 LULUCF`)
data_crfs <- gather(data_crfs,gas,value,CH4:SF6)

save(data_crfs,file="data/data_crfs_2023.RData")
