#library(RColorBrewer)
#ggplot <- function(...) ggplot2::ggplot(...) + scale_fill_brewer(palette="Set2")

theme_wl <- function() {
  
  font <- "sans"
  
  theme_bw() %+replace%
    
    theme(
      
      # Grid elements
      panel.grid.minor = element_blank(),
      axis.ticks = element_line(color="#636363",linewidth =0.25),
      
      
      # Border lines
      panel.border = element_blank(),
      panel.background = element_blank(),
      axis.line = element_line(color="#636363",linewidth  = 0.25),
      
      
      # Facet strip
      strip.background = element_blank(),
      strip.text = element_text(size=12,hjust=0,margin=margin(b=3),color = '#636363'),
      
      
      # Text elements
      plot.title = element_text( 
        family = font,           
        size = 14,               
        face = 'bold',           
        hjust = 0,               
        vjust = 3,               
        color = '#636363',
        margin = margin(t = 10)),
      
      plot.subtitle = element_text(
        family = font,
        size = 12,
        hjust = 0,
        vjust = 2,
        color = '#636363',
        margin = margin(b = 3)),
      
      plot.caption = element_text(
        family = font,            
        size = 9,                 
        hjust = 0,                
        color = '#bdbdbd'),       
      
      axis.title = element_text(
        family = font,          
        size = 10,              
        color = '#636363'),     
      
      axis.text = element_text(
        family = font,         
        size = 9,              
        color = '#636363'),    
      
      axis.text.x = element_text(
        margin=margin(5, b = 10)),
      
      text = element_text(
        family = font,
        color = '#636363')
      
    )
  
}

theme_wl_bar_trend <- function() {
  
  theme_wl() %+replace%
    
    theme(
      
      # Grid elements
      panel.grid.major.x = element_blank(),
      strip.background = element_blank(),
      strip.text = element_text(size=12,color='#636363',hjust=0,face = "bold",margin=margin(b=5)),
      
      
      # Legend
      legend.position="right",
      legend.title=element_blank(),
      legend.key.size = unit(0.5, "cm"),
      
    )
      
}

theme_wl_empty <- function () {
  
  theme_wl() %+replace%
    
    theme(
      
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks = element_blank(),
      axis.line = element_blank(),
      panel.grid = element_blank(),
      panel.background = element_blank(),
      panel.border = element_blank(),
      
    )
  
}


# colours_9_sectors <- c(
#   "Energy | Power" = "#27426aff",
#   "Energy | Industry" = "#3a5e91ff",
#   "Energy | Transport" = "#5185b5ff",
#   "Energy | Buildings & other" = "#74a6d4ff",
#   "Energy | Fuel production" = "#17becfff",
#   "Industrial processes" = "#e5b82cff",
#   "Agriculture" = "#4f8455ff",
#   "Land use change (LULUCF)" = "#7dd396ff",
#   "Waste" = "#818181ff")


colours_9_sectors <- c(
  "Energy | Power" = "#17becfff",
  "Energy | Industry" = "#3a5e91ff",
  "Energy | Transport" = "#5185b5ff",
  "Energy | Buildings & other" = "#74a6d4ff",
  "Energy | Fuel production" = "#ff9055ff",
  "Industrial processes" = "#e5b82cff",
  "Agriculture" = "#4f8455ff",
  "Land use change (LULUCF)" = "#7dd396ff",
  "Waste" = "#818181ff")



colours_9_qual <- c("#ff9055ff", "#659cccff", "#e5b82cff", "#7dd396ff", "#818181ff", "#9467bdff", "#17becfff", "#d45087ff")


colours_fuels <- c("Coal"="#846752",
                   "Oil"="#9467bdff",
                   "Gas"="#d45087ff",
                   "Other fossil"="#8FB7B9",
                   "Biofuels"="#7dd396ff",
                   "Non-fossil"="#818181ff")


colours_gases <- c("CH4" = "#ff9055ff", 
                   "CO2 Fossil" = "#659cccff",
                   "N2O" = "#e5b82cff",
                   "CO2 LULUCF" = "#7dd396ff",
                   "F-gases" = "#17becfff",
                   "CO2" = "#659cccff")


colours_top6 <- c("Russia" = "#ff9055ff", 
                  "United States" = "#659cccff",
                  "China" = "#e5b82cff",
                  "European Union" = "#17becfff",
                  "Brazil" = "#7dd396ff",
                  "India" = "#d45087ff",
                  "World" = "#818181ff")



colours_socdr <- c("Afforestation, reforestation, forest management" = "#91BA72",
                   "Agroforestry" = "#3A6B61FF",
                   "Alkalinity enhancement of water bodies"= "",
                   "Bio-oil storage" = "#2B587B",
                   "Biochar soil amendment" = "#846752",
                   "Biochar" = "#846752",
                   "Bioenergy with Carbon Capture and Storage" = "#F4D3F2",
                   "Bioenergy with carbon capture and storage" = "#F4D3F2",
                   "BECCS" = "#F4D3F2",
                   "Biomass sinking" = "#8FB7B9",
                   "CDR (general)" = "#DFA126",
                   "Direct Air Carbon Capture and Storage" = "#73617F",
                   "Direct air carbon capture and storage" = "#73617F",
                   "DACCS" = "#73617F",
                   "Direct ocean carbon capture and storage" = "#BB93C3",
                   "Durable wood products" = "#E6A964",
                   "Enhanced weathering" = "#B55627",
                   "Mineral products" = "#762764",
                   "Ocean fertilisation" = "#33888E",
                   "Peatland and coastal wetland restoration"="#9FCCDD",
                   "Soil carbon sequestration in croplands and grasslands" = "#C4AA97",
                   "Soil carbon sequestration" = "#C4AA97",
                   "Other"="#DFA126")


# more_set2_colors <- function(number) {
#   
#   colors = colorRampPalette(brewer.pal(8, "Set2"))(number)
#   
# }



