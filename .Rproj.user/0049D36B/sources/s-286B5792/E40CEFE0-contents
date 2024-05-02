library(RColorBrewer)

ggplot <- function(...) ggplot2::ggplot(...) + scale_fill_brewer(palette="Set2")

theme_wl <- function() {
  
  font <- "sans"
  
  theme_bw() %+replace%
    
    theme(
      
      # Grid elements
      
      panel.grid.minor = element_blank(),    #strip minor gridlines
      axis.ticks = element_line(color="#636363",size = 0.7),          #strip axis ticks
      
      
      # Border lines
      panel.border = element_rect(color="#636363",fill=NA,size = 0.7),
      panel.background = element_blank(),
      
      # Text elements
      
      plot.title = element_text(             #title
        family = font,            #set font family
        size = 14,                #set font size
        face = 'bold',            #bold typeface
        hjust = 0,                #left align
        vjust = 3,                #raise slightly
        color = '#636363'),       #color
      
      plot.subtitle = element_text(
        family = font,
        size = 12,
        hjust = 0,
        vjust = 2,
        color = '#636363'),
      
      plot.caption = element_text(           #caption
        family = font,            #font family
        size = 9,                 #font size
        hjust = 0,                #right align
        color = '#bdbdbd'),       #color
      
      axis.title = element_text(             #axis titles
        family = font,            #font family
        size = 10,                #font size
        color = '#636363'),       #color
      
      axis.text = element_text(              #axis text
        family = font,            #axis famuly
        size = 9,                 #font size
        color = '#636363'),       #color
      
      axis.text.x = element_text(            #margin for axis text
        margin=margin(5, b = 10)),
      
      text = element_text(
        family = font,
        color = '#636363')
      
    )
  
}


more_set2_colors <- function(number) {
  
  colors = colorRampPalette(brewer.pal(8, "Set2"))(number)
  
}



