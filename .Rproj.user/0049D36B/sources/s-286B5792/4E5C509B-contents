## these functions assume a syntax of var=key, value=value, labels= the name of the label column in the long data

gather_with_labels <- function(data,gather_cols) {
  
  data_labels <- labelled::get_variable_labels(data)
  data <- gather(data,var,value,gather_cols)
  data$label = NA
  for (i in 1:length(data_labels)) {
    data <- data %>% 
      mutate(label=ifelse(var==names(data_labels)[i],data_labels[i],label))
  }
  return(data)
} 

spread_with_labels <- function(data) {
  
  data_label <- data %>% ungroup() %>% select(var,label) %>% distinct()
  data <- spread(data %>% select(-label),var,value)
  for (i in 1:length(data_label)) {
    
    var_label(data[data_label$var[i]]) <-  data_label$label[[i]]
    
  }
  
  return(data)
}