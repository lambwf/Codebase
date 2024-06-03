

growth_rate <- function(years,y) {
  
  data <- data.frame(years,y)
  
  data <- data %>%
    mutate(leap_years = leap_year(years)) %>%
    mutate(adjusted = ifelse(leap_years==TRUE,y*365/366,y))
  data <- data %>%
    mutate(y=adjusted)
  
  fit <- lm(log(y) ~ years,data = data)
  
  data <- data %>% 
    mutate(rate=fit$coefficients[2]) %>% 
    mutate(predicted_x = exp(predict(fit,data %>% select(years)))) %>% 
    mutate(st_error = sqrt(diag(vcov(fit)))[2])
  
  return(list("rate"=fit$coefficients[2],"data"=data))
}
