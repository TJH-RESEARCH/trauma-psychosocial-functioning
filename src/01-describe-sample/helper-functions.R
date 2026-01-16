
# Creates helper functions to count the percentage of demographics

count_perc <-
  function(x, sort){
    x <- 
      x %>% 
      count(sort = sort) %>% 
      ungroup() %>% 
      mutate(perc = n / sum(n)) %>% 
      rename(category = 1)
    
    if(!is.character(x$category)){
      x = x %>% mutate(category = as.character(category))
    }
    return(x)
  }


create_percentage_table <- function(x){
  x <- 
    x %>% 
    tidyr::pivot_longer(everything(), names_to = 'category', values_to = 'response') %>% 
    group_by(category) %>% 
    summarize(total = n(), n = sum(response, na.rm = T), perc = n/total) %>% 
    select(!total)
  
  return(x)
}


