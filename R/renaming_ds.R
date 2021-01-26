# rename datasets

modColDef <- function(x) {
  colDef(name = gsub(".+>", "", x))
  
} 

creatGroups <- function(x) {
  y <- tibble(name = as.character(), columns = as.character())
  i <- 1
  for (i in 1:length(x)) {
    y <- rbind(y, tibble(name = gsub(">\\w+", "", x[i]), 
                         columns = list(x %>% 
                                          grep(gsub(">\\w+", "", x[i]),., value = T))
    ))
    
    i <- i +1
  }
  y
}