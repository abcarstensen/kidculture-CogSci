

count_rows_for_files <- function(raw_data_directory){
  
  raw_files <- str_c(raw_data_directory, dir(here(raw_data_directory), "*.csv"))
  
  # count how many rows in each file 
  RAW_count <- map_df(raw_files, function(file) {
    d <- read_csv(file) %>% 
      count() %>% 
      mutate(
        file_name = file 
      )
  })   
  
}





trim_data <- function(count_df, min_row){
  
  RAW_DATA <- map_df((count_df %>% filter(n > min_row))$file_name,
                     function(file){
                       d <- read_csv(file)
                       
                       # if doesn't include responses probably bad file
                       if ("responses" %in% colnames(d)){
                         
                         if("audio_data" %in% colnames(d)){
                           print(file)
                           d <- d %>% select(-audio_data)
                         }
                         
                         d <- d %>% 
                           mutate(across(any_of(c("rt", "RT", "button_pressed")),
                                         as.character))
                         
                         return(d)
                       }
                       
                     })
  
  return(RAW_DATA)
  
}

