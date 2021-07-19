

extractor <- function(clue,value){ 
  
  library(tidyverse)
  
  setwd("C:/Users/andre/OneDrive/RohosGroup/PROPOSALS")
  
  proposal_folder <-  list.files() %>% as_tibble() %>% 
    filter(grepl(clue,value)) %>% pull()
  
  direction <-  getwd() %>% as_tibble() %>%
    mutate(value = paste0(value,"/")) %>% 
    mutate(value = paste0(value,proposal_folder)) %>% 
    pull()
  
  setwd(direction)
  
  files_list <- getwd() %>% list.files() %>% as_tibble()
  
  specific_file <- list.files() %>% as_tibble() %>% 
    filter(!grepl(".pdf|Client|client|auto",value)) %>% 
    filter(grepl(".xlsx",value))
  
  new_file_name <- specific_file %>% 
    mutate(value = str_replace_all(value," ","_")) %>%
    mutate(value = paste0("auto_estimate_",value)) %>% 
    pull()
  
  
  service <- openxlsx::read.xlsx(specific_file$value, sheet = value) %>% 
    as_tibble() %>% janitor::clean_names() %>% 
    mutate_if(is.character, str_to_lower)  

  names(service) <- NULL
  names(service) <- service[1,]  
  service<- service[-1,]  
  
  service_data <- service %>% 
    filter(material != "grand_total") %>% 
    na.omit()
  
  
  setwd("C:/Users/andre/Downloads")
  
  
  return(service_data)
  
}
