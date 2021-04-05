
library(tidyverse)
library(openxlsx)

file <- read.xlsx("payments_track.xlsx") %>% as_tibble


subct <- read.xlsx("subct.xlsx") %>% as_tibble %>% select(Subcontract.id, Project,Service) %>% mutate_all(.,str_trim)


file <- file %>% mutate(Transaction.Date =  as.Date(Transaction.Date, origin = "1899-12-30")) %>% mutate_all(.,str_trim) %>% 
                 mutate(Transaction.Date = lubridate::as_date(Transaction.Date)) %>% 
                 mutate(Project = ifelse(Project == "421 S Harbor", "421 South Harbor Dr.", Project)) %>% 
                 mutate(Project = ifelse(Project == "421 S Harbor Owner Randy", "421 South Harbor Dr.", Project)) %>% 
                 mutate(Project = ifelse(Project == "30 Angelfish Cay Dr. Exterior Painting","30 Angelfish Cay Dr. Job 1", Project)) %>% 
                 mutate(Project = ifelse(Project == "21 Caloosa","21 Caloosa Rd.", Project)) %>% 
                 mutate(Project = ifelse(Project == "17 Caloosa","17 Caloosa-Maintenance", Project)) 

  
  
  
  
data <- file %>% left_join(subct, by = c("Project", "Service")) %>% relocate(.before = Project, Subcontract.id) 



missing_subct <- data %>% filter(is.na(Subcontract.id)) %>% select(Project,Service) %>% distinct()



openxlsx::write.xlsx(data,"payments_track_subct.xlsx")

openxlsx::write.xlsx(missing_subct,"missing_subct.xlsx")

##########

