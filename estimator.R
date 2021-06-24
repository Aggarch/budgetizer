
# This script rudiments how to automate the estimation of services, based on 
# a tibble with prices and the dynamic grouping of services a criterias. 

# https://stackoverflow.com/questions/12945687/read-all-worksheets-in-an-excel-workbook-into-an-r-list-with-data-frames



# WEB-APP -----------------------------------------------------------------

setwd("C:/Users/andre/Downloads")


prices <- openxlsx::read.xlsx("prices_tibble.xlsx") %>% 
  as_tibble() %>% filter(!is.na(price)) %>% 
  mutate(difficulty = as.character(difficulty))

# d2 <- core %>% mutate(difficulty = 2, price = price+1)
# d3 <- core %>% mutate(difficulty = 3, price = price+2)
# d4 <- core %>% mutate(difficulty = 3, price = price+3)
# 
# prices <- core %>% bind_rows(d2,d3,d4) %>% 
#   rename(design = surface)
# 
# openxlsx::write.xlsx(prices, "prices_tibble.xlsx")
# 


# Replace 'toff' document with dynamic search proposal fucntion

toff <- openxlsx::read.xlsx("toff.xlsx") %>% 
  as_tibble() %>% 
  
  mutate(Service = str_to_lower(Service)) %>% 
  mutate(Project = str_to_lower(Project)) %>% 
  mutate(Difficulty = as.character(Difficulty)) %>% 
  
  janitor::clean_names() %>% 
  
  mutate(location = str_to_lower(location)) %>% 
  
  
  mutate(design = ifelse(service == "drywall_installation", material, design)) %>% 
  mutate(design = case_when(str_detect(design,"gypsum")~"gypsum",
                            str_detect(design,"durock")~"durock",
                            str_detect(design,"green_board")~"durock",
                            TRUE ~ as.character(design)
  )
  ) %>% 
  mutate(service = case_when(str_detect(service,"exterior")~"exterior_painting",
                             str_detect(service,"interior")~"interior_painting",
                             TRUE ~ as.character(service))
  
  )


p_stucco <- toff %>% filter(service == "stucco") %>% 
  select(project,location,service,difficulty,design,
         texture,thickness,total_sf) %>%
  mutate_if(is.character, str_trim) %>% 
  group_by(project,location,service,difficulty,design,texture,thickness) %>% 
  summarise(sqft = sum(total_sf),
            yards= sqft/9,
            .groups = "drop") %>% 
  left_join(prices, by = c("service", "difficulty",
                           "design", "texture", "thickness")) %>% 
  distinct() %>% 
  mutate(total_price = price*yards)%>%
  mutate(difficulty = as.character(difficulty)) %>% 
  mutate(price = as.character(price)) %>% 
  janitor::adorn_totals()


p_dryw <- toff %>% filter(service == "drywall_installation") %>% 
  select(project,location,service,difficulty,design,
         thickness,total_sf) %>%
  mutate_if(is.character, str_trim) %>% 
  group_by(project,location,service,difficulty,design,thickness) %>% 
  summarise(sqft = sum(total_sf),.groups = "drop") %>% 
  left_join(prices, by = c("service", "difficulty",
                           "design", "thickness")) %>% 
  mutate(total_price = price*sqft)%>%
  rename(material = design) %>% 
  select(-texture) %>% 
  mutate(difficulty = as.character(difficulty)) %>% 
  mutate(price = as.character(price)) %>% 
  janitor::adorn_totals()


p_finish <- toff %>% filter(service == "finish") %>% 
  select(project, location,service,
         difficulty,design,total_sf) %>%
  mutate_if(is.character, str_trim) %>% 
  group_by(project,location,service,difficulty,design) %>% 
  summarise(sqft = sum(total_sf),.groups = "drop") %>% 
  left_join(prices, by = c("service", "difficulty",
                           "design")) %>% 
  mutate(total_price = price*sqft)%>%
  rename(material = design) %>% 
  select(-texture) %>% 
  mutate(difficulty = as.character(difficulty)) %>% 
  select(-thickness) %>% 
  mutate(price = as.character(price)) %>% 
  janitor::adorn_totals()



p_paint <- toff %>% filter(grepl("paint",service)) %>% 
  mutate(hours = as.double(hours)) %>% 
  group_by(project, location,
           service, difficulty) %>% 
  summarise(hours = sum(hours),
            .groups = "drop") %>% 
  mutate(days = round(hours/8)) %>% 
  mutate(days = case_when(between(days, 0,30)~ days + 1,
                          between(days, 31,60)~ days + 2, 
                          between(days, 60,Inf)~ days + 3,
                          TRUE ~ as.double(days),
  )) %>% 
  rename(texture = location) %>% 
  left_join(prices, by = c("service","difficulty","texture")) %>% 
  select(project,service, difficulty, location = texture, hours, days, price) %>% 
  mutate(labor = days*8*price,
         materials = labor * .20) %>% 
  rowwise() %>% 
  mutate(total_price = sum(labor,materials)) %>% 
  relocate(.before = service, location) %>% 
  janitor::adorn_totals()



# create procedure to store each of these services in differents sheets. 





# revision Pumpkin --------------------------------------------------------

pump <- invoices %>% filter(grepl("72 P", name))
transactions %>% filter(num %in% pump$num) 



prices <- openxlsx::read.xlsx("prices_tibble.xlsx") %>% as_tibble()

ns <- prices %>% filter(service == "stucco") %>% filter(grepl("1",thickness)) %>% mutate(thickness = '7/8\"')

prices <- prices %>% bind_rows(ns)

prices <- prices %>% filter(difficulty != 4)



# Stucco stimator example -------------------------------------------------

# create a workbook to save estimation by service in diff sheets. 
# it's possible to embeed all process in one function?¿

# check the logic of functions; explorer() & search_proposal()


files <- explorer("PROPOSALS") %>% 
  filter(grepl(clue,files))


search_proposal <- function(clue){ 


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


take_off <- openxlsx::read.xlsx(specific_file$value) %>% 
  as_tibble() %>% janitor::clean_names() %>% 
  mutate_if(is.character, str_to_lower)


sheets <- openxlsx::getSheetNames(specific_file$value) %>%
  as_tibble() %>% rename(sheets = value)

setwd("C:/Users/andre/Downloads")


return(list(sheets = sheets,take_off = take_off,
            path = direction,new_file_name = new_file_name,
            files_list = files_list))
}

estimator <- function(clue){ 

toff <- search_proposal(clue)$take_off
path <- search_proposal(clue)$path
file <- search_proposal(clue)$new_file_name


interest <- prices %>% colnames()

object <- toff %>% 
  mutate_if(is.character, str_to_lower) %>%
  # select(project,location,contains(interest),total_sf) %>% 
  left_join(prices,by = c("service", "difficulty", "texture", "design", "thickness")) 


stucco_estim <- object %>%
  group_by(service,difficulty, texture,design,thickness) %>%
  summarise(price = max(price),total_sqft = round(sum(total_sf)),.groups = "drop") %>% 
  na.omit %>%
  ungroup() %>% 
  mutate(total_yards = round(total_sqft/9)) %>% 
  mutate(total_price = price*total_yards) %>% 
  mutate(price = as.character(price), 
         difficulty = as.character(difficulty)) %>% 
  janitor::adorn_totals()
  # mutate(price = as.numeric(price),
  #        difficulty = as.numeric(difficulty))


estimate = list(take_off = object, stucco = stucco_estim)


hs <- openxlsx::createStyle(
  textDecoration = "BOLD", fontColour = "#FFFFFF", fontSize = 9,
  fontName = "Calibri", fgFill = "#4F80BD"
)

setwd(path)
getwd()

openxlsx::write.xlsx(estimate, file,
           startCol = c(1,2), startRow = 2,colNames = TRUE,
           borders = "rows", headerStyle = hs,
           asTable = T, withFilter = TRUE)



resume <- getwd() %>% list.files() %>% as_tibble() %>% rename(files = value)

print("DONE")

return(list(resume = resume, estimate = estimate))


}

estimator("6720")



