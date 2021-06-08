
# This script rudiments how to automate the estimation of services, based on 
# a tibble with prices and the dynamic grouping of services a criterias. 

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

