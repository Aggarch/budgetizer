
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

ns <- prices %>% filter(service == "stucco") %>%
  filter(grepl("1",thickness)) %>% mutate(thickness = '7/8\"')

prices <- prices %>% bind_rows(ns)

prices <- prices %>% filter(difficulty != 4)



# Stucco stimator example -------------------------------------------------

# create a workbook to save estimation by service in diff sheets. 
# it's possible to embeed all process in one function?¿

# check the logic of functions; explorer() & search_proposal()

explorer <- function(folder){
  
  
  setwd("C:/Users/andre/OneDrive/RohosGroup") 
  
  getwd()%>% 
    list.files() %>% as_tibble() %>%
    rename(folders = value) %>% print()
  
  
  direction <-  getwd() %>% as_tibble() %>%
    mutate(value = paste0(value,"/")) %>% 
    mutate(value = paste0(value,folder))
  
  setwd(direction$value)
  
  files <- getwd() %>% list.files() %>% as_tibble() %>% 
    rename(files = value)
  
  getwd() %>% print()
  
  setwd("C:/Users/andre/Downloads")
  
  return(files)
  
}

files <- explorer("PROPOSALS")

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

pricing <- "C:/Users/andre/OneDrive/RohosGroup/FINANCIAL & BOOKEEPING/Pricing/"
setwd(pricing)
prices <- openxlsx::read.xlsx("prices.df.xlsx") %>% as_tibble()
setwd("C:/Users/andre/Downloads")

  
toff <- search_proposal(clue)$take_off
path <- search_proposal(clue)$path
file <- search_proposal(clue)$new_file_name


# take off
object <- toff %>%  mutate_if(is.character, str_to_lower) %>% 
  filter(!is.na(project)) %>% 
  filter(!grepl("not|tot",project)) %>% 
  mutate_if(is.character, str_trim)

prods <- object %>% 
  group_by(service) %>% 
  summarise(area = sum(total_sf)) 


if(nrow(prods)>0 & sum(prods$area)>1){ 

if("framing" %in% prods$service){ 
framing_estim <- object %>%
  filter(grepl("framing",service)) %>%   
  group_by(service,difficulty,surface,material,thickness,caliber) %>%
  summarise(total_sqft = round(sum(total_sf)),.groups = "drop") %>% 
  na.omit %>%
  ungroup() %>% 
  mutate_if(is.character, str_trim)
}
else{framing_estim <- as_tibble(x=0); 
framing <- as_tibble("framing") %>% mutate(dimens = 0)}

if("drywall_installation" %in% prods$service){ 
drywall_estim <- object %>%
filter(grepl("drywall",service)) %>%   
  group_by(service,difficulty,material,thickness) %>%
  summarise(total_sqft = round(sum(total_sf)),.groups = "drop") %>% 
  na.omit %>%
  ungroup() %>% 
  left_join(prices, by=c("service","difficulty",
                         "material" = "design","thickness")) %>% 
  select(-texture) %>% 
  mutate(total_price = round(price*total_sqft)) %>% 
  mutate(materials = case_when(price == 1.88~ 1.04,
                               price == 2.01~ 1.17,
                               price == 1.30~ 0.46,
                               price == 1.34~ 0.50,
                               price == 1.22~ 0.38,
                               price == 1.18~ 0.34)) %>% 
  mutate(materials_cost = materials * total_sqft) %>% 
  mutate_if(is.character, str_trim) %>% 
  mutate(price = as.character(price), 
         difficulty = as.character(difficulty)) %>% 
  janitor::adorn_totals()
  drywall <- as_tibble("drywall") %>% mutate(dimens = nrow(drywall_estim)) 
}
else{drywall_estim <- as_tibble(x=0);
drywall <- as_tibble("drywall") %>% mutate(dimens = 0)}

if("plywood_installation" %in% prods$service){ 
# plywood installation
  plywood_estim <- object %>% filter(grepl("plywood",service)) %>% 
    group_by(difficulty,service) %>% 
    summarise(total_sf = round(sum(total_sf)),.groups = "drop") %>% 
    left_join(prices, by = c("service","difficulty")) %>% 
    select(-texture,-design,-thickness)%>% 
    mutate(materials = 2.5) %>% 
    mutate(total_price = round(price * total_sf),
           materials_cost = materials * total_sf) %>% 
    mutate(price = as.character(price), 
           materials = as.character(materials),
           difficulty = as.character(difficulty)) %>% 
  janitor::adorn_totals()  
  plywood <- as_tibble("plywood") %>% mutate(dimens = nrow(plywood_estim))
}
else{plywood_estim <- as_tibble(x=0);
plywood <- as_tibble("plywood") %>% mutate(dimens = 0)}

if("finish" %in% prods$service){ 
  finish_estim <- object %>% filter(grepl("finish",service)) %>% 
    group_by(difficulty,service,material,texture,design) %>% 
    summarise(total_sf = round(sum(total_sf)),.groups = "drop") %>% 
    left_join(prices, by = c("service","difficulty",
                             "material"= "texture","design")) %>% 
    select(-texture,-design,-thickness)%>% 
    mutate(total_price = round(price * total_sf)) %>% 
    mutate(price = as.character(price), 
           difficulty = as.character(difficulty)) %>% 
    janitor::adorn_totals()  
  finish <- as_tibble("finish") %>% mutate(dimens = nrow(finish_estim))
}
else{finish_estim <- as_tibble(x=0);
finish <- as_tibble("finish") %>% mutate(dimens = 0)}
  
if("stucco" %in% prods$service){ 
  stucco_estim <- object %>%
  filter(grepl("stucc",service)) %>% 
    group_by(service,difficulty, texture,design,thickness) %>%
    summarise(total_sqft = round(sum(total_sf)),.groups = "drop") %>% 
    na.omit %>%
    ungroup() %>% 
    left_join(prices, by= c("service", "difficulty",
                            "texture", "design", "thickness")) %>% 
    mutate(total_yards = round(total_sqft/9)) %>% 
    mutate(total_price = price*total_yards) %>% 
    mutate(price = as.character(price), 
             difficulty = as.character(difficulty)) %>% 
    janitor::adorn_totals()
    stucco <- as_tibble("stucco") %>% mutate(dimens = nrow(stucco_estim))
}
else{stucco_estim <- as_tibble(x=0);
      stucco <- as_tibble("stucco") %>% mutate(dimens = 0)}

}else{stop("no services")}

estimate_list = list(take_off = object,
                     framing  = framing_estim,
                     stucco   = stucco_estim,
                     drywall  = drywall_estim,
                     plywood  = plywood_estim,
                     finish   = finish_estim)


exclution <- stucco  %>%
  bind_rows(drywall) %>%
  bind_rows(plywood) %>%
  bind_rows(finish)  %>% 
  bind_rows(framing) %>% 
  filter(dimens<=1)  %>% 
  pull(value)

estimate <- list.remove(estimate_list,exclution)


hs <- openxlsx::createStyle(
  textDecoration = "BOLD", fontColour = "#FFFFFF", fontSize = 11,
  fontName = "Calibri", fgFill = "#4F80BD"
)

setwd(path)
getwd()

openxlsx::write.xlsx(estimate, file,
           startCol = 1, startRow = 1,colNames = TRUE,
           borders = "rows", headerStyle = hs,
           asTable = T, withFilter = TRUE)



resume <- getwd() %>% list.files() %>% as_tibble() %>% rename(files = value)

print("DONE")

return(list(resume = resume, estimate = estimate))


}

estimator("6720")


# build 3323 auto_estimate enrich the prices tibble 



# Distance of project  ----------------------------------------------------

as_distance <- function(
  lat1, long1, lat2, long2,
  unit = "km", R = c("km" = 6371, "miles" = 3959)[[unit]]
) {
  
  rad <- pi / 180
  d1 <- lat1 * rad
  d2 <- lat2 * rad
  dlat <- (lat2 - lat1) * rad
  dlong <- (long2 - long1) * rad
  a <- sin(dlat / 2) ^ 2 + cos(d1) * cos(d2) * sin(dlong / 2) ^ 2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  R * c
}

home <- geo(street = "24 Dockside Ln", city = "Key Largo",
            state = "FL", method = "census")

pj_address <- geo(street = "629 aledo ave", city = "Miami",
                  state = "FL", method = "census")

as_distance(home$lat, home$long,
            pj_address$lat, pj_address$long, unit = "km")

