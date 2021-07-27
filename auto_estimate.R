
# Automatic Estimator -----------------------------------------------------

# Program to detect a proposal folder given a 'clue',import & clean the take off 
# & estimate the prices of the services included in the Architect's take off.
# the algorithm also creates the resume & save the result in original folder
# it produces a sheet by services existing in take off file. 


# Functions of the Algorithm::: -------------------------------------------

# explorer():Additional, not crucial for the task, displays proposals folders. 
# search_proposal():Detect the folder of interest,create paths & extracts toff.
# estimator():Import,wrangler & estimation engine, file creation & cloud storage. 

# Libs
library(tidyverse)
library(openxlsx)
library(rlist)

# Notes:
# ADD Waterproofing & Painting to pricing process. 
# ADD Resumen tibble, silent warnings of janitor:: 

# Compare test projects. 

# Declare -----------------------------------------------------------------
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

search_proposal <- function(clue){ 
  
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
  
  library(tidyverse)
  library(rlist)
  
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
    mutate_if(is.character, str_trim) %>% 
    mutate(difficulty = as.character(difficulty))
  
  prods <- object %>% 
    group_by(service) %>% 
    summarise(area = sum(total_sf)) 
  
  
  if(nrow(prods)>0 & sum(prods$area)>1){ 
    
    if("framing" %in% prods$service){ 
      framing_estim <- object %>%
        filter(grepl("framing",service)) %>%  
        mutate(thickness = str_replace_all(thickness, "''",'"')) %>%
        group_by(service,surface,material,thickness,design) %>%
        summarise(total_sqft = round(sum(total_sf)),.groups = "drop") %>% 
        mutate(design = ifelse(is.na(design),"20ga",design)) %>% 
        na.omit %>%
        ungroup() %>% 
        mutate_if(is.character, str_trim) %>% 
        left_join(prices, by = c("service","material"="difficulty",
                                 "surface"="texture","design","thickness")) %>% 
        mutate(total_price = total_sqft * price) %>% 
        mutate(price = as.character(price)) %>% 
        distinct() %>% 
        janitor::adorn_totals() %>%   
        mutate(price = as.numeric(price))
      framing <- as_tibble("framing") %>% mutate(dimens = nrow(framing_estim)) 
      }
    else{framing_estim <- as_tibble(x=0); 
    framing <- as_tibble("framing") %>% mutate(dimens = 0)}
    
    
    if("drywall_installation" %in% prods$service){ 
      drywall_estim <- object %>%
        filter(grepl("drywall",service)) %>%   
        mutate(thickness = str_replace_all(thickness, "''",'"')) %>%
        mutate(material = ifelse(material == "gypsum_type_x","gypsum",material)) %>% 
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
        janitor::adorn_totals() %>% 
        mutate(price = as.numeric(price)) %>%
        mutate(difficulty = as.numeric(difficulty)) 
      drywall <- as_tibble("drywall") %>% mutate(dimens = nrow(drywall_estim)) 
    }
    else{drywall_estim <- as_tibble(x=0);
    drywall <- as_tibble("drywall") %>% mutate(dimens = 0)}
    
    
    
    if("insulation" %in% prods$service){ 
      
      insulation_estim <- object %>% filter(grepl("insulat",service)) %>% 
        group_by(service,material,thickness) %>% 
        summarise(total_sf = sum(total_sf), .groups = "drop") %>% 
        ungroup() %>% 
        mutate(price =  case_when(str_detect(material, "batt") ~ 1.2,
                                  str_detect(material, "r") ~ 1.6)) %>% 
        rowwise() %>% 
        mutate(total_price = total_sf * price) %>% 
        mutate(material =  case_when(str_detect(material, "batt") ~ 0.72,
                                     str_detect(material, "r") ~    0.74)) %>% 
        mutate(materials_cost = total_sf * material) %>% 
        relocate(.after = total_price, material) %>% 
        janitor::adorn_totals()
        
      insulation <- as_tibble("insulation") %>% mutate(dimens = nrow(insulation_estim)) 
    }
    else{insulation_estim <- as_tibble(x=0);
    insulation<-as_tibble("insulation") %>% mutate(dimens = 0)}
    
    
    
    
    if("plywood_installation" %in% prods$service){ 
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
        janitor::adorn_totals()  %>% 
        mutate(price = as.numeric(price)) %>%
        mutate(difficulty = as.numeric(difficulty)) 
      plywood <- as_tibble("plywood") %>% mutate(dimens = nrow(plywood_estim))
    }
    else{plywood_estim <- as_tibble(x=0);
    plywood <- as_tibble("plywood") %>% mutate(dimens = 0)}
    
    if("finish" %in% prods$service){ 
      finish_estim <- object %>% 
        filter(grepl("finish",service)) %>% 
        mutate(material = ifelse(material == "gypsum_type_x","gypsum",material)) %>% 
        group_by(difficulty,service,material,texture,design) %>% 
        summarise(total_sf = round(sum(total_sf)),.groups = "drop") %>% 
        left_join(prices, by = c("service","difficulty",
                                 "material"= "texture",
                                 "texture" = "thickness", 
                                 "design")) %>% 
        select(-texture,-design)%>% 
        mutate(total_price = round(price * total_sf)) %>% 
        mutate(price = as.character(price), 
               difficulty = as.character(difficulty)) %>% 
        janitor::adorn_totals()  %>% 
        mutate(price = as.numeric(price)) %>%
        mutate(difficulty = as.numeric(difficulty)) %>% 
        select(service, everything())
      finish <- as_tibble("finish") %>% mutate(dimens = nrow(finish_estim))
    }
    else{finish_estim <- as_tibble(x=0);
    finish <- as_tibble("finish") %>% mutate(dimens = 0)}
    
    if("stucco" %in% prods$service){ 
      stucco_estim <- object %>%
        filter(grepl("stucc",service)) %>% 
        mutate(thickness = str_replace_all(thickness, "''",'"')) %>%
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
        janitor::adorn_totals() %>% 
        mutate(price = as.numeric(price)) %>%
        mutate(difficulty = as.numeric(difficulty)) 
      stucco <- as_tibble("stucco") %>% mutate(dimens = nrow(stucco_estim))
    }
    else{stucco_estim <- as_tibble(x=0);
    stucco <- as_tibble("stucco") %>% mutate(dimens = 0)}
    
    
    if("waterproofing" %in% prods$service){ 
      waterp_estim <- object %>% filter(grepl("waterpr",service)) %>% 
        group_by(service) %>% 
        summarise(total_hours = round(sum(hours)),.groups = "drop") %>% 
        mutate(days = ceiling(total_hours/8)) %>% 
        mutate(price = 50) %>% 
        mutate(labor = (price*days*8)) %>% 
        mutate(materials = labor*.22) %>% 
        mutate(total_price = labor+materials) %>%
        mutate(price = as.character(price)) %>% 
        janitor::adorn_totals()  %>% 
        mutate(price = as.numeric(price))
      waterproofing <- as_tibble("waterproofing") %>% mutate(dimens = nrow(waterp_estim))
    }
    else{waterp_estim <- as_tibble(x=0);
    waterproofing <- as_tibble("waterproofing") %>% mutate(dimens = 0)}
    
    
    
    if("interior_paint" %in% prods$service){ 
      interior_paint_estim <- object %>% filter(grepl("interior_p",service)) %>% 
        group_by(service,difficulty,location,type) %>% 
        summarise(total_hours = round(sum(hours)),.groups = "drop") %>% 
        mutate(days = ceiling(total_hours/8)) %>% 
        left_join(prices, by = c("service",
                                 "difficulty", 
                                 "location"= "texture", 
                                 "type" = "design")) %>% 
        select(-thickness) %>% 
        mutate(labor = (price*days*8)) %>% 
        mutate(materials = labor*.20) %>% 
        mutate(total_price = labor+materials) %>%
        mutate(price = as.character(price)) %>% 
        janitor::adorn_totals()  %>% 
        mutate(price = as.numeric(price))
      interior_paint <- as_tibble("interior_paint") %>% 
      mutate(dimens = nrow(interior_paint_estim))
    }
    else{interior_paint_estim <- as_tibble(x=0);
    interior_paint <- as_tibble("interior_paint") %>% mutate(dimens = 0)}
    
    
    
    if("exterior_paint" %in% prods$service){ 
      exterior_paint_estim <- object %>% filter(grepl("exterior_p",service)) %>% 
        group_by(service,difficulty,location,type) %>% 
        summarise(total_hours = round(sum(hours)),.groups = "drop") %>% 
        mutate(days = ceiling(total_hours/8)) %>% 
        left_join(prices, by = c("service",
                                 "difficulty", 
                                 "location"= "texture", 
                                 "type" = "design")) %>% 
        select(-thickness) %>% 
        mutate(labor = (price*days*8)) %>% 
        mutate(materials = labor*.20) %>% 
        mutate(total_price = labor+materials) %>%
        mutate(price = as.character(price)) %>% 
        janitor::adorn_totals()  %>% 
        mutate(price = as.numeric(price))
        exterior_paint <- as_tibble("exterior_paint") %>% 
        mutate(dimens = nrow(exterior_paint_estim))
    }
    else{exterior_paint_estim <- as_tibble(x=0);
    exterior_paint <- as_tibble("exterior_paint") %>% mutate(dimens = 0)}
    
    
    
    
    
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


# Testing Projects --------------------------------------------------------

# 13090 SW 248 ST
# 12 North Bridge Lane
# 1871 SW 7 ST
# 11091 NW 17TH ST (Fire Station #68)
# 14500 Biscayne ºBlvd (Tropical Smoothie Café)
# 3603 Thomas Ave Coconut Grove
# 7535 N Kendall Dr (Psycho Bunny)
# 7525 Coral Way.
# 6801 Collins Ave Apt 1116
# 3603 Thomas Ave Coconut Grove
# 90 East 5th St. Family Dollar
# 125 NE 55th St.
# 3457 N University Dr.
# 3323 NE 166th St.
# 540 W. 83rd St. Warehouse addition
# 629 Aledo Ave
# 254 Minorca Ave - Parking Garage #7
# 530 Sound Dr
# 3154 N Miami Ave - T-Mobile
# 17651 SW 272 St
# 3015 Grand Ave. (Skinlab)
# 15 Al Canta Lane (Litkin Residence)
# BLDG 704 ^0 705 Homestead Air Reserve Base

# Execution ---------------------------------------------------------------
proposals <- explorer("PROPOSALS")

estimator("6720")
