
library(tidyverse)
library(lubridate)


setwd("C:/Users/andre/Downloads")

invoice_list <- openxlsx::read.xlsx("invoice_list_010121_022221.xlsx") %>% as_tibble() %>% filter(open_balance != 0) %>% 
  mutate(pending_perce = open_balance/amount,
         paid_perce = 1-pending_perce) %>% 
  mutate(expected_payment = ifelse(amount > 6000, amount*.25,open_balance)) %>% 
  mutate(date = as.Date(date, tryFormats = c("%Y-%m-%d", "%m/%d/%Y"),optional = F),
         due_date = as.Date(due_date, tryFormats = c("%Y-%m-%d", "%m/%d/%Y"),optional = F)) %>% 
  mutate(date_interval = lubridate::interval(date, today()),
         debt_days = time_length(date_interval, "days")) %>% select(-date_interval)
# mutate(dup = duplicated(name)) %>% filter(dup == T)

openxlsx::write.xlsx(invoice_list, "invoice_list.xlsx")



# Same wth project name >>> 

list <-   openxlsx::read.xlsx("invoice_list_pj.xlsx") %>% as_tibble


invoicing <- list %>% mutate(name = str_trim(name)) %>% 
  filter(open_balance != 0) %>% 
  mutate(project_name = ifelse(name == "Nivar Group Builders, LLC:21 Caloosa New Painting", "21 Caloosa", project_name)) %>% 
  mutate(project_name = ifelse(name == "Mrs. Monique  Domovitch", "570 Coral Ln", project_name)) %>% 
  mutate(project_name = ifelse(name == "Mrs. Monique  Domovitch:570 Coral Ln.- Installations", "570 Coral Ln", project_name)) %>% 
  mutate(project_name = ifelse(name == "Mr. Pete Diaz:89070 Overseas Hwy", "89070 Overseas Hwy", project_name)) %>% 
  mutate(project_name = ifelse(name == "50 Island - Ivo Nowak", "50 Island", project_name)) %>% 
  mutate(project_name = ifelse(name == "54 Tarpon - Bacher", "54 Tarpon", project_name)) %>% 
  mutate(project_name = ifelse(name == "17 Caloosa - Carter & Hopkins", "17 Caloosa", project_name)) %>% 
  mutate(project_name = ifelse(project_name == "31 & 31 Riviera Village. Stucco", "52 Tarpon",project_name)) %>% 
  mutate(project_name = ifelse(project_name == "46 HH", "46 Harbor House",project_name)) %>% 
  

  mutate(pending_perce = open_balance/amount,
         paid_perce = 1-pending_perce) %>% 
  
  mutate_at(c("amount","open_balance"), as.double) %>% 
  
  mutate(expected_payment = ifelse(amount > 6000, amount*.25,open_balance)) %>% 
  
  mutate(date = as.Date(date, tryFormats = c("%Y-%m-%d", "%m/%d/%Y"),optional = F),
         due_date = as.Date(due_date, tryFormats = c("%Y-%m-%d", "%m/%d/%Y"),optional = F)) %>% 
  
  mutate(date_interval = lubridate::interval(date, today()),
         debt_days = time_length(date_interval, "days")) %>% select(-date_interval) %>% 
  
  rename(project = project_name) %>% 
  mutate_at(c("date","due_date"), as_date) %>% mutate_at(c("name", "project"),str_trim)



# mutate(dup = duplicated(name)) %>% filter(dup == T)

openxlsx::write.xlsx(invoicing, "invoice_list.xlsx")

# Summary 
invoicing %>% 
  group_by(project) %>% 
  summarise(expected_payment = sum(expected_payment))
  
  
  
  
  
  
  
