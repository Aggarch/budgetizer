
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
  
  
  
  
setwd("C:/Users/andre/Downloads")

interest <- c("Rmodl|52 T|162 Sem|27 FLAM|Marina|421 South|11000 SW|53 N|89070")

  
inv_qb <- openxlsx::read.xlsx("invoices_qb.xlsx") %>% as_tibble() %>% 
  mutate(date = as.Date(date, origin = "1899-12-30")) %>% 
  select(-memo, -due_date, -type) %>% 
  filter(grepl(interest, name)) %>% 
  separate(name, c("client","project"),sep = "([:])")

zinterest <- c("Remodel|52 T|162 Sem|27 FLAM|Marina|421 S|11000 SW|53 N")


inv_zoho <- openxlsx::read.xlsx("invoices_zoho.xlsx") %>% as_tibble() %>% 
  rename(date = Invoice.Date) %>% 
  mutate(date = as.Date(date, origin = "1899-12-30")) %>% 
  rename(num = Reference) %>% mutate(num = str_trim(num))

inv_zoho <- inv_zoho %>%  filter(grepl(zinterest, Project))
  

# Missing Invoices::: 


missing_invoices <-  inv_qb %>% anti_join(inv_zoho, by = c("date","num"))

inv_zoho%>% anti_join(inv_qb, by = c("date","num"))


st <- openxlsx::read.xlsx("subct_test.xlsx") %>% as_tibble

test <- st %>% filter(Project %in% open_pj$Project.Name) %>%
  mutate(Subcontract.Name = str_trim(Subcontract.Name)) %>% 
  group_by(Project, Subcontract.Name) %>%
  summarise(n = n(),.groups = "drop")



# Invoicing 
invoices <-  openxlsx::read.xlsx("invoices_qb.xlsx") %>% as_tibble() %>% 
  mutate(date = as.Date(date, origin = "1899-01-01")) %>% 
  mutate(open_balance = as.double(open_balance)) %>% 
  separate(name, c("client","project"),sep = "([-])") %>% 
  mutate(project = ifelse(is.na(project),client, project)) %>% 
  mutate(client = str_trim(client),
         project = str_trim(project)) %>% 
  distinct()


open_pj_invo <- transactions %>% 
  #filter(grepl("CBT", name)) %>%
  select(-memo, -account, -split, -class) %>% 
  arrange(desc(date)) %>% filter(type == "Invoice") %>% 
  select(-name) %>% 
  separate(customer, c("client","project"),sep = "([:])") %>% 
  filter(grepl(interest, project)) %>% 
  select(-amount, -balance, -type) %>% 
  distinct() %>% 
  left_join(invoices %>% 
              select(num, amount, open_balance), by = "num") %>% 
  filter(!is.na(project))


# Invoices 

# Invoices from transactions ----------------------------------------------

# filter by projects of interest,
# able to separate labor from materiasl. 

library(tidyverse)
library(openxlsx)

setwd("C:/Users/andre/Downloads")


interest <- c("Rmodl|52 T|162 Sem|27 FLAM|Marina|421 South|11000 SW|53 N|89070")

invoices_detailed <- transactions %>% 
  filter(type == "Invoice", grepl(".Operational|Material", account)) %>%
  filter(grepl(interest, customer)) %>% 
  separate(account, c("source","class"),sep = "([:])") %>% 
  separate(customer, c("client","project"),sep = "([:])") %>% 
  select(date,client,project,class,num,amount)

#inv_qb <- cleaner("invoices_qb")

invoices <-  openxlsx::read.xlsx("invoices.xlsx") %>% as_tibble() %>% 
  mutate(date = as.Date(date, origin = "1899-01-01")) %>% 
  mutate(open_balance = as.double(open_balance)) %>% 
  separate(name, c("client","project"),sep = "([-])") %>% 
  mutate(project = ifelse(is.na(project),client, project)) %>% 
  mutate(client = str_trim(client),
         project = str_trim(project)) %>% 
  distinct()


invo_detail <- invoices_detailed %>% 
  left_join(invoices %>% select(num, open_balance), by = "num") %>% 
  distinct()




# Account Statement F(x) --------------------------------------------------
account_statement <- function(client){
  
  setwd("C:/Users/andre/Downloads")
  dir.create("account_statement")
  setwd("C:/Users/andre/Downloads/account_statement")
  
  
  wb <- createWorkbook()
  addWorksheet(wb,"account_detailed")
  addWorksheet(wb,"consolidated")


detailed <- invo_detail %>% filter(grepl("CBT", client)) %>% arrange(project) %>%
  mutate(project = case_when(str_detect(project,"162 S")~"162 Seminole",
                             str_detect(project,"27 F")~"27 Flamingo",
                             str_detect(project,"52 T")~"52 Tarpon")) %>% 
  filter(open_balance>1) %>% 
  mutate(class = case_when(class == "Materials Income"~"Materials",
                           class == "Home Maintenance"~"Maintenance",
                           TRUE ~ as.character(class))) %>% 
  arrange(project)


consolidated <- detailed %>% 
  group_by(date,client,project,num) %>% 
  summarise(amount  = sum(amount), 
            open_balance = max(open_balance),
            class = str_flatten(class,","),
            .groups = "drop") %>% 
  relocate(.before = num, class) %>% 
  arrange(project)



  writeData(wb, "account_detailed", detailed)
  writeData(wb, "consolidated", consolidated)
  
  
  
  openxlsx::saveWorkbook(wb, 
                         file = paste0(client,"_account_statement.xlsx"),
                         overwrite = TRUE)
  
  return(list(detailed,consolidated))


}



# Homogenize the project name to successfully join it with subct data          D
# Manually create a tibble with the name of the projects of interest,          D
# create a column with the invo_detail project name, zoho name and the         D
# desirable name, include the quote line of service by project.                D
# smartly join data sources using homogenized project name as a key.           P
# once structure is completed, join together;

# Projects:: subct:: invoices:: expenses:: ::Labor & Materials.


open_tibble <- openxlsx::read.xlsx("open_tibble.xlsx") %>% as_tibble()



check <- openxlsx::read.xlsx("checking.xlsx") %>% as_tibble() %>% 
  filter(VALOR > 0) %>% filter(grepl("INTERNAC",DESCRIPCIÓN)) %>% 
  mutate(FECHA = as.Date(FECHA, origin = "1899-01-01")) %>% 
  select(-DOCUMENTO,-OFICINA,-REFERENCIA)


