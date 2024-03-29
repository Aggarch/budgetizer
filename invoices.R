
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





# State of account ----------------------------------------------------

transact %>% 
  filter(name %in% named) %>% 
  filter(amount >0, !grepl("Accounts R",account)) %>%
  group_by(name,type) %>% 
  summarise(amount = sum(amount))



# transact <- cleaner("transactions")
# invoices   <- cleaner("invo") 

state_of_account <- function(clue){ 

openb <- invoices %>% 
  filter(grepl(clue,name)) %>% 
  select(-memo) %>% 
  mutate(open_balance = as.double(open_balance)) %>% 
  select(-creation_date, - modification_date,-created_by)
  


detailed <- transact %>%
  filter(type == "Invoice") %>%
  filter(grepl(clue,name)) %>% 
  filter(!is.na(account)) %>% 
  filter(!grepl("Receiv",account)) %>% 
  separate(name, c("client","project"),sep = "([:])") %>% 
  separate(account, c("income","service"),sep = "([:])") %>% 
  select(-memo,-class, -customer, -balance,-split) %>%
  mutate(income = str_replace(income, ".","")) %>% 
  mutate(income = case_when(str_detect(income,"Operat")~"operational",
                            str_detect(income,"aterial")~"material")) %>% 
  mutate(service = str_to_lower(service)) %>% 
  mutate(service = str_replace(service, "income","")) %>% 
  mutate(service = str_trim(service)) %>% 
  select(-creation_date, - modification_date,-created_by)



compacted <- detailed %>% 
  left_join(openb %>% 
              select(num,open_balance), by="num") %>% 
  mutate(service = ifelse(service =="materials",
                          lag(service),service)) %>% 
  mutate(service = ifelse(service =="materials",
                          lag(service),service)) 


compacted_grouped <- compacted %>%
  group_by(project,num) %>% 
  summarise(amount = sum(amount),
            open_balance = last(open_balance),
            .groups = "drop") %>%
  filter(!grepl("142",project)) %>% 
  janitor::adorn_totals()


return(list(openb = openb,
       detailed = detailed,
       compacted = compacted,
       compacted_grouped = compacted_grouped
       ))

}

# Exclude the 142 seminole & reorganize ::: 

detail <- cbt_compacted %>% filter(!grepl("142",project)) %>%
  mutate(service = ifelse(service == "home maintenance","installations",service)) %>%
  mutate(service = ifelse(service == "drywall installation","drywall & finish",service)) %>% 
  select(-open_balance) %>% 
  janitor::adorn_totals()




# CBT Already charged by service 

cbt_charged <- cbt_compacted %>% 
  group_by(project,num,service) %>% 
  summarise(amount = sum(amount),
            .groups = "drop") %>% 
  janitor::adorn_totals()





transactions_0512
# current transactions not yesterday:
transactions %>% anti_join(transactions_0512, by = c("date","name","amount"))



# Note --------------------------------------------------------------------

# new process improved to download data with timestamp. 

transactions %>% filter(num %in% c( "S00840", 
                                    "S00841", 
                                    "S00842", 
                                    "S00843", 
                                    "S00844", 
                                    "S00845", 
                                    "S00846" )) %>%
  filter(!grepl("Recei",account)) %>% 
  replace_na(list(amount = 0)) %>% 
  select(-create_date,-memo,-balance,-name) %>%
  filter(!is.na(account)) %>% 
  janitor::adorn_totals()


cbt <- transactions %>% filter(type %in% c("Invoice", "Credit Memo")) %>% 
  filter(grepl("CBT",name), grepl("Account",account)) %>% 
  select(-memo,-split,-class,-created_by,-modification_date,-name,-balance) %>% 
  left_join(invoices %>% select(num,open_balance), by=c("num")) %>% 
  mutate(open_balance = ifelse(is.na(open_balance),amount,open_balance)) %>% 
  mutate(open_balance = as.double(open_balance)) %>% 
  separate(customer, c("client","project"),sep = "([:])") %>% 
  select(-client) %>% arrange(project) %>% 
  relocate(.after = project,open_balance)

