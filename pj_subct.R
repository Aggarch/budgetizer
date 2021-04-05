

# Business Analitycs ------------------------------------------------------
# Update cycle, weekly, Friday  



# All data here available, come from Zoho app creator, data was verified with 
# quickbooks transactions records and other reports preexistent in OneDrive.


# Does subcontracts really absorb the materials costs?
# if subcts do not, why are they so high?


# libs
library(tidyverse)
library(lubridate)
library(readxl)
library(xlsx)


# config desk 
setwd("C:/Users/andre/Downloads")

# 421 South Harbor Dr. Subcontracts still pending in zoho data
# insufficient information available in onedrive. 


# Data Preparation  -------------------------------------------------------

# open projects and contracts amounts 
open_pj <- openxlsx::read.xlsx("pjs.xlsx") %>% as_tibble() %>% 
  mutate(start_date = as.Date(start_date, origin = "1899-12-30")) %>%
  select(-site_visit) %>% distinct()


# all subcontracts 
subct <- openxlsx::read.xlsx("subct.xlsx") %>% as_tibble() %>% 
  mutate(subct_date = as.Date(subct_date, origin = "1899-12-30")) %>% 
  select(-payout, -retainage,-check) %>% distinct()


# Material Purchases 
materials <- openxlsx::read.xlsx("material_purchases.xlsx") %>% as_tibble() %>% 
  mutate(trans_date = as.Date(date,tryFormats = c("%Y-%m-%d", "%m/%d/%Y"),optional = F)) %>% 
  distinct() %>% select(-date) %>% mutate(project = ifelse(is.na(project),name,project)) %>% 
  filter(service != "Maintenance")


# all payments in Zoho 
payments <- openxlsx::read.xlsx("paymnts.xlsx") %>% as_tibble() %>% 
  mutate(payment_date = as.Date(payment_date, origin = "1899-12-30")) %>% 
  mutate(resource = str_trim(resource))


# all Invoices 
invoices <- openxlsx::read.xlsx("invoices.xlsx") %>% as_tibble() %>%
  # separate(name, c("name", "project"), ":") %>% 
  mutate(project = str_trim(project), name = str_trim(name)) %>% 
  mutate(inv_due_date = as.Date(due_date,
                                tryFormats = c("%Y-%m-%d", "%m/%d/%Y"),optional = F)) %>% 
  select(-date, - due_date) %>% arrange(project) %>% 
  mutate(project = ifelse(project == "53 Seagate Blvd", "53 Seagate Blvd Exterior Paint", project)) %>% 
  mutate(project = ifelse(project == "421 South Harbor Drive. Additional Work", "421 South Harbor Dr.", project)) %>% 
  mutate(project = ifelse(project == "421 South Harbor Drive. Additional Work", "421 South Harbor Dr.", project)) %>% 
  mutate(project = ifelse(project == "81906 Overseas Hw #8", "81906 Overseas Hwy. Touch Ups", project)) %>% 
  mutate(project = ifelse(project == "81906 Overseas Hw #8 Keys Const", "81906 Overseas Hwy. Touch Ups", project)) %>% 
  mutate(project = ifelse(project == "162 Seminole","162 Seminole Blvd. All Services", project)) %>% 
  mutate(project = ifelse(project == "31 & 31 Riviera Village. Stucco","52 Tarpon Ave", project)) %>% 
  mutate(project = ifelse(project == "421 South Harbor Drive","421 South Harbor Dr.", project)) %>% 
  mutate(project = ifelse(project == "162 Seminole Blvd.","162 Seminole Blvd. All Services", project)) %>% 
  mutate(project = ifelse(project == "30 Angelfish Cay Dr. Job 1","30 Angelfish Cay Dr.", project)) %>% 
  mutate(project = ifelse(project == "30 Angelfish Cay Dr. Job 2","30 Angelfish Cay Dr.", project)) %>% 
  mutate(project = ifelse(project == "52 Tarpon (31 & 32 Riviera Village. Stucco Int Ext Paint)","52 Tarpon Ave", project)) %>% 
  mutate(project = ifelse(project == "27 Flamingo","27 FLAMINGO HAMMOCK ROAD. Paint, Stucco, Framing and Drywall", project)) %>% 
  mutate(project = ifelse(is.na(project),name,project))


# Account status ::: 

titan <- invoices %>% filter(grepl("Titan",name))





pj_invoices <- invoices %>%
  filter(project %in% open_pj$project) %>% 
  select(-name) %>% left_join(open_pj, by = "project") %>% 
  relocate(.before = num, c(start_date,project,main_services,full_contract_amount)) %>% 
  rename(invoice_num = num) %>% arrange(project)


abstract <- pj_invoices %>% 
  group_by(project) %>% 
  summarise(start_date = max(start_date),
            contract = max(full_contract_amount), 
            invoice_amount = sum(amount),
            open_balance = sum(open_balance),
            inv_due_date = max(inv_due_date))



#openxlsx::write.xlsx(pj_invoices, "projects_invoices_status.xlsx")



# wrangling 
  data <- payments %>% 
    mutate(project = ifelse(project == "30 Angelfish Cay Dr. Job 1",
                            "30 Angelfish Cay Dr.", project)) %>% 
    
    left_join(subct %>%  select(subct,project,service, 
                                        contract, subct_date), 
              by = c("subct","project")) %>% 
  filter(trans_type != "Internal Member") %>% 
    select(-retainage, -trans_type) %>% 
  mutate(resource = str_trim(resource)) %>% 
  mutate(resource = ifelse(is.na(resource) & 
                             service == "Framing", "Victor Included",resource)) %>% 
  filter(!grepl("Maintenance",project)) %>%
  mutate(status = ifelse(project %in% open_pj$project, "active" , "close")) %>% 
  rename(subcontract = contract)
    



# status distribution
  data %>% count(status)

    
# Active projects with active subcontracts payments  ----------------------

  open_projects <- data %>% filter(status == "active") %>%
  left_join(open_pj, by = "project") %>% 
  mutate(date_interval = lubridate::interval(start_date, payment_date),
         days_to_pay = time_length(date_interval, "days")) %>%
  select(-date_interval) %>% 
  distinct() %>%
  mutate(test = duplicated(.)) %>% 
  filter(test == F) %>% 
  relocate(.before = payment_date, start_date) %>% 
  relocate(.before = subct, days_to_pay) %>% 
  select(-test,-status) %>%
  arrange(project)%>% 
  relocate(.after = full_contract_amount, c(payment_date, days_to_pay)) %>% 
  relocate(.before = subct, c(project, main_services, full_contract_amount)) %>% 
  relocate(.after = subct, service)


  open_projects_grouped <- open_projects %>% group_by(project,service,subct,resource) %>% 
  summarise(full_contract_amount = max(full_contract_amount), 
            check = sum(check), subcontract = max(subcontract),.groups = "drop") %>% 
  relocate(.before = service, full_contract_amount) %>% 
  relocate(.after = subct, subcontract)
  
  
  
  
  # Invoices recursive
  projects_detailed <- open_projects %>% 
    left_join(invoices %>% 
                select(-name), by = "project") %>% 
    mutate(date_interval = lubridate::interval(inv_due_date, today()),
           delate_invoiced = time_length(date_interval, "days")) %>%
    select(-date_interval) %>% 
    select(-main_services) %>%
    relocate(.before = subct,c(service, subct_date)) %>% 
    relocate(.after = subct, subcontract) %>% 
    rename(invoice_num = num,
           invoice_amount = amount, 
           subct_amount = subcontract)
    
  # filter(!is.na(open_balance))
  
  
  
  # Invoices recursive, group detailed invoices to observe overview:::
  
  
  
# openxlsx::write.xlsx(projects_detailed,"projects_detailed.xlsx")
# openxlsx::write.xlsx(open_projects_grouped,"open_projects_grouped,xlsx")
# openxlsx::write.xlsx(open_projects_grouped,"open_pj.xlsx")



# Subct analisys by project -----------------------------------------------

# filtered by open projects in open_pj$projects ||| open projects module in ZOHO.

subct_contract <- subct %>% left_join(open_pj, by = "project") %>% 
  filter(!grepl("Maintenance", project)) %>% select(-start_date,-main_services)%>%
  rename(subcontract = contract) %>% 
  mutate(ratio_subct_contract = subcontract/full_contract_amount) %>% 
  mutate(perce_pending_subct = pending_amount/subcontract) %>% 
  filter(project %in% open_pj$project) %>% 
  relocate(.before = service, subct) %>% 
  arrange(project)


#openxlsx::write.xlsx(subct_contract, "subct_contract_analysis.xlsx")



# Seminole Case -----------------------------------------------------------
seminole_case <- subct_contract %>% filter(grepl("162 Sem",project))
seminole_rohos_profit <- (46462-sum(seminole_case$ratio_subct_contract)*46462)/46462

pending_data_pj <- subct_contract %>% 
  filter(subcontract == 1) %>% select(subct, project,service)


invoices_listed <- openxlsx::read.xlsx("invoices.xlsx") %>% as_tibble() %>%
  mutate(project = str_trim(project), name = str_trim(name)) %>% 
  mutate(inv_due_date = as.Date(due_date,
                                tryFormats = c("%Y-%m-%d", "%m/%d/%Y"),optional = F)) %>% 
  select(-date, - due_date) %>% arrange(project) %>% 
  mutate(project = ifelse(is.na(project), name, project)) %>% 
  filter(open_balance != 0)


# Group by project and manage summarized values ::: 
# segregate open project and already closed projects 



# Aclaration  ------------------------------------------------------------------------------------
#                                                                                                 #
# NOTES > the reason why some subcontracts do not appear in consolidated date is                  #
#         because there is no payment associated to it, example is 27 FLAMINGO.                   #
#                                                                                                 #
#                                                                                                 #
# subct >>> All existing subcontracts available in Zoho exist in this query.                      #
#                                                                                                 #
# open_projects>>> Only those subcontracts that already receive a payment exist in this query.    #
#                                                                                                 #
###################################################################################################



# Summary of projects flow >>> ZOHO >>> 

# Open Projects with subcontract in zoho 
pj_subct <- subct_contract %>% distinct(project)

# Open Projects with paymnts applied to zoho subct 
pj_subct_paymnts <- open_projects %>% distinct(project)

# Open Projects with open subct and without paymnts applied
open_nopaid <- pj_subct %>% anti_join(pj_subct_paymnts, by = "project")

# Invoices of closed projects
close_project <- invoices %>% filter(!project %in% open_pj$project)
close_project %>% filter(open_balance !=0) -> x 
sum(x$open_balance)

# Open Projects with no Invoices. 
project_na_invoice <- open_pj %>% anti_join(invoices, by="project")

#sum(z$amount)

# Open projects with Invoice. 
project_with_invo <- open_pj %>% anti_join(project_na_invoice, by = "project")


# Open projects with open balance Invoices above zero.  
project_with_invo %>% left_join(invoices, by = "project") %>%
  filter(open_balance != 0 ) #


# invoices of Open Projects 
open_inv <- invoices %>% filter(project %in% open_pj$project) %>% 
  
  left_join()
  
  arrange(project) %>% 
  group_by(project) %>% 
  
  summarise(start_date = max(start_date),
            full_contract_amount = max(full_contract_amount),
            invoice_amount = sum(amount),
            open_balance = sum(open_balance),
            inv_due_date = max(inv_due_date)) %>% 
  mutate(pending_to_invoice = full_contract_amount-invoice_amount+open_balance)


# Main goal of Projectlytics structure:::

# project, service, labor_estimate, materials_estimate, proposal, CO, subct, invoiced, material_cost

# advances ::  project,service,proposal,subct,invoiced

# pending :::  labor_estimate, materials_estimate,CO,material_cost
  

# Functions ---------------------------------------------------------------

# Sum off all payments of a project
  
paid <- function(clue){
  
  
  record <- payments %>% filter(grepl(clue,project)) %>% 
    left_join(subct %>% select(-project), by = "subct") %>%
    group_by(project,subct, service, resource) %>%
    summarise(check = sum(check), contract = max(contract),
              .groups = "drop")
  
  return(record)
  
}

  
# Invoices by Project 

invoicing <- function(clue){
  
  setwd("C:/Users/andre/Downloads")
  invoices <- openxlsx::read.xlsx("invoices.xlsx") %>% 
    as_tibble()
  
  data <- invoices %>% filter(grepl(clue, project))
  
  return(data)
  
}
  


transdate <- openxlsx::read.xlsx("transdate.xlsx") %>% as_tibble()


romeo <- transdate %>% 
  distinct() %>%
  filter(grepl("Romeo",name)) %>% 
  mutate(date = as.Date(date,
                        tryFormats = c("%Y-%m-%d", "%m/%d/%Y"),optional = F)) %>% 
  mutate(name = ifelse(name == "Romeo Garcia", "Romeo Garcia.",name))
  

# openxlsx::write.xlsx(romeo_transf, "romeo_payments.xlsx")
  

romeo_transf <- romeo %>% 
  mutate(split = "Cost of Goods Sold:Direct Labor (DL):Subcontractor Labor:Romeo Garcia")
  
  #mutate(date = as.Date(date, origin = "1899-12-30"))
  
  


adp_romeo <- openxlsx::read.xlsx("nested_adp_romeo.xlsx") %>% as_tibble()

romeo_adp <- adp_romeo %>% mutate(date = str_trim(date), amount = str_trim(amount)) %>%
  mutate(amount = as.double(amount)) %>% 
  mutate(date = as.Date(date,
                        tryFormats = c("%Y-%m-%d", "%m/%d/%Y"),optional = F))


#openxlsx::write.xlsx(romeo_adp, "romeo_adp.xlsx")

romeo_payments <- openxlsx::read.xlsx("romeo_payments.xlsx") %>% 
  mutate(date = as.Date(date, origin = "1899-12-30")) %>% as_tibble()
  
  
consolid_check <- openxlsx::read.xlsx("consolid_romeo_checks.xlsx") %>% as_tibble() %>% 
  mutate(date = as.Date(date,
                        tryFormats = c("%Y-%m-%d", "%m/%d/%Y"),optional = F))


consolid_adp <- openxlsx::read.xlsx("romeo_consolid_adp.xlsx") %>% as_tibble() %>% 
  mutate(datee = substr(date,0,10)) %>% 
  mutate(dateee=as.Date(datee,  tryFormats = c("%Y-%m-%d", "%m/%d/%Y"),optional = F)) %>% 
  select(-date,-datee) %>% relocate(.before = amount, dateee) %>% rename(date = dateee) %>% 
  mutate(amount = (gsub(",","",amount))) %>% mutate(amount = as.double(amount))
  

consolid <- consolid_adp %>% 
            bind_rows(consolid_check)%>%
            mutate(date = format(as.Date(date), "%m/%d/%Y")) %>% 
            arrange(date) %>% 
            mutate(check_id = ifelse(is.na(check_id), "prior", check_id))



# consolid %>% filter(source == "adp") %>% 
# openxlsx::write.xlsx(., "romeo_consolid_adp.xlsx")
# 
# consolid %>% filter(source == "qb_check") %>% 
#   openxlsx::write.xlsx(., "romeo_consolid_check.xlsx")
# 
# consolid %>% openxlsx::write.xlsx(., "romeo_consolid.xlsx")


# Example to see what's in quickbooks that is not in ZOHO creator app.

caloosa_zoho <- openxlsx::read.xlsx("caloosa21_zoho.xlsx") %>% as_tibble()%>% 
  mutate(date = as.Date(date, origin = "1899-12-30")) %>% 
  mutate(date=  as.Date(date,  tryFormats = c("%m/%d/%Y"),optional = F)) 


caloosa_qb <- openxlsx::read.xlsx("caloosa21_qb.xlsx") %>% as_tibble() %>% 
 mutate(date = as.Date(date, tryFormats = c("%m/%d/%Y"),optimal = F))


caloosa_qb %>% anti_join(caloosa_zoho, by = c("date", "amount"))-> camila


# Zoho Transactions::: 
tzoho <- openxlsx::read.xlsx("tzoho.xlsx") %>% as_tibble() %>%
  mutate_all(., str_trim) %>% mutate(amount = as.double(amount)) %>% 
  mutate(date = as.double(date)) %>% 
  mutate(date = as.Date(date, origin = "1899-12-30")) %>% 
  mutate(date = as.Date(date, tryFormats = c("%m/%d/%Y"),optional = F ))

# Business Model QA -------------------------------------------------------
#  
# 1) ¿Do subct really absorb the implicit material cost of each project? 
# 2) ¿What should be the full_contract_amount, subct ratio? 
# 3) ¿Whats the avg of the ratio in all open or active projects and services? 
# 4) ¿How can we collect available data related to material purchases? 




# Transaction History -----------------------------------------------------
# Protocol for transaction record downloading:
# Enter quickbooks, Transaction Report, add class & customer.
# Ungroup the report by account & filter out old dates. (Last Year to date)

browseURL("https://app.qbo.intuit.com/app/reportv2?token=GENERIC_QZREPORT&show_logo=false&date_macro=lastyeartodate&low_date=01/01/2020&high_date=03/23/2020&cash_basis=no&customized=yes&groupby=none&parenttoken=PANDL_BYCUST&columns=~date%3ATxDate%2C~txn_type_label%3ATxTypeID%2C~doc_num%3ATxHeader%2FDocNum%2C~name_label%3ATxHeader%2FNameID%2C~memo_desc_label%3AMemoText%2C~account_id_label%3AAccountID%2C~split_label%3AOtherAccountID%2C-~amount_label%3ANaturalAmount%2C%3D~balance_label%3ANaturalAmount%2C~cs_customer_label%3ACustomerID%2C~ls_class_col_header%3AKlassID&collapsed_rows=&edited_sections=false&divideby1000=false&hidecents=false&negativenums=1&negativered=false&show_header_title=true&show_header_range=true&show_footer_custom_message=true&show_footer_date=true&show_footer_time=true&show_footer_basis=true&header_alignment=Center&footer_alignment=Center&show_header_company=true&company_name=ROHO%27S%20GROUP%20CORP&title=Transaction%20Report&footer_custom_message=")

setwd("C:/Users/andre/Downloads")


# Function cost extraction from all transactions record: 

# xlsx files::: follow transaction history instructions to download.


# Libraries PCKGS  --------------------------------------------------------

library(tidyverse)
library(lubridate)
library(openxlsx)
library(janitor)




#0) Synthesizer: QB Projects Dynamic Reporting  ---------------------------------

# 8 Steps to create each project report from all transactions source. 

#1) Clean transactions data  ------------------------------------------------
# Import & Clean Data ::: 

cleaner <- function(file){
  
file_name <-   paste0(file,".xlsx")
setwd("C:/Users/andre/Downloads")

data <- openxlsx::read.xlsx(file_name) %>% as_tibble()
data <- data[-1:-2,-1]
names(data) <- as.character(data[1,])
tibble <- data %>% janitor::clean_names()

transact_tibble <- tibble[-1,] %>% filter(date != is.na(date))%>% 
  mutate_all(., str_trim) %>% mutate(amount = as.double(amount)) %>% 
  mutate(date = as.Date(date, tryFormats = c("%m/%d/%Y"),optional = F )) %>% 
  rename(type = transaction_type,memo = memo_description)


openxlsx::write.xlsx(transact_tibble,file_name)

return(transact_tibble)


}

#transactions <- cleaner("transactions")

#Out of Algorithm: 
setwd("C:/Users/andre/Downloads")
transactions <- openxlsx::read.xlsx("transactions.xlsx") %>%
  as_tibble() %>%  
  mutate(date = as.Date(date, origin = "1899-12-30")) %>% 
  mutate(date = as.Date(date, tryFormats = c("%m/%d/%Y"),optional=F))

  

transactions %>% group_by(type) %>% 
  summarise(n = n()) %>% arrange(desc(n)) %>% 
  janitor::adorn_totals()




#2) Project Seeker ----------------------------------------------------------


# EX
expenses <- function(project){
  
  pj_data <- transactions %>% 
    filter(grepl(project,customer)) %>% 
    filter(grepl("Indirect|Direct Labor|Direct Material|Fees|Services",account))
  
  return(pj_data)
  
}

grouped_expenses <- function(project){ 
  
project_expenses <- expenses(project) %>% 
  group_by(account) %>% 
  summarise(amount= sum(amount)) %>% 
  separate(account, c("main","second","third"),sep = "([.?:])") %>% 
  mutate(second = ifelse(main == "Cost of Goods Sold", second, main)) %>% 
  mutate(main = case_when(str_detect(second,"Direct")~"Cost",
                          str_detect(second,"Indirect")~"Expense"))

return(project_expenses)

}

expenses_level <- function(project){ 
  
project <- expenses(project) %>% 
  group_by(date,account,class) %>% 
  summarise(amount= sum(amount), .groups = "drop") %>% 
  separate(account, c("main","second","third"),sep = "([.?:])") %>% 
  select(-third)
}


# IN 
incomes <- function(project){
  
  data_pj <- transactions %>% 
    filter(grepl(project, customer)) %>% 
    filter(type == "Invoice") %>% 
    filter(grepl(".Operational|Material",account))
  
  return(data_pj)
  
}

grouped_incomes <- function(project){ 
  
  project_incomes <- incomes(project) %>% 
    group_by(account,num) %>% 
    summarise(amount= sum(amount), .groups = "drop") %>% 
    separate(account, c("main","second"),sep = "([:])")
 
  
  return(project_incomes)
  
}

incomes_level <- function(project){
  
  incomes(project) %>% 
  group_by(date,account,num) %>% 
  summarise(amount= sum(amount), .groups = "drop") %>% 
  separate(account, c("main","second"),sep = "([:])") %>% 
  rename(class = second,
         second = num) %>% 
    relocate(.after = main,second)
}



# INFO
vendor_client <-  function(vendor, project){  
  transactions %>% 
    filter(grepl(vendor,name)) %>% 
    arrange(desc(date)) %>% 
    filter(grepl(project,customer)) 
}



# Contrast payments between QB & ZOHO 

# NOTES:: 
# interest <- c("Rmodl|52 T|162 Sem|27 FLAM|Marina|421 S")
# Execute this function by project of interest 
# function output is a list containing: 
# transactions in QB that are not in ZOHO and viceversa,

# Ubung:::

# tarpon <- contraster("52 T")
# less <- tarpon[[5]] %>% select(amount) %>% pull
# more <- tarpon[[3]] %>% select(amount) %>% pull
# sum(zoho$amount) - sum(less) + sum(more)


contraster <- function(pj){ 

  
  qb   <- expenses(pj) %>% select(date,amount)
  
  zoho <- openxlsx::read.xlsx("zoho_pjs.xlsx") %>% 
    as_tibble %>%
    rename(date = Payment.Date, type = Transaction.Type) %>% 
    mutate(date = as.Date(date, origin = "1899-12-30")) %>% 
    mutate(date = as.Date(date, tryFormats = c("%m/%d/%Y"),optional=F)) %>% 
    filter(grepl(pj,Project)) %>% 
    select(date, amount = Amount.Paid)
  
  
  diff1 <- qb %>% anti_join(zoho, by = c("date","amount"))
  d1 <- print("in QB not in Zoho")
  diff2 <- zoho %>% anti_join(qb, by = c("date","amount"))
  d2 <- print("in Zoho not in Qb")
  
  d3 <- print("instruction: delete all in Zoho that's not in QB, add all in Zoho that's in QB but not at ZH")
  
  return(list(qb,zoho, diff1,d1,diff2,d2,d3))

}


#3) Rudiment calls -------------------------------------------------------------------------


# Expenses queried from accounts DL,DM,IL, fees & services:
expenses("Rmodl")

# Expenses grouped by account and summarized
grouped_expenses("Rmodl")%>% 
  janitor::adorn_totals()

# Incomes queried from Invoices:
incomes("Rmodl")

# Expenses grouped by account and summarized
grouped_incomes("Rmodl")%>% 
  janitor::adorn_totals()




#4) Project wrangling  -------------------------------------------------------

# The most relevant projects by number of transactions & volume :::
# Explore in charts of accounts & find a way to classify inflows and outflows 

pjs <- transactions %>% 
  filter(!is.na(customer)) %>% 
  filter(date >= today() %m-% months(1)) %>%
  group_by(customer) %>%
  mutate(amount =  ifelse(is.na(amount),0,amount)) %>% 
  summarise(n = n(), volume = sum(amount)) %>% 
  arrange(desc(n)) %>% 
  separate(customer, c("customer","project"),sep = "([:])") %>% 
  mutate(project = ifelse(is.na(project),customer,project)) %>% 
  mutate(file = substr(project,0,9)) %>%
  mutate(mnemo = substr(project,0,9)) %>%
  mutate(mnemo = str_trim(mnemo)) %>% 
  mutate(file = str_to_lower(file)) %>% 
  mutate(file = paste0(str_replace(file, " ", "_"),".xlsx")) %>% 
  rename(project.name = mnemo, file.name = file) %>% 
  relocate(.before = n, project.name) %>% 
  mutate(file.name = str_replace(file.name, " ", ""))
  


# discard repeat names bcause of projects ::: 
pjs %>% group_by(file.name) %>% summarise(n = n()) %>% arrange(desc(n))

# # FIX the naming and specify :::
# pjs <- pjs %>% 
#   mutate(file.name = ifelse(str_detect(project,"Pool"), paste0("pool_",file.name),file.name),
#          file.name = ifelse(str_detect(project, "Repair"), paste0("repair_",file.name),file.name),
#          file.name = ifelse(str_detect(project, "Garage"), paste0("garage_",file.name),file.name),
#          file.name = ifelse(str_detect(project, "Remodel"), paste0("remodel_",file.name),file.name),
#          file.name = ifelse(str_detect(project, "#8"), paste0("8_",file.name),file.name),
#          file.name = ifelse(str_detect(project, "#2"), paste0("2_",file.name),file.name)
#          )


# select the inputs for the function iteration  
clues <- pjs %>%
  select(project.name,file.name)%>%
  mutate(file.name = str_replace(file.name, " ", ""))




#5) Project abstraction -----------------------------------------------------
# ABST ::::


projects_abstract <- function(project.name, file.name){ 

  
setwd("C:/Users/andre/Downloads")
dir.create("projects_abstract")
setwd("C:/Users/andre/Downloads/projects_abstract")


wb <- createWorkbook()
addWorksheet(wb,"transactions_detail")
addWorksheet(wb,"incomes")
addWorksheet(wb,"costs_&_expenses")



detail   <- expenses(project.name) %>% mutate(direction = "OUT") %>%
   bind_rows(incomes(project.name) %>% mutate(direction = "IN"))%>% 
   mutate(amount = ifelse(direction == "OUT", amount*-1,amount)) %>% 
   # janitor::adorn_totals() %>% 
   mutate(project = project.name) %>% 
   relocate(.before = type, project)


  pj_incomes  <- grouped_incomes(project.name)
  # janitor::adorn_totals()

  pj_expenses <- grouped_expenses(project.name)
  # janitor::adorn_totals()



writeData(wb, "transactions_detail", detail)
writeData(wb, "incomes", pj_incomes)
writeData(wb, "costs_&_expenses", pj_expenses)


openxlsx::saveWorkbook(wb, file = file.name, overwrite = TRUE)

return(list(detail,pj_incomes,pj_expenses))

}



#6) Execution   ------------------------------------------------------
data <- pjs %>% mutate(details = pmap(clues, projects_abstract))



#7) Diving - Summary --------------------------------------------------------
status <- data  %>%  unnest(cols = (details)) %>% 
  group_by(project.name) %>%
  slice(1) %>% ungroup() %>% 
  group_by(project.name) %>% 
  unnest() %>% ungroup() %>% 
     group_by(file.name) %>% 
     summarise(amount = sum(amount)) %>% 
     rename(status = amount)

data_nested <- data %>% left_join(status, by = "file.name") %>% 
  select(-n) %>% relocate(.after = volume, status)



#8) Merging all folder files ------------------------------------------------
filenames_list <- list.files()

all_data <- lapply(filenames_list,function(filename){
  print(paste("Merging",filename,sep = " "))
  read.xlsx(filename)
})


consolid_df <- do.call(rbind.data.frame, all_data) %>% 
  as_tibble() %>% 
  mutate(date = as.Date(date, origin = "1899-12-30")) %>%
  mutate(date = as.Date(date, tryFormats = c("%m/%d/%Y"),optional = F ))











#9) Identify POI ::: (projects of interest) -------------------------------


pj_recent <- openxlsx::read.xlsx("transactions.xlsx") %>% as_tibble()%>%
  mutate(date = as.Date(date, origin = "1899-12-30")) %>% 
  mutate(date = as.Date(date, tryFormats = c("%m/%d/%Y"),optional = F )) %>% 
  filter(date >= today() %m-% days(10)) %>% 
  filter(!is.na(customer))%>% 
  group_by(customer) %>%
  summarise(n = n(), amount = sum(amount)) %>% 
  arrange(desc(amount)) %>% 
  filter(amount != 0)










# Section -----------------------------------------------------------------



#a) Project Analysis  ---------------------------------------------------
projects_analisys <- function(project.name, file.name){ 
  
  
  setwd("C:/Users/andre/Downloads")
  dir.create("projects_analysis")
  setwd("C:/Users/andre/Downloads/projects_analysis")
  
  wb <- createWorkbook()
  addWorksheet(wb,"transactions_detailed")
  
  
  
  detail   <- expenses_level(project.name) %>% mutate(direction = "outflow") %>%
    bind_rows(incomes_level(project.name) %>% mutate(direction = "inflow")) %>% 
    mutate(amount = ifelse(direction == "outflow", amount*-1,amount)) %>% 
    mutate(project = project.name) %>% 
    mutate(class = case_when(class == "Drywall Installation"~"Drywall / Finish",
                             class == "Reapirs / Install"~"Repairs / Installation",
                             TRUE ~ as.character(class)))
  
  
  writeData(wb, "transactions_detailed", detail)
 
  openxlsx::saveWorkbook(wb, file = file.name, overwrite = TRUE)
  
  return(detail)
  
}



#b) Focus tibble & execution ------------------------------------------------

interest <- c("Rmodl|52 T|162 Sem|27 FLAM|Marina|421 S")

# pjs originated in project wrangling module. 
# examine that data

focus <- pjs %>% 
  filter(grepl(interest, project)) %>% 
  select(project.name, file.name)


analisys <- pmap(focus, projects_analisys)



#c) Dynamic merge file write ---------------------------------------------


interest <- c("Rmodl|52 T|162 Sem|27 FLAM|Marina|421 S")


condenzation <- function(){ 

print(getwd())  
  
filenames_list <- list.files()

all_data <- lapply(filenames_list,function(filename){
  print(paste("Merging",filename,sep = " "))
  read.xlsx(filename)
})


consolid_analisys <- do.call(rbind.data.frame, all_data) %>% 
  as_tibble() %>% 
  mutate(date = as.Date(date, origin = "1899-12-30")) %>%
  mutate(date = as.Date(date, tryFormats = c("%m/%d/%Y"),optional = F ))%>% 
  mutate(project = case_when(project == "162 Semin"~"162 Seminole",
                             project == "Rmodl 50"~"50 Island Remodel",
                             project == "27 FLAMIN"~"27 Flamingo",
                             project == "Marina Vi"~"Marina Village",
                             project == "421 S Har"~"421 South Harbour",
                             project == "52 Tarpon"~ "52 Tarpon Av"
         )) %>% 
  mutate(second = ifelse(is.na(second),main, second)) %>% 
  mutate(concept = case_when(str_detect(second,"Materials")~"Materials",
                             str_detect(second,"Labor")~"Labor",
                             str_detect(second,"Supplies")~"Materials",
                             str_detect(second, "Tools")~"Materials",
                             str_detect(second, "Fees")~"Materials",
                             str_detect(second, "S00")~"Invoice",
                             TRUE ~ as.character(second))
         ) %>% 
  relocate(.after = date, project) %>% 
  relocate(.before = main,concept) %>% 
  relocate(.after = concept,class)

  grouped_consolid <- consolid_analisys %>%
    group_by(date,project, concept,class,direction) %>% 
    summarise(amount,.groups = "drop") %>% ungroup() %>% 
    group_by(date,project,concept,class,direction) %>%
    summarise(amount = sum(amount),.groups = "drop")%>%
    arrange(project,direction,concept,class)
  
  consolid_resume <- consolid_analisys %>%
    group_by(project, concept,class,direction) %>% 
    summarise(amount,.groups = "drop") %>% ungroup() %>% 
    group_by(project,concept,class,direction) %>% 
    summarise(amount = sum(amount),.groups = "drop")%>%
    arrange(project,direction,concept,class)


  
openxlsx::write.xlsx(consolid_analisys, "consolid_analisys.xlsx")
openxlsx::write.xlsx(grouped_consolid, "grouped_consolid.xlsx")
openxlsx::write.xlsx(consolid_resume, "consolid_resume.xlsx")


return(consolid_analisys)

}


consolid_analisys <- condenzation()



# Out of algorithm  ------------------------------------------------------


setwd("C:/Users/andre/Downloads/projects_analysis")


consolid <- openxlsx::read.xlsx("consolid_analisys.xlsx") %>% 
            as_tibble() %>% 
            mutate(date = as.Date(date, origin = "1899-12-30")) %>% 
            mutate(date = as.Date(date, tryFormats = c("%m/%d/%Y"),optional=F))


consolid %>% group_by(project) %>% summarise(amount = sum(amount)) %>% 
  janitor::adorn_totals()





# EXECUTIVE FLASH ---------------------------------------------------------

# B4 executing this function, keep in mind to download up to date transacts
# ready at ::: Transaction Report Detailed Pj: Customized
# transactions records takes a little to be updated. 




executive <- function(){
  
  transactions <- cleaner("transactions")
  analisys <- pmap(focus, projects_analisys)
  consolid_analisys <- condenzation()
  
  setwd("C:/Users/andre/Downloads")
  
}

# Add the subcontracts by project - class or service 
# Left join from ZOHO 


# Invoices ----------------------------------------------------------------


# Download invoices resume and filter it by those existing in consolid_analisys
invoices<- cleaner("invoices") 

invoices_filtered <- invoices %>% 
  filter(num %in% consolid_analisys$second) %>% 
  mutate(open_balance = as.numeric(open_balance)) %>% 
  select(-memo) %>% 
  mutate(amount_paid = amount - open_balance) %>% 
  janitor::adorn_totals()
  




# Contrasted : Compare ZOHO vs QB registers  ------------------------------

# Expose differences between both data sources, remember QB predominates.



pj_qb <- expenses("52 T") %>% select(date,amount) 

pj_zoho <- openxlsx::read.xlsx("tarpon_zoho.xlsx") %>% 
  as_tibble() %>% 
  select(date = Payment.Date, amount = Amount.Paid) %>% 
  mutate(date = as.Date(date, origin = "1899-12-30")) %>% 
  mutate(date = as.Date(date, tryFormats = c("%m/%d/%Y"),optional = F ))


# Totals 
sum(pj_zoho$amount)
sum(pj_qb$amount)

# divergencies ::: 

pj_qb %>% anti_join(pj_zoho, by = c("date", "amount"))

pj_zoho %>% anti_join(pj_qb, by = c("date", "amount"))



