

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
library(openxlsx)



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

# Functions ---------------------------------------------------------------

# Sum off all payments of a project
  
# Transaction History -----------------------------------------------------
# Protocol for transaction record downloading:
# Enter quickbooks, Transaction Report, add class & customer.
# Ungroup the report by account & filter out old dates. (Last Year to date)

browseURL("https://app.qbo.intuit.com/app/reportv2?token=GENERIC_QZREPORT&show_logo=false&date_macro=lastyeartodate&low_date=01/01/2020&high_date=03/23/2020&cash_basis=no&customized=yes&groupby=none&parenttoken=PANDL_BYCUST&columns=~date%3ATxDate%2C~txn_type_label%3ATxTypeID%2C~doc_num%3ATxHeader%2FDocNum%2C~name_label%3ATxHeader%2FNameID%2C~memo_desc_label%3AMemoText%2C~account_id_label%3AAccountID%2C~split_label%3AOtherAccountID%2C-~amount_label%3ANaturalAmount%2C%3D~balance_label%3ANaturalAmount%2C~cs_customer_label%3ACustomerID%2C~ls_class_col_header%3AKlassID&collapsed_rows=&edited_sections=false&divideby1000=false&hidecents=false&negativenums=1&negativered=false&show_header_title=true&show_header_range=true&show_footer_custom_message=true&show_footer_date=true&show_footer_time=true&show_footer_basis=true&header_alignment=Center&footer_alignment=Center&show_header_company=true&company_name=ROHO%27S%20GROUP%20CORP&title=Transaction%20Report&footer_custom_message=")

setwd("C:/Users/andre/Downloads")


# Function cost extraction from all transactions record: 

# xlsx files::: follow transaction history instructions to download.




#0) Synthesizer: QB Projects Dynamic Reporting  ---------------------------------

# 8 Steps to create each project report from all transactions source. 

#1) Clean transactions data  ------------------------------------------------
# Import & Clean Data ::: 


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



cleaner <- function(file){
  
file_name <-   paste0(file,".xlsx")


if(file_name == "ROHO'S+GROUP+CORP_Transaction+Report.xlsx"){
  
   final_file_name <- "expenses.xlsx"

} else if (file_name == "ROHO'S+GROUP+CORP_Transaction+Report (1).xlsx"){
     
  final_file_name <- "transactions.xlsx"
} else {
     
  final_file_name <- "invo.xlsx"
   
}

setwd("C:/Users/andre/Downloads")

data <- openxlsx::read.xlsx(file_name) %>% as_tibble()
data <- data[-1:-2,-1]
names(data) <- as.character(data[1,])

tibble <- data %>% janitor::clean_names()

transact_tibble <- tibble[-1,] %>% filter(date != is.na(date))%>% 
  mutate_all(., str_trim) %>% mutate(amount = as.double(amount)) %>% 
  mutate(date = as.Date(date, tryFormats = c("%m/%d/%Y"),optional = F )) %>% 
  rename(type = transaction_type,memo = memo_description) %>% 
  mutate(creation_date = lubridate::parse_date_time(create_date, orders="mdy HMS")) %>% 
  mutate(modification_date = lubridate::parse_date_time(last_modified, orders="mdy HMS")) %>% 
  arrange(desc(creation_date)) %>% select(-create_date,-last_modified)


openxlsx::write.xlsx(transact_tibble,final_file_name)

return(transact_tibble)


}



transactions <- cleaner("ROHO'S+GROUP+CORP_Transaction+Report")


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
    filter(grepl("Indirect|Labor|Direct Material|Fees|Services|Officer",account))
  
  return(pj_data)
  
}

expenses_ts <- function(project){
  
  pj_data <- transactions %>% 
    filter(grepl(project,customer)) %>% 
    filter(grepl("Indirect|Labor|Direct Material|Fees|Services|Officer",account)) %>% 
    group_by(customer,date) %>% summarise(amount = sum(amount))
  
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

# Exercise:::
# Delete from ZOHO transactions not in QB or edit 

# IF date & amount are the same and one of the amount contains a
# DOT at the end of number, means the differece is a about cents
# Decimals. 


contraster <- function(pj,pjs){ 
  
  # feed by quickbooks transactions & Zoho's all payments. 
  
  setwd("C:/Users/andre/Downloads")
  
  qb   <- expenses(pj) %>% select(date,amount) %>% 
    arrange(desc(date))
    # mutate(date = date-1)
  
  zoho <- openxlsx::read.xlsx("payments.xlsx") %>% 
    as_tibble %>%
    rename(date = Payment.Date, type = Transaction.Type) %>% 
    mutate(date = as.Date(date, origin = "1899-12-30")) %>% 
    mutate(date = as.Date(date, tryFormats = c("%m/%d/%Y"),optional=F)) %>% 
    filter(grepl(pjs,Project)) %>% 
    select(date, amount = Amount.Paid) %>% 
    arrange(desc(date)) %>% 
    mutate(amount = ifelse(is.na(amount),0,amount)) %>% 
    arrange(desc(date))
  
  
  z <- print(paste("ZOHO:",sum(zoho$amount)))
  q <- print(paste("QuickB:",sum(qb$amount)))
  
  
  diff1 <- qb %>% anti_join(zoho, by = c("date","amount"))
  d1 <- print("in QB not in Zoho to ADD")
  diff2 <- zoho %>% anti_join(qb, by = c("date","amount"))
  d2 <- print("in Zoho not in Qb to ERASE")
  
  d3 <- print("instruction: delete all in Zoho that's not in QB, add all in Zoho that's in QB but not at ZH")
  
  return(list(z,q,qb,zoho, diff1,d1,diff2,d2,d3))
  
}



# Ubung:::

remodel <- contraster("Rmod", "Remodel")
less <- remodel[[5]] %>% select(amount) %>% pull
more <- remodel[[3]] %>% select(amount) %>% pull
sum(zoho$amount) - sum(less) + sum(more)



# By date Comparisson -----------------------------------------------------



compare_by_date <- function(pj,pjs,date_input) {    

  
  qb <- transactions %>% filter(grepl(pj, customer), date == date_input) %>% 
    select(-balance,-customer,-split,-account) %>% 
    janitor::adorn_totals()
  
  zoho <- openxlsx::read.xlsx("all_payments.xlsx") %>% 
    as_tibble %>%
    rename(date = Payment.Date, type = Transaction.Type) %>% 
    mutate(date = as.Date(date, origin = "1899-12-30")) %>% 
    mutate(date = as.Date(date, tryFormats = c("%m/%d/%Y"),optional=F)) %>% 
    filter(grepl(pjs,Project)) %>% 
    filter(date == date_input) %>% 
    rename(name = Resource, amount = Amount.Paid) %>% 
    mutate(name = str_trim(name)) %>% 
    select(-Vendor, -Amount.to.Pay, -Retainage.Amount) %>% 
    janitor::adorn_totals()
  
  
  zoho %>% anti_join(qb, by = c("date", "amount"))
  qb %>% anti_join(zoho, by = c("date", "amount"))
  
  
  return(list(qb, zoho))
  
}

# In 162 Seminole case, all transactions registered in qb are in zoho 
# there are 4 missing transactions in zoho that are not in qb. 
# All transaction are recorded but with diff date (18899)
# transactions from quickbooks are adding a day, or zoho's are lagging. ?¿ 
 
 
 
 # Not only the scenario where deleting all in qb thats not in zoho is possible 
 # qb transactions that are not registered properly within customer and class 
 # specifications may produce a bias in the filtering executions, this uncompleted
 # registration seems like data it's missing, additionally takes longer to update in 
 # quickbooks visual report.
 
 


 # Why is the accounts diverging again after introducing all the payments in both systems. 
 


#3) Rudiment calls -------------------------------------------------------------------------


# Expenses queried from accounts DL,DM,IL, fees & services:
dd("Rmodl")

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
  

invoices <- openxlsx::read.xlsx("invoices.q.xlsx") %>% as_tibble() %>%
  mutate(date = as.Date(date, origin = "1899-12-30"))
  
  


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




# Invoices contraster  ----------------------------------------------------


invoices_divergence <- function(file){
  
  
  inv.z <- openxlsx::read.xlsx("invoices.z.xlsx") %>% 
    as_tibble() %>% 
    mutate(Invoice.Date = as.Date(Invoice.Date, origin = "1899-12-30"))%>%
    select(date = Invoice.Date, ref = Reference, amount = Amount) %>% 
    mutate(ref = str_trim(ref))
  
  inv.q <- cleaner("invoices.q")
  
  inv.q <- inv.q %>% filter(open_balance > 0) %>% select(date, ref = num, amount)
  
  
  missing <- inv.q %>% anti_join(inv.z, by = "ref")  
  
  invoices_quick <- openxlsx::read.xlsx("invoices.q.xlsx") %>% as_tibble() %>% 
    mutate(date = as.Date(date, origin = "1899-12-30"))
    
  
}



# Expenses Analisys -------------------------------------------------------


islandr <- expenses("Rmod") %>% select(date,account,class,amount)


island_rem <- islandr %>%   mutate(concept = case_when(str_detect(account,"Materials")~"Materials",
                                                       str_detect(account, "Supplies")~"Materials",
                                                       str_detect(account, "Labor")~"Labor",
                                                       TRUE ~ as.character(account))) %>% 
  mutate(phase = ifelse(date <=  "2021-03-25", "1st","2nd")) %>% 
  mutate(class = case_when(class == "Painting"~"Int Painting",
                           class == "Drywall / Finish" ~ "Finish",
                           TRUE ~ as.character(class))) %>% 
  arrange(date)



# Simple ProjectLytics ----------------------------------------------------

projects <- function(){
  
  setwd("C:/Users/andre/Downloads")
  dir.create("project_analysis")
  setwd("C:/Users/andre/Downloads/project_analysis")
  
  
  wb <- createWorkbook()
  addWorksheet(wb,"detail")
  addWorksheet(wb,"consolidated")
  

  
  interest <- c("53 N|Rmodl|52 T|162 Sem|27 FLAM|Marina|421 South")
  
  
  
  detail <- transactions %>% filter(grepl(interest,customer)) %>% 
    separate(customer, c("client","project"),sep = "([:])") %>% 
    filter(grepl("Indirect|Labor|Direct Material|Fees|Services|Officer",account)) %>% 
    mutate(concept = case_when(str_detect(account,"Materials")~"Materials",
                               str_detect(account,"Labor")~"Labor",
                               str_detect(account,"Supplies")~"Materials",
                               str_detect(account, "Tools")~"Materials",
                               str_detect(account, "Fees")~"Materials",
                               str_detect(account, "S00")~"Invoice",
                               TRUE ~ as.character(account))) %>% 
    select(date, project,class,concept,amount) %>% 
    arrange(project) %>% 
    mutate(project = 
             case_when(str_detect(project,"52 T")~"52 Tarpon",
                       str_detect(project,"162 S")~"162 Seminole",
                       str_detect(project,"421")~"421 South H",
                       str_detect(project, "53 N")~"53 N Blackwater",
                       str_detect(project, "Rmodl")~"50 Island R",
                       str_detect(project, "27 F")~"27 Flamingo",
                       str_detect(project, "Marina")~"Marina Village",
                        TRUE ~ as.character(project))
    )
    

  
  
  consolid <- detail %>% group_by(project,class,concept) %>% 
    summarise(amount = sum(amount),.groups = "drop")
  
  
  
  writeData(wb, "detail", detail)
  writeData(wb, "consolidated", consolid)
  
  
  
  openxlsx::saveWorkbook(wb, 
                         file = "open_projects.xlsx",
                         overwrite = TRUE)  
  
}


# Zoho circunsperspective;  -----------------------------------------------

zoho_projects <- function(){
  
  setwd("C:/Users/andre/Downloads")
  
  subs <- openxlsx::read.xlsx("subct.xlsx")
  pays <- openxlsx::read.xlsx("payments.xlsx")
  
  
  dir.create("project_analysis")
  setwd("C:/Users/andre/Downloads/project_analysis")
  
  
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb,"detail_payments")
  openxlsx::addWorksheet(wb,"consolid_payments")
  openxlsx::addWorksheet(wb,"subct_raw")
  openxlsx::addWorksheet(wb,"consolid")
  
  

interest <- c("53 N|Remodel|52 T|162 Sem|27 FLAM|Marina|421 South")

subct <-subs %>% as_tibble() %>% 
  filter(grepl(interest, Project)) %>% 
  select(project = Project,service = Service,
         subct_name = Subcontract.Name,
         subct_amount = Contract.Amount, 
         budget_days = Budget.Days) %>% 
  mutate(project = 
           case_when(str_detect(project,"52 T")~"52 Tarpon",
                     str_detect(project,"162 S")~"162 Seminole",
                     str_detect(project,"421")~"421 South H",
                     str_detect(project, "53 N")~"53 N Blackwater",
                     str_detect(project, "Remodel")~"50 Island R",
                     str_detect(project, "27 F")~"27 Flamingo",
                     str_detect(project, "Marina")~"Marina Village",
                     TRUE ~ as.character(project))) %>% 
  arrange(project) %>% filter(!grepl("Old",subct_name)) %>% 
  mutate(subct_name = str_trim(subct_name))



# interest <- c("53 N|Remodel|52 T|162 Sem|27 FLAM|Marina|421 South")

payments <- pays %>% as_tibble() %>% 
  filter(grepl(interest, Project)) %>% 
  mutate(date = as.Date(Payment.Date, origin = "1899-01-01-01")) %>% 
  rename(project = Project, type = Transaction.Type) %>% 
  mutate(project = 
  case_when(str_detect(project,"52 T")~"52 Tarpon",
            str_detect(project,"162 S")~"162 Seminole",
            str_detect(project,"421")~"421 South H",
            str_detect(project, "53 N")~"53 N Blackwater",
            str_detect(project, "Remodel")~"50 Island R",
            str_detect(project, "27 F")~"27 Flamingo",
            str_detect(project, "Marina")~"Marina Village",
            TRUE ~ as.character(project))) %>% 
  select(date,project,service = Services,
         subct_name = Subcontract.Name,
         subct_id=Subcontract,type, amount=Amount.Paid) %>% 
  mutate(type = ifelse(type == "Expense","Materials","Labor")) %>% 
  mutate(subct_name = str_trim(subct_name))



grouped <- payments %>%
  group_by(project,service,type) %>% 
  summarise(amount = sum(amount),.groups = "drop") %>% 
  ungroup() %>% 
  pivot_wider(names_from = type,
              values_from = amount)


consolid <- subct %>%
  left_join(grouped, by = c("project","service")) %>% 
  distinct()



openxlsx::writeData(wb, "consolid", consolid)
openxlsx::writeData(wb, "detail_payments", payments)
openxlsx::writeData(wb, "consolid_payments", grouped)
openxlsx::writeData(wb, "subct_raw",subct)


openxlsx::saveWorkbook(wb, 
                       file = "open_projects.xlsx",
                       overwrite = TRUE) 

}

checker <- function(pj){ 
  
pays %>% as_tibble() %>%
  rename(type = Transaction.Type)%>% 
  # filter(!grepl("Expense",type))  %>% 
  mutate(Amount.Paid = ifelse(is.na(Amount.Paid),0,Amount.Paid)) %>% 
  filter(grepl(pj, Project)) %>% 
  group_by(Subcontract.Name, Subcontract,type) %>% 
  summarise(amount = sum(Amount.Paid),
            .groups = "drop") %>%
  janitor::adorn_totals() 

}





# By date payments distribution ------------------------------------------------------------

dist_paid <- function(date){
  data %>% filter(Payment.Date == date) %>%
    group_by(Project,Subcontract,Subcontract.Name) %>%
    summarise(amount = sum(Amount.Paid),.groups = "drop") %>%
    mutate(payment_date = date) %>% 
    relocate(.before = Project,payment_date) %>% 
    janitor::adorn_totals()
  
}


# PROJECT ANALYSIS FROM ZOHO --------------------------------------------------------


# Wrangler ----------------------------------------------------------------


project_report <- function(){ 

  setwd("C:/Users/andre/Downloads")
  
  
# Open projects in Zoho 
open_pj <- openxlsx::read.xlsx("open_pj.xlsx") %>% as_tibble() %>% 
  select(project = Project.Name) %>% 
  filter(!grepl("52 T|162 S|27 F",project))

# All subcontracts in Zoho 
subct <- openxlsx::read.xlsx("subct.xlsx") %>% as_tibble() %>% 
  janitor::clean_names() %>% 
  select(project,subct_id = controlabor_id,
         contract_amount, budget_days,subcontract_name = controlabor_name) %>% 
  replace_na(list(contract_amount = 0,
                  budget_days = 0)) %>% 
  mutate(start_date = as.Date(start_date, origin = "1899-01-01")) %>% 
  semi_join(open_pj, by = "project")


# Wrangling data 
data <- openxlsx::read.xlsx("payments.xlsx") %>% as_tibble() %>% 
  mutate(Payment.Date = as.Date(Payment.Date, origin = "1899-12-30")) %>% 
  janitor::clean_names() %>% 
  rename(subcontract_name = controlabor_name,
         subcontract = controlabor)
  


# Project Analysis Report -------------------------------------------------

projects <- data %>%
  filter(!is.na(project))%>% 
  filter(project %in% open_pj$project) %>% 
  group_by(project, services, subcontract,
           subcontract_name, transaction_type) %>% 
  summarise(amount = sum(amount_paid),.groups = "drop") %>% 
  pivot_wider(names_from = transaction_type, 
                           values_from = amount) %>% 
  janitor::clean_names() %>%
  replace_na(list(expense = 0, subcontract_2 = 0, 
                  hourly_labor = 0))%>% 
  rowwise() %>% 
  mutate(labor_paid = sum(subcontract_2,hourly_labor)) %>% 
  ungroup() %>% 
  rename(subct_id = subcontract,
         subcontract_payment = subcontract_2,
         materials_paid = expense) %>% 
  relocate(.before = materials_paid, labor_paid) %>%
  left_join(subct %>% select(-subcontract_name),
            by = c("subct_id","project")) %>% 
  rename(labor_budget = contract_amount) %>% 
  mutate(completion = 1) %>% 
  mutate(accrued_labor = completion * labor_budget) %>% 
  mutate(labor_ratio = 1-(labor_paid/accrued_labor)) %>% 
  mutate(materials_budget = case_when(str_detect(subcontract_name,"Dryw")~labor_budget*.4,
                                      str_detect(subcontract_name,"Fram")~labor_budget*.6,
                                      str_detect(subcontract_name,"Stuc")~labor_budget*.12,
                                      str_detect(subcontract_name,"Inst")~labor_budget*.20,
                                      str_detect(subcontract_name,"Inst")~labor_budget*.20,
                                      str_detect(subcontract_name,"Fini")~labor_budget*.15,
                                      str_detect(subcontract_name,"Pain")~labor_budget*.20)) %>% 
  mutate(accrued_materials = completion * materials_budget) %>%
  mutate(materials_ratio = 1-(materials_paid/accrued_materials)) %>% 
  select(project, services, subct_id, subcontract_name,completion,
         labor_budget,accrued_labor,labor_paid,labor_ratio,
         materials_budget,materials_paid,accrued_materials,materials_ratio,
         budget_days) %>% 
  mutate(subcontract_name = str_trim(subcontract_name)) %>% 
  mutate(subct_id = str_extract(subct_id,"[[:digit:]]+")) %>% 
  select(-contains("days")) %>% relocate(.before = materials_paid, accrued_materials)

# Short names 
names = projects %>% distinct(project) %>% 
  mutate(projects = substr(project, 0,12)) %>% 
  mutate(projects = str_trim(projects)) %>% 
  mutate(projects = str_to_title(projects))
  

# Final Anatomy 
project_analysis <- projects %>% left_join(names, by = "project") %>% 
  relocate(.before = project,projects) %>% select(-services)

project_analysis_resumen <- project_analysis %>%
  select(projects=project,subcontract_name,
         labor_budget,labor_paid,
         materials_budget,materials_paid) %>% 
  rowwise() %>% 
  mutate(total_cost_realized = sum(labor_paid+materials_paid)) %>% 
  ungroup() %>% 
  relocate(.before = labor_budget, total_cost_realized) %>% 
  select(-contains("budget")) %>% 
  rename(Projects = projects, 
         Servicios = subcontract_name,
         'Costo Realizado' = total_cost_realized,
         'Costo Labor' = labor_paid,
         'Costo Material' = materials_paid) %>% 
  mutate('Precio de Venta' = "fill from quote lines") %>% 
  relocate(.after = Servicios, 'Precio de Venta') %>% 
  mutate(Resultado = '(PdV - CR)') %>% 
  mutate(Retorno = '1-(CR/PdV)') %>% 
  relocate(.after = 'Costo Realizado',Resultado) %>% 
  relocate(.after = Resultado, Retorno)


# Change of orders for open_pj with transactions history. 

changed_orders <- openxlsx::read.xlsx("changed_orders.xlsx") %>% 
  as_tibble() %>% filter(Project %in% projects$project) %>% 
  mutate(co_num = paste0("CO000", Change.Order.Number)) %>% 
  janitor::clean_names() %>% 
  relocate(.after = project,co_num) %>% 
  select(-change_order_number,-acceptance_date) %>% 
  mutate(agreement_date = as.Date(agreement_date, origin = "1899-12-30"))

return(list(
            project_analysis_resumen=project_analysis_resumen,
            project_analysis=project_analysis,
            changed_orders = changed_orders,
            transactions = data
            
))

}

# Materials %
# 
# D&F  ::: 40%
# STUC ::: 12%
# FAMG ::: 60%
# APNT ::: 20%
# INST ::: 20%

# Execution 
project_report()

# Local Storage 
date <- lubridate::today() %>% str_replace_all(.,"-","_")
project_report() %>%
  openxlsx::write.xlsx(., paste0(date,"_project_analysis.xlsx"),asTable = T)




# ControLabor -------------------------------------------------------------


report_projects <- function(){ 


setwd("C:/Users/andre/Downloads")


# Open projects in Zoho 
open_pj <- openxlsx::read.xlsx("open_pj.xlsx") %>% as_tibble() %>% 
  rename(project = Project.Name)

# All subcontracts in Zoho 
clab <- openxlsx::read.xlsx("clab.xlsx")%>% as_tibble() %>% 
  janitor::clean_names() %>% 
  select(project,controlabor_name,start_date,controlabor_id,
         bid_price,labor_amount,materials,budget_days, used_days) %>% 
  mutate(start_date = as.Date(start_date, origin = "1899-12-30")) %>% 
  semi_join(open_pj, by = "project") 

data <- openxlsx::read.xlsx("payments.xlsx") %>% as_tibble() %>% 
  mutate(Payment.Date = as.Date(Payment.Date, origin = "1899-12-30")) %>% 
  janitor::clean_names() %>%
  filter(!is.na(project))%>% 
  filter(project %in% open_pj$project)

last_week <- data %>% 
  filter(payment_date > lubridate::today() %m-% days(8) ) %>%
  group_by(project,controlabor_name,transaction_type) %>%
  mutate(controlabor_name = str_trim(controlabor_name)) %>%
  summarise(amount_paid = sum(amount_paid),.groups = "drop")


previous <- data %>% 
  filter(payment_date <= lubridate::today() %m-% days(8) ) %>%
  semi_join(last_week, by=c("project", controlabor_name)) %>% 
  filter(payment_date <= lubridate::today() %m-% days(8) ) %>%
  group_by(project,controlabor_name,transaction_type) %>%
  mutate(controlabor_name = str_trim(controlabor_name)) %>%
  summarise(amount_paid = sum(amount_paid),.groups = "drop")

projects <- data %>% 
  group_by(project, controlabor,
           controlabor_name, transaction_type) %>% 
  summarise(amount = sum(amount_paid),.groups = "drop") %>% 
  pivot_wider(names_from = transaction_type, 
              values_from = amount) %>% 
  janitor::clean_names() %>%
  replace_na(list(expense = 0, subcontract = 0, 
                  hourly_labor = 0))%>% 
  rowwise() %>% 
  mutate(labor_paid = sum(subcontract,hourly_labor)) %>% 
  ungroup() %>% 
  rename(material_paid = expense,controlabor_id = controlabor) %>% 
  select(-hourly_labor,-subcontract) %>% 
  left_join(clab,by=c("project","controlabor_id","controlabor_name")) %>% 
  select(project,servicio=controlabor_name,start_date,bid_price,
         labor_budget = labor_amount, labor_paid,
         materials_budget = materials, material_paid,
         budget_days, used_days) %>% 
  filter(!grepl("Maintenance",servicio)) %>% 
  mutate_if(is.character, str_trim)


closed_tl <- openxlsx::read.xlsx("closed_time_logs.xlsx") %>% 
  as_tibble() %>% janitor::clean_names() %>%
  mutate(task_date = as.Date(task_date, origin = "1899-12-30")) %>% 
  group_by(project_name,controlabor) %>%
  summarise(duration = sum(duration),task_date = min(task_date),.groups = "drop") %>% 
  mutate(spent = duration * 21) %>% 
  filter(project_name %in% open_pj$project, !grepl("Maintenance",controlabor)) %>% 
  separate(controlabor, c("servicio","controlabor"),sep = "([-])") %>% 
  mutate_if(is.character, str_trim) %>% select(-controlabor) %>% 
  rename(hours_spent = duration, estimated_cost = spent,
         project = project_name) %>% 
  relocate(.before = project, task_date)


project_analysis <- projects %>%
  left_join(closed_tl, by = c("project","servicio")) %>% 
  replace_na(list(hours_spent = 0, estimated_cost = 0)) %>% 
  select(-start_date) %>% relocate(.after = servicio, task_date) %>% 
  rename(start_date = task_date) %>% 
  mutate(Revision = lubridate::today()) %>% 
  relocate(.after = bid_price, Revision) %>% 
  mutate("Completion %" = 1) %>% 
  relocate(.after = Revision, "Completion %") %>% 
  mutate("Labor Budget Completed"= labor_budget*1) %>% 
  rename("Labor Budget Original" = labor_budget) %>% 
  relocate(.before = labor_paid, "Labor Budget Completed") %>% 
  mutate("Paid - Budget (Labor)" = 'labor_paid - Labor Budget Completed') %>% 
  relocate(.after = labor_paid, "Paid - Budget (Labor)") %>% 
  mutate(materials_b_completed = materials_budget * `Completion %`) %>% 
  relocate(.after = materials_budget, materials_b_completed) %>% 
  mutate("Paid - Budget (Materials)" = "materials_paid - materials_b_completed") %>% 
  relocate(.after = material_paid,"Paid - Budget (Materials)") %>% 
  mutate(remaining_days = budget_days - used_days) %>% 
  relocate(.after = hours_spent,remaining_days) %>% 
  select(-estimated_cost) %>% 
  rename(Project = project, Servicio = servicio,'Start Date' = start_date,
         'Bid Price' = bid_price, 'Labor Paid' = labor_paid, 
         'Materials Budget Original' = materials_budget,
         'Materials B Completed' = materials_b_completed,
         'Materials Paid' = material_paid,
         'Budget Days' = budget_days, 'Used Days' = used_days,
         'Hours Registered' = hours_spent, 'Remaining days' = remaining_days) %>% 
  mutate('Exp Profit' = "bid - materials budget - labor budget") %>% 
  mutate('Current Profit' = "bid - labor paid - materials paid") %>% 
  mutate('Real Profit' = "((bid - material) * completion %) - labor_paid") 
  
  


changed_orders <- openxlsx::read.xlsx("changed_orders.xlsx") %>% 
  as_tibble() %>% filter(Project %in% projects$project) %>% 
  mutate(co_num = paste0("CO000", Change.Order.Number)) %>% 
  janitor::clean_names() %>% 
  relocate(.after = project,co_num) %>% 
  select(-change_order_number,-acceptance_date) %>% 
  mutate(agreement_date = as.Date(agreement_date, origin = "1899-12-30"))


return(list(data = data,
            project_analysis = project_analysis,
            changed_orders = changed_orders))

}


# Local Storage 
date <- lubridate::today() %>% str_replace_all(.,"-","_")
report_projects() %>%
  openxlsx::write.xlsx(., paste0(date,"_project_analysis.xlsx"),asTable = T)




update <- function(pj,subct){
  
  
  x <- report_projects()$data
  
  
  updated <-   x %>% 
    filter(grepl(pj,project)) %>% 
    filter(transaction_type != "Expense") %>% 
    group_by(project,controlabor_name) %>% 
    summarise(amount_paid = sum(amount_paid),.groups = "drop") %>% 
    filter(grepl(subct,controlabor_name))  

    return(updated)
  
  
}



# # By period Income:::  --------------------------------------------------


period_invoiced <- function(i_date,f_date) {
  
  invoiced <- transactions %>% 
    filter(between(date, as.Date(i_date),
                         as.Date(f_date))) %>% 
    filter(type == "Invoice", !is.na(amount), !grepl("Receiva",account)) %>% 
    select(-balance)
  
  pj_invoiced <- invoiced %>% group_by(customer) %>%
    summarise(amount = sum(amount)) 
    # filter(amount > 1) %>% 
    # janitor::adorn_totals()
  
  total <-  sum(pj_invoiced$amount)
  
  
  return(list(invoiced,pj_invoiced,total))
  
}


# # By period Expenses:::  ------------------------------------------------



period_expensed <- function(i_date,f_date){
  
  expensed <- transactions %>% 
    filter(between(date, as.Date(i_date),
                         as.Date(f_date))) %>% 
    filter(grepl("Indirect|Labor|Direct Material|Fees|Services|Officer",account)) %>% 
    select(-balance) %>% 
    filter(!grepl("ROHO",customer), !is.na(customer))
  

  
  pj_expensed <- expensed %>% group_by(customer) %>%
    summarise(amount = sum(amount))
    # filter(amount > 1) %>% 

  
  total <-  sum(pj_expensed$amount)
  
  
  return(list(expensed, pj_expensed,total))
  
}



# By project result of income and expenses by date rage -------------------


compacted_p <- function(i_date, f_date){
  
  inflow  <- period_invoiced(i_date,f_date)[[2]] %>% rename(amount_invoiced = amount)
  outflow <- period_expensed(i_date,f_date)[[2]] %>% rename(amount_expensed = amount)
  
  
  result_a <- inflow %>% left_join(outflow, by = "customer") %>% 
    replace_na(list(amount_invoiced = 0, amount_expensed = 0))

  
  result_b <- outflow %>% left_join(inflow, by = "customer") %>% 
    replace_na(list(amount_invoiced = 0, amount_expensed = 0))

  
  result <- result_a %>% bind_rows(result_b) %>% distinct()
  
  return(result)
  
}

pricing <- function(serv,diff,tex,des,thick){ 
prices <- openxlsx::read.xlsx("price_tibble.xlsx") %>% as_tibble() %>% 
  filter(service == serv, texture == tex, design == des, difficulty == diff) %>% 
  filter(grepl(thick,thickness))

return(prices)
  
}




# Explore OneDrive files  -------------------------------------------------

# Works when onedrive it's sync with local machine. 

# MENU :::

# explorer


# Folders
setwd("C:/Users/andre/OneDrive/RohosGroup") %>% 
  list.files() %>% as_tibble() %>% rename(folders = value)



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




all_proposals <-   setwd("C:/Users/andre/OneDrive/RohosGroup/PROPOSALS") %>% 
  list.files() %>% as_tibble() %>% rename(Proposals = value)



# Search proposal ---------------------------------------------------------

# https://stackoverflow.com/questions/12945687/read-all-worksheets-in-an-excel-workbook-into-an-r-list-with-data-frames


search_proposal <- function(pj){ 
  
  proposal_folder <- setwd("C:/Users/andre/OneDrive/RohosGroup/PROPOSALS") %>%
    list.files() %>% as_tibble() %>% 
    filter(grepl(pj,value)) %>% pull()
  
  direction <-  getwd() %>% as_tibble() %>%
    mutate(value = paste0(value,"/")) %>% 
    mutate(value = paste0(value,proposal_folder)) %>% 
    pull()
  
  setwd(direction)
  
  specific_file <- getwd() %>% list.files() %>% as_tibble() %>% 
    filter(!grepl(".pdf|Client|client",value)) %>% 
    filter(grepl(".xlsx",value))
  
  
  take_off <- openxlsx::read.xlsx(specific_file$value) %>% 
    as_tibble() %>% janitor::clean_names()

  print(direction) 
  
  print(direction %>% list.files())
  
  openxlsx::getSheetNames(specific_file$value) %>%
    as_tibble() %>% rename(sheets = value) %>% print()
  
  setwd("C:/Users/andre/Downloads")
  
  
return(take_off)

}


# Check out from transactions those from ADP 

adp <- transactions %>% filter(grepl("ADP",name)) %>% 
  filter(amount >0) %>% 
  filter(between(date, as.Date("2020-01-24"),
                       as.Date("2021-01-24"))) %>%
  filter(!grepl("Fees",account)) %>% 
  filter(grepl("DL",account)) %>% 
  mutate(memo_desc = str_replace(memo,"BUSINESS TO BUSINESS ACH ADP","")) %>%
  mutate(description = substr(memo_desc,0,8)) %>%
  mutate(description = str_trim(description)) %>%
  mutate(description = str_to_lower(description)) %>% 
  mutate(concept = case_when(str_detect(description,"tax")~"tax",
                                 str_detect(description,"wage")~"wage",
                                 str_detect(description,"fees")~"fees",
                                 str_detect(description,"payr")~"fees",
                                 TRUE ~ as.character(description))
         ) %>% select(-memo_desc,-description,-class,-customer,-num,-balance)


# Between thos miscathegorized ADP transactions, there was a transaction of SW
# as an ADP payment, transactions was re-cathegorized. 

# (Direct Labor), ADP transactions correct distribution.
adp %>%
  group_by(concept) %>% 
  summarise(cant = n(),
            amount = sum(amount)) %>% 
  janitor::adorn_totals()


# filter from ADP transactions, those directly related to Wage Pay 
adp_wages <- adp %>% filter(grepl("WAGE PAY",memo))
# transactions of this kind are group by date in the system and under (DL) account 
# the total of this payments, equals 19555.19 USD, in time range of "2020-01-24"-"2021-01-24"
# the existing time frame in this query, corresponds to: "2020-06-26" - "2021-01-22"

adp_wages %>% summary()


# Keeping this into account, we create a adp_summary file. downloading each year report.
# the we bind rows to create only one table, and filter for the time range existing in adp_wages.

# the adp_consolidated file, correspond to de direct labor paid by the company 
# during the period range from "2020-01-24" to "2021-01-24" and it's divided by worker 


adp_consol <- openxlsx::read.xlsx("adp_consolidated.xlsx") 

 adp_consol %>%
   mutate(date = as.Date(date,origin="1899-12-30"))


# the proof of this analysis, it's that if we group adp_consol transactions by date,
# and summarise the amount, the total of 19555.19 USD will match, and the number of 
# transactions will be equivalent to the adp_wages query, 16 rows. 

adp_consol %>%
  mutate(date = as.Date(date,origin="1899-12-30")) %>% 
  group_by(date) %>% summarise(amount = sum(net_paid)) %>% 
  janitor::adorn_totals()

# same as ::: 

adp_wages %>% select(date,amount) %>%
  arrange(date) %>% 
  janitor::adorn_totals()

# Analysis it's reproducible after refreshing the transact data. 

library(ggthemes)
chart <-
#ggplotly( 
transactions %>%
  filter(grepl(interest,customer), !is.na(class)) %>% 
  filter(grepl("Materials",account)) %>% 
  ggplot(aes(x = date, y = amount, color = class)) +
  
  #geom_boxplot()
  geom_point(size = 4)+
  labs(title = "DoD Materials Expensed", 
       subtitle = "Recent Projects only", 
       caption = "50 island Rmodl, 52 T,162 S,27 F,Marina,421 South,11000 SW,53 N,89070")
  #)
chart + theme_economist()
  
# scale_colour_economist()



# Examination of Direct Labor (deleted) in P&L report; contrasted with ADP 
dld <- transactions %>% filter(grepl("(deleted)",account))




# transformed, grouped and summarize by date range and name. 
grouped_dld <- dld %>%
  mutate(date_reference = ifelse(date<"2020-03-31",
                                 "Prior to 2020-03-31",
                                 "Prior to 2020-05-14")) %>%
  group_by(name,date_reference) %>% 
  summarise(amount = sum(amount),
            .groups="drop") %>%
  janitor::adorn_totals()






# WC observation ----------------------------------------------------------



# INCOME
inc_wc <- transactions %>% filter(type=="Invoice") %>% 
  filter(between(date,as.Date("2020-01-24"),
                      as.Date("2021-01-24"))) %>%
  filter(grepl("Income",account))


# Direct Materials (DM)
dm_wc <- transactions %>% 
  filter(between(date,as.Date("2020-01-24"),
                 as.Date("2021-01-24"))) %>% 
  filter(grepl("(DM)",account)) %>% replace_na(list(amount = 0))




# (Labor Cost)
labor_wc <- transactions %>% 
  filter(between(date,as.Date("2020-01-24"),
                 as.Date("2021-01-24"))) %>% 
  filter(grepl("Cost of Goods Sold",account)) %>%
  replace_na(list(amount = 0))



# Cost of goods sold (COGS)
cogs_wc <- transactions %>% 
  filter(between(date,as.Date("2020-01-24"),
                 as.Date("2021-01-24"))) %>% 
  filter(grepl("Cost of Goods Sold|Direct Material",account)) %>%
  replace_na(list(amount = 0))




start <- function(pj){
  
  
  
  ctl <- openxlsx::read.xlsx("closed_time_logs.xlsx") %>%
    as_tibble() %>% janitor::clean_names() %>%     
    mutate(task_date = as.Date(task_date, origin = "1899-12-30")) %>% 
    filter(grepl(pj,project_name)) %>% 
    arrange(task_date)

  
  return(ctl)
  
}



detail <- transactions %>%
  separate(customer, c("client","project"),sep = "([:])") %>% 
  filter(grepl("Indirect|Labor|Direct Material|Fees|Services|Officer",account)) %>% 
  mutate(concept = case_when(str_detect(account,"Materials")~"Materials",
                             str_detect(account,"Labor")~"Labor",
                             str_detect(account,"Supplies")~"Materials",
                             str_detect(account, "Tools")~"Materials",
                             str_detect(account, "Fees")~"Materials",
                             str_detect(account, "S00")~"Invoice",
                             TRUE ~ as.character(account))) %>% 
  filter(between(date, as.Date("2021-07-10"), today())) %>% 
  select(date, project,class,concept,amount) %>% 
  arrange(project) %>% 
  ungroup() %>% 
  group_by(project,class,concept) %>% 
  summarise(amount = sum(amount),.groups = "drop")


