

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
    filter(grepl("Indirect|Labor|Direct Material|Fees|Services|Officer",account))
  
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
  
  setwd("C:/Users/andre/Downloads")
  
  qb   <- expenses(pj) %>% select(date,amount) %>% 
    arrange(desc(date))
    # mutate(date = date-1)
  
  zoho <- openxlsx::read.xlsx("zoho_pjs.xlsx") %>% 
    as_tibble %>%
    rename(date = Payment.Date, type = Transaction.Type) %>% 
    mutate(date = as.Date(date, origin = "1899-12-30")) %>% 
    mutate(date = as.Date(date, tryFormats = c("%m/%d/%Y"),optional=F)) %>% 
    filter(grepl(pjs,Project)) %>% 
    select(date, amount = Amount.Paid) %>% 
    arrange(desc(date)) %>% 
    mutate(amount = ifelse(is.na(amount),0,amount)) %>% 
    arrange(desc(date))
  
  
  
  diff1 <- qb %>% anti_join(zoho, by = c("date","amount"))
  d1 <- print("in QB not in Zoho to ADD")
  diff2 <- zoho %>% anti_join(qb, by = c("date","amount"))
  d2 <- print("in Zoho not in Qb to ERASE")
  
  d3 <- print("instruction: delete all in Zoho that's not in QB, add all in Zoho that's in QB but not at ZH")
  
  return(list(qb,zoho, diff1,d1,diff2,d2,d3))
  
}


# Ubung:::

 seminole <- contraster("162 S", "162 S")
 less <- seminole[[5]] %>% select(amount) %>% pull
 more <- seminole[[3]] %>% select(amount) %>% pull
 sum(zoho$amount) - sum(less) + sum(more)
 
 
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
  
  subs <- openxlsx::read.xlsx("subct_zoho.xlsx")
  pays <- openxlsx::read.xlsx("payments.xlsx")
  
  
  dir.create("project_analysis")
  setwd("C:/Users/andre/Downloads/project_analysis")
  
  
  wb <- createWorkbook()
  addWorksheet(wb,"detail_payments")
  addWorksheet(wb,"consolid_payments")
  addWorksheet(wb,"subct_raw")
  addWorksheet(wb,"consolid")
  
  

interest <- c("53 N|Remodel|52 T|162 Sem|27 FLAM|Marina|421 South")

subct <-subs %>% as_tibble() %>% 
  filter(grepl(interest, Project)) %>% 
  select(project = Project,service = Service,
         subct_name = Subcontract.Name,
         subct_amount = Contract.Amount, 
         budget_days = Budget.Days,
         remaining_days = Remaining.Days) %>% 
  mutate(project = 
           case_when(str_detect(project,"52 T")~"52 Tarpon",
                     str_detect(project,"162 S")~"162 Seminole",
                     str_detect(project,"421")~"421 South H",
                     str_detect(project, "53 N")~"53 N Blackwater",
                     str_detect(project, "Remodel")~"50 Island R",
                     str_detect(project, "27 F")~"27 Flamingo",
                     str_detect(project, "Marina")~"Marina Village",
                     TRUE ~ as.character(project))) %>% 
  arrange(project) %>% filter(!grepl("Old",subct_name))



interest <- c("53 N|Remodel|52 T|162 Sem|27 FLAM|Marina|421 South")

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
  mutate(type = ifelse(type == "Expense","Materials","Labor")) 


grouped <- payments %>%
  group_by(project,service,type) %>% 
  summarise(amount = sum(amount),.groups = "drop") %>% 
  ungroup() %>% 
  pivot_wider(names_from = type,
              values_from = amount)


consolid <- subct %>%
  left_join(grouped, by = c("project","service")) %>% 
  distinct()



writeData(wb, "consolid", consolid)
writeData(wb, "detail_payments", payments)
writeData(wb, "consolid_payments", grouped)
writeData(wb,"subct_raw",subct)


openxlsx::saveWorkbook(wb, 
                       file = "open_projects.xlsx",
                       overwrite = TRUE) 

}

checker <- function(pj){ 
  
pays %>% as_tibble() %>%
  rename(type = Transaction.Type)%>% 
  filter(!grepl("Expense",type))  %>% 
  mutate(Amount.Paid = ifelse(is.na(Amount.Paid),0,Amount.Paid)) %>% 
  filter(grepl(pj, Project)) %>% 
  group_by(Subcontract.Name, Subcontract) %>% 
  summarise(amount = sum(Amount.Paid),
            .groups = "drop") %>%
  janitor::adorn_totals() 

}


lala <- function(num){
  
  
  d <- (num*3620)+2000000-(2500000) 
  f <- d-5000000
  
  return(list(d,f))
}



# Pricing Model  ----------------------------------------------------------

core <- openxlsx::read.xlsx("prices_tibble.xlsx") %>% as_tibble()

d2 <- core %>% mutate(difficulty = 2, price = price+1)
d3 <- core %>% mutate(difficulty = 3, price = price+2)
d4 <- core %>% mutate(difficulty = 3, price = price+3)

prices <- core %>% bind_rows(d2,d3,d4) %>% 
  rename(design = surface)

openxlsx::write.xlsx(prices, "prices_tibble.xlsx")



toff <- openxlsx::read.xlsx("toff.xlsx") %>% 
  as_tibble() %>% 
  mutate(Service = str_to_lower(Service)) %>% 
  janitor::clean_names()


pricing <- toff %>% filter(service == "stucco") %>% 
  select(service,difficulty,design,
         texture,thickness,total_sf) %>%
  left_join(prices, by=c("service", "difficulty", 
                         "design", "texture", "thickness"))

estimation <- pricing %>%
  mutate_if(is.character, str_trim) %>% 
  group_by(service,difficulty,design,texture,thickness) %>% 
  summarise(sqft = sum(total_sf),
            price = price*sqft,
            .groups = "drop") %>% 
  distinct() %>%
  janitor::adorn_totals()



# revision Pumpkin --------------------------------------------------------

pump <- invoices %>% filter(grepl("72 P", name))
transactions %>% filter(num %in% pump$num) 






