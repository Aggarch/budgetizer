
# Profit Estimator --------------------------------------------------------

# Task; create a long table of profits, based on transactions; where:: 

# Total Income(988) - Direct Material Cost(224) = Income Labor
# Total Cost(588) - Direct Material Cost(224)   = Labor Cost 
# 
# 1-(Labor Cost/ Income Labor) = Labor Gross Profit 

# group by date, and bring the option to observe it by month, quarter & year

# Total Income         <-  type = invoices & account = Income 
# Direct Material Cost <-  account = DM
# Cost of Goods Sold   <-  account %in% COGDS & DM 


# Download and clean data 


library(tidyverse)
library(lubridate)
library(zoo)

setwd("C:/Users/andre/Downloads")

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




# Recursive functions  ----------------------------------------------------

tei <- function(){ 

transact <- cleaner("ROHO'S+GROUP+CORP_Transaction+Report (1)")
exp      <- cleaner("ROHO'S+GROUP+CORP_Transaction+Report")
invoices <- cleaner("ROHO'S+GROUP+CORP_Invoice+List+by+Date")
 
return(list(t=transact,
            e=exp,
            i=invoices))

}


# Transactions Query ------------------------------------------------------


# Data Master 
data_master <- function(){ 
  
  transact <- cleaner("ROHO'S+GROUP+CORP_Transaction+Report (1)")
  exp      <- cleaner("ROHO'S+GROUP+CORP_Transaction+Report")
  invoices <- cleaner("ROHO'S+GROUP+CORP_Invoice+List+by+Date")
  
  expenses_vector <- exp %>%  distinct(account)%>%
    add_row(account = "Automobile Expense:Leasing") %>%
    add_row(account = "Other Expenses:Storage Rent") %>% 
    pull(account)  
  
  
  transactions <- transact 
  
  # minimalist queries 
  
  
  operational_income <- transactions %>% 
    replace_na(list(amount=0)) %>% 
    filter(grepl(".Operational Income:",account))
  
  billable_income <- transactions %>% 
    replace_na(list(amount=0)) %>% 
    filter(grepl("Billable",account)) 
  
  material_income <- transactions %>% 
    replace_na(list(amount=0)) %>% 
    filter(grepl("Material Income:",account))
  
  direct_labor <- transactions %>% 
    replace_na(list(amount=0)) %>% 
    filter(grepl("Direct Labor",account)) 
  
  direct_material <- transactions %>% 
    replace_na(list(amount=0)) %>% 
    filter(grepl("Direct Material",account)) 
  
  expenses <- transactions %>% filter(account %in% expenses_vector) %>% 
    replace_na(list(amount=0))
  
  
  cross_tibble_raw <- operational_income %>% 
    bind_rows(material_income,
              direct_labor,
              direct_material,
              billable_income,
              expenses)%>% 
    mutate(year_month = as.yearmon(date, "%Y-%m")) %>% 
    mutate(year_quarter = as.yearqtr(date, format = "%Y-%m-%d"))
  
  master <-  cross_tibble_raw %>% 
    replace_na(list(amount = 0)) %>% 
    mutate(source = case_when(str_detect(account,"Income:")~"income",
                              str_detect(account,"Billable")~"income",
                              str_detect(account,"Direct Labor")~"labor_cost",
                              str_detect(account,"Direct Material")~"material_cost",
                              TRUE ~ as.character(account)))%>%
    select(-modification_date) %>% 
    mutate(source = ifelse(account %in% expenses_vector,"expense",source))
  
  return(master)
  
  
}


# Profit Stimator ---------------------------------------------------------

# DECLARE {i_date and f_date}

profit_estimator <- function(i_date,f_date){ 
  
  expenses_vector <- exp %>%  distinct(account)%>%
    add_row(account = "Automobile Expense:Leasing") %>%
    add_row(account = "Other Expenses:Storage Rent") %>% 
    pull(account)

  transactions <- transact %>% filter(between(date,as.Date(i_date),as.Date(f_date))) %>% as_tibble()

  # minimalist queries


  operational_income <- transactions %>%
    replace_na(list(amount=0)) %>%
    filter(grepl(".Operational Income:",account))

  billable_income <- transactions %>%
    replace_na(list(amount=0)) %>%
    filter(grepl("Billable",account))

  material_income <- transactions %>%
    replace_na(list(amount=0)) %>%
    filter(grepl("Material Income:",account))

  direct_labor <- transactions %>%
    replace_na(list(amount=0)) %>%
    filter(grepl("Direct Labor",account))

  direct_material <- transactions %>%
    replace_na(list(amount=0)) %>%
    filter(grepl("Direct Material",account))

  expenses <- transactions %>% filter(account %in% expenses_vector) %>%
    replace_na(list(amount=0))

  
  
  # Distributions -----------------------------------------------------------
  
  operational_income_dist <- operational_income %>%
    group_by(type,date) %>%
    summarise(n = n(),amount = sum(amount),
              .groups="drop") %>%
    mutate(account = "operational_income" )
  
  material_income_dist <- material_income %>%
    group_by(type,date) %>%
    summarise(n = n(),amount = sum(amount),
              .groups="drop") %>%
    mutate(account = "materials_income" )
  
  direct_labor_dist <- direct_labor %>%
    group_by(type,date) %>%
    summarise(n = n(),amount = sum(amount),
              .groups="drop") %>%
    mutate(account = "direct_labor" )
  
  direct_material_dist <- direct_material %>%
    group_by(type,date) %>%
    summarise(n = n(),amount = sum(amount),
              .groups="drop") %>%
    mutate(account = "direct_material")
  
  
  # Grouped by type commutation
  
  accounts_distribution <- operational_income_dist %>%
    bind_rows(material_income_dist,
              direct_labor_dist,
              direct_material_dist) %>%
    mutate(year_month = as.yearmon(date, "%Y-%m")) %>%
    mutate(year_quarter = as.yearqtr(date, format = "%Y-%m-%d"))
  
  
  # RE-grouped overview 
  overview <- accounts_distribution %>% 
    group_by(type,account) %>% 
    summarise(cant = sum(n),
              amount=sum(amount),
              .groups = "drop") %>% 
    arrange(account)
  
  
  # Sense commutation 
  profitability <- data_master()
  
  
  # Sumarization & Arithmetics
  profit_resumen <- profitability %>%
    group_by(date,source) %>% 
    summarise(amount = sum(amount),.groups = "drop") %>% 
    ungroup() %>% 
    mutate(year_quarter = as.yearqtr(date, format = "%Y-%m-%d")) %>% 
    pivot_wider(names_from = source,
                values_from = amount) %>% 
    replace_na(list(income = 0, 'material_cost' = 0, 'labor_cost' = 0,expense=0)) %>% 
    mutate(period = substr(date,0,7)) %>% 
    relocate(.before = date,period) %>% 
    group_by(period,year_quarter) %>% 
    summarise(income = sum(income),
              material_cost = sum(material_cost),
              labor_cost = sum(labor_cost),
              expenses = sum(expense),
              .groups = "drop") %>% 
    ungroup() %>% 
    rowwise() %>% 
    mutate(labor_income = income-material_cost) %>% 
    mutate(total_cost = material_cost+labor_cost) %>% 
    mutate(operational_profit = 1-(total_cost/income)) %>% 
    mutate(labor_gross_profit = 1-(labor_cost/labor_income)) %>% 
    mutate(operational_cost = (total_cost/income)) %>% 
    mutate(labor_gross_cost = (labor_cost/labor_income)) %>% 
    mutate(difference = labor_income - labor_cost) %>% 
    mutate(net_result = difference - expenses) %>% 
    ungroup() %>% 
    mutate(year_month = as.yearmon(period, "%Y-%m")) %>% 
    relocate(.after = period,year_month) %>% 
    relocate(.after = difference, expenses)
  
  quarter_profit_resumen <- profit_resumen %>% 
    group_by(year_quarter) %>% 
    summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>% 
    mutate(operational_profit = 1-(total_cost/income)) %>% 
    mutate(labor_gross_profit = 1-(labor_cost/labor_income)) %>% 
    mutate(operational_cost = (total_cost/income)) %>% 
    mutate(labor_gross_cost = (labor_cost/labor_income)) %>% 
    ungroup() %>% 
    rowwise() %>% 
    mutate(net_result = sum(net_result)) %>% 
    ungroup() %>% relocate(.after = difference,expenses)
  
  income_distribution <- profitability  %>%
    filter(source == "income") %>% 
    group_by(year_quarter,customer) %>%
    summarise(amount = sum(amount),.groups = "drop") %>% 
    filter(amount != 0, !is.na(customer))
  
  expenses_distribution <- profitability %>%
    filter(source == "expense") %>% 
    group_by(year_quarter,account) %>%
    summarise(amount = sum(amount),.groups = "drop") %>% 
    arrange(desc(amount)) %>% 
    # separate(account, c("main_account","sub_account"),sep = "([:])") %>% 
    filter(amount != 0 )
  
  
  
  
  return(list(
    quarter_profit_resumen = quarter_profit_resumen,
    monthly_profit_resumen = profit_resumen,
    income_distribution = income_distribution,
    expenses_distribution = expenses_distribution,
    classified_transactions = profitability,
    accounts_dristribution=accounts_distribution,
    accounts_distribution_overview=overview
  ))
  
}


# Resource Execution ----------------------------------------------------------------


datap <- profit_estimator(i_date="2020-01-01",f_date = "2021-06-20")


# Local Storage 
date <- lubridate::today() %>% str_replace_all(.,"-","_")

# data %>%
#   openxlsx::write.xlsx(., paste0(date,"_Business_review.xlsx"),asTable = T)


monthly_profit_resumen <-  datap$monthly_profit_resumen

quarter_profit_resumen <- datap$quarter_profit_resumen

records <- datap$classified_transactions



# idea ::: create table in gt() with formatting and stylish to produce a Rmd.

# Graphics ----------------------------------------------------------------


# Gross Profit by Quarter 
quarter_gpc <- function(){
  
  
  f_quarter <- max(quarter_profit_resumen$year_quarter)
  i_quarter <- min(quarter_profit_resumen$year_quarter)
  
  
  quarter_gross_profit_chart <- quarter_profit_resumen %>%
    ggplot(aes(x = year_quarter, y = operational_profit))+
    geom_line()+
    geom_area(color = "#91bbbf",fill="#d7eff7")+
    geom_point(size = 3,color = "#cdaed6")+
    labs(title = "Quarterly Estimated Operational Profit", 
         subtitle = paste("Based on Quickbooks Historical Transactions",i_quarter,f_quarter),  
         caption = "Where: Estimated Operational Profit = Total Cost/Income")
  
  gpc <- quarter_gross_profit_chart + theme_wsj() + scale_colour_economist()
  
  stats <- quarter_profit_resumen %>%
    select(year_quarter,income,material_cost,labor_cost,
           labor_income,total_cost,operational_profit,
           labor_gross_profit,difference) %>% summary()
  
  print("Quarterly Gross Profit Estimation")
  
  return(list(chart=gpc,stats=stats))
}

# Gross Profit Chart
gpc <- function(){ 
  
  f_quarter <- max(quarter_profit_resumen$year_quarter)
  i_quarter <- min(quarter_profit_resumen$year_quarter)
  
  
  gross_profit_chart <- profit_resumen %>%
    ggplot(aes(x = year_month, y = operational_profit))+
    geom_line()+
    geom_area(color = "#91bbbf",fill="#d7eff7")+
    geom_point(size = 3,color = "#cdaed6")+
    labs(title = "Monthly Estimated Operational Profit", 
         subtitle = paste("Based on Quickbooks Historical Transactions",i_quarter,f_quarter),  
         caption = "Where: Estimated Operational Gross Profit = 1-(Total Cost/Income)")
  
  gpc <- gross_profit_chart + theme_wsj() + scale_colour_economist()
  
  stats <- profit_resumen %>%
    mutate(date = lubridate::make_date(year=substr(period,0,4),
                                       month = substr(period,6,7))) %>% 
    select(date,income,material_cost,labor_cost,
           labor_income,total_cost,operational_profit,
           labor_gross_profit,difference) %>% summary()
  
  print("Monthly Gross Profit Estimation")
  
  return(list(chart=gpc,stats=stats))
}

# Labor Gross Profit by Quarter 
quarter_lgpc <- function(){ 
  
  f_quarter <- max(quarter_profit_resumen$year_quarter)
  i_quarter <- min(quarter_profit_resumen$year_quarter)
  
  labor_gross_profit_chart <- quarter_profit_resumen %>%
    ggplot(aes(x = year_quarter, y = labor_gross_profit))+
    geom_line()+
    geom_area(color = "#91bbbf",fill="#d7eff7")+
    geom_point(size = 3,color = "#cdaed6")+
    labs(title = "Quarterly Estimated Labor Gross Profit", 
         subtitle = paste("Based on Quickbooks Historical Transactions",i_quarter,f_quarter),  
         caption = "Where: Labor Gross Profit = 1-(Labor Cost/Labor Income)")
  
  lgpc <- labor_gross_profit_chart + theme_wsj() + scale_colour_economist()
  
  stats <- quarter_profit_resumen %>%
    select(year_quarter,income,material_cost,labor_cost,
           labor_income,total_cost,operational_profit,
           labor_gross_profit,difference) %>% summary()
  
  print("Quarterly Labor Gross Profit Estimation")
  
  
  return(list(chart=lgpc,stats=stats))
}

# Labor Gross Profit Chart 
lgpc <- function(){ 
  
  f_quarter <- max(quarter_profit_resumen$year_quarter)
  i_quarter <- min(quarter_profit_resumen$year_quarter)
  
  
  labor_gross_profit_chart <- profit_resumen %>%
    ggplot(aes(x = year_month, y = labor_gross_profit))+
    geom_line()+
    geom_area(color = "#91bbbf",fill="#d7eff7")+
    geom_point(size = 3,color = "#cdaed6")+
    labs(title = "Monthly Estimated Operational Profit", 
         subtitle = paste("Based on Quickbooks Historical Transactions",i_quarter,f_quarter),  
         caption = "Where: Labor Gross Profit = 1-(Labor Cost/Labor Income)")
  
  lgpc <- labor_gross_profit_chart + theme_wsj() + scale_colour_economist()
  
  stats <- profit_resumen %>%
    mutate(date = lubridate::make_date(year=substr(period,0,4),
                                       month = substr(period,6,7))) %>% 
    select(date,income,material_cost,labor_cost,
           labor_income,total_cost,operational_profit,
           labor_gross_profit,difference) %>% summary()
  
  print("Monthly Labor Gross Profit Estimation")
  
  
  return(list(chart=lgpc,stats=stats))
}

# Monthly Net Profit Chart
netpc <- function(){ 
  
  f_quarter <- max(quarter_profit_resumen$year_quarter)
  i_quarter <- min(quarter_profit_resumen$year_quarter)
  
  
  net_profit_chart <- profit_resumen %>%
    ggplot(aes(x = year_month, y = net_result))+
    geom_col(color = "#91bbbf",fill="#d7eff7")+
    labs(title = "Monthly Estimated Net Profit", 
         subtitle = paste("Based on Quickbooks Historical Transactions",i_quarter,f_quarter),  
         caption = "Where: Estimated Operational Gross Profit = 1-(Total Cost/Income)")
  
  netpc <- net_profit_chart + theme_wsj() + scale_colour_economist()
  
  stats <- profit_resumen %>%
    mutate(date = lubridate::make_date(year=substr(period,0,4),
                                       month = substr(period,6,7))) %>% 
    select(date,income,material_cost,labor_cost,
           labor_income,total_cost,operational_profit,
           labor_gross_profit,difference) %>% summary()
  
  print("Monthly Gross Profit Estimation")
  
  return(list(chart=netpc,stats=stats))
}

# Monthly Net Profit Chart
quarter_netpc <- function(){ 
  
  f_quarter <- max(quarter_profit_resumen$year_quarter)
  i_quarter <- min(quarter_profit_resumen$year_quarter)
  
  
  net_profit_chart <- quarter_profit_resumen %>%
    ggplot(aes(x = year_quarter, y = net_result))+
    geom_col(color = "#91bbbf",fill="#d7eff7")+
    labs(title = "Quarterly Estimated Net Profit", 
         subtitle = paste("Based on Quickbooks Historical Transactions",i_quarter,f_quarter),  
         caption = "Where: Estimated Net Profit = 1-(Total Cost/Income)")
  
  netpc <- net_profit_chart + theme_wsj() + scale_colour_economist()
  
  stats <- profit_resumen %>%
    mutate(date = lubridate::make_date(year=substr(period,0,4),
                                       month = substr(period,6,7))) %>% 
    select(date,income,material_cost,labor_cost,
           labor_income,total_cost,operational_profit,
           labor_gross_profit,difference) %>% summary()
  
  print("Monthly Gross Profit Estimation")
  
  return(list(chart=netpc,stats=stats))
}

# Monthly Total Income Vs Total Cost
netdif <- function(){ 
  
  f_quarter <- max(quarter_profit_resumen$year_quarter)
  i_quarter <- min(quarter_profit_resumen$year_quarter)
  
  
  dif = ggplot(profit_resumen %>% filter(year_quarter > "2020 Q1"), aes(x=year_month)) + 
    geom_line(aes(y = total_cost), color = "darkred",size = 2) + 
    geom_line(aes(y = income), color="steelblue",size = 2, linetype="twodash")+
    labs(title = "Income vs Total Cost", 
         subtitle = paste("Based on Quickbooks Historical Transactions",i_quarter,f_quarter),  
         caption = "Where: Income = Total Invoiced, Cost = Materials + Labor Costs")
  
  netdif <- dif + theme_wsj() + scale_colour_economist()
  
 
  print("Monthly Income Vs Cost")
  
  return(list(chart=netdif))
}

# Monthly Labor Income Vs Labor Cost
netdif_lab <- function(){ 
  
  f_quarter <- max(quarter_profit_resumen$year_quarter)
  i_quarter <- min(quarter_profit_resumen$year_quarter)
  
  
  dif = ggplot(profit_resumen %>% filter(year_quarter > "2020 Q1"), aes(x=year_month)) + 
    geom_line(aes(y = labor_cost), color = "darkred",size = 2) + 
    geom_line(aes(y = labor_income), color="steelblue",size = 2, linetype="twodash")+
    labs(title = "Labor Income vs Labor Cost", 
         subtitle = paste("Based on Quickbooks Historical Transactions",i_quarter,f_quarter),  
         caption = "Where: Labor Income = Invoiced - Material Cost ")
  
  netdif <- dif + theme_wsj() + scale_colour_economist()
  
  
  print("Monthly Labor Income Vs Labor Cost")
  
  return(list(chart=netdif))
}

# Monthly Labor Income Vs Labor Cost
total_net <- function(){ 
  
  f_quarter <- max(quarter_profit_resumen$year_quarter)
  i_quarter <- min(quarter_profit_resumen$year_quarter)
  
  
  dif = ggplot(profit_resumen %>% filter(year_quarter > "2020 Q1"), aes(x=year_month)) + 
    geom_col(aes(y = net_result), color="steelblue",size = 2, linetype="twodash",fill="steelblue")+

    
    labs(title = "Result", 
         subtitle = paste("Based on Quickbooks Historical Transactions",i_quarter,f_quarter),  
         caption = "Where: Labor Income = Invoiced - Material Cost ")
  
  netdif <- dif + theme_wsj() + scale_colour_economist()
  
  
  print("Monthly Labor Income Vs Labor Cost")
  
  return(list(chart=netdif))
}







stats <- gpc()[[2]]
stats %>% openxlsx::write.xlsx(.,"stats.xlsx")







# Expenses Analysis  ------------------------------------------------------



expenses_analysis <- function(limit){ 
  


expend <- data_master() %>% 
  filter(source == "expense") %>% 
  select(-source)
  

vendors <- expend %>%
  filter(date>=  limit) %>% 
  filter(account != "Ask Accountant") %>% 
  select(account,name) %>% distinct() %>% 
  separate(account,c("main_account","sub_account"),sep="([:])") %>%
  mutate(sub_account = ifelse(is.na(sub_account),
                              main_account,
                              sub_account)) %>%
  mutate(name = ifelse(is.na(name),"-",name)) %>%
  distinct() %>%
  group_by(main_account, sub_account) %>% 
  summarise(vendor = str_flatten(name,collapse = ","), .groups = "drop") %>%
  ungroup()


expenses_distribution <- expend %>% 
  group_by(year_month,account) %>% 
  summarise(amount = sum(amount),
            .groups = "drop") %>% 
  pivot_wider(names_from = year_month, values_from = amount) %>% 
  separate(account, c("main_account","sub_account"),sep = "([:])") %>% 
  ungroup() %>%
  janitor::clean_names() %>% 
  mutate_all(funs(replace(., is.na(.), 0))) %>% 
  group_by(main_account,sub_account) %>% 
  summarise_if(is.numeric, sum, na.rm = TRUE) %>% 
  ungroup() %>% 
  filter(main_account != "Ask Accountant")



last_period <- as.character(year(today()) - 1 )

averages_last_period <- expenses_distribution %>% 
  select(main_account,sub_account,contains(last_period)) %>% 
  rowwise() %>% 
  mutate(all_period_sum = rowSums(across(where(is.numeric)))) %>% 
  mutate("2020_average" = all_period_sum/12) %>% 
  select(main_account, sub_account, "2020_average")
  

complete <- expenses_distribution %>% 
  select(main_account,sub_account,contains("_2021")) %>% 
  left_join(averages_last_period,by = c("main_account","sub_account")) %>% 
  relocate(.after = sub_account, "2020_average") %>% 
  mutate(sub_account = ifelse(sub_account == 0, main_account,sub_account)) %>% 
  left_join(vendors, by =c("main_account","sub_account")) %>% 
  filter(!is.na(vendor))
  # filter(!sub_account %in% c("Automobile Expense",
  #                            "Loan","Renting","Bad Debts"))

period_observe <- expend %>%
  filter(year_month >  as.yearmon( today()-months(3))) %>% 
  distinct(year_month) %>% 
  pull() %>%
  str_to_lower() %>%
  str_replace_all(., " ", "_")


complete_convex<- complete %>% 
  select(sub_account,contains(period_observe),-vendor) %>% 
  mutate(no_zeros = rowSums(. == 0)) %>% 
  mutate(all_period_sum = rowSums(across(where(is.numeric)))) %>% 
  mutate(test = all_period_sum < 100) %>% 
  filter(no_zeros <3 ) %>% filter(test == 0)
  
complete_cleaned <- complete %>%
  filter(sub_account %in% complete_convex$sub_account) %>% 
  filter(sub_account != "Automobile Expense")%>% 
  mutate(t = main_account == sub_account) %>% 
  mutate(number_v = str_count(vendor,",")) %>% 
  mutate(number_vendors = number_v+1) %>% 
  select(-t,-number_v)


expenses_month <- 
  expenses_distribution %>% 
  group_by(main_account) %>% 
  summarise_if(is.numeric, sum, na.rm = TRUE) %>% 
  pivot_longer(!main_account,
               names_to = "period",
               values_to = "amount") %>% 
  filter(main_account != "Ask Accountant") %>% 
  ungroup() 
  

# zoom into details: 

account_dist <- function(account_name){ 

account_zoom <- expend %>% 
  filter(grepl(account_name,account)) %>%
  group_by(year_month,account) %>%
  summarise(amount = sum(amount),.groups = "drop") %>% 
  pivot_wider(names_from = year_month, values_from = amount) %>% 
  mutate_all(funs(replace(., is.na(.), 0))) %>%
  rowwise() %>% 
  mutate(all_period_sum = rowSums(across(where(is.numeric))))%>% 
  arrange(desc(account,all_period_sum)) %>% 
  ungroup()


return(account_zoom)
  
}

data_accounts <- map(expenses_distribution$main_account, account_dist) %>% 
                 map_dfr(., bind_rows) %>% distinct() %>% 
                 janitor::clean_names()%>%   
                 mutate_all(funs(replace(., is.na(.),0))) %>% 
                 distinct() %>% 
                 separate(account, c("main_account",
                                     "sub_account",
                                     "spec_account"),sep = "([:])") 

                 



return(list(expend=expend,
            budget = complete_cleaned,
            data_accounts = data_accounts,
            expenses_month=expenses_month)) 

}



datae <- expenses_analysis("2020-01-01")
datae %>% openxlsx::write.xlsx(.,"expenses_analysis.xlsx",asTable = T)




# Vendors  ----------------------------------------------------------------



vend_rolling_window <- function(initial,final,future_idate,future_fdate){ 

# new vendors in one month period window 
vendor_vector <- expend %>% 
  filter(date <= as.Date(initial)) %>% 
  distinct(name,account) %>% na.omit()


not_classics <- expend %>% 
  filter(between(date,as.Date(future_idate) ,
                      as.Date(future_fdate)))%>% 
  select(date,type,name,account,amount) %>% 
  mutate(classic_vendor = name %in% vendor_vector$name) %>% 
  mutate(classic_account = name %in% vendor_vector$account) %>% 
  filter(classic_vendor == 0) %>%
  filter(classic_vendor == 0) %>%
  na.omit()

return(not_classics)

}


unclassics <- function(){
  
time_windows <- expend %>%
  select(date) %>%
  mutate(date2 = date%m-% months(1)) %>%
  mutate(to_first = rollback(date,roll_to_first = T))%>% 
  mutate(to_last  = rollback(date,roll_to_first = F)) %>% 
  select(to_first,to_last) %>% 
  distinct() %>% 
  mutate( to_first = to_first %m-% months(1)) %>% 
  rename(final = to_last,
         initial = to_first) %>% 
  mutate(future_idate = initial %m+% months(1)) %>% 
  mutate(future_fdate = final %m+% months(1)) %>% 
  arrange(desc(initial)) %>% 
  mutate_if(is.Date, as.character) %>% 
  filter(initial >= "2020-01-01") 
  # split(., seq(nrow(.)))
  # split(.,seq(ncol(.)))

  
  
divergencies <- pmap(
                    list( 
                    time_windows$initial,
                    time_windows$final,
                    time_windows$future_idate,
                    time_windows$future_fdate),
                    .f=vend_rolling_window) %>% 
                map_dfr(., bind_rows) %>% distinct() %>% 
                # select(-classic_vendor,-classic_account)%>% 
                mutate(year_month = as.yearmon(date))


return(divergencies)

}


# State of Account --------------------------------------------------------



state_of_account <- function(clue){ 
  
  openb <- invoices %>% 
    filter(grepl(clue,name)) %>% 
    select(-memo) %>% 
    mutate(open_balance = as.double(open_balance)) %>% 
    select(-creation_date, - modification_date,-created_by) %>% 
    mutate(project = clue)
  
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
    select(-creation_date, - modification_date,-created_by) %>% 
    mutate(project = clue)
  
  payments <- transact %>% 
    filter(type == "Payment") %>% 
    filter(grepl(clue,customer)) %>% 
    filter(amount>0)%>% 
    select(date,type,name,account,amount) %>% 
    mutate(project = clue)
  
  
  
  return(list(openb = openb,
              payments = payments,
              detailed = detailed
  ))
  
}


# 421 SH House state of account up to date. 

statement <- state_of_account("421")$openb %>% select(-due_date) %>% 
  separate(name, c("client","project"),sep = "([:])") %>%
  mutate(amount_paid = amount - open_balance) %>% 
  janitor::clean_names(case = "title") %>% 
  rename(Reference = Num) %>% 
  mutate(Service = case_when(str_detect(Project,"421 S Harbor")~"Stucco",
                             str_detect(Project,"Additional")~"Additionals",
                             str_detect(Project,"Painting")~"Painting",
                             TRUE ~ as.character(Project))) %>% 
  arrange(Project,Date) %>% relocate(.before = Amount,Service) %>% select(-Type) %>% 
  mutate(Client = ifelse(Client == "Greenhaus Design-build, Inc.","Greenhaus",Client)) 




# To Do: 

# Create a list of each check received by Greenhaus and Furshman,
# Merge all checks into a PDF, calculate the total amount received 
# re-balance the total amount paid between the different invoices 
# re-run the statement and resume randyf_statement.docx
# send email and call. 


global_state <- function(clue){ 
  
  openb <- invoices %>% 
    filter(grepl(clue,name)) %>% 
    select(-memo) %>% 
    mutate(open_balance = as.double(open_balance)) %>% 
    select(-creation_date, - modification_date,-created_by) %>% 
    mutate(project = clue)
  
  payments <- transact %>% 
    filter(type == "Payment") %>% 
    filter(grepl(clue,customer)) %>% 
    filter(amount>0)%>% 
    select(date,type,name,account,amount) %>% 
    mutate(project = clue)
  
  smart_object <-  tibble(
    project = clue,
    invoiced = sum(openb$amount),
    total_paid = sum(payments$amount),
    diff_invoiced_paid = (sum(openb$amount)-sum(payments$amount)),
    open_balance = sum(openb$open_balance),
    logical_match = round(sum(openb$amount)-sum(payments$amount)) == round(sum(openb$open_balance)))
  
  
  return(smart_object)
  
}

perspective <- function(distance){ 
  
cut_date = rollback(today()-months(distance),roll_to_first = T)

print(cut_date)

running_pj <- transact %>% 
  filter(date >= cut_date ) %>% 
  replace_na(list(amount = 0 )) %>% 
  group_by(customer) %>% 
  summarise(amount = sum(amount)) %>% 
  arrange(desc(amount)) %>% 
  mutate(proport = amount/sum(amount)) %>%
  mutate(pareto = cumsum(proport))  
  # filter(pareto >= .8) 

divergencies <- map(running_pj$customer, global_state) %>% 
                map_dfr(., bind_rows) %>% distinct() %>% 
                janitor::clean_names()%>%   
                mutate_all(funs(replace(., is.na(.),0))) %>% 
                distinct() %>% arrange(desc(open_balance))


return(divergencies)


}

sh_pjs <- perspective(5) %>% 
  filter(grepl("421",project))

mismatch <- sh_pjs %>% 
  filter(logical_match == 0) %>% 
  pull(project)



