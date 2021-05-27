
# Profit Estimator --------------------------------------------------------

# Task; create a long table of profits, based on transactions; where:: 

# Total Income(988) - Direct Material Cost(224) = Income Labor
# Total Cost(588) - Direct Material Cost(224)   = Labor Cost 
# 
# Labor Cost/ Income Labor = Labor Gross Profit 

# group by date, and bring the option to observe it by month, quarter & year

# Total Income         <-  type = invoices & account = Income 
# Direct Material Cost <-  account = DM
# Cost of Goods Sold   <-  account %in% COGDS & DM 


# Download and clean data 

transact <- cleaner("transactions")
exp      <- cleaner("expenses")


library(tidyverse)
library(lubridate)
library(zoo)

setwd("C:/Users/andre/Downloads")


profit_estimator <- function(i_date,f_date){ 
  
  
  expenses_vector <- exp %>%  distinct(account) %>% pull(account)
  
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
  cross_tibble_raw <- operational_income %>% 
    bind_rows(material_income,
              direct_labor,
              direct_material,
              billable_income,
              expenses)%>% 
    mutate(year_month = as.yearmon(date, "%Y-%m")) %>% 
    mutate(year_quarter = as.yearqtr(date, format = "%Y-%m-%d"))
  
  
  # Detailed Transacts classified
  
  # Acuracy contrasted using 2 diff methods.
  profitability <-  cross_tibble_raw %>% 
    replace_na(list(amount = 0)) %>% 
    mutate(source = case_when(str_detect(account,"Income:")~"income",
                              str_detect(account,"Billable")~"income",
                              str_detect(account,"Direct Labor")~"labor_cost",
                              str_detect(account,"Direct Material")~"material_cost",
                              TRUE ~ as.character(account)))%>%
    # mutate(source = ifelse(type == "Invoice","income",source)) %>% 
    # mutate(amount = ifelse(source == "income" & amount < 0,amount*-1,amount)) %>% 
    select(-modification_date) %>% 
    mutate(source = ifelse(account %in% expenses_vector,"expense",source))
  
  
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
    resumen = profit_resumen,
    income_distribution = income_distribution,
    expenses_distribution = expenses_distribution,
    classified_transactions = profitability,
    accounts_dristribution=accounts_distribution,
    accounts_distribution_overview=overview
  ))
  
}




# Resource Execution ----------------------------------------------------------------


data <- profit_estimator("2019-01-01","2021-05-26")


# Local Storage 
date <- lubridate::today() %>% str_replace_all(.,"-","_")

data %>%
  openxlsx::write.xlsx(., paste0(date,"_Business_review.xlsx"),asTable = T)


profit_resumen = data$resumen

quarter_profit_resumen <- data$quarter_profit_resumen

records <- data$classified_transactions



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


stats <- gpc()[[2]]
stats %>% openxlsx::write.xlsx(.,"stats.xlsx")




# Expenses Analysis  ------------------------------------------------------



expenses_analysis <- function(){ 

expend <- records %>%
   filter(between(date,as.Date("2021-01-01"),
                  as.Date("2021-04-30"))) %>% 
  filter(source == "expense")


expenses_distribution <- expend %>%
  group_by(year_month,account) %>% 
  summarise(amount = sum(amount),.groups = "drop") %>% 
  pivot_wider(names_from = year_month, values_from = amount) %>% 
  separate(account, c("main_account","sub_account"),sep = "([:])") %>% 
  ungroup() %>%
  janitor::clean_names() %>% 
  replace_na(list(main_account = "unknown",sub_account="none",
                  jan_2021 = 0, feb_2021 = 0, mar_2021 = 0, apr_2021 = 0)) %>% 
  group_by(main_account) %>% 
  summarise(jan_2021 = sum(jan_2021),feb_2021 = sum(feb_2021),
            mar_2021 = sum(mar_2021),apr_2021= sum(apr_2021)) %>% 
  filter(main_account != "Ask Accountant") %>% 
  ungroup() 


  
expenses_distribution_comp <- records %>%
  filter(date >= "2020-01-01") %>% 
  filter(source == "expense") %>%
  group_by(year_month,account) %>% 
  summarise(amount = sum(amount),.groups = "drop") %>% 
  pivot_wider(names_from = year_month, values_from = amount) %>% 
  separate(account, c("main_account","sub_account"),sep = "([:])") %>% 
  ungroup() %>%
  janitor::clean_names() %>% 
  mutate_all(funs(replace(., is.na(.), 0))) %>% 
  group_by(main_account) %>% 
  summarise_if(is.numeric, sum, na.rm = TRUE) %>% 
  ungroup() %>% 
  filter(main_account != "Ask Accountant")

expenses_month <- 
  expenses_distribution_comp %>% 
  pivot_longer(!main_account,
               names_to = "period",
               values_to = "amount") %>% 
  filter(main_account != "Ask Accountant") %>% 
  ungroup() 
  

# zoom into details: 

account_dist <- function(account_name){ 

account_zoom <- records %>% 
  filter(between(date, as.Date("2021-01-01"),
                       as.Date("2021-04-30"))) %>% 
  filter(source == "expense") %>%
  filter(grepl(account_name,account)) %>%
  group_by(year_month,account) %>%
  summarise(amount = sum(amount),.groups = "drop") %>% 
  pivot_wider(names_from = year_month, values_from = amount) %>% 
  mutate_all(funs(replace(., is.na(.), 0))) 

return(account_zoom)
  
}

data_accounts <- map(expenses_distribution$main_account, account_dist) %>% 
                 map_dfr(., bind_rows) %>% distinct()                                



return(list(expend=expend,
            data_accounts = data_accounts,
            expenses_month=expenses_month,
            expenses_distribution=expenses_distribution,
            expenses_distribution_comp=expenses_distribution_comp)) 

}


data <- expenses_analysis()
data %>% openxlsx::write.xlsx(.,"expenses_analysis.xlsx",asTable = T)