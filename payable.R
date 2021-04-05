
# Accounts Payable 

library(tidyverse)
setwd("C:/Users/andre/Downloads")

data <- openxlsx::read.xlsx("accounts_payable.xlsx") %>% as_tibble() %>% 
      mutate(date = as.Date(date, tryFormats = c("%Y-%m-%d", "%m/%d/%Y"),optional = F),
             due_date = as.Date(due_date, tryFormats = c("%Y-%m-%d", "%m/%d/%Y"),optional = F))


openxlsx::write.xlsx(data, "accounts_payable.xlsx")
