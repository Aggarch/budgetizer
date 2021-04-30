
library(tidyverse)
setwd("C:/Users/andre/Downloads")

account <- openxlsx::read.xlsx("checking.xlsx") %>% as_tibble() %>% 
  select(date = FECHA, ref = "DESCRIPCIÓN", amount = VALOR) %>% 
  mutate(date = as.Date(date, origin = "1899-01-01"))


income <- account %>% 
  filter(amount > 0) %>% 
  filter(grepl("INTERNA",ref)) %>% 
  mutate(month = lubridate::month(date))
