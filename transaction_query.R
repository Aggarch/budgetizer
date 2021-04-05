
# Query people, expenses and specific accounts 


library(tidyverse)

data <- openxlsx::read.xlsx("C:/Users/Hp/Desktop/transaccion_list.xlsx") %>% as_tibble

data <- data[-(1:4),]

colnames(data) <- c('check','date',"type","num","posting",
                    "name","memo","account","split","amount")

patterns <- c("Victor Pino|Rogelio Perez|Camila Calcetero|Andres Eduardo Garcia|David Eduardo Garzon|Carolina Parra")

data <- data %>% filter(grepl(paste(patterns),split)) %>% select(-check)


data %>% mutate(name = case_when(str_detect(split, "Victor")~"Victor",
                                 str_detect(split,"Rogelio")~"Rogelio",
                                 str_detect(split,"Camila")~"Camila",
                                 str_detect(split,"Carolina")~"Carolina",
                                 str_detect(split,"David")~"David",
                                 str_detect(split,"Andres")~"Andres")) %>%
  mutate(class = ifelse(name %in% c("Victor","Rogelio"),"Bosses","Admins")) %>%
  select(-split,-memo) %>% mutate(num = as.numeric(num)) %>%
  mutate(amount = str_remove(amount,"-")) %>%
  mutate(amount = str_remove(amount,",")) %>%
  mutate(amount = as.numeric(amount)) %>%
  mutate(dateFixed = as.Date(date, tryFormats = c("%Y-%m-%d", "%m/%d/%Y"),optional = F))-> report


report %>% filter(!name %in% c("Victor", "Rogelio")) %>%
  filter(lubridate::year(dateFixed) >= 2021) %>% select(-num,-posting,-date) %>%
  rename(date = dateFixed) %>% relocate(.before = type,date)-> office



#"original <- c(project status conditions date dateFixed verification type num service name split subpayment retainage check)
