# Connection to ZOHO database. 

library(tidyverse)
library(RODBC)
library(odbc)

# ZOHO
conn <- odbcConnect("zoho_cdata")
table_det <-  sqlTables(conn)

con <- DBI::dbConnect(odbc(), "zoho_cdata")
tables <- dbListTables(con)


deals  <-  tbl(con, "Deals")
stages  <-  tbl(con, "StageHistories")





# deals <- sqlQuery(conn, "SELECT *, FROM Deals")

custom <- tbl(con,"CustomFieldHistoryTracking") %>% head()

custom <- sqlQuery(conn, "SELECT ModuleName, FROM CustomFieldHistoryTracking, WHERE ModuleName = CustomModule5001")

custom <- sqlQuery(conn, "SELECT FIRST(ModuleName,EntityId,FieldName,HistoryName) FROM CustomFieldHistoryTracking;")


