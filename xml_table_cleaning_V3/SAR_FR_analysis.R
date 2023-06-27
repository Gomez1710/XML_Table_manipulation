library(tidyverse)
library(openxlsx)
library(xml2)

#Read in financial data from the 1.data folder
sar_fr <- read.csv("../1.Raw Data/SAR_FR.csv")

#rename column that contains the financial table
colnames(sar_fr)[9] <- "financial.table"

#############################################################################################
# Using the map function, apply the xml_read on all tables, extract only the nodes and then convert to a character
# once convert, use the unnest function to separte by line item.
sar_fr <- sar_fr %>% 
  mutate(financial.table = map(financial.table, read_xml),
         financial.table = map(financial.table, xml_find_all, "//Row"),
         financial.table = map(financial.table, as.character)) %>% 
  unnest(financial.table) %>% 
  
  # Since each line item is in a XML format, apply the read_xml function again
  mutate(financial.table = map(financial.table, read_xml),
         
         # next create a new columns and use xml_find_all to find each node(child). For example the first node is "Expense-Category"
         Expense_category = map(financial.table, xml_find_all, "//Row/Expense-Category"),
         # next use xml_text to convert to a character. this will convert all into a list
         Expense_category = map(Expense_category, xml_text),
         # the last step is to apply the unlist function to make the list into a vector. Repeat the 3 steps until all data is extracted
         Expense_category = map_chr(Expense_category, unlist),
         
         Date = map(financial.table, xml_find_all, "//Row/Date"),
         Date = map(Date, xml_text),
         Date = map_chr(Date, unlist),
         
         Description = map(financial.table, xml_find_all, "//Row/Description"),
         Description = map(Description, xml_text),
         Description = map_chr(Description, unlist),
         
         Amount = map(financial.table, xml_find_all, "//Row/Amount"),
         Amount = map(Amount, xml_text),
         Amount = map_chr(Amount, unlist)) %>% 
  select(-financial.table)

# convert amount column to double to keep decimals and convert to an integer
sar_fr$Amount <- as.double(sar_fr$Amount)

write.xlsx(sar_fr, "../3.Cleaned Data/SAR_Financial_Report.xlsx", overwrite = TRUE)
