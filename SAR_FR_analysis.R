library(dplyr)
library(qdapRegex)
library(stringr)
library(tidyr)
library(reshape2)

sar_fr <- read.csv("finance_table.csv", stringsAsFactors = FALSE)

#rename the column that contains the financial report
colnames(sar_fr)[4] <- "expenses"

sar_fr <- sar_fr %>% filter(expenses != "")

#split value column at the "-"
ncols <- max(str_count(sar_fr$Grant.ID, "-")) + 1
colmn <- paste("col", 1:ncols)
sar_fr <- cbind(sar_fr, colsplit(sar_fr$Grant.ID, "-", colmn))

sar_fr <- sar_fr %>% select(-c(Type, Status, Grant.ID, 'col 1'))

colnames(sar_fr)[3] <- "FY"

#to remove tabs
sar_fr$expenses <- str_remove_all(sar_fr$expenses, "\\t")
#remove the ?
sar_fr$expenses <- str_remove_all(sar_fr$expenses, "\\?")
#this is to remove any quotes
sar_fr$expenses <- str_remove_all(sar_fr$expenses, regex("\""))

#remove any/change unnecessary characters
sar_fr$expenses <- str_remove_all(sar_fr$expenses, "<xml version=1.0 encoding=UTF-8><worksheet>")
sar_fr$expenses <- str_remove_all(sar_fr$expenses, "<Expenses-Reporting-Period>")
sar_fr$expenses <- str_remove_all(sar_fr$expenses, "<Row>")
sar_fr$expenses <- str_replace_all(sar_fr$expenses, "<Expense-Category>", "Category: ")
sar_fr$expenses <- str_replace_all(sar_fr$expenses, "</Expense-Category>", " HERE") #edit this to split column
sar_fr$expenses <- str_replace_all(sar_fr$expenses, "<Date>", "Date: ")
sar_fr$expenses <- str_replace_all(sar_fr$expenses, "</Date>", " HERE") #edit this to split column
sar_fr$expenses <- str_replace_all(sar_fr$expenses, "<Description>", "Description: ")
sar_fr$expenses <- str_replace_all(sar_fr$expenses, "</Description>", " HERE") #edit this to split column
sar_fr$expenses <- str_replace_all(sar_fr$expenses, "<Amount>", "Amount: ")
sar_fr$expenses <- str_remove_all(sar_fr$expenses, "</Amount>")
sar_fr$expenses <- str_replace_all(sar_fr$expenses, "<Date/>", "Date, HERE") #edit this to split column
sar_fr$expenses <- str_replace_all(sar_fr$expenses, "<Description/>", "Description, HERE") #edit this to split column
sar_fr$expenses <- str_replace_all(sar_fr$expenses, "<total>", " split") #split here to remove unnecessary characters


#split expenses column at the word "split"
ncols <- max(str_count(sar_fr$expenses, "split")) + 1

#generate necessary column names
colmn <- paste("col", 1:ncols)

#separate the GrantID to get the year
sar_fr <- cbind(sar_fr, colsplit(sar_fr$expenses, "split", colmn))
#remove unnesscary colum
sar_fr <- sar_fr %>% select(-`col 2`)

colnames(sar_fr)[4] <- "Split_this"

rm(list = c("colmn", "ncols"))

#reduce the white space in resident table
sar_fr$Split_this <- str_squish(sar_fr$Split_this)

#split expenses column at the word "split"
ncols <- max(str_count(sar_fr$Split_this, "</Row>")) + 1
colmn <- paste("col", 1:ncols)

#separate again but at "</Row>"
sar_fr <- cbind(sar_fr, colsplit(sar_fr$Split_this, "</Row>", colmn))
rm(list = c("colmn", "ncols"))

sar_fr <- sar_fr %>% select(-c(expenses, Split_this))

 #use pivot longer to transpose data

fr_long <- sar_fr %>% pivot_longer(cols = starts_with("col"),
                               names_to = "financial_Table",
                               values_to = "financial_data",
                               values_drop_na = TRUE) %>% 
  filter(financial_data != "")

#remove original dataset
rm(sar_fr)

#remove some words before split
fr_long$financial_data <- str_remove_all(fr_long$financial_data, "Category: ")
fr_long$financial_data <- str_remove_all(fr_long$financial_data, "Date: ")
fr_long$financial_data <- str_remove_all(fr_long$financial_data, "Description: ")
fr_long$financial_data <- str_remove_all(fr_long$financial_data, "Amount: ")


#split value column at the word "HERE"
ncols <- max(str_count(fr_long$financial_data, "HERE")) + 1
colmn <- paste(c("Category", "Date", "Description", "Amount"))

#separate again but at "</Row>"
fr_long <- cbind(fr_long, colsplit(fr_long$financial_data, "HERE", colmn))
rm(list = c("colmn", "ncols"))

#rename new columns "Category"

#reduce the white space in category column
fr_long$Category <- str_squish(fr_long$Category)
fr_long$Organization <- str_squish(fr_long$Organization)

#remove this column
fr_long <- fr_long %>% select(-c(financial_data, financial_Table))

#create new column for disicipline
fr_long$Discipline <- ifelse(grepl("Family", fr_long$Organization), "FM",
                       ifelse(grepl("Emergency", fr_long$Organization), "EM", 
                              ifelse(grepl("Internal", fr_long$Organization), "IM", 
                                     ifelse(grepl("Obstet", fr_long$Organization), "OBGYN", 
                                            ifelse(grepl("Pediat", fr_long$Organization), "Peds",
                                                   ifelse(grepl("Child", fr_long$Organization), "Peds",NA))))))


fr_long <- fr_long %>% select(c(Organization, Discipline, FY, Category, Description, Amount))


#group summary statistics 
summary.x <- fr_long %>%
  group_by(Discipline, FY, Category) %>% 
  summarize(total = sum(Amount)) %>% 
  arrange(FY)



write.csv(fr_long, "sar_financial_report.csv", row.names = FALSE)
