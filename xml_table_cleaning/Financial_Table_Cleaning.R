library(tidyverse)
library(xml2)
library(XML)

# Read in the financial report spreadsheet
df <- read.csv("SAR_FR.csv", stringsAsFactors = FALSE)

# use the map function to apply the read_xml function to each table,
# then only extract the rows from the table
# after rows are extracted, use unnest to separate by row then remove the original and test column

df <- df %>% 
  mutate(xml_table = map(table, read_xml)) %>%
  #mutate(xml_table = map(table2, xml_child, 1)) %>%
  mutate(rows = map(xml_table, xml_find_all, "//Row")) %>%
  mutate(rows = map(rows, as.character)) %>%
  unnest(rows) %>%
  select(-c(table, xml_table))
  
# clean the column and replace certain strings with a delimiter
df$rows <- str_remove_all(df$rows, "<Row>")
df$rows <- str_trim(df$rows, side = "left")
df$rows <- str_replace_all(df$rows, "</Expense-Category>", "SPLIT")
df$rows <- str_replace_all(df$rows, "</Date>", "SPLIT")
df$rows <- str_replace_all(df$rows, "</Description>", "SPLIT")

# use the separate function to delineate the column to create 4 new columns
df <- df %>% 
  separate(rows, c("category", "date", "description", "amount"), sep = "SPLIT")

#clean up the new columns by removing the unnecessary strings
df$category <- str_remove_all(df$category, "<Expense-Category>")
df$date <- str_remove_all(df$date, "<Date>")
df$description <- str_remove_all(df$description, "<Description>")
df$amount <- str_remove_all(df$amount, "<Amount>")
df$amount <- str_remove(df$amount, "</Row>")
df$amount <- str_remove_all(df$amount, "</Amount>")

write.csv(df, "cleaned.Data.csv", row.names = FALSE)