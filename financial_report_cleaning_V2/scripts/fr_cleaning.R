library(tidyverse)
library(openxlsx)
library(xml2)
library(showtext)
library(here)

font_add_google("Montserrat", "Montserrat")

# read in financial data 
ar_fr <- read.csv(here("raw/AR_FR.csv"), stringsAsFactors = FALSE)


#############################################################################################
# Using the map function, apply the xml_read on all tables, extract only the nodes and then convert to a character
# once convert, use the unnest function to separte by line item.
ar_fr <- ar_fr %>% 
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
         
         Date = map(financial.table, xml_find_all, "//Row/Date"),
         Date = map(Date, xml_text),
         
         Description = map(financial.table, xml_find_all, "//Row/Description"),
         Description = map(Description, xml_text),
         
         Amount = map(financial.table, xml_find_all, "//Row/Amount"),
         Amount = map(Amount, xml_text)) %>%
  
  select(-financial.table)

# convert amount column to double to keep decimals and convert to an integer
ar_fr$Amount <- as.double(ar_fr$Amount)

write.xlsx(ar_fr, here("rodeo/AR_Financial_Report.xlsx"), overwrite = TRUE)

################################################################################################################

# create ggplot of the top 5 expense categories 

fr_summary <- ar_fr %>% 
  group_by(Residency.Program.Discipline, Expense_category) %>% 
  summarise(Amount_expended = sum(Amount))

fr_summary <- as.data.frame(fr_summary)

fr_summary$Expense_category <- str_wrap(fr_summary$Expense_category, width = 35)

fr_summary <- fr_summary %>% filter(Expense_category != "Unused Funds")

fr_summary$text <- scales::dollar(fr_summary$Amount_expended, big.mark = ",")


showtext_auto()

ymid <- mean(range(fr_summary$Amount_expended))

fr_graph <- ggplot(fr_summary) +
  geom_bar(aes(Expense_category, Amount_expended, fill = Residency.Program.Discipline), stat = "identity") +
  scale_fill_manual(values = c("#65081F",
                               "#10C637",
                               "#FF982C",
                               "#DFE0DE",
                               "#18C4D6",
                               "#042B46")) +
  geom_text(data = fr_summary, aes(x = Expense_category, label = text, y = Amount_expended),
            hjust = ifelse(fr_summary$Amount_expended < ymid, -0.1, 1.1),
            color = "black",
            family = "Montserrat",
            fontface = "bold") +
  scale_y_continuous(labels = scales::dollar_format()) +
  facet_wrap(~Residency.Program.Discipline) +
  coord_flip() +
  theme_bw(base_family = "Montserrat") +
  theme(axis.title = element_blank(),
        legend.position = "none")

fr_graph

ggsave(here("visuals/1.FR_graph_1.png"), fr_graph, width = 21, height = 10, dpi = 96)

#10C637
################################################
# take 2
fr_graph2 <- ggplot(fr_summary) +
  geom_bar(aes(Residency.Program.Discipline, Amount_expended, fill = Residency.Program.Discipline), stat = "identity") +
  scale_fill_manual(values = c("#65081F",
                               "#DFE0DE",
                               "#FF982C",
                               "#10C637",
                               "#18C4D6",
                               "#042B46")) +
  guides(fill = guide_legend(title = "Discipline")) +
  geom_text(data = fr_summary, aes(x = Residency.Program.Discipline, label = text, y = Amount_expended),
            hjust = ifelse(fr_summary$Amount_expended < ymid, -0.1, 1.1),
            color = ifelse(fr_summary$Amount_expended < ymid, "black", "white"),
            family = "Montserrat",
            fontface = "bold") +
    scale_y_continuous(labels = scales::dollar_format()) +
  facet_wrap(~Expense_category) +
  coord_flip() +
  theme_bw(base_family = "Montserrat") +
  theme(axis.title = element_blank())
        
fr_graph2

ggsave(here("visuals/2.FR_graph_2.png"), fr_graph2, width = 21, height = 10, dpi = 96)
