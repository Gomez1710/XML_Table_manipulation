# String Manipulation

The following example comes from one of my tasks as a program analyst at PHC. The following data labeled "finance_table.csv" is financial data from our awardees that
comes from the reporting process. The data is pulled from our grant management system. However, the data is stored in a dynamic table and when pulled it shows as a
XML type data within the csv file. 

The process that I used to clean the data can be found in the following R script labled "SAR_FR_analysis.R". For the cleaning process i used the following libraries;
dplyr, qdapRegex, stringr, tidyr and reshape2. 

The new and tidy version of the financial table is labeled as "sar_financial.report.csv". 
