# XML table Manipulation

The following example comes from one of my tasks as a program analyst at PHC. The following data labeled **"finance_table.csv"** is financial data from our awardees that
comes from the reporting process. The data is pulled from our grant management system. However, the data is stored in a dynamic table and when pulled it shows as a
XML type data within the csv file. This folder showers my progress as I use different techniques to clean and extract values from an XML table.

- The process that I used to clean the data can be found in the following R script labled **"SAR_FR_analysis.R"**. For the cleaning process i used the following libraries;
dplyr, qdapRegex, stringr, tidyr and reshape2. The new and tidy version of the financial table is labeled as **"sar_financial.report.csv"**. 

- The folder labeled as "XML_Table_Cleaning" is the same description above, however this time i used the **XML** and **xml2** packages to clean the spreadsheet that contains the XML tables as a column. The newer version uses less code and it's cleaner than the original.

- The new folder labled **"xml_table_cleaning_V3"** is my newest draft of working with and cleaning XML data. In this new version, I used the map function from the purrr library as well as several XML functions form the xml2 library. In this version, I no longer need to do text cleaning. This new version is cleaner and easier to read and follow. 
