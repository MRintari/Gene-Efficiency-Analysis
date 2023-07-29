library(readxl) # Reads Excel file
library(knitr)
desc <- read_excel("C:/Users/mwend/OneDrive/Documents/Mwenda Rintari/University/Data Science/gene_efficiency/data/data_description.xlsx")
desc$Notes[desc$Notes == 'N/A'] <- ""
