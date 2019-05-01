library(readr)

convict_registers <- read_csv("slqbritishconvictregisters201605.csv")

convict_records <- read.delim("tmpMA2c8PConvict_records.txt", header = FALSE, sep = "\t")

View(convict_records)
