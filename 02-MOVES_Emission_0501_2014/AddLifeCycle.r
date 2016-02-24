setwd("H:/00-Dissertation/02_Preliminary_Research/MasterData_0501_2014")

# clear memory 
rm(list=ls(all=TRUE)) 
# Read trip and emission data
HH <- read.dbf('H:/00-Dissertation/02_Preliminary_Research/HHTS/HH.dbf')
HH<- HH[c("HHID","LIFECYCLE")]
library(XLConnect)
DataMaster0501 <- readWorksheet(loadWorkbook('H:/00-Dissertation/02_Preliminary_Research/MasterData_0501_2014/DataMaster0501.xlsx'),sheet=1)
# join two tables
total <- merge(DataMaster0501, HH,by="HHID")

[1] 2580

# write data
library(xlsx)
write.xlsx(x = total, file = "H:/00-Dissertation/02_Preliminary_Research/MasterData_0501_2014/MasterData0502.xlsx",
        sheetName = "all", row.names = FALSE)
		




# Loading XLConnect package
require(XLConnect)
 # Creating a new workbook
wb <- loadWorkbook(file = "H:/00-Dissertation/02_Preliminary_Research/MasterData_0501_2014/MasterData0502.xlsx", create = TRUE)
 
# Creating a new sheet called "Red Sheet"
createSheet(wb, name = "Red Sheet")
# Writing the mtcars data frame on the Red Sheet, with header and row names
writeWorksheet (wb, data=total, sheet="Red Sheet", header = TRUE, rownames="Cars")