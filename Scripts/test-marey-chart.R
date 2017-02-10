###################################################
#
# project: Marey charts for patient pathways
# date: 02/17
#
###################################################

### ---load packages--- ###
.libPaths("P:/User/Amanda.Teo/R/Packages")
library(XLConnect)
library(ggplot2)

### ---load data---###
base.dir <- dirname(getwd())
datafile <- loadWorkbook(paste0(base.dir,"/Data", "/test-data.xlsx"))
df <- readWorksheet(datafile, "Sheet1")

### ---clean data--- ###
convert.visits <- function(x) {
  y <- 1
  if (x == "AE") y <- 2
  if (x == "IP") y <- 3
  y
}
df[, c("visit_type_num")] <- sapply(df[,"visit_type"], convert.visits)

### ---plot data---###

#will plot visit type over time
#time period of one year

#simple bar diagram
df.plot <- df[df$patientid == "A7777",]


df.fulldates <- as.data.frame(cbind(rep("A777", 365), 1:365), 
                              col.names = c("patientid", "date"),
                              stringsAsFactors = FALSE)
ggplot(df.plot, aes(x = date, y = visit_type_num)) + geom_bar(stat = "identity")
