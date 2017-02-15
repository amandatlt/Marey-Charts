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
library(ggrepel)

### ---load data---###
base.dir <- dirname(getwd())
datafile <- loadWorkbook(paste0(base.dir,"/Data", "/test-data.xlsx"))
df <- readWorksheet(datafile, "Sheet1")

### ---clean data--- ###

#convert visit type to numeric (not important)
convert.visits <- function(x) {
  y <- 1
  if (x == "AE") y <- 2
  if (x == "IP") y <- 3
  y
}
df[, c("visit_type_num")] <- sapply(df[,"visit_type"], convert.visits)

#set visit_type to factor (so ggplot recognises the full range of discrete x axis)
df[,"visit_type"] <- as.factor(df$visit_type)

#replace all specialities and ep_icd10 with spaces if there is NA
df[c("ep_icd10", "Specialty")][is.na(df[c("ep_icd10", "Specialty")])] <- ""

### ---plot data---###

#will plot visit type over time
#time period of one year

#simple scatter plot diagram
df.plot <- subset(df, patientid == "C4453")
g <- ggplot(df.plot, aes(x=visit_type, y = date, colour = visit_type)) + 
      geom_point() +
      scale_y_continuous(name = "day of patient journey", trans = "reverse", limits = c(365,1)) +
      scale_x_discrete(drop = FALSE) +
      theme(panel.border = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line( size=.1, color="black"),
            panel.grid.major.y = element_blank(),
            panel.grid.major.x = element_line( size=.1, color="light grey"))
g
#points too small
#x axis looks too wide for now

#layer on specialty with repel
g + geom_label_repel(aes(label =Specialty, fill = Specialty),
                     colour = "white",
                     size = 3.5, 
                     nudge_x = 0.2,
                     show.legend = FALSE)

#layer on text describing journey
#use geom_text_repel
