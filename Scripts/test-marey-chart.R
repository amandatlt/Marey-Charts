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

# 1) - simple scatter plot diagram (all visits on same line)

df.plot <- subset(df, patientid == "C4453")

df.plot[, "singlept"] <- 1

g <- ggplot(df.plot, aes(x=date, y = singlept, colour = visit_type)) + 
      geom_point(aes(colour = visit_type),shape = 15, size = 1.5) +
      scale_x_continuous(name = "day of patient journey", limits = c(1,365)) +
      scale_y_discrete(drop = FALSE) +
      theme(panel.border = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line( size=.1, color="black"),
            panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_line( size=.1, color="light grey"))
g

#layer on specialty with repel
g + geom_label_repel(aes(label=Specialty, fill = Specialty, angle = 90),
                     colour = "white",
                     size = 3.5, 
                     nudge_x = 0.2,
                     show.legend = FALSE)
#angle not working - need to slant text by 45 degrees


#2) - simple scatter plot diagram (scatter points depending on type of visit)

p <- ggplot(df.plot, aes(y=date, x = visit_type, colour = visit_type)) + 
  geom_point(aes(colour = visit_type),shape = 15, size = 2) +
  scale_y_continuous(name = "day of patient journey",trans = "reverse", limits = c(365,1)) +
  scale_x_discrete(drop = FALSE) +
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line( size=.1, color="black"),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line( size=.1, color="light grey"))
p

#layer on specialty with repel
p + geom_label_repel(aes(label=Specialty, fill = Specialty, angle = 90),
                     colour = "white",
                     size = 3.5, 
                     nudge_x = 0.2,
                     show.legend = FALSE)
