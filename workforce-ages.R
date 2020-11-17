# load packages and read in data
library(readr)
library(tidyverse)
library(memisc)
library(gridExtra)
library(grid)

agesLicensed <- read_csv("agesCurEmployLicensed.csv")

# n
ntotal <- nrow(agesLicensed)
ntotal

# median
medianage <- median(agesLicensed$Age)
medianage

# count and percent over 60
over60 <- which(agesLicensed$Age > 60)
over60Count <- length(over60)
over60Count
percentOver60 <- over60Count/ntotal
percentOver60

# employed licensed bar chart
ggplot(agesLicensed, aes(x = Age)) + theme_minimal() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        plot.caption = element_text(size =12, hjust = 0)) +
  labs(y = "Count",
       caption = "Ages of the Montana licensed early childhood workforce as of March 2020 (n = 3404). \n Median age is 33, ploted as the red dashed line. There are 270 individuals are over age 60, \n which is approximately 8% of the workforce.") +
  geom_histogram(breaks = seq(10, 80, 10), fill = blues9[8]) +
  geom_density() +
  geom_vline(xintercept = medianage, size = 1, colour = "red",
             linetype = "dashed")

dev.off()

# age groups table
agesLicensed$Agecat <- cut(agesLicensed$Age, seq(11, 81, 10), right = FALSE,
                           labels = c(1:7))

agesLicensed[which(agesLicensed$Age > 80),9] <- "7"

levels(agesLicensed$Agecat) <- c("20 or younger", "21-30", "31-40",
                                 "41-50", "51-60", "61-70", "over 70", "Total")
ageTable <- table(agesLicensed$Agecat)
agePerc <- percentages(ageTable)

agePercTable <- full_join(as.data.frame(ageTable), as.data.frame(agePerc))

names(agePercTable)[1] <- "Age group"
names(agePercTable)[2] <- "Count"
agePercTable[8,] <- c("Total", 3404, 100)

agePercTable$Count <- as.numeric(agePercTable$Count) 
agePercTable$Percentage <- as.numeric(agePercTable$Percentage) 

agePercTable$Percentage <- round(agePercTable$Percentage, 1)
agePercTable
grid.table(agePercTable, rows = NULL, 
           theme = ttheme_default(core = list(bg_params = 
                                                list(fill = blues9[2:3])),
                                  colhead = list(bg_params = 
                                                   list(fill = blues9[5]))))
