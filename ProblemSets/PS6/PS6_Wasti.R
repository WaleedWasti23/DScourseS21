library(tidyverse)
library(grid)
library(rscorecard)
library(rvest)
library(httr)
library(listviewer)
library(lubridate)
library(ggrepel)
library(ggplot2)
library(maps)
library(dplyr)
library(purrr)
library(janitor)
library(tidyr)
library(scales)
library(XML)
library(ggplot2);
library(maps);
library(plyr)








dat <- read.csv("C:/Users/walee/OneDrive - University of Oklahoma/OU/PhD (Economics)/Semester 4/Data Science for Economists/EconomistData.csv")

pc1 <- ggplot(dat, aes(x = CPI, y = HDI, color = Region))
pc1 + geom_point()


#Adding the trend line
(pc2 <- pc1 +
    geom_smooth(aes(group = 1),
                method = "lm",
                formula = y ~ log(x),
                se = FALSE,
                color = "red")) +
  geom_point()



pc2 +
  geom_point(shape = 1, size = 4)

(pc3 <- pc2 +
    geom_point(size = 4.5, shape = 1) +
    geom_point(size = 4, shape = 1) +
    geom_point(size = 3.5, shape = 1))

pointsToLabel <- c("Russia", "Venezuela", "Iraq", "Myanmar", "Sudan",
                   "Afghanistan", "Congo", "Greece", "Argentina", "Brazil",
                   "India", "Italy", "China", "South Africa", "Spane",
                   "Botswana", "Cape Verde", "Bhutan", "Rwanda", "France",
                   "United States", "Germany", "Britain", "Barbados", "Norway", "Japan",
                   "New Zealand", "Singapore")


#Labelling points
(pc4 <- pc3 +
    geom_text(aes(label = Country),
              color = "gray20",
              data = subset(dat, Country %in% pointsToLabel)))


pc3 +
  geom_text_repel(aes(label = Country),
                  color = "gray20",
                  data = subset(dat, Country %in% pointsToLabel),
                  force = 10)


dat$Region <- factor(dat$Region,
                     levels = c("EU W. Europe",
                                "Americas",
                                "Asia Pacific",
                                "East EU Cemt Asia",
                                "MENA",
                                "SSA"),
                     labels = c("OECD",
                                "Americas",
                                "Asia &\nOceania",
                                "Central &\nEastern Europe",
                                "Middle East &\nnorth Africa",
                                "Sub-Saharan\nAfrica"))


pc4$data <- dat
pc4





(pc5 <- pc4 +
    scale_x_continuous(name = expression(italic("Corruption Perceptions Index, 2011 (10=least corrupt)")),
                       limits = c(.9, 10.5),
                       breaks = 1:10) +
    scale_y_continuous(name = expression(italic("Human Development Index, 2011 (1=Best)")),
                       limits = c(0.2, 1.0),
                       breaks = seq(0.2, 1.0, by = 0.1)) +
    scale_color_manual(name = "",
                       values = c("#24576D",
                                  "#099DD7",
                                  "#28AADC",
                                  "#248E84",
                                  "#F2583F",
                                  "#96503F")) +
    ggtitle("Corruption and Human development"))


(pc6 <- pc5 +
    theme_minimal() + # start with a minimal theme and add what we need
    theme(text = element_text(color = "gray20"),
          legend.position = "top", # position the legend in the upper left 
          legend.direction = "horizontal",
          legend.justification = c(0.1,0), # anchor point for legend.position.
          legend.text = element_text(size = 11, color = "gray10"),
          axis.text = element_text(face = "italic"),
          axis.title.x = element_text(vjust = -1), # move title away from axis
          axis.title.y = element_text(vjust = 2), # move away for axis
          axis.ticks.y = element_blank(), # element_blank() is how we remove elements
          axis.line = element_line(color = "gray40", size = 0.5),
          axis.line.y = element_blank(),
          panel.grid.major = element_line(color = "gray50", size = 0.5),
          panel.grid.major.x = element_blank()
    ) + guides(colour = guide_legend(nrow = 1))) # forces legend to be in a single line



(mR2 <- summary(lm(HDI ~ log(CPI), data = dat))$r.squared)

png(file = "economist.png", width = 800, height = 600)
pc6 
grid.text("Sources: Transparency International; UN Human Development Report",
          x = .02, y = .03,
          just = "left",
          draw = TRUE)
grid.segments(x0 = 0.81, x1 = 0.825,
              y0 = 0.90, y1 = 0.90,
              gp = gpar(col = "red"),
              draw = TRUE)
grid.text(paste0("R² = ",
                 as.integer(mR2*100),
                 "%"),
          x = 0.835, y = 0.90,
          gp = gpar(col = "gray20"),
          draw = TRUE,
          just = "left")

dev.off()

