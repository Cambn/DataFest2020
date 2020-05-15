################ Cumulative Lines Animation Bcq

rescale <- read.csv("Rescaled.csv")
############## Data Clean
library(ggplot2)
library(gifski)
library(gganimate)
library(tidyverse)
library(dplyr)
library(lubridate)
rescale$date <- mdy(rescale$date)
rescale1 <- rescale %>% select(c(2,3,5,6)) 
recale1 <- rescale %>% group_by(group)
rescale1_group1 <- rescale1 %>% filter(group=="Personal Care/Household")
rescale1_group2 <- rescale1 %>% filter(group == "Non-Perishable Food")
rescale1_group3 <- rescale1 %>% filter(group == "Health Care")
rescale1_group4 <- rescale1 %>% filter(group == "Others")

############# Graph Animation
## Graph 1
anim1 <- ggplot(rescale1_group1,aes(x = date, y = price, group = category, color = category))+
  geom_line(size = 1.1)+
  geom_point()+
  theme(legend.title = element_text(size = 20),
        legend.text = element_text(size = 15),
        plot.title = element_text(hjust = 0.5, vjust = 1, size = 30),
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        panel.background = element_rect(fill = "#ffffff",size = 0.25, linetype = "solid"),
        panel.grid.major= element_line(size = 0.25,linetype = 'solid', colour = "grey"),
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "grey"))+
  ggtitle("Standardized Trend of Product: Personal Care/Household")+
  ylab("Standardized Price")+
  transition_reveal(date)

animate(anim1,200, fps = 10,  width = 1200, height = 1000, 
         renderer = gifski_renderer("Care_Household.gif"))

## Graph 2
anim2 <- ggplot(rescale1_group2,aes(x = date, y = price, group = category, color = category))+
  geom_line(size = 1.1)+
  geom_point()+
  theme(legend.title = element_text(size = 20),
        legend.text = element_text(size = 15),
        plot.title = element_text(hjust = 0.5, vjust = 1, size = 30),
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        panel.background = element_rect(fill = "#ffffff",size = 0.25, linetype = "solid"),
        panel.grid.major= element_line(size = 0.25,linetype = 'solid', colour = "grey"),
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "grey"))+
  ggtitle("Standardized Trend of Product: Non-Perishable Food")+
  ylab("Standardized Price")+
  transition_reveal(date)
animate(anim2,200, fps = 10,  width = 1200, height = 1000, 
        renderer = gifski_renderer("Line_Animation_Non-Perishable_Food.gif"))
## Graph 3
anim3 <- ggplot(rescale1_group3,aes(x = date, y = price, group = category, color = category))+
  geom_line(size = 1.1)+
  geom_point()+
  theme(legend.title = element_text(size = 20),
        legend.text = element_text(size = 15),
        plot.title = element_text(hjust = 0.5, vjust = 1, size = 30),
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        panel.background = element_rect(fill = "#ffffff",size = 0.25, linetype = "solid"),
        panel.grid.major= element_line(size = 0.25,linetype = 'solid', colour = "grey"),
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "grey"))+
  ggtitle("Standardized Trend of Product: Health Care")+
  ylab("Standardized Price")+
  transition_reveal(date)
animate(anim3,200, fps = 10,  width = 1200, height = 1000, 
        renderer = gifski_renderer("Line_Animation_Health_Care.gif"))
## Graph 4
anim4 <- ggplot(rescale1_group4,aes(x = date, y = price, group = category, color = category))+
  geom_line(size = 1.1)+
  geom_point()+
  theme(legend.title = element_text(size = 20),
        legend.text = element_text(size = 15),
        plot.title = element_text(hjust = 0.5, vjust = 1, size = 30),
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        panel.background = element_rect(fill = "#ffffff",size = 0.25, linetype = "solid"),
        panel.grid.major= element_line(size = 0.25,linetype = 'solid', colour = "grey"),
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "grey"))+
  ggtitle("Standardized Trend of Product: Others")+
  ylab("Standardized Price")+
  transition_reveal(date)
animate(anim4,200, fps = 10,  width = 1200, height = 1000, 
        renderer = gifski_renderer("Line_Animation_Others.gif"))
