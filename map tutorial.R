install.packages("ggplot2")   #only do this once
install.packages("tidyverse") #only do this once
install.packages('maps')

library(ggplot2)              #needs to be done each r session
library(tidyverse)            #needs to be done each r session

library(maps)



#####################
library(ggplot2)
library(dplyr)
require(maps)
require(viridis)    #necessary libraries for mapping
theme_set(
  theme_void()
)
#######################
EUvax <- read.csv("C:/Users/Abdulrhman Al-Tabali/Downloads/EUvaccine.csv") #####this reads in the data file I made you will need to change the path to your computer
View(EUvax)

mapdata <- map_data("world") ##ggplot2
View(mapdata)
mapdata <- left_join(mapdata, EUvax, by="region")
View(mapdata)

mapdata1<-mapdata %>% filter(!is.na(mapdata$Perc_vaccinated))
View(mapdata1)

map1 <- mapdata1 %>% ggplot(aes( x = long, y = lat, group=group)) +
  geom_polygon(aes(fill = Perc_vaccinated), color = "black")
        #here we can't lable every single place it will cause clutter
map1           #this is enough 
#####################################
#if you want to add colors 
map2 <- map1 + scale_fill_gradient(name = "% vaccinated", low = "red", high =  "green", na.value = "grey50")+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        rect = element_blank())
map2


########################################################################################
#Make a map for the world
########################################################################################
install.packages("viridis")
library(ggplot2)
library(dplyr)
require(maps)
require(viridis)
theme_set(
  theme_void()
)
world_map <- map_data("world")
ggplot(world_map, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill="lightgray", colour = "white")


########################################################################################
#
########################################################################################


