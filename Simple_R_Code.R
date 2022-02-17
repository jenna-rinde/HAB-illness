# Map of FHABs Illness Team reports to the 
## CDC One Health HABs database (OHHABS)
##Originally created by Keith Bouma-Gregson
##Updated by Jenna Rinde (CDFW) in February 2022

#If new to R, then you'll likely need to start by downloading these packages or download if you don't have them already. 
# skip this if you already have a certain package already download
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("ggmap")
install.packages("ggsn")
install.packages("rgdal")
install.packages("broom")
install.packages("wesanderson")
install.packages("GISTools")
install.packages("readr")
install.packages("dplyr")

## Libraries, bascially tell R we are going to use these languages to give commands
library(tidyverse)
library(ggplot2)
library(ggmap)
library(ggsn) # for the scalebar() function
library(rgdal)
library(broom)
library(wesanderson)
library(GISTools)
library(readr)
library(dplyr)


#set work id, copy the pathway where files are stored on your computer
#Note: you'll need to switch the back slashes from \ to /
setwd("C:/Users/jenna/OneDrive/Data_Repository/HAB-illness")


#### READ IN COUNTY GIS DATA ####
county.names <- read_tsv("county_names.tsv") 

county.names1<- county.names %>% 
  mutate(id=as.character(id))


counties <- readOGR(dsn= "ca_county", layer= "ca_county")
counties.reproj <- spTransform(counties, CRS("+proj=longlat +datum=WGS84")) # reproject to match ggmap
counties.df <- tidy(counties.reproj) %>% 
  left_join(., county.names1, by= "id") 

#### Import OHHABS INCIDENTS ####
#you can get this information from Rebecca (Beckye) Stanton from OEHHA. 
#you will take the information she gives you and have to reformat it accordingly
#I recommend opening the previous year's text files into excel, update the rows and save as a text file

incidents_2021 <- read_tsv("Illness_incidents_OHHABs_2021.txt") %>% 
  mutate(Year= "2021") %>% 
  mutate(Year=as.character(Year))

incidents_2018_2020 <-read_tsv("Illness_incidents_OHHABs_2018_2020.txt") %>% 
  mutate(Year= "2018_2020") %>% 
  mutate(Year=as.character(Year))

incidents_2018_2021 <-read_tsv("Illness_incidents_OHHABs_2018_2021.txt") %>% 
  mutate(Year= "2018_2021") %>% 
  mutate(Year=as.character(Year))

## Combine with Shape files
counties.incidents.2021 <- counties.df %>% 
  filter(.$County %in% unique(incidents_2021$County)) %>% 
  left_join(., incidents_2021)

counties.incidents.2018_2020 <- counties.df %>% 
  filter(.$County %in% unique(incidents_2018_2020$County)) %>% 
  left_join(., incidents_2018_2020)

counties.incidents.2018_2021 <- counties.df %>% 
  filter(.$County %in% unique(incidents_2018_2021$County)) %>% 
  left_join(., incidents_2018_2021)

## COLOR SCHEME FOR MAP ##  
illness_colors <- c(wes_palette("FantasticFox1")[3], wes_palette("FantasticFox1")[1], wes_palette("Moonrise3")[5])

plot_caption_text <- str_wrap(width= 50, string= "Map of counties in 2018-21 where freshwater HABs illness incidents were reported to the CDC One Health HAB System by CA Dep. of Public Health staff.")


map_theme <- theme(panel.grid = element_blank(),
                   plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
                   text= element_text(size= 12),
                   panel.background = element_rect(fill= "gray80"),
                   panel.border = element_rect(color= "black", fill= NA),
                   plot.background = element_rect(fill= "transparent", color= NA),
                   panel.grid.minor = element_blank(),
                   panel.grid.major = element_blank(),
                   plot.caption = element_text(hjust= 0, size= 14),
                   legend.position = c(0.83, 0.89),
                   legend.background = element_blank(),
                   legend.key= element_blank(),
                   legend.text = element_text(size= 20),
                   legend.title= element_text(size= 20)
)


#### 2021 MAP ####

illness_map_2021 <- ggplot() +
  geom_polygon(data= counties.df, aes(x= long, y= lat, group=group), fill= "snow1", color= "black", size= 0.25) +
  geom_polygon(data= counties.incidents.2021, aes(x= long, y= lat, group=group, fill= Map_key2), color= "black", size= 0.25) +
  scalebar(data= counties.df, dist= 100, dist_unit = "km", location= "bottomleft", transform= TRUE, model= "WGS84", st.bottom= FALSE, 
           st.dist = 0.02,
           st.size= 3, 
           border.size = 0.5) +
  coord_map(xlim= c(-124.6, -114), ylim= c(32.3, 42.1)) +
  labs(x= NULL, y= NULL) +
  scale_x_continuous(breaks= NULL, labels= NULL) +
  scale_y_continuous(breaks= NULL, labels= NULL) +
  scale_fill_manual(values= illness_colors,
                    breaks= c("Human only", "Human and animal", "Animal only"),
                    name= "Freshwater HABs \nillness types 2021") +
  map_theme +
  theme(legend.position = c(0.72, 0.8)) 

illness_map_2021

pdf(file="HAB_illnesses_2021_map.pdf", width= 5.5, height=7, bg= "transparent", colormodel= "cmyk")
north2(illness_map_2021, x= 0.175, y= 0.19, scale = 0.075, symbol = 12)
dev.off()

#### 2018_2020 MAP ####

illness_map_2018_2020 <- ggplot() +
  geom_polygon(data= counties.df, aes(x= long, y= lat, group=group), fill= "snow1", color= "black", size= 0.25) +
  geom_polygon(data= counties.incidents.2018_2020, aes(x= long, y= lat, group=group, fill= Map_key2), color= "black", size= 0.25) +
  scalebar(data= counties.df, dist= 100, dist_unit = "km", location= "bottomleft", transform= TRUE, model= "WGS84", st.bottom= FALSE, 
           st.dist = 0.02,
           st.size= 3, 
           border.size = 0.5) +
  coord_map(xlim= c(-124.6, -114), ylim= c(32.3, 42.1)) +
  labs(x= NULL, y= NULL) +
  scale_x_continuous(breaks= NULL, labels= NULL) +
  scale_y_continuous(breaks= NULL, labels= NULL) +
  scale_fill_manual(values= illness_colors,
                    breaks= c("Human only", "Human and animal", "Animal only"),
                    name= "Freshwater HABs \nillness types \n 2018-2020") +
  map_theme +
  theme(legend.position = c(0.72, 0.8)) 

illness_map_2018_2020

pdf(file="HAB_illnesses_2018_2020_map.pdf", width= 5.5, height=7, bg= "transparent", colormodel= "cmyk")
north2(illness_map_2018_2020, x= 0.175, y= 0.19, scale = 0.075, symbol = 12)
dev.off()



#### 2018-2021 MAP ####

illness_map_2018_2021 <- ggplot() +
  geom_polygon(data= counties.df, aes(x= long, y= lat, group=group), fill= "snow1", color= "black", size= 0.25) +
  geom_polygon(data= counties.incidents.2018_2021, aes(x= long, y= lat, group=group, fill= Map_key2), color= "black", size= 0.25) +
  scalebar(data= counties.df, dist= 100, dist_unit = "km", location= "bottomleft", transform= TRUE, model= "WGS84", st.bottom= FALSE, 
           st.dist = 0.02,
           st.size= 3, 
           border.size = 0.5) +
  coord_map(xlim= c(-124.6, -114), ylim= c(32.3, 42.1)) +
  labs(x= NULL, y= NULL) +
  scale_x_continuous(breaks= NULL, labels= NULL) +
  scale_y_continuous(breaks= NULL, labels= NULL) +
  scale_fill_manual(values= illness_colors,
                    breaks= c("Human only", "Human and animal", "Animal only"),
                    name= "Freshwater HABs \nillness types \n 2018-2021") +
  map_theme +
  theme(legend.position = c(0.72, 0.8)) 

illness_map_2018_2021

pdf(file="HAB_illnesses_2018_2021_map.pdf", width= 5.5, height=7, bg= "transparent", colormodel= "cmyk")
north2(illness_map_2018_2021, x= 0.175, y= 0.19, scale = 0.075, symbol = 12)
dev.off()
