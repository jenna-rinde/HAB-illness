## Map of FHABs Illness Team reports to the 
## CDC One Health HABs database (OHHABS)

## Libraries
library(tidyverse)
library(ggplot2)
#library(ggmap)
library(ggsn) # scalebar() function
library(rgdal)
library(broom)
library(wesanderson)
#library(GISTools)
#source("/Users/kbg/R_functions/ggplot_scalebar_north_arrow.R")



#### READ IN COUNTY GIS DATA ####
county.names <- read_tsv("../GIS_files/county_names.tsv") %>% 
  mutate(id= as.character(id))

counties <- readOGR(dsn= "../GIS_files/ca_county", layer= "ca_county")
counties.reproj <- spTransform(counties, CRS("+proj=longlat +datum=WGS84")) # reproject to match ggmap
counties.df <- tidy(counties.reproj) %>% 
  left_join(., county.names, by= "id") 


#### OHHABS INCIDENTS ####
# incidents_2018 <- read_tsv("../Illness_team/Illness_incidents_OHHABs_2018.txt") %>% 
#   mutate(Year= "2018")
# incidents_2019 <- read_tsv("../Illness_team/Illness_incidents_OHHABs_2019.txt") %>% 
#   mutate(Year= "2019")

incidents_2018 <- read_tsv("Illness_incidents_OHHABs_2018.txt") %>%
  mutate(Year= "2018")
incidents_2019 <- read_tsv("Illness_incidents_OHHABs_2019.txt") %>%
  mutate(Year= "2019")
incidents_2020 <- read_tsv("Illness_incidents_OHHABs_2020.txt") %>%
  mutate(Year= "2020")

incidents_all <- full_join(select(incidents_2018, Map_key, Map_key2, County, Year), select(incidents_2019, -Type)) %>% 
  rbind(., select(incidents_2020, -Type))

## Change Madera Co. to "Human and Animal" for combined maps
## because was human-only in 2018 and animal-only in 2019, 
## therefore, human-and-animal on any combined maps
incidents_all[incidents_all$County == "Madera", ]$Map_key2 <- "Human and animal"
  # filter(incidents_all, County == "Madera") %>% 
  # mutate(Map_key2= "Human and animal")
  
#write_tsv(incidents_all, "Illness_incidents_OHHABs_2018_2019_2020.tsv")  

# count(incidents_all, year, County)
# count(incidents_all, County)
# count(incidents_all, County, Map_key2)

## Combine with Shape files
counties.incidents.2018 <- counties.df %>% 
  filter(.$County %in% unique(incidents_2018$County)) %>% 
  left_join(., incidents_2018)
  

counties.incidents.2019 <- counties.df %>% 
  filter(.$County %in% unique(incidents_2019$County)) %>% 
  left_join(., incidents_2019)

counties.incidents.2020 <- counties.df %>% 
  filter(.$County %in% unique(incidents_2020$County)) %>% 
  left_join(., incidents_2020)


counties.incidents.all <- counties.df %>% 
  filter(.$County %in% unique(incidents_all$County)) %>% 
  left_join(., incidents_all)


#### GGPLOT THEME ####


#Animal, Human and Animal, Human
#illness_colors <- c( "orange", "tomato", "firebrick3")
#illness_colors <- c(wes_palette("Darjeeling1")[3], wes_palette("Darjeeling1")[2], wes_palette("Darjeeling1")[5])
#illness_colors <- c(wes_palette("FantasticFox1")[1], wes_palette("FantasticFox1")[2], wes_palette("FantasticFox1")[3])
#illness_colors <- c(wes_palette("FantasticFox1")[2], wes_palette("FantasticFox1")[1], wes_palette("FantasticFox1")[3])
#illness_colors <- c(wes_palette("FantasticFox1")[1],wes_palette("Moonrise3")[5],  wes_palette("FantasticFox1")[3])
#illness_colors <- c(wes_palette("Moonrise3")[5], wes_palette("FantasticFox1")[1], wes_palette("FantasticFox1")[3])
illness_colors <- c(wes_palette("FantasticFox1")[3], wes_palette("FantasticFox1")[1], wes_palette("Moonrise3")[5])


#plot_caption_text <- str_wrap(width= 80, string= "Map of counties in 2018-19 where freshwater HABs illness incidents were reported to the CDC One Health HAB System by CA Dep. of Public Health staff. \n(https://mywaterquality.ca.gov/habs/resources/human_health.html#ohhabs)")
plot_caption_text <- str_wrap(width= 50, string= "Map of counties in 2018-20 where freshwater HABs illness incidents were reported to the CDC One Health HAB System by CA Dep. of Public Health staff.")


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


#### 2018 MAP ###
illness_map_2018 <- ggplot() +
  geom_polygon(data= counties.df, aes(x= long, y= lat, group=group), fill= "snow1", color= "black", size= 0.25) +
  geom_polygon(data= counties.incidents.2018, aes(x= long, y= lat, group=group, fill= Map_key2), color= "black", size= 0.25) +
  scalebar(data= counties.df, dist= 100, dist_unit = "km", location= "bottomleft", transform= TRUE, model= "WGS84", st.bottom= FALSE, 
           st.dist = 0.02,
           st.size= 3, 
           border.size = 0.5) +
  coord_map(xlim= c(-124.6, -114), ylim= c(32.3, 42.1)) +
  labs(x= NULL, y= NULL, caption= "Map of counties in 2018 where freshwater HABs illness incidents were reported\nto the CDC One Health HAB System by CA Dep. of Public Health staff. \n(https://mywaterquality.ca.gov/habs/resources/human_health.html#ohhabs)") +
  scale_x_continuous(breaks= NULL, labels= NULL) +
  scale_y_continuous(breaks= NULL, labels= NULL) +
  scale_fill_manual(values= illness_colors,
                   breaks= c("Human only", "Human and animal", "Animal only"),
                   name= "Freshwater HABs \nillness types 2018") +
  map_theme
illness_map_2018


## With north arrow 
# https://stackoverflow.com/questions/50105184/r-saving-a-graph-with-ggsave-after-using-north2-to-create-map-with-north-arro
pdf(file="Illness_team_maps/HAB_illnesses_2018_map.pdf", width= 5.5, height=7, bg= "transparent", colormodel= "cmyk")
north2(illness_map_2018, x= 0.1425, y= 0.2, scale = 0.075, symbol = 12)
dev.off()


#### 2019 MAP ####
illness_map_2019 <- ggplot() +
  geom_polygon(data= counties.df, aes(x= long, y= lat, group=group), fill= "snow1", color= "black", size= 0.25) +
  geom_polygon(data= counties.incidents.2019, aes(x= long, y= lat, group=group, fill= Map_key2), color= "black", size= 0.25) +
  scalebar(data= counties.df, dist= 100, dist_unit = "km", location= "bottomleft", transform= TRUE, model= "WGS84", st.bottom= FALSE, 
           st.dist = 0.02,
           st.size= 3, 
           border.size = 0.5) +
  coord_map(xlim= c(-124.6, -114), ylim= c(32.3, 42.1)) +
  labs(x= NULL, y= NULL, caption= "Map of counties in 2019 where freshwater HABs illness incidents were reported\n to the CDC One Health HAB System by CA Dep. of Public Health staff. \n(https://mywaterquality.ca.gov/habs/resources/human_health.html#ohhabs)") +
  scale_x_continuous(breaks= NULL, labels= NULL) +
  scale_y_continuous(breaks= NULL, labels= NULL) +
  scale_fill_manual(values= illness_colors,
                    breaks= c("Human only", "Human and animal", "Animal only"),
                    name= "Freshwater HABs \nillness types 2019") +
  map_theme
illness_map_2019


## With north arrow 
# https://stackoverflow.com/questions/50105184/r-saving-a-graph-with-ggsave-after-using-north2-to-create-map-with-north-arro
pdf(file="Illness_team_maps/HAB_illnesses_2019_map.pdf", width= 5.5, height=7, bg= "transparent", colormodel= "cmyk")
north2(illness_map_2019, x= 0.1425, y= 0.2, scale = 0.075, symbol = 12)
dev.off()

#### 2020 MAP ####
illness_map_2020 <- ggplot() +
  geom_polygon(data= counties.df, aes(x= long, y= lat, group=group), fill= "snow1", color= "black", size= 0.25) +
  geom_polygon(data= counties.incidents.2020, aes(x= long, y= lat, group=group, fill= Map_key2), color= "black", size= 0.25) +
  scalebar(data= counties.df, dist= 100, dist_unit = "km", location= "bottomleft", transform= TRUE, model= "WGS84", st.bottom= FALSE, 
           st.dist = 0.02,
           st.size= 3, 
           border.size = 0.5) +
  coord_map(xlim= c(-124.6, -114), ylim= c(32.3, 42.1)) +
  labs(x= NULL, y= NULL, caption= "Map of counties in 2020 where freshwater HABs illness incidents were reported\n to the CDC One Health HAB System by CA Dep. of Public Health staff. \n(https://mywaterquality.ca.gov/habs/resources/human_health.html#ohhabs)") +
  scale_x_continuous(breaks= NULL, labels= NULL) +
  scale_y_continuous(breaks= NULL, labels= NULL) +
  scale_fill_manual(values= illness_colors,
                    breaks= c("Human only", "Human and animal", "Animal only"),
                    name= "Freshwater HABs \nillness types 2020") +
  map_theme
illness_map_2020


## With north arrow 
# https://stackoverflow.com/questions/50105184/r-saving-a-graph-with-ggsave-after-using-north2-to-create-map-with-north-arro
pdf(file="HAB_illnesses_2020_map.pdf", width= 5.5, height=7, bg= "transparent", colormodel= "cmyk")
north2(illness_map_2020, x= 0.1425, y= 0.2, scale = 0.075, symbol = 12)
dev.off()




#### 2018 & 2019 MAP ####

illness_map_2018_19 <- ggplot() +
  geom_polygon(data= counties.df, aes(x= long, y= lat, group=group), fill= "snow1", color= "black", size= 0.25) +
  geom_polygon(data= filter(counties.incidents.all, Year != "2020"), aes(x= long, y= lat, group= group, fill= Map_key2), color= "black", size= 0.25) +
  scalebar(data= counties.df, dist= 100, dist_unit = "km", location= "bottomleft", transform= TRUE, model= "WGS84", st.bottom= FALSE, 
           st.dist = 0.02,
           st.size= 3, 
           border.size = 0.5) +
  coord_map(xlim= c(-124.6, -114), ylim= c(32.3, 42.1)) +
  #labs(x= NULL, y= NULL, caption= "Map of counties in 2018-19 where freshwater HABs illness incidents were reported\nto the CDC One Health HAB System by CA Dep. of Public Health staff. \n(https://mywaterquality.ca.gov/habs/resources/human_health.html#ohhabs)") +
  labs(x= NULL, y= NULL, caption= plot_caption_text) +
  scale_x_continuous(breaks= NULL, labels= NULL) +
  scale_y_continuous(breaks= NULL, labels= NULL) +
  scale_fill_manual(values= illness_colors,
                    breaks= c("Human only", "Human and animal", "Animal only"),
                    name= "Freshwater HAB \nillness types \n2018 & 2019") +
  map_theme +
  theme(legend.position = c(0.725, 0.84))
illness_map_2018_19

#### 2018 - 2020 MAP ####
illness_map_2018_2020 <- ggplot() +
  geom_polygon(data= counties.df, aes(x= long, y= lat, group=group), fill= "snow1", color= "black", size= 0.25) +
  geom_polygon(data= counties.incidents.all, aes(x= long, y= lat, group= group, fill= Map_key2), color= "black", size= 0.25) +
  scalebar(data= counties.df, dist= 100, dist_unit = "km", location= "bottomleft", transform= TRUE, model= "WGS84", st.bottom= FALSE, 
           st.dist = 0.02,
           st.size= 3, 
           border.size = 0.5) +
  coord_map(xlim= c(-124.6, -114), ylim= c(32.3, 42.1)) +
  labs(x= NULL, y= NULL, caption= plot_caption_text) +
  scale_x_continuous(breaks= NULL, labels= NULL) +
  scale_y_continuous(breaks= NULL, labels= NULL) +
  scale_fill_manual(values= illness_colors,
                    breaks= c("Human only", "Human and animal", "Animal only"),
                    name= "Freshwater HAB \nillness types \n2018-2020") +
  map_theme +
  theme(legend.position = c(0.725, 0.84))

illness_map_2018_2020


## With north arrow 
# https://stackoverflow.com/questions/50105184/r-saving-a-graph-with-ggsave-after-using-north2-to-create-map-with-north-arro
pdf(file="HAB_illnesses_2018_2020_map.pdf", width= 5.5, height=7, bg= "transparent", colormodel= "srgb")
north2(illness_map_2018_2020, x= 0.2, y= 0.27, scale = 0.075, symbol = 12)
dev.off()

?pdf


illness_map_all_no_caption <- ggplot() +
  geom_polygon(data= counties.df, aes(x= long, y= lat, group=group), fill= "snow1", color= "black", size= 0.25) +
  geom_polygon(data= counties.incidents.all, aes(x= long, y= lat, group= group, fill= Map_key2), color= "black", size= 0.25) +
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
                    name= "Freshwater HAB \nillness types \n2018 - 2020") +
  map_theme +
  theme(legend.position = c(0.725, 0.84))

illness_map_all_no_caption

## With north arrow 
# https://stackoverflow.com/questions/50105184/r-saving-a-graph-with-ggsave-after-using-north2-to-create-map-with-north-arro
pdf(file="HAB_illnesses_2018_2020_map_NoCaption.pdf", width= 5.5, height=7, bg= "transparent", colormodel= "srgb")
north2(illness_map_all_no_caption, x= 0.144, y= 0.16, scale = 0.075, symbol = 12)
dev.off()

## 2020 map no caption ##
illness_map_2020_no_caption <- ggplot() +
  geom_polygon(data= counties.df, aes(x= long, y= lat, group=group), fill= "snow1", color= "black", size= 0.25) +
  geom_polygon(data= counties.incidents.2020, aes(x= long, y= lat, group= group, fill= Map_key2), color= "black", size= 0.25) +
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
                    name= "Freshwater HAB \nillness types \n2020") +
  map_theme +
  theme(legend.position = c(0.725, 0.84))

illness_map_2020_no_caption

## With north arrow 
# https://stackoverflow.com/questions/50105184/r-saving-a-graph-with-ggsave-after-using-north2-to-create-map-with-north-arro
pdf(file="HAB_illnesses_2020_map_NoCaption.pdf", width= 5.5, height=7, bg= "transparent", colormodel= "srgb")
north2(illness_map_2020_no_caption, x= 0.175, y= 0.19, scale = 0.075, symbol = 12)
dev.off()




#### 2018 - 2020 No caption ####
illness_map_2018_2020_no_caption <- ggplot() +
  geom_polygon(data= counties.df, aes(x= long, y= lat, group=group), fill= "snow1", color= "black", size= 0.25) +
  geom_polygon(data= counties.incidents.all, aes(x= long, y= lat, group= group, fill= Map_key2), color= "black", size= 0.25) +
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
                    name= "Freshwater HAB \nillness types \n2018 - 2020") +
  map_theme +
  theme(legend.position = c(0.725, 0.84))

illness_map_2018_2020_no_caption

## With north arrow 
# https://stackoverflow.com/questions/50105184/r-saving-a-graph-with-ggsave-after-using-north2-to-create-map-with-north-arro
pdf(file="HAB_illnesses_2018_2020_map_NoCaption.pdf", width= 5.5, height=7, bg= "transparent", colormodel= "srgb")
north2(illness_map_2018_2020_no_caption, x= 0.175, y= 0.19, scale = 0.075, symbol = 12)
dev.off()





## 2018 & 2019 Human Only
#filter(counties.incidents.all, str_detect(Map_key2, "Human"))
illness_map_human <- ggplot() +
  geom_polygon(data= counties.df, aes(x= long, y= lat, group=group), fill= "snow1", color= "black", size= 0.25) +
  geom_polygon(data= filter(counties.incidents.all, str_detect(Map_key2, "Human")), aes(x= long, y= lat, group= group, fill= Map_key2), color= "black", size= 0.25) +
  scalebar(data= counties.df, dist= 100, dist_unit = "km", location= "bottomleft", transform= TRUE, model= "WGS84", st.bottom= FALSE, 
           st.dist = 0.02,
           st.size= 3, 
           border.size = 0.5) +
  coord_map(xlim= c(-124.6, -114), ylim= c(32.3, 42.1)) +
  labs(x= NULL, y= NULL, caption= "Map of counties in 2018-19 where freshwater HABs human illness incidents were reported\nto the CDC One Health HAB System by CA Dep. of Public Health staff. \n(https://mywaterquality.ca.gov/habs/resources/human_health.html#ohhabs)") +
  scale_x_continuous(breaks= NULL, labels= NULL) +
  scale_y_continuous(breaks= NULL, labels= NULL) +
  # scale_fill_manual(values=illness_colors,
  #                   breaks= c("Human only", "Human and animal", "Animal only"),
  #                   name= "Freshwater HABs \nillness types 2018 & 2019") +
  scale_fill_manual(values= illness_colors[3],
                    breaks= c("Human only"),
                    labels= c("Human illness"),
                    name= "Freshwater HABs \n2018 & 2019") +
  map_theme +
  theme(legend.position = c(0.83, 0.93))

illness_map_human


## With north arrow 
# https://stackoverflow.com/questions/50105184/r-saving-a-graph-with-ggsave-after-using-north2-to-create-map-with-north-arro
pdf(file="Illness_team_maps/HAB_illnesses_2018_2019_map_human.pdf", width= 5.5, height=7, bg= "transparent", colormodel= "cmyk")
north2(illness_map_human, x= 0.1425, y= 0.2, scale = 0.075, symbol = 12)
dev.off()



## 2018 & 2019 Animal Only
#filter(counties.incidents.all, str_detect(Map_key2, "nimal"))
illness_map_animal <- ggplot() +
  geom_polygon(data= counties.df, aes(x= long, y= lat, group=group), fill= "snow1", color= "black", size= 0.25) +
  geom_polygon(data= filter(counties.incidents.all, str_detect(Map_key2, "nimal")), aes(x= long, y= lat, group= group, fill= Map_key2), color= "black", size= 0.25) +
  scalebar(data= counties.df, dist= 100, dist_unit = "km", location= "bottomleft", transform= TRUE, model= "WGS84", st.bottom= FALSE, 
           st.dist = 0.02,
           st.size= 3, 
           border.size = 0.5) +
  coord_map(xlim= c(-124.6, -114), ylim= c(32.3, 42.1)) +
  labs(x= NULL, y= NULL, caption= "Map of counties in 2018-19 where freshwater HABs animal illness incidents were reported\nto the CDC One Health HAB System by CA Dep. of Public Health staff. \n(https://mywaterquality.ca.gov/habs/resources/human_health.html#ohhabs)") +
  scale_x_continuous(breaks= NULL, labels= NULL) +
  scale_y_continuous(breaks= NULL, labels= NULL) +
  # scale_fill_manual(values=illness_colors,
  #                   breaks= c("Human only", "Human and animal", "Animal only"),
  #                   name= "Freshwater HABs \nillness types 2018 & 2019") +
  scale_fill_manual(values= illness_colors[2],
                    breaks= c("Animal only"),
                    labels= c("Animal illness"),
                    name= "Freshwater HABs \n2018 & 2019") +
  map_theme +
  theme(legend.position = c(0.83, 0.93))

illness_map_animal


## With north arrow 
# https://stackoverflow.com/questions/50105184/r-saving-a-graph-with-ggsave-after-using-north2-to-create-map-with-north-arro
pdf(file="Illness_team_maps/HAB_illnesses_2018_2019_map_animal.pdf", width= 5.5, height=7, bg= "transparent", colormodel= "cmyk")
north2(illness_map_animal, x= 0.1425, y= 0.2, scale = 0.075, symbol = 12)
dev.off()

#### OTHER MAP STYLES ####


illness_map <- ggplot() +
  geom_polygon(data= counties.df, aes(x= long, y= lat, group=group), fill= "snow1", color= "black", size= 0.25) +
  geom_polygon(data= counties.incidents, aes(x= long, y= lat, group=group, fill= Map_key), color= "black", size= 0.25) +
  scalebar(data= counties.df, dist= 100, dist_unit = "km", location= "bottomleft", transform= TRUE, model= "WGS84", st.bottom= FALSE, 
           st.dist = 0.02,
           st.size= 3, 
           border.size = 0.5) +
  coord_map(xlim= c(-124.6, -114), ylim= c(32.3, 42.1)) +
  labs(x= NULL, y= NULL, caption= "Map of counties in 2018 where freshwater HABs illness incidents were reported\nto the CDC One Health HAB System (www.cdc.gov/habs/ohhabs/html)") +
  scale_x_continuous(breaks= NULL, labels= NULL) +
  scale_y_continuous(breaks= NULL, labels= NULL) +
  scale_fill_manual(values= c( "dodgerblue3", "steelblue1", "firebrick", "tomato", "goldenrod3"), 
                    breaks= c("Human only", "Human and wildlife", "Domestic animal only", "Domestic animal and Wildlife", "Wildlife"),
                    name= "Illness type") +
  map_theme
illness_map

## Without north arrow
#ggsave(illness_map, filename= "HAB_illnesses_2018_map.pdf", path= "/Users/kbg/Google Drive/Maps/Illness_team_maps", height= 6, width= 8, units= "in", device= cairo_pdf)

## With north arrow 
# https://stackoverflow.com/questions/50105184/r-saving-a-graph-with-ggsave-after-using-north2-to-create-map-with-north-arro
pdf(file="/Users/kbg/Google Drive/Maps/Illness_team_maps/HAB_illnesses_2018_map.pdf", width= 8, height=6, bg= "transparent", colormodel= "cmyk")
north2(illness_map, x= 0.097, y= 0.125, scale = 0.075, symbol = 3)
dev.off()


illness_map2 <- ggplot() +
  geom_polygon(data= counties.df, aes(x= long, y= lat, group=group), fill= "snow1", color= "black", size= 0.25) +
  geom_polygon(data= counties.incidents, aes(x= long, y= lat, group=group, fill= Map_key2), color= "black", size= 0.25) +
  scalebar(data= counties.df, dist= 100, dist_unit = "km", location= "bottomleft", transform= TRUE, model= "WGS84", st.bottom= FALSE, 
           st.dist = 0.02,
           st.size= 3, 
           border.size = 0.5) +
  coord_map(xlim= c(-124.6, -114), ylim= c(32.3, 42.1)) +
  labs(x= NULL, y= NULL, title= "Freshwater HABs illness incidents \nreported to OHHABs") +
  scale_x_continuous(breaks= NULL, labels= NULL) +
  scale_y_continuous(breaks= NULL, labels= NULL) +
  scale_fill_manual(values= c( "orange", "tomato", "firebrick3"), 
                    breaks= c("Human only", "Human and animal", "Animal only"),
                    name= "Illness type") +
  map_theme
illness_map2

## With north arrow 
# https://stackoverflow.com/questions/50105184/r-saving-a-graph-with-ggsave-after-using-north2-to-create-map-with-north-arro
pdf(file="/Users/kbg/Google Drive/Maps/Illness_team_maps/HAB_illnesses_2018_map2.pdf", width= 8, height=6, bg= "transparent", colormodel= "cmyk")
north2(illness_map2, x= 0.15, y= 0.125, scale = 0.075, symbol = 3)
dev.off()




