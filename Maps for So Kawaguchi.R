library(ggnewscale)
library(cowplot)
library(patchwork)
library(ggplot2)
library(xfun)
library(ggtext)
library(dplyr)
library(reshape)
library(showtext)
library(ggthemes)
library(plotly)
library(maps)
library(mapdata)
library(SOmap)
library(mapview)
library(sf)
library(leaflet)
library(leafsync)
library(tidyverse)

setwd("C:/Users/luke_bro2/OneDrive - Antarctic/Desktop/R/Learning/Making Maps")

datanew <- read.csv("CPR Database JK 2023 December 01 LB.csv")

data_new2 <- melt(datanew, id = c("Latitude", "Longitude"))

data_new_krill = subset(datanew, select = -c(1, 2, 3, 4, 5, 6, 7, 10, 11, 12, 13:109, 122:334))

data_krill_melt <- melt(data_new_krill, id = c("Latitude", "Longitude"))

data_new2 <- data_new2[order(data_new2$Longitude),]

colnames(data_krill_melt)[3] = "Species"
colnames(data_krill_melt)[4] = "Abundance"

nrow(data_new2)

Superba_count <- dplyr::filter(data_new2, Longitude == 135.6514)

data_new_rounded <- dplyr::mutate(data_new2, Latitude = round(Latitude, digits = 0))
data_new_rounded <- dplyr::mutate(data_new_rounded, Longitude = round(Longitude, digits = 0))

#all CPR
Indian_count <- data_new2 %>% filter(between(Longitude, -20, 90))
Weddel_count <- data_new2 %>% filter(between(Longitude, -60, -20))
Amundsen_count <- data_new2 %>% filter(between(Longitude, -135, -60))
Ross_count <- data_new2 %>% filter(between(Longitude, -135, 150))
Western_Pac_count <- data_new2 %>% filter(between(Longitude, 90, 150))

#Krill adult specific
Indian_count_adult <- datanew__krill0 %>% filter(between(Longitude, -20, 90))
Weddel_count_adult <- datanew__krill0 %>% filter(between(Longitude, -60, -20))
Amundsen_count_adult <- datanew__krill0 %>% filter(between(Longitude, -135, -60))
Ross_count_adult <- datanew__krill0 %>% filter(between(Longitude, -135, 150))
Western_Pac_count_adult <- datanew__krill0 %>% filter(between(Longitude, 90, 150))

#Krill caly specific
Indian_count_caly <- datanew__krill0 %>% filter(between(Longitude, -20, 90))
Weddel_count_caly <- datanew__krill0 %>% filter(between(Longitude, -60, -20))
Amundsen_count_caly <- datanew__krill0 %>% filter(between(Longitude, -135, -60))
Ross_count_caly <- datanew__krill0 %>% filter(between(Longitude, -135, 150))
Western_Pac_count_caly <- datanew__krill0 %>% filter(between(Longitude, 90, 150))

#Krill furc specific
Indian_count_furc <- datanew__krill0 %>% filter(between(Longitude, -20, 90))
Weddel_count_furc <- datanew__krill0 %>% filter(between(Longitude, -60, -20))
Amundsen_count_furc <- datanew__krill0 %>% filter(between(Longitude, -135, -60))
Ross_count_furc <- datanew__krill0 %>% filter(between(Longitude, -135, 150))
Western_Pac_count_furc <- datanew__krill0 %>% filter(between(Longitude, 90, 150))

data_new2 = data_new2[data_new2$Abundance > 0.1,]

data_new2$Abundance <- as.double(data_new2$Abundance)

Superba_adult <- dplyr::filter(data_krill_melt, Species == "Euphausia.superba")
Superba_C1 <- dplyr::filter(data_krill_melt, Species == "Euphausia.superba.C1")
Superba_C2 <- dplyr::filter(data_krill_melt, Species == "Euphausia.superba.C2")
Superba_C3 <- dplyr::filter(data_krill_melt, Species == "Euphausia.superba.C3")
Superba_calyptopis <- dplyr::filter(data_krill_melt, Species == "Euphausia.superba.calyptopis")
Superba_F1 <- dplyr::filter(data_krill_melt, Species == "Euphausia.superba.F1")
Superba_F2 <- dplyr::filter(data_krill_melt, Species == "Euphausia.superba.F2")
Superba_F3 <- dplyr::filter(data_krill_melt, Species == "Euphausia.superba.F3")
Superba_F4 <- dplyr::filter(data_krill_melt, Species == "Euphausia.superba.F4")
Superba_F5 <- dplyr::filter(data_krill_melt, Species == "Euphausia.superba.F5")
Superba_F6 <- dplyr::filter(data_krill_melt, Species == "Euphausia.superba.F6")
Superba_furcilia <- dplyr::filter(data_krill_melt, Species == "Euphausia.superba.furcilia")
Salps <- dplyr::filter(data_new2, Species == "Salpa.thompsoni")

Superba_adult$Abundance <- as.double(Superba_adult$Abundance)
Superba_calyptopis$Abundance <- as.double(Superba_calyptopis$Abundance)
Superba_furcilia$Abundance <- as.double(Superba_furcilia$Abundance)

world <- map_data("world")
fronts <- SOmap_data$fronts_park


#### Careful here, the points are jittered ####
map2 <- ggplot()+
  geom_polygon(data = world, aes(x=long, y=lat, group=group), colour="darkgrey",fill="grey", alpha=1) +
  #geom_point(data=datanew, aes(x=Longitude, y=Latitude), colour="grey", pch=20, size=2) +
  geom_jitter(data=Superba_adult, aes(x=Longitude, y=Latitude, size=Abundance, shape=Abundance), width = 1, colour="steelblue4", pch=20, alpha=0.5) +
  geom_jitter(data=Superba_calyptopis, aes(x=Longitude, y=Latitude, size=Abundance, shape=Abundance), width = 1, colour="olivedrab4", pch=20, alpha=0.5) +
  geom_jitter(data=Superba_furcilia, aes(x=Longitude, y=Latitude, size=Abundance, shape=Abundance), width = 1, colour="red", pch=20, alpha=0.5) +
  geom_jitter(data=Superba_C1, aes(x=Longitude, y=Latitude, size=Abundance, shape=Abundance), width = 1, colour="olivedrab4", pch=20, alpha=0.5) +
  geom_jitter(data=Superba_C2, aes(x=Longitude, y=Latitude, size=Abundance, shape=Abundance), width = 1, colour="olivedrab4", pch=20, alpha=0.5) +
  geom_jitter(data=Superba_C3, aes(x=Longitude, y=Latitude, size=Abundance, shape=Abundance), width = 1, colour="olivedrab4", pch=20, alpha=0.5) +
  geom_jitter(data=Superba_F1, aes(x=Longitude, y=Latitude, size=Abundance, shape=Abundance), width = 1, colour="red", pch=20, alpha=0.5) +
  geom_jitter(data=Superba_F2, aes(x=Longitude, y=Latitude, size=Abundance, shape=Abundance), width = 1, colour="red", pch=20, alpha=0.5) +
  geom_jitter(data=Superba_F3, aes(x=Longitude, y=Latitude, size=Abundance, shape=Abundance), width = 1, colour="red", pch=20, alpha=0.5) +
  geom_jitter(data=Superba_F4, aes(x=Longitude, y=Latitude, size=Abundance, shape=Abundance), width = 1, colour="red", pch=20, alpha=0.5) +
  geom_jitter(data=Superba_F5, aes(x=Longitude, y=Latitude, size=Abundance, shape=Abundance), width = 1, colour="red", pch=20, alpha=0.5) +
  geom_jitter(data=Superba_F6, aes(x=Longitude, y=Latitude, size=Abundance, shape=Abundance), width = 1, colour="red", pch=20, alpha=0.5) +
  #scale_color_gradient(low = "red", high = "blue", space = "Lab") +
  scale_color_manual(values = c("Euphausia superba adult" = "steelblue4", "Euphausia superba calyptopis" = "olivedrab4", 
  "Euphausia superba furcilia" = "red", labels = c(1, 5, 10, 50, 100))) +
  scale_size_continuous(breaks=c(1, 5, 10, 50, 100, 150), labels=c(1, 5, 10, 50, 100, 150)) +
  theme(legend.position = "bottom", legend.text=element_text(size=12), plot.caption = element_text(hjust = 0)) +
  labs(legend.title = "Individuals", title = "Euphausia superba abundance", subtitle = "from CPR tows 1991-2023", caption = "Grey lines show Southern Ocean fronts - From North to South: Northern Boundary, Sub-Antarctic Front, Polar Front, Southern Antarctic Circumpolar Current Front, Southern Boundary", plot.title = element_text(size=22),  
       plot.subtitle = element_text(size=18)) +
  geom_sf(data=fronts, inherit.aes = FALSE, colour = "darkgrey") +
  coord_sf(xlim = c(-90,170), ylim=c(-80,-42))

map2

Superba_adultR <- dplyr::mutate(Superba_adult, Latitude = round(Latitude, digits = 0))
Superba_adultR <- dplyr::mutate(Superba_adult, Longitude = round(Longitude, digits = 0))
Superba_calyptopisR<- dplyr::mutate(Superba_calyptopis, Latitude = round(Latitude, digits = 0))
Superba_calyptopisR <- dplyr::mutate(Superba_calyptopis, Longitude = round(Longitude, digits = 0))
Superba_furciliaR <- dplyr::mutate(Superba_furcilia, Latitude = round(Latitude, digits = 0))
Superba_furciliaR <- dplyr::mutate(Superba_furcilia, Longitude = round(Longitude, digits = 0))


map3 <- ggplot()+
  geom_polygon(data = world, aes(x=long, y=lat, group=group), colour="darkgrey",fill="grey", alpha=1) +
  #geom_point(data=datanew, aes(x=Longitude, y=Latitude), colour="grey", pch=20, size=2) +
  geom_point(data=Superba_adultR, aes(x=Longitude, y=Latitude, colour = "Euphausia superba adult"), size=2, pch=20) +
  geom_point(data=Superba_calyptopisR, aes(x=Longitude, y=Latitude, colour= "Euphausia superba calyptopis"), size=2, pch=20) +
  geom_point(data=Superba_furciliaR, aes(x=Longitude, y=Latitude, colour="Euphausia superba furcilia"), size=2, pch=20) +
  #scale_color_gradient(low = "red", high = "blue", space = "Lab") +
  scale_color_manual(values = c("Euphausia superba adult" = "steelblue4", "Euphausia superba calyptopis" = "olivedrab4", 
                                "Euphausia superba furcilia" = "red", labels = c(1, 5, 10, 50, 100))) +
  scale_size_continuous(breaks=c(1, 5, 10, 50, 100, 150), labels=c(1, 5, 10, 50, 100, 150)) +
  theme(legend.title = element_blank(), legend.position = "bottom", legend.text=element_text(size=12), plot.caption = element_text(hjust = 0)) +
  labs(title = "Euphausia superba distribution", subtitle = "from CPR tows 1991-2023", caption = "Grey lines show Southern Ocean fronts - From North to South: Northern Boundary, Sub-Antarctic Front, Polar Front, Southern Antarctic Circumpolar Current Front, Southern Boundary", plot.title = element_text(size=22),  
       plot.subtitle = element_text(size=18)) +
  geom_sf(data=fronts, inherit.aes = FALSE, colour = "darkgrey") +
  coord_sf(xlim = c(-90,170), ylim=c(-80,-42))

map3

### Careful here, the data has been jittered ####
map4 <- ggplot()+
  geom_polygon(data = world, aes(x=long, y=lat, group=group), colour="darkgrey",fill="grey", alpha=1) +
  #geom_point(data=datanew_melt0, aes(x=Longitude, y=Latitude, colour="CPR sampling effort"), pch=20, size=1) +
  geom_jitter(data=Superba_C1, aes(x=Longitude, y=Latitude), width= 3, colour="olivedrab4", pch=20, size=2, alpha=0.5) +
  geom_jitter(data=Superba_C2, aes(x=Longitude, y=Latitude), width= 3, colour="olivedrab4", pch=20, size=2, alpha=0.5) +
  geom_jitter(data=Superba_C3, aes(x=Longitude, y=Latitude), width= 3, colour="olivedrab4", pch=20, size=2, alpha=0.5) +
  geom_jitter(data=Superba_F1, aes(x=Longitude, y=Latitude), width= 3, colour="red", pch=20, size=2, alpha=0.5) +
  geom_jitter(data=Superba_F2, aes(x=Longitude, y=Latitude), width= 3, colour="red", pch=20, size=2, alpha=0.5) +
  geom_jitter(data=Superba_F3, aes(x=Longitude, y=Latitude), width= 3, colour="red", pch=20, size=2, alpha=0.5) +
  geom_jitter(data=Superba_F4, aes(x=Longitude, y=Latitude), width= 3, colour="red", pch=20, size=2, alpha=0.5) +
  geom_jitter(data=Superba_F5, aes(x=Longitude, y=Latitude), width= 3, colour="red", pch=20, size=2, alpha=0.5) +
  geom_jitter(data=Superba_F6, aes(x=Longitude, y=Latitude), width= 3, colour="red", pch=20, size=2, alpha=0.5) +
  geom_jitter(data=Superba_adult, aes(x=Longitude, y=Latitude, colour = "Euphausia superba adult"), width= 3, size = 2, pch=20, alpha = 0.5) +
  geom_jitter(data=Superba_calyptopis, aes(x=Longitude, y=Latitude, colour = "Euphausia superba calyptopis"), width= 3, size = 2, pch=20, alpha = 0.5) +
  geom_jitter(data=Superba_furcilia, aes(x=Longitude, y=Latitude, colour = "Euphausia superba furcilia"), width= 3, size = 2, pch=20, alpha = 0.5) +
  #scale_color_gradient(low = "red", high = "blue", space = "Lab") +
  scale_color_manual(values = c("CPR sampling effort" = "grey", "Euphausia superba adult" = "steelblue4", "Euphausia superba calyptopis" = "olivedrab4", 
                                "Euphausia superba furcilia" = "red", labels = c(1, 5, 10, 50, 100))) +
  scale_size_continuous(breaks=c(1, 5, 10, 50, 100, 150), labels=c(1, 5, 10, 50, 100, 150)) +
  theme(legend.title = element_blank(), legend.position = "bottom", legend.text=element_text(size=12), plot.caption = element_text(hjust = 0)) +
  labs(legend.title = "Individuals", title = "Euphausia superba distribution", subtitle = "from CPR tows 1991-2023", caption = "Grey lines show Southern Ocean fronts - From North to South: Northern Boundary, Sub-Antarctic Front, Polar Front, Southern Antarctic Circumpolar Current Front, Southern Boundary", plot.title = element_text(size=22),  
       plot.subtitle = element_text(size=18)) +
  geom_sf(data=fronts, inherit.aes = FALSE, colour = "darkgrey") +
  coord_sf(xlim = c(-90,170), ylim=c(-80,-42))

map4


### PIPING WITH FILTERS INTO GGPLOT INSTEAD OF MAKING NEW DATAFRAMES ####

data_new2 %>% 
  filter(variable %in% c("Euphausia.superba", "Euphausia.superba.calyptopis", "Euphausia.superba.furcilia")) %>% 
  filter(value > 0) %>% 
  ggplot() +
  geom_polygon(data = world, aes(x=long, y=lat, group=group), colour="darkgrey",fill="grey", alpha=1) +
  geom_point(aes(x = Longitude, y = Latitude, size = value, color = value)) +
  geom_sf(data=fronts, inherit.aes = FALSE, colour = "darkgrey") +
  coord_sf(xlim = c(-90,170), ylim=c(-80,-42)) +
  facet_wrap(~variable, ncol=2)


# Turn our mapviews into objects, and using sync - 
m1 <- mapview(df2, burst=TRUE)
m3 <- mapview(df, burst=TRUE)
m2 <- mapview(fronts, color= "grey")
sync(m2, m3)

m1
m3


df2 %>% 
  filter(variable %in% c("Euphausia.superba.furcilia")) %>% 
  filter(value > 0) %>% 
mapview(burst = TRUE, legend = TRUE, color="black", col.regions = "red")

?mapview

projcrs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

df <- st_as_sf(x = Salps, coords = c("Longitude", "Latitude"), crs = projcrs)

df2 <- st_as_sf(x = data_krill_melt, coords = c("Longitude", "Latitude"), crs = projcrs)

leaflet() %>%
  addTiles() %>%

data_new2 %>% 
  filter(variable %in% c("Euphausia.superba")) %>% 
  filter(value > 0) %>% 
  ggplot(., aes(x = Longitude,
             y = Latitude,
             color = value)) +
  geom_point()
  
  