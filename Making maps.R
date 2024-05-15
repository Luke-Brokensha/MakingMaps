## Making maps ##
## Luke Brokensha ##
## October 2023 ##

#Set our working directory - I have this code to set my directory quickly each time I open this file.
setwd("C:/Users/luke_bro2/OneDrive - Antarctic/Desktop/R/Learning/Making Maps/")

#install and load packages
install.packages("SOmap", repos = c(SCAR = "https://scar.r-universe.dev",
                                    CRAN = "https://cloud.r-project.org"))

library(ggplot2)
library(xfun)
library(dplyr)
library(reshape)
library(maps)
library(mapdata)
library(ggtext)
library(SOmap)
library(gganimate)

#Load data
data <- read.csv("AADC_Zoop_CPR_Data.csv")

#Clean the data
data_clean = subset(data, select = -c(1, 2, 3, 4, 7, 10, 11))

#Turn it into a more usable format
data_clean <- melt(data_clean, id = c("Month", "Year", "Latitude", "Longitude"))

#Change column names
colnames(data_clean)[5] = "Species"
colnames(data_clean)[6] = "Abundance"

#Remove 0's
data_clean = data_clean[data_clean$Abundance > 0.1,]

#Make a Calanus propinquus only data frame
Cal_prop <- dplyr::filter(data_clean, Species == "Calanus.propinquus")

#Make data to map the world
world <- map_data("world")

#Plot world map
ggplot() +
  geom_polygon(data = world, aes(x=long, y=lat, group=group), colour="darkgrey",fill="grey", alpha=1)

#Zoom in on our target region
ggplot() +
  geom_polygon(data = world, aes(x=long, y=lat, group=group), colour="darkgrey",fill="grey", alpha=1) +
  coord_sf(xlim = c(50,160), ylim=c(-70,-30))

#Add Calanus data to our map
ggplot()+
  geom_polygon(data = world, aes(x=long, y=lat, group=group), colour="darkgrey",fill="grey", alpha=1) +
  geom_point(data=Cal_prop, aes(x=Longitude, y=Latitude), colour="steelblue4", pch=20, size=2) +
  coord_sf(xlim = c(50,160), ylim=c(-70,-30))

#Add CPR too
ggplot()+
  geom_polygon(data = world, aes(x=long, y=lat, group=group), colour="darkgrey",fill="grey", alpha=1) +
  geom_point(data=data, aes(x=Longitude, y=Latitude), colour="grey", pch=20, size=2) +
  geom_point(data=Cal_prop, aes(x=Longitude, y=Latitude), colour="steelblue4", pch=20, size=2) +
  coord_sf(xlim = c(50,160), ylim=c(-70,-30))

#Rounding lat and long to no decimal points (i.e. -52, 100)
data <- dplyr::mutate(data, Latitude = round(Latitude, digits = 0))
data <- dplyr::mutate(data, Longitude = round(Longitude, digits = 0))
data_clean <- dplyr::mutate(data_clean, Latitude = round(Latitude, digits = 0))
data_clean <- dplyr::mutate(data_clean, Longitude = round(Longitude, digits = 0))
Cal_prop <- dplyr::mutate(Cal_prop, Latitude = round(Latitude, digits = 0))
Cal_prop <- dplyr::mutate(Cal_prop, Longitude = round(Longitude, digits = 0))

#Make CPR data points smaller than the species
ggplot()+
  geom_polygon(data = world, aes(x=long, y=lat, group=group), colour="darkgrey" ,fill="grey", alpha=1) +
  geom_point(data=data, aes(x=Longitude, y=Latitude), colour="grey", pch=20, size=1) +
  geom_point(data=Cal_prop, aes(x=Longitude, y=Latitude), colour="steelblue4", pch=20, size=2) +
  coord_sf(xlim = c(50,160), ylim=c(-70,-30))

#Add legend
ggplot()+
  geom_polygon(data = world, aes(x=long, y=lat, group=group), colour="darkgrey",fill="grey", alpha=1) +
  geom_point(data=data, aes(x=Longitude, y=Latitude, size=2), colour="grey", pch=20, size=1) +
  geom_point(data=Cal_prop, aes(x=Longitude, y=Latitude, size=2, colour="Calanus propinquus"), pch=20, size=2) +
  scale_color_manual(values = c("Calanus propinquus" = "steelblue4")) +
  coord_sf(xlim = c(50,160), ylim=c(-70,-30))

#Move legend to bottom
ggplot()+
  geom_polygon(data = world, aes(x=long, y=lat, group=group), colour="darkgrey",fill="grey", alpha=1) +
  geom_point(data=data, aes(x=Longitude, y=Latitude, size=2), colour="grey", pch=20, size=1) +
  geom_point(data=Cal_prop, aes(x=Longitude, y=Latitude, size=2, colour="Calanus propinquus"), pch=20, size=2) +
  scale_color_manual(values = c("Calanus propinquus" = "steelblue4")) +
  theme(legend.title = element_blank(), legend.position = "bottom") +
  coord_sf(xlim = c(50,160), ylim=c(-70,-30))

#Add plot title
ggplot()+
  geom_polygon(data = world, aes(x=long, y=lat, group=group), colour="darkgrey",fill="grey", alpha=1) +
  geom_point(data=data, aes(x=Longitude, y=Latitude, size=2), colour="grey", pch=20, size=1) +
  geom_point(data=Cal_prop, aes(x=Longitude, y=Latitude, size=2, colour="Calanus propinquus"), pch=20, size=2) +
  scale_color_manual(values = c("Calanus propinquus" = "steelblue4")) +
  theme(legend.title = element_blank(), legend.position = "bottom") +
  labs(title = "Calanus Propinquus", subtitle = "Southern Ocean distribution") +
  coord_sf(xlim = c(50,160), ylim=c(-70,-30))

#Add Euphausia vallentini
Euph_val <- dplyr::filter(data_clean, Species == "Euphausia.vallentini")

ggplot()+
  geom_polygon(data = world, aes(x=long, y=lat, group=group), colour="darkgrey",fill="grey", alpha=1) +
  geom_point(data=data, aes(x=Longitude, y=Latitude, size=2), colour="grey", pch=20, size=1) +
  geom_point(data=Cal_prop, aes(x=Longitude, y=Latitude, size=2, colour="Calanus propinquus"), pch=20, size=2) +
  geom_point(data=Euph_val, aes(x=Longitude, y=Latitude, size=2, colour="Euphausia vallentini"), pch=20, size=2) +
  scale_color_manual(values = c("Calanus propinquus" = "steelblue4", "Euphausia vallentini" = "olivedrab4")) +
  theme(legend.title = element_blank(), legend.position = "bottom") +
  labs(title = "Calanus Propinquus + Euphausia vallentini", subtitle = "Southern Ocean distribution") +
  coord_sf(xlim = c(50,160), ylim=c(-70,-30))

#Add Southern Ocean fronts from SOmap package
fronts <- SOmap_data$fronts_park

#Map everything
ggplot()+
  geom_polygon(data = world, aes(x=long, y=lat, group=group), colour="darkgrey",fill="grey", alpha=1) +
  geom_point(data=data, aes(x=Longitude, y=Latitude, size=2), colour="grey", pch=20, size=1) +
  geom_point(data=Cal_prop, aes(x=Longitude, y=Latitude, size=2, colour="Calanus propinquus"), pch=20, size=2) +
  geom_point(data=Euph_val, aes(x=Longitude, y=Latitude, size=2, colour="Euphausia vallentini"), pch=20, size=2) +
  scale_color_manual(values = c("Calanus propinquus" = "steelblue4", "Euphausia vallentini" = "olivedrab4")) +
  theme(legend.title = element_blank(), legend.position = "bottom") +
  labs(title = "Calanus Propinquus + Euphausia vallentini", subtitle = "Southern Ocean distribution") +
  geom_sf(data=fronts, inherit.aes = FALSE, colour = "darkgrey") +
  coord_sf(xlim = c(50,160), ylim=c(-70,-30))



