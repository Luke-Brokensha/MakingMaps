library(ggmap)
library(devtools)

# there used to be a map package called "stamen" maps, for getting map tiles. This is no longer in use, and all the tiles
# have moved to "stadia". The code below ensures that I can use those map tiles.
# If ggmap is already installed, use this code below to remove it, so that we can use the code below 
# that to install the specific version of ggmap that we need

remove.packages("ggmap")

devtools::install_github("stadiamaps/ggmap")

# set our bounding box (lats and longs)

bbox <- c(left = 60, bottom = -65, right = 170, top = -20)

# make our map. higher zooms make exponentially higher need for tiles, so processing power increases very quickly.  

SO <- ggmap(get_stadiamap(bbox, maptype = "stamen_terrain", zoom = 5)) +
  geom_point(data=data, aes(x=Longitude, y=Latitude), colour="black", pch=20, size=1) +
  geom_point(data=Cal_prop, aes(x=Longitude, y=Latitude, colour="Calanus propinquus"), pch=20, size=2) +
  scale_color_manual(values = c("Calanus propinquus" = "olivedrab4"))

# if you run into trouble, the code below is my specific API and might fix the issue

register_stadiamaps(key = "98ee1c13-eda8-447f-9a55-72e63d0acefc")

# map it :)

SO
