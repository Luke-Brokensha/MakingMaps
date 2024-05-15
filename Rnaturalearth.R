rnaturalearth

library(rnaturalearth)

Antice <- ne_download(scale = 10, type = "antarctic_ice_shelves_lines", category = "physical", returnclass = "sf")
Antice_polys <- ne_download(scale = 10, type = "rivers_lake_centerlines", category = "physical", returnclass = "sf")
ocean <- ne_download(scale = 10, type = "ocean", category = "physical", returnclass = "sf")
rivers50 <- ne_download(scale = 10, type = "rivers_lake_centerlines", category = "physical", returnclass = "sf")
rivers50 <- ne_download(scale = 10, type = "rivers_lake_centerlines", category = "physical", returnclass = "sf")

ggplot(ocean)+
  geom_sf() +
  theme_minimal() +
  geom_point(data=Cal_prop, aes(x=Longitude, y=Latitude, colour="Calanus propinquus"), pch=20, size=4) +
  geom_point(data=data, aes(x=Longitude, y=Latitude), colour="black", pch=20, size=2) +
  scale_color_manual(values = c("Calanus propinquus" = "olivedrab4")) +
  coord_sf(xlim = c(50,160), ylim=c(-70,-30))


library(sf)

require(sf)

shape <- read_sf(dsn = "", layer = "ne_50m_antarctic_ice_shelves_polys.shp")

df_layers_physical
