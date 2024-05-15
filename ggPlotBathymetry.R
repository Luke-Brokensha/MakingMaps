library("marmap")
library("mapdata")
library("ggplot2")
library("rnaturalearth")

get.bathymetry <- function(lon1 = lon.min, lon2 = lon.max,
                           lat1 = lat.min, lat2 = lat.max,
                           bathy.breaks = 8, keep = TRUE){
  bathymetry <- getNOAA.bathy(lon1 = 4, lon2 = 30,
                              lat1 = 53, lat2 = 67,
                              resolution = 1, keep = TRUE) # keep = TRUE saves downloaded data as csv-File
  fortyfied.bathy <- fortify(bathymetry) # make a df out of bathy so ggplot can fully use the data
  fortyfied.bathy <- subset(fortyfied.bathy, z <= 0) # limit to values below sea surface
  fortyfied.bathy$z <- -fortyfied.bathy$z # make depths positive values
  names(fortyfied.bathy) <- c("Longitude", "Latitude", "Depth_m")
  fortyfied.bathy$Depthsteps_m <- cut(fortyfied.bathy$Depth_m,
                                      breaks = bathy.breaks,
                                      dig.lab = 10) # generate depth intervals for contour plot
  ## 'cut()' returns intervals in unpleasant format. Thus, cumbersome renaming for nice legend:
  fortyfied.bathy <- subset(fortyfied.bathy, !is.na(fortyfied.bathy$Depthsteps_m))
  levels(fortyfied.bathy$Depthsteps_m) <- sub(",", " - ", levels(fortyfied.bathy$Depthsteps_m))
  levels(fortyfied.bathy$Depthsteps_m) <- sub("\\(", "", levels(fortyfied.bathy$Depthsteps_m))
  levels(fortyfied.bathy$Depthsteps_m) <- sub("\\]", "", levels(fortyfied.bathy$Depthsteps_m))
  levels(fortyfied.bathy$Depthsteps_m) <- sub("(.*) - Inf", ">\\1", levels(fortyfied.bathy$Depthsteps_m))
  return(fortyfied.bathy)
}

lon.min = c(50)
lon.max = c(170) 
lat.min = c(-30)
lat.max = c(-70)

plot.bathymetry <- function(lon.min = lon.min, lon.max = lon.max,
                            lat.min = lat.min, lat.max = lat.max,
                            bathy.breaks = c(seq(0, 50, length.out = 6),
                                             seq(100, 300, length.out = 3),
                                             +Inf),
                            land.colour = NA, border.colour = "black",
                            keep = TRUE)
  bathy <- get.bathymetry(lon1 = lon.min, lon2 = lon.max, 
                          lat1 = lat.min, lat2 = lat.max, keep = TRUE)
  coastlines <- map_data('worldHires', xlim = c(50, 170), ylim = c(-30, -70))
  
  ggplot() +
    coord_quickmap(#projection= "azequalarea",
      xlim=c(50, 170), ylim=c(-70, -30)) +
    geom_tile(data = bathy, aes(x=Longitude, y=Latitude, fill=Depthsteps_m)) +
    scale_fill_brewer(palette = "Blues", name = "Water depth [m]") +
    geom_polygon(data=coastlines, aes(x=long, y=lat, group=group),
                 fill="grey", colour = "light grey", lwd=.2) +
    scale_y_continuous(expand = c(0,0)) + scale_x_continuous(expand = c(0,0)) + # so that the whole plot area is filled with bathy and coastlines.
    theme_minimal()


source("ggPlotBathymetry2.R")
worldl <- antarctic_ice(scale = "large", returnclass = "sf")

# Base on CCAMLR 48.3 area
c = plot.bathymetry(lon.min = 50, lon.max = 170, lat.min = -30, lat.max = -70, 
                    bathy.breaks = c(0, 1000, 2000, 3000, 4000, 5000, 6000, 7000, +Inf),
                    land.colour = "white") +
  geom_sf(data = worldl, fill = "white")

mp = c + coord_sf(xlim = c(50, 170), ylim = c(-37, -70)) +
  #scale_y_continuous(breaks=c(-57,-56,-55,-54,-53,-52,-51,-50), expand = c(0, 0))+
  #geom_polygon(x = c(-40, -36), y = c(-54, -52), fill = NA, color = "black")+
  theme_minimal() +
  #theme()+
  theme(panel.background = element_rect(fill = "white"),
        axis.text = element_text(size = 10),
        legend.position = "none", plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"))
  #annotate(geom = "polygon", x = c(-40, -36.5, -36.5, -40), y = c(-54, -54, -52, -52),
           #fill = "#124E63", alpha = 0.2, color = "#F6A895", linewidth = 1)+
  #annotate(geom = "polygon", x = c(-50, -50, -30, -30), y = c(-57, -50, -50, -57), fill = NA,
           #alpha = 0.5,  color = "black", size = 1)+
  #annotate(geom = "text",x = -47 ,y = -50.5, label = "CCAMLR area 48.3", color = "black", size = 2)

mp
?ne_countries
