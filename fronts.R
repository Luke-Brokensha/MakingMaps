library("ggplot2")
library("SOmap")

world <- map_data("world")
theme_set(theme_bw())

fronts2 <- (SOmap_data$fronts_park)
as.data.frame(fronts2)

NB <- fronts[1,]
SAF <- fronts[2,]
PF <- fronts[3,]
SACCF <- fronts[4,]
SB <- fronts[5,]

SOmap(trim = -35)
SOplot(NB, col = "#CC79A7", lty = 5, lwd = 2)
SOplot(SAF, col = "#D55E00", lty = 4, lwd = 2)
SOplot(PF, col = "#0072B2", lty = 2, lwd = 2)
SOplot(SACCF, col = "#56B4E9", lty = 3, lwd = 2)
SOplot(SB, col = "#009E73", lty = 1, lwd = 2)

fronts <- rbind(NB, SAF, PF, SACCF, SB)

ggplot()+
  geom_polygon(data = world, aes(x=long, y=lat, group=group), colour="darkgrey",fill="grey", alpha=1) +
  geom_sf(data=NB, inherit.aes=FALSE, col="#CC7987", lty = 5, lwd = 1) +
  geom_sf(data=SAF, inherit.aes=FALSE, col="#D55E00", lty = 4, lwd = 1) +
  geom_sf(data=PF, inherit.aes=FALSE, col="#0072B2", lty = 2, lwd = 1) +
  geom_sf(data=SACCF, inherit.aes=FALSE, col="#56B4E9", lty = 7, lwd = 1) +
  geom_sf(data=SB, inherit.aes=FALSE, col="#009E73", lty = 1, lwd = 1) +
  geom_point(data=data, aes(x=Longitude, y=Latitude), colour="grey", pch=20, size=1) +
  geom_point(data=Cal_prop, aes(x=Longitude, y=Latitude, size=abundance_bin), colour="steelblue4", pch=20) +
  scale_color_manual(values = c("Calanus propinquus" = "steelblue4")) +
  scale_size_continuous(breaks=c(0.5, 1, 2, 3, 4, 5, 6), labels=c(5, 10, 50, 250, 500, 750, 1000)) +
  theme(legend.title = element_blank(), legend.position = "bottom", legend.text=element_text(size=12)) +
  guides(size = guide_legend(nrow = 1)) +
  labs(title = "Calanus Propinquus", subtitle = "Abundance", plot.title = element_text(size=22), plot.subtitle = element_text(size=18)) +
  coord_sf(xlim = c(50,160), ylim=c(-70,-30))
