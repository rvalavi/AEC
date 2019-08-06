
# vector data -------------------------------------------------------------
wallabies <- read.csv("data/Swamp wallabies on Phillips Island, Australia.csv")
head(wallabies)
str(wallabies)

plot(wallabies$location.long, wallabies$location.lat)

library(sf)

st_crs(4326)

wlbs <- st_as_sf(wallabies, 
                 coords = c("location.long", "location.lat"),
                 crs = 4326)
wlbs

plot(wlbs["tag.local.identifier"])
plot(st_geometry(wlbs), axes = TRUE)

ph <- st_read("data/Phillip_island.shp")
plot(st_geometry(ph))
plot(wlbs$geometry, add = TRUE)

library(mapview)
mapview(wlbs)


# raster data -------------------------------------------------------------
library(raster)

dem <- raster("data/ASTGTM2_S39E145_dem.tif")
dem

plot(dem)
plot(st_geometry(ph), add = TRUE)

dem_ph <- crop(dem, ph)
plot(dem_ph)

sp1 <- as(ph, "Spatial")
sf1 <- st_as_sf(sp1)


wlbs$Elevation <- raster::extract(dem_ph, as(wlbs, "Spatial"))
plot(wlbs["Elevation"])


# Distance to road --------------------------------------------------------
library(tidyverse)

# road <- st_read("data/roads.gpkg")
# road_234 <- filter(road, rdclass %in% c(2, 3, 4))

road <- st_read("data/roads.gpkg") %>% 
  filter(rdclass %in% c(2, 3, 4))

unique(road$rdclass)
plot(st_geometry(road))
plot(wlbs["tag.local.identifier"], add = TRUE)

wlb <- filter(wlbs, tag.local.identifier == "MF031")
plot(wlb)

distm <- st_distance(wlb, road)
dim(distm)

wlb$mindist <- apply(distm, 1, min)

plot(wlb["mindist"])


# check the time column
class(wlb$timestamp)

wlb$timestamp <- as.POSIXct(wlb$timestamp)
wlb$timestamp[1]

library(lubridate)

wlb$hours <- hour(wlb$timestamp)


ggplot(data = wlb, aes(x = as.factor(hours), y = mindist)) +
  geom_boxplot() +
  labs(x = "Daily hours", y = "Distance from roads") +
  theme_bw()

esquisse::esquisser()

ggplot(wlb) +
  aes(colour = ground.speed) +
  geom_sf(size = 1L) +
  scale_color_gradient() +
  theme_minimal()

library(viridis)

plt2 <- ggplot() +
  # geom_sf(data = ph) +
  geom_sf(data = wlb, aes(colour = mindist)) +
  scale_color_viridis() +
  theme_bw() +
  labs(x = "Longitude", y = "Latitude", col = "Distance")


# projection --------------------------------------------------------------
st_crs(32755)
wlbs_proj <- st_transform(wlbs, crs = 3111)
plot(st_geometry(wlbs_proj), axes = TRUE)

road_proj <- st_transform(road, crs = 3111)

road_buf <- road_proj %>% 
  st_geometry() %>% 
  st_buffer(dist = 70) %>% 
  st_union()

# st_union(st_buffer(st_geometry(road_proj), dist = 70))

plot(road_buf)
mapview(road_buf)

plot(wlbs_proj$geometry, add = TRUE)

wlbs_at_risk <- st_intersection(wlbs_proj, road_buf)
plot(st_geometry(wlbs_at_risk), 
     add = TRUE, 
     pch = 1, 
     col = "blue")

unique(wlbs_at_risk$tag.local.identifier)

mapview(wlbs_at_risk)


# data aggregation --------------------------------------------------------

mat <- matrix(sample(1:25), nrow = 5)
mat
r <- raster()
plot(r)
text(r)

r <- raster(wlbs_proj, resolution = c(1000, 1000))
values(r) <- (1:198)
plot(r)

ct <- rasterize(wlbs_proj, r, fun = "count", background = 0)[[1]]
plot(ct)
text(ct, cex = 0.5)


# ggplot for rasters ------------------------------------------------------
# take a sample of the data
sam <- dem_ph %>% 
  mask(ph) %>% 
  sampleRegular(size = 3e5, asRaster = TRUE) %>% 
  as.data.frame(xy = TRUE, na.rm = TRUE) %>% 
  setNames(c("x", "y","layer"))

head(sam)

plt1 <- ggplot() +
  geom_raster(data = sam, aes(x = x, y = y, fill = layer)) +
  scale_fill_viridis(option = "A") +
  geom_sf(data = wlbs, col = "gray", alpha = 0.5) +
  coord_sf(crs = 4326) +
  theme_bw()

plt1 <- plt1 + labs(x = "Longitude", y = "Latitude", fill = "Elevation")

library(cowplot)

cowplot::plot_grid(plt1, plt2)


















