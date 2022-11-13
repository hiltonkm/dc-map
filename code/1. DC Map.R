#### KATIE HILTON'S MEGA MAP OF DC ####

#### REF ####

setwd("C:/Users/kathe/OneDrive/Documents/Data Projects/dcMap")

library(tidyverse)
library(sf)
library(ggplot2)
library(ggrepel)

#### INITIAL DATA LOAD ####

## DC OUTLINE

dc_outline <- st_transform(read_sf("input/Washington_DC_Boundary-shp", "Washington_DC_Boundary"), crs=4326)
limits <- as.data.frame(st_coordinates(dc_outline))
x_1 <- max(limits$X)
x_2 <- min(limits$X)
y_1 <- max(limits$Y)
y_2 <- min(limits$Y)

##WARDS

wards <- read_sf("input/Ward_from_2012-shp", "Ward_from_2012")

##NEIGHBORHOODS: https://opendata.dc.gov/datasets/f6c703ebe2534fc3800609a07bad8f5b_17?geometry=-77.941%2C38.707%2C-76.088%2C39.081

neighborhoods <- read_sf("input/Neighborhood_Clusters-shp", "Neighborhood_Clusters")
neighborhoods_label <- read_sf("input/Neighborhood_Labels-shp", "Neighborhood_Labels")
neighborhoods_want <- c("Adams Morgan")
neigh_map <- neighborhoods_label %>%
  select(NAME, geometry) %>%
  filter(NAME %in% neighborhoods_want)

##RIVERS
rivers <- st_transform(read_sf("input/USA_Rivers_and_Streams-shp", "9ae73184-d43c-4ab8-940a-c8687f61952f2020328-1-r9gw71.0odx9"), crs=4326)
rivers_dc <- st_intersection(dc_outline, rivers)

##DC WATERBODIES (https://opendata.dc.gov/datasets/db65ff0038ed4270acb1435d931201cf_24)
water <- st_transform(read_sf("input/Waterbodies-shp", "Waterbodies"), crs=4326)
water <- water %>% filter(DESCRIPTIO == "River")

##STATES
states <- st_transform(read_sf("input/cb_2018_us_state_500k", "cb_2018_us_state_500k"), crs=4326)

##COVID NUMBERS: https://coronavirus.dc.gov/data

##### MAP (BASICS) ####
dc <- ggplot() +
  geom_sf(data=dc_outline, size=1, color="red") +
  #geom_sf(data=wards) +
  #geom_sf_label(data=wards, aes(label=WARD))
  geom_sf(data=neighborhoods) +
  geom_text_repel(data=neighborhoods_label, aes(label=NAME, geometry=geometry), stat="sf_coordinates")

dc

##### MAP (HOME) ####

dc <- ggplot() +
  geom_sf(data=states, fill="lightgray", color="purple4") +
  #geom_sf(data=neighborhoods, fill="white", color="black") +
  geom_sf(data=dc_outline, size=1, color="black", fill="white") +
  # geom_sf(data=filter(neighborhoods, NBH_NAMES == "Kalorama Heights, Adams Morgan, Lanier Heights"), 
  #         color="black", 
  #         size=1.5) +
  geom_sf(data=filter(neighborhoods, NBH_NAMES == "Rock Creek Park"),
          color="black",
          fill="forestgreen") +
  geom_point(aes(y=38.92281029483982, x=-77.04311128925329, color="YOU ARE HERE"), 
             shape=21,
             fill="red", 
             size=5)+
  geom_sf(data=water, fill="lightblue") +
  scale_color_manual(values="black", name="") +
  coord_sf(xlim=c(x_2,x_1), ylim=c(y_2,y_1))+
  theme_minimal()

dc
##IDEAS: put a dot for white house
# LSB, 
# shade rock creek park, 
# washington mon,
# national mall,
# metro lines?