# Reference map of DC ----------------------------------------------------------

# Library ----------------------------------------------------------------------
rm(list=ls())
library(tidyverse)
library(sf)
library(ggplot2)
library(ggrepel)

# Globals ----------------------------------------------------------------------
wd <- "C:/Users/kathe/OneDrive/Documents/Data Projects/GIT/dc-map/"
output <- paste0(wd, "output/")
sf_path <- "C:/Users/kathe/OneDrive/Documents/Data Projects/shapefiles/"
mytheme <- theme(
  axis.title.x=element_blank(),
  axis.text.x=element_blank(),
  axis.ticks.x=element_blank(),
  axis.title.y=element_blank(),
  axis.text.y=element_blank(),
  axis.ticks.y=element_blank(),
  legend.position = "right",
  legend.direction = "vertical",
  legend.box="vertical",
  legend.margin = margin(1),
  legend.box.margin = margin(1),
  legend.key = element_rect(size=.3, fill = "white"),
  legend.key.size = unit(.5, "cm"),
  legend.text=element_text(size=11,family="Garamond", color="#172A61"),
  plot.title = element_text(hjust = 0, size=14,family="Garamond", color="#172A61"),
  plot.subtitle = element_text(hjust = 0, size=12,family="Garamond", color="#172A61"),
  plot.caption = element_text(size = 10, hjust=0, vjust=.2,family="Garamond", color="#172A61"),
  panel.background = element_blank()
)
safe_colorblind_palette <- c("#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499",
                             "#44AA99", "#999933", "#882255", "#661100", "#6699CC", "#888888")
colorBlindGrey8   <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
                       "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# Data Load --------------------------------------------------------------------

dc_outline <- st_transform(read_sf(paste0(sf_path,"Washington_DC_Boundary-shp")), crs=4326)

limits <- as.data.frame(st_coordinates(dc_outline))
x_1 <- max(limits$X)
x_2 <- min(limits$X)
y_1 <- max(limits$Y)
y_2 <- min(limits$Y)

wards <- read_sf(paste0(sf_path,"Ward_from_2012-shp"))

neighborhoods <- read_sf(paste0(sf_path, "Neighborhood_Clusters-shp"))
neighborhoods_label <- read_sf(paste0(sf_path, "Neighborhood_Labels-shp"))
sort(unique(neighborhoods_label$NAME))
neighborhoods_label <- neighborhoods_label %>%
  select(NAME, geometry) %>%
  filter(NAME %in%  c("Adams Morgan", "Park View"))

water <- st_transform(read_sf(paste0(sf_path,"Waterbodies-shp")), crs=4326)
water <- water %>% filter(DESCRIPTIO == "River")

states <- st_transform(read_sf(paste0(sf_path,"cb_2018_us_state_500k")), crs=4326)

metro <- st_transform(read_sf(paste0(sf_path, "Metro_Lines_Regional")), crs=4326)

# Map (wards) -----------------------------------------------------------------

dc <- ggplot() +
  geom_sf(data=dc_outline, size=.5, color="white") +
  geom_sf(data=wards, aes(fill=as.character(WARD)), color="white",size=.1, show.legend=FALSE) +
  geom_sf_label(data=wards, aes(label=WARD), size=3, family="Garamond", color="#172A61",label.size = NA) +
  scale_fill_manual(values=safe_colorblind_palette) +
  labs(title="Ward Outlines", subtitle = "Washington, DC", caption = "Source: Office of Planning, DC City Government") +
  coord_sf(xlim=c(x_2,x_1), ylim=c(y_2,y_1), crs=4326) +
  mytheme
ggsave(paste0(output, "Ward Lines.png"), dc, width=4, height=5)


# Map (You Are Here) -----------------------------------------------------------
# insert lat/long to plot here!
l1 <- NA
l2 <- NA
dc <- ggplot() +
  geom_sf(data=dc_outline, size=.5, color="white") +
  geom_sf(data=neighborhoods, fill="#DDCC77", color="white",size=.1, show.legend=FALSE) +
  geom_sf(data=water, fill="#88CCEE", color='white', size=.1) +
  geom_sf(data=filter(neighborhoods, NBH_NAMES == "Rock Creek Park"),color="white",fill="#117733", size=.1) +
  geom_point(aes(y=l1, x=l2),shape=21,fill="red", size=5, color="#172A61") +
  geom_text(aes(y=l1, x=l2, label="YOU ARE HERE"), nudge_x=.01, nudge_y=-.015, family="Garamond", color="#172A61")  +
  labs(title="Neighborhoods", subtitle = "Washington, DC", caption = "Source: Office of Planning, DC City Government") +
  coord_sf(xlim=c(x_2,x_1), ylim=c(y_2,y_1), crs=4326) +
  mytheme
ggsave(paste0(output, "Neighborhoods.png"), dc, width=4, height=5)
dc

# Map (Metro Lines)-------------------------------------------------------------
dc <- ggplot() +
  geom_sf(data=states, fill="grey") +
  geom_sf(data=dc_outline, size=.5, color="#172A61", fill="grey96", linetype = "dashed") +
  geom_sf(data=filter(metro, NAME != "blue", NAME != "green",  NAME != "silver"), aes(color=as.character(NAME)),size=1,fill=NA, show.legend = FALSE) +
  geom_sf(data=st_jitter(filter(metro, NAME == "blue"), .002), color="#0072B2", size=1) +
  geom_sf(data=st_jitter(filter(metro, NAME == "green"), .002), color="#009E73", size=1) +
  geom_sf(data=st_jitter(filter(metro, NAME == "silver"), .002), color="#999999", size=1) +
  scale_color_manual(values=c("blue"="#0072B2",
                             "silver"="#999999",
                             "orange"="#E69F00",
                             "green"="#009E73",
                             "red"="#e41a1c",
                             "yellow"="#F0E442")) +
  labs(title="Metro Lines", subtitle = "Washington, DC", caption = "Source: Office of Planning, DC City Government") +
  coord_sf(xlim=c(x_2,x_1), ylim=c(y_2,y_1), crs=4326) +
  mytheme
ggsave(paste0(output, "Metro.png"), dc, width=4, height=5)

# Sources ----------------------------------------------------------------------
## WARDS: https://planning.dc.gov/maps-wards-2022
## NEIGHBORHOODS: https://opendata.dc.gov/datasets/f6c703ebe2534fc3800609a07bad8f5b_17?geometry=-77.941%2C38.707%2C-76.088%2C39.081
## DC WATERBODIES: https://opendata.dc.gov/datasets/db65ff0038ed4270acb1435d931201cf_24
## COVID NUMBERS: https://coronavirus.dc.gov/data
## HOW TO SET UP FONTS FOR GGPLOT: https://yjunechoe.github.io/posts/2021-06-24-setting-up-and-debugging-custom-fonts/
## STATES: https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html
## DC OUTLINE: https://opendata.dc.gov/datasets/7241f6d500b44288ad983f0942b39663/explore
## METRO LINES: https://opendata.dc.gov/datasets/DCGIS::metro-lines-regional/about
## COLOR PALETTE: https://stackoverflow.com/questions/57153428/r-plot-color-combinations-that-are-colorblind-accessible