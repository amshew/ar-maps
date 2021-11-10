rm(list=ls()) # Caution: this clears the Environment

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  sf, # vector data operations
  raster, # raster data operations
  tidyverse,
  exactextractr,
  rgdal,
  cowplot,
  rcartocolor
  )

# Study region
ar_co <- readOGR("Data/AR_shapefile/tl_2010_05_county10.shp",
              stringsAsFactors = FALSE)
ar_b <- st_as_sf(aggregate(ar_co, dissolve = TRUE))
delta_co <- c("Arkansas","Chicot","Clay","Craighead","Desha","Drew","Greene","Lee","Mississippi","Monroe",
             "Phillips","Poinsett","St. Francis","Jackson","Lawrence", "Jefferson","Lonoke","Crittenden","Woodruff",
             "Prairie","Randolph","White","Pulaski","Lincoln","Ashley","Cross","Lonoke")
gp <- c("Arkansas","Prairie","Lonoke")
ar_delta <- ar_co[as.character(ar_co@data$NAME10) %in% delta_co, ]
gp_co <- ar_delta[as.character(ar_delta@data$NAME10) %in% gp, ]
gp_b <- st_as_sf(aggregate(gp_co, dissolve = TRUE))
ar_delta_b <- st_as_sf(aggregate(ar_delta, dissolve = TRUE))
ar_map <- st_as_sf(readOGR("Data/Delta_Alluvial_Shapefile/Delta.shp", stringsAsFactors = FALSE))

# Get all in UTM15N/NAD83
all_sf <- list(ar_b, ar_delta_b, ar_map, gp_b)
ar_all <- lapply(all_sf, st_transform, crs = 26915)

ar <- ar_all[[1]]
delta <- ar_all[[2]]
map <- ar_all[[3]]
gp <- ar_all[[4]]

# Groundwater raster
dtw <- raster("Data/krig_Spring2009.tif")
dtw <- projectRaster(dtw, crs = 26915)

dtw_r <- crop(dtw, extent(gp)) %>% 
                        mask (., gp)
dtw_pts <- rasterToPoints(dtw_r, spatial = TRUE)
dtw_df <- data.frame(dtw_pts)
dtw_df$krig_Spring2009_m <- dtw_df$krig_Spring2009*0.3048

# Depth measurements
wells <- readRDS("Data/well_reading.rds")
wells <- st_as_sf(wells, coords = c("Longitude", "Latitude"))
st_crs(wells) <- 4326
wells <- st_transform(wells, crs = 26915) 

hucs <- st_read("Data/AR_huc12/AR_huc12.shp")
hucs <- st_transform(hucs, crs = 26915)

library(Rcpp)
(inset <- ggplot() +
  geom_sf(data=ar, fill="grey", color="black",size=1)+
  geom_sf(data = map, fill="#f7fcb9")+
  geom_sf(data = gp, fill="#f7fcb9", color="blue", size=1.2)+
  theme_void()
  )
inset
  
(gg_gp <- ggplot() +
  geom_sf(data=ar, fill="grey")+
  geom_sf(data=map, aes(color="MS Alluvial Plain"), show.legend = "polygon")+
  geom_sf(data=map, fill="#f7fcb9")+
  geom_sf(data=gp, fill="#f7fcb9")+
  geom_raster(data=dtw_df, aes(x = x, y = y, fill = krig_Spring2009_m))+
  scale_fill_viridis_c(option = "turbo", direction = 1)+
  geom_sf(data=gp, fill=NA, color="blue", size=1.2) +
  geom_sf(data=hucs, fill=NA, aes(color = "HUC-12s"), size=0.75, show.legend = "line")+
  geom_sf(data=wells, aes(color="Wells"), size=2)+
  labs(fill = "Depth-to-Water\n(m)", colour="", size=12) +
  #scale_fill_manual(values=c("MS Alluvial Plain"="#f7fcb9"))+
  scale_color_manual(values=c("Wells"="#0c2c84", "HUC-12s"="dark grey","MS Alluvial\nPlain"="#f7fcb9"), 
                     name = NULL, guide = guide_legend(override.aes = list(linetype = c("blank", "solid", "blank"), 
                                                              shape = c(16, NA, 15),
                                                              fill=c(NA,NA,NA))))+
  theme(legend.position = c(0.13,0.22), legend.background = element_blank(), legend.key = element_blank(),
          axis.title.x=element_blank(), axis.title.y=element_blank(),
          legend.text=element_text(size=12))+
  coord_sf(xlim = st_bbox(gp)[c(1, 3)],
           ylim = st_bbox(gp)[c(2, 4)])
)

(gg_inset <- ggdraw() +
  draw_plot(gg_gp) +
  draw_plot(inset, x=0.66, y=0.65, width=0.35, height=0.35) +
  theme(plot.background = element_rect(fill = "White"))
)
ggsave("dtw_inset_fig1.png", width = 7, height=6, units="in")
