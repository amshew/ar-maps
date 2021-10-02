rm(list=ls()) # Caution: this clears the Environment

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  sf, # vector data operations
  raster, # raster data operations
  tidyverse,
  exactextractr,
  rgdal
  )

ar_co <- readOGR("Data/AR_shapefile/tl_2010_05_county10.shp",
              stringsAsFactors = FALSE)
ar_b <- aggregate(ar_co, dissolve = TRUE)
delta_co <- c("Arkansas","Chicot","Clay","Craighead","Desha","Drew","Greene","Lee","Mississippi","Monroe",
             "Phillips","Poinsett","St. Francis","Jackson","Lawrence", "Jefferson","Lonoke","Crittenden","Woodruff",
             "Prairie","Randolph","White","Pulaski","Lincoln","Ashley","Cross","Lonoke")
gp <- c("Arkansas","Prairie","Lonoke")
ar_delta <- ar_co[as.character(ar_co@data$NAME10) %in% delta_co, ]
gp_co <- ar_delta[as.character(ar_delta@data$NAME10) %in% gp, ]
gp_b <- aggregate(gp_co, dissolve = TRUE)
ar_delta_b <- aggregate(ar_delta, dissolve = TRUE)
ar_map <- readOGR("Data/Delta_Alluvial_Shapefile/Delta.shp", stringsAsFactors = FALSE)

dtw <- raster("Data/krig_Spring2009.tif")
gp_huc12 <- spTransform(gp_b, CRS(proj4string(dtw)))
gp_huc12_r <- crop(dtw, extent(gp_huc12)) %>% 
                        mask (., gp_huc12)

wells
