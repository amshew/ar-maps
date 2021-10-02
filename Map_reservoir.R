rm(list=ls()) # Caution: this clears the Environment

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  sf, # vector data operations
  raster, # raster data operations
  tidyverse,
  exactextractr, # fast raster data extraction for polygons
  maps, # to get county boundary data
  data.table, # data wrangling
  dplyr, # data wrangling
  lubridate, # Date object handling
  #tmap, # for map creation
  stargazer, # regression table generation
  future.apply, # parallel computation
  cdlTools, # download CDL data
  rgdal, # required for cdlTools
  prism, # dow/nload PRISM data
  stringr # string manipulation
)  

list.files()
#ARkansas Shapefile
AR <- readOGR("Data/AR_shapefile/tl_2010_05_county10.shp",
             stringsAsFactors = FALSE)
plot(AR)
#dissolved state shapefile
AR_d<-aggregate(AR, dissolve = TRUE)
nestates <-c("Arkansas","Chicot","Clay","Craighead","Desha","Drew","Greene","Lee","Mississippi","Monroe",
             "Phillips","Poinsett","St. Francis","Jackson","Lawrence", "Jefferson","Lonoke","Crittenden","Woodruff",
             "Prairie","Randolph","White","Pulaski","Lincoln","Ashley","Cross","Lonoke")
gp <-c("Arkansas","Prairie","Jefferson","Lonoke","Phillips")
cp<-c("Clay","Greene","Craighead","Poinsett","Cross","St. Francis","Lee")
#Arkansas Delta
AR.Delta <- AR[as.character(AR@data$NAME10) %in% nestates, ]
plot(AR.Delta)
#Fully designated counties
gp_AR<-AR.Delta[as.character(AR.Delta@data$NAME10) %in% gp, ]
plot(gp_AR)

Full_CGA<-c("Clay","Greene","Craighead","Poinsett","Cross","St. Francis","Lee","Arkansas","Prairie","Jefferson","Lonoke","Phillips")
Full_gca_AR<-AR.Delta[as.character(AR.Delta@data$NAME10) %in% Full_CGA, ]
plot(Full_gca_AR)

#partial designated counties
cp_AR<-AR.Delta[as.character(AR.Delta@data$NAME10) %in% cp, ]
plot(cp_AR)
#dissolve shapefile
#AR.Delta_d<-aggregate(AR.Delta, dissolve = TRUE)

#Delta Shapefile showing Crowley Ridge
Delta<- readOGR("Data/Delta_Alluvial_Shapefile/Delta.shp",
                stringsAsFactors = FALSE)

cp_AR_d<-aggregate(cp_AR, dissolve = TRUE)
plot(cp_AR_d)
plot(Delta, add=T)
memory.limit(size=20000000) 
crs(cp_AR_d)

st_gp <-c("Arkansas","Prairie","Lonoke")
st_gp<-AR.Delta[as.character(AR.Delta@data$NAME10) %in% st_gp, ]
plot(st_gp)
st_gp<-aggregate(st_gp, dissolve = TRUE)
plot(st_gp)

#bring in the data and plot the DTW for the fall on 2019 
avav <- raster("Data/krig_Spring2009.tif")

#####delta
GP_huc12 <- spTransform(st_gp, CRS(proj4string(avav)))
GP_huc12_r<-crop(avav, extent(GP_huc12)) %>% 
  mask (., GP_huc12)

plot(GP_huc12_r)

crs<-"+proj=aea +lat_0=0 +lon_0=-120 +lat_1=34 +lat_2=40.5 +x_0=0 +y_0=-4000000 +datum=NAD83 +units=km +no_defs "
test_spdf <- as(GP_huc12_r, "SpatialPixelsDataFrame")
plot(test_spdf)
crs(test_spdf)
crs(AR.Delta)
plot(AR.Delta)
AR_d<-aggregate(AR, dissolve = TRUE)
gp_AR_d<-aggregate(gp_AR, dissolve = TRUE)
plot(gp_AR_d)
(test_sf<-st_as_sf(test_spdf))
(Delta_sf<-st_as_sf(AR_d))
(Delta_sd<-st_as_sf(AR.Delta))
(Delta_gp<-st_as_sf(GP_huc12))
(Delta_cp<-st_as_sf(cp_AR_d))
plot(AR_d)
(test_sf<-st_transform(test_sf,crs = crs(AR.Delta)))
crs(test_sf)
crs(Delta_sd)

#plot
library(ggspatial)
library(Rcpp)
colnames(test_sf)[1]<-"value"
(Delta_delta<-st_as_sf(Delta))
( ADTW<-ggplot()+  
    geom_sf(data=Delta_sd,fill="white", color=NA) +
    geom_sf(data=Delta_delta, fill=NA, color="grey50", size=0.4)+
    geom_sf(data=Delta_sd,fill=NA, color="grey50", size=1) +
    geom_sf(data=Delta_gp,fill=NA, color = "red", alpha = 5, linetype = 1, size=1)+
    geom_sf(data=Delta_cp,fill=NA, color = "grey50", alpha = 5, linetype = 1, size=1)+
    scale_color_distiller(palette = "Spectral",type = "seq", direction = -1,
                          name ="Depth to water(ft.)")+theme_bw()+
    theme(legend.title.align = 0.5)+theme(legend.text=element_text(size=8))+
    theme(legend.title = element_text(size=10, face="italic"))+
    xlab("Longitude")+ ylab("Latitude")+theme(legend.justification = c(1, 0), 
                                              legend.position = c(1.2,0))+
    theme(legend.title.align=0.8)  )+ theme(axis.text = element_text(size=8))

ADTW

#####delta
well_reading <- readRDS("Data/well_reading.rds")

#collapse to every point in the data
TA <- CRS("+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +datum=NAD83 +units=m +ellps=GRS80 +towgs84=0,0,0")

well_reading<-well_reading%>%group_by(Longitude,Latitude,Station_Na)%>%summarise(well_depth = mean(well_depth))
n_occur <- data.frame(table( well_reading$Station_Na))

dsp <- SpatialPoints( well_reading[,1:2], proj4string=CRS("+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +datum=NAD83 +units=m +ellps=GRS80 +towgs84=0,0,0"))
dsp <- SpatialPointsDataFrame(dsp, well_reading)
dta <- spTransform(dsp,TA)

AR_huc12 <- readOGR("Data/AR_huc12/AR_huc12.shp",
                    stringsAsFactors = FALSE)

crs(AR_huc12)
dta <- spTransform(dsp, TA)
crs(dta)
crs(AR_huc12)

plot(dta)
plot(AR_huc12,add=T)
#clip -both worked
crs(dta)

point_well<-raster::intersect(dta,st_gp)
plot(point_well)


GP_huc12 <- spTransform(st_gp, CRS(proj4string(avav)))


GP_huc12_r<-crop(avav, extent(GP_huc12)) %>% 
  mask (., GP_huc12)

plot(GP_huc12_r)

crs<-"+proj=aea +lat_0=0 +lon_0=-120 +lat_1=34 +lat_2=40.5 +x_0=0 +y_0=-4000000 +datum=NAD83 +units=km +no_defs "


test_spdf <- as(GP_huc12_r, "SpatialPixelsDataFrame")
plot(test_spdf)
crs(test_spdf)
crs(AR.Delta)
plot(AR.Delta)
AR_d<-aggregate(AR, dissolve = TRUE)
gp_AR_d<-aggregate(gp_AR, dissolve = TRUE)
plot(gp_AR_d)
(test_sf<-st_as_sf(test_spdf))
(Delta_sf<-st_as_sf(AR_d))
(Delta_sd<-st_as_sf(AR.Delta))
(Delta_gp<-st_as_sf(GP_huc12))
(Delta_cp<-st_as_sf(cp_AR_d))
(point<-st_as_sf(point_well))
plot(AR_d)
(test_sf<-st_transform(test_sf,crs = crs(AR.Delta)))
crs(test_sf)
crs(Delta_sd)
plot(point)


#plot
library(cowplot)
library(ggspatial)
library(Rcpp)
colnames(test_sf)[1]<-"value"
colnames(point)[3]<-"Station_Na"
( DTW<-ggplot()+  
    #geom_sf(data=Delta_sd,fill="grey70", color=NA) +
    geom_sf(data=test_sf, aes(color = value),size=0.4)+
    #geom_sf(data=Delta_sd,fill=NA, color="grey50", size=1) +
    geom_sf(data=Delta_gp,fill=NA, color ="grey50", alpha = 1, linetype = 1, size=1)+
    geom_point(data = point, 
               aes(x = Longitude, y = Latitude), 
               color = "black", size = 1)+
    scale_color_distiller(palette = "spectral",type = "seq", direction = 1,
                          name ="Depth to water(ft.)")+theme_bw()+
    annotation_scale(location = "bl", width_hint = 0.2,height = unit(0.15, "cm")) +
    annotation_north_arrow(location = "bl", which_north = "true", 
                           pad_x = unit(0.15, "in"), pad_y = unit(0.15, "in"),
                           style = north_arrow_fancy_orienteering)+
    theme(legend.title.align = 0.5)+theme(legend.text=element_text(size=10))+
    theme(legend.title = element_text(size=12, face="italic"))+
    xlab("Longitude")+ ylab("Latitude")+theme(legend.justification = c(1, 0), 
                                              legend.position = c(1.2,0))+
    theme(legend.title.align=0.8)  )+ theme(axis.text = element_text(size=10))

DTW

 



gg_inset_map1 = ggdraw() +
  draw_plot(DTW) +
  draw_plot(ADTW, x = 0.02, y = 0.65, width = 0.3, height = 0.3)
gg_inset_map1


################Figure 2####################
Surface_water<-read.csv('C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Full_CGA/surface_water/New folder _2/SA.csv')
Surface_water$group <- factor(Surface_water$group,levels = c("<25% Percentile","25-50% Percentile","50-75% Percentile",">75% Percentile"))
( Water <- ggplot(Surface_water,aes( x=factor(year) , y=SA))  +
    geom_boxplot(fill = 'green')+
    stat_boxplot(geom ='errorbar', width = 0.6)
  
  +
    theme(panel.grid.minor.x = element_blank()) +
    # remove facet spacing on x-direction
    theme(panel.spacing.x = unit(1,"line"))+
    theme(strip.placement = 'outside',
          strip.background.x = element_blank())+
    labs(x = "Year", y =  "Surface Water Area (ha)", size = 10) +
    scale_y_continuous(breaks = seq(0,1600,200), limits=c(0,1600),labels = comma) +
    scale_x_discrete(breaks=seq(2005, 2019, 2))+
    theme(axis.text = element_text(size=8), axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = c(0.5, 0.92), legend.background = element_rect(fill="transparent"))+theme_bw() +
    coord_cartesian(ylim = c(0, 1600))+
    facet_wrap(~group, ncol = 2, scales = "free") +
    theme(panel.grid.minor.x = element_blank()) + 
    # remove facet spacing on x-direction
    theme(panel.spacing.x = unit(1,"line")) +
    # switch the facet strip label to outside 
    # remove background color
    theme(strip.placement = 'outside',
          strip.background.x = element_blank())+
    theme(legend.justification = c(0, 1), 
          legend.position = "bottom",
          legend.box.margin=margin(c(0,10,0,10)))+
    theme(legend.justification = c(0.9, 2), legend.position = c(1,0.05)    )
)

###depth to water
( dtw <- ggplot(Surface_water,aes( x=factor(year) , y=spring_dtw))  +
    geom_boxplot(fill = 'green')+
    stat_boxplot(geom ='errorbar', width = 0.6)
  
  +
    theme(panel.grid.minor.x = element_blank()) +
    # remove facet spacing on x-direction
    theme(panel.spacing.x = unit(1,"line"))+
    theme(strip.placement = 'outside',
          strip.background.x = element_blank())+
    labs(x = "Year", y =  " Depth-to-water(ft.)", size = 10) +
    scale_y_continuous(breaks = seq(0,140,10), limits=c(0,140),labels = comma) +
    scale_x_discrete(breaks=seq(2005, 2019, 2))+
    theme(axis.text = element_text(size=8), axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = c(0.5, 0.92), legend.background = element_rect(fill="transparent"))+theme_bw() +
    coord_cartesian(ylim = c(0, 140))+
    facet_wrap(~group, ncol = 2, scales = "free") +
    theme(panel.grid.minor.x = element_blank()) + 
    # remove facet spacing on x-direction
    theme(panel.spacing.x = unit(1,"line")) +
    # switch the facet strip label to outside 
    # remove background color
    theme(strip.placement = 'outside',
          strip.background.x = element_blank())+
    theme(legend.justification = c(0, 1), 
          legend.position = "bottom",
          legend.box.margin=margin(c(0,10,0,10)))+
    theme(legend.justification = c(0.9, 2), legend.position = c(1,0.05)    )
)


############################## Figure 4##################
depth<-read.dta13("C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Full_CGA/Map/Data/plot.dta")


Level_Impact <- factor(depth$group,levels = c("<25%","25-50%","50-75%",">75%"))

Result<-ggplot(depth,aes( x=Level_Impact , average_change_2) )+
  geom_point(  position = position_dodge(.3)) +
  geom_errorbar(aes(ymax = max5,
                    ymin = min5),position = position_dodge(0.3),
                width = 0.1)+
  theme(axis.text = element_text(size = 10),)+
  
  scale_x_discrete(label = c( "<25%","25-50%","50-75%",">75%")  )+
  theme_bw()+
  labs(y = "Reduction in Depth-to-Groundwater  ",
       x = "Watershed",
       colour = "")+
  guides(colour=guide_legend(ncol=3))+
  theme(legend.text=element_text(size=rel(1)))+
  theme(legend.key.size = unit(1.5,"line"))+
  theme(axis.title  = element_text(size = 12))+
  theme(axis.text = element_text(size = 10))+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))+
  theme(axis.title.x = element_text(margin =  margin(15,0,0,0)))+
  theme(legend.title=element_blank())+
  theme(plot.title = element_text(size=12, face="plain",hjust = 0.5, vjust=1)) +
  coord_cartesian(ylim = c(-2, 0))+
  theme(legend.justification = c(0, 1), 
        legend.position = "bottom",
        legend.box.margin=margin(c(0,10,0,10)))+
  theme(legend.justification = c(0.9, 2), legend.position = c(0.98,0.06)    )+
  geom_abline(aes(slope = 0, intercept = 0), size = 0.5,   show.legend = FALSE,linetype="dashed") 

Result

