### Claire McNellan ###
### Final Project ###
### Spatial Epi ###
### Winter 2016 ###

rm (list=ls())

set.seed(123)
library(SpatialEpi)
library(sp)
library(Matrix)
library(splines)
library(RColorBrewer)
library(maps)
library(maptools)
library(classInt)
library(foreign)
library(spatial)
library(ggplot2)

if(.Platform$OS.type == "windows"){
    setwd("J:/Project/Evaluation/GAVI/")
}
if(.Platform$OS.type == "unix"){
    setwd("/home/j/Project/Evaluation/GAVI/")
}

.Platform$OS.type

## Bring in data

map <- readShapePoly("mapping/uga/output/uga_analytic_area_map.shp")
names <- read.dbf("mapping/uga/output/uga_analytic_area_map.dbf", as.is = FALSE)
child <- read.csv("hhs/uga/data_production/release/child_external.csv",
                  numerals="no.loss")

household <- read.csv("hhs/uga/data_production/release/household_external.csv",
                      numerals="no.loss")

## Make latitude negative if the latitude is labeled "S" for south

household <- subset(household, longitude != 0)
switch_me <- (household$lat_which == "S 0") & !is.na(household$latitude)
switch_me[is.na(switch_me)] <- FALSE
household$latitude[switch_me] <- 
    household$latitude[switch_me] * -1

## Plot the households

projection <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
proj4string(map) <- projection

###### ASK NEAL ABOUT THE BELOW LINE #####
#pointdf <- SpatialPointsDataFrame(coords=household[,c("latitude", "longitude")],
                                  data=household[,c("customid", "hh_rooms")],
                                  proj4string = CRS(projection))

polygondf <- fortify(map)
uganda_point_plot <- ggplot() +
    geom_polygon(data=polygondf, aes(x=long,y=lat,group=group), 
                 fill='grey90', color='grey') +
    geom_point(data=household[,c("latitude", "longitude")], 
               aes(x=longitude, y=latitude), col="red",pch=20,cex=.03) +
    coord_fixed() +
    ggtitle("Households") +
    theme_minimal() 

uganda_point_plot

## merge the data set
merged_df <- merge(x = child, y = household, by = "customid", all.x = TRUE)

## Now plot circles representing the prevalence of diarrhea
merged_df$dia_bi<-factor(as.numeric(merged_df$dia))
    levels(merged_df$dia_bi)[1]<-0
    levels(merged_df$dia_bi)[2]<-1
    
merged_df$dia_prop <- as.numeric(merged_df$dia_bi) / merged_df$children
merged_df$district_dia <- ## take the average over the district


uganda_point_plot2 <- ggplot() +
    geom_polygon(data=polygondf, aes(x=long,y=lat,group=group), 
                 fill='grey90', color='grey') +
    geom_point(data=household[,c("latitude", "longitude")], 
               aes(x=longitude, y=latitude), col="red",pch=20,cex=.03) +
    coord_fixed() +
    ggtitle("Households") +
    theme_minimal() 


spplot(map,"dist77")