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
library(spatstat)

if(.Platform$OS.type == "windows"){
    setwd("J:/Project/Evaluation/GAVI/")
}
if(.Platform$OS.type == "unix"){
    setwd("/home/j/Project/Evaluation/GAVI/")
}

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

hh_dist <- (subset(household, latitude > 0 & latitude < 2 & 
                       longitude > 31.5 & longitude < 33))

## Plot the households

projection <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
proj4string(map) <- projection

###### ASK NEAL ABOUT THE BELOW LINE #####
#pointdf <- SpatialPointsDataFrame(coords=household[,c("latitude", "longitude")],
#                                  data=household[,c("customid", "hh_rooms")],
#                                  proj4string = CRS(projection))

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
merged_df <- merge(x = subset(child, customid %in% hh_dist$customid),
                   y = hh_dist, by = "customid", all.x = TRUE)

## Now plot circles representing the prevalence of diarrhea
merged_df$dia_bi<-as.numeric(merged_df$dia == "Yes")

# are diahhrea age and sex independent?
hist(subset(merged_df, dia_bi == 1)$child_age)
hist(subset(merged_df, dia_bi == 0)$child_age)
hist(subset(merged_df, !is.na(dia_bi))$child_age)
table(subset(merged_df, dia_bi == 0)$child_sex)
table(subset(merged_df, dia_bi == 1)$child_sex)

# build proportions to plot
agg_dia <- aggregate(dia_bi ~ customid, merged_df, sum)
agg_dia[,"children_dia"] <- agg_dia$dia_bi
agg_dia[,"dia_bi"] <- NULL
child_count <- as.data.frame(table(merged_df$customid[!is.na(merged_df$dia_bi)]))
names(child_count) <- c("customid", "child_count")
agg_dia <- merge(x=agg_dia, y=child_count, by="customid", all.x=T)
agg_dia$dia_prop <- agg_dia$children_dia / agg_dia$child_count

hh_dist <- merge(x=hh_dist, y=agg_dia, by="customid", all.x=TRUE)

hh_sub <- subset(hh_dist, !is.na(dia_prop))

# actually we are only gonna plot numbers cause thats more important
uganda_point_plot2 <- ggplot() +
    geom_polygon(data=polygondf, aes(x=long,y=lat,group=group), 
                 fill='grey90', color='grey') +
    geom_point(data=hh_sub[,c("latitude", "longitude")], 
               aes(x=longitude, y=latitude), 
               col=(hh_sub$children_dia*10)+1,cex=(hh_sub$children_dia*2),pch=20) +
    coord_fixed() +
    ggtitle("Diarrhea") +
    theme_minimal() 

uganda_point_plot2

# now use the KDE to build a log odds plot for areas
pointdf <- SpatialPointsDataFrame(coords=merged_df[,c("longitude", "latitude")],
                                  data=merged_df[,c("child_age", "dia_bi")],
                                  proj4string = CRS(projection))

pointdf <- subset(pointdf, !is.na(dia_bi))

test_bandwith <- function(bw_dia, p = pointdf){
    ppp_dia <- as(pointdf, "ppp")
    ppp_dia$window <- as(map, "owin")
    cases <- unmark(subset(ppp_dia, marks(ppp_dia)$dia_bi == 1))
    ncases <- npoints(cases)
    controls <- unmark(subset(ppp_dia, marks(ppp_dia)$dia_bi == 0))
    ncontrols <- npoints(controls)
    kcases <- density(cases, bw_dia)
    kcontrols <- density(controls, bw_dia)
    spkratio0 <- as(kcases, "SpatialGridDataFrame")
    names(spkratio0) <- "kcases"
    spkratio0$kcontrols <- as(kcontrols, "SpatialGridDataFrame")$v
    spkratio <- as(spkratio0, "SpatialPixelsDataFrame")
    spkratio$kratio <- (spkratio$kcases + .00000001)/(spkratio$kcontrols + .00000001)
    spkratio$krationorm <- spkratio$kratio/(ncases/ncontrols)
    spkratio$logratio <-log(spkratio$kratio)-log(ncases/ncontrols)
    spkratio
}

smooth <- .01

spkratio_sub <- test_bandwith(bw_dia=smooth, p = pointdf)
p_sub <- spplot(spkratio_sub, "logratio", main=paste0("logkratio: BW = ", smooth))
print(p_sub)
