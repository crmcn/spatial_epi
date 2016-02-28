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


map <- readShapePoly("J:/Project/Evaluation/GAVI/mapping/uga/output/uga_analytic_area_map.shp")
names <- read.dbf("J:/Project/Evaluation/GAVI/mapping/uga/output/uga_analytic_area_map.dbf", as.is = FALSE)
child <- read.csv("J:/Project/Evaluation/GAVI/hhs/uga/data_production/release/child_external.csv",
                  numerals="no.loss")
# length(unique(child$customid))
col_classes <- sapply(child[1,], class)
col_classes["custom_id"] <- "character"
child2 <- read.csv("J:/Project/Evaluation/GAVI/hhs/uga/data_production/release/child_external.csv",
                  colClasses=col_classes)
household <- read.csv("J:/Project/Evaluation/GAVI/hhs/uga/data_production/release/household_external.csv",
                      numerals="no.loss")
col_classes <- sapply(household[1,], class)
col_classes["custom_id"] <-"character"
household2 <- read.csv("J:/Project/Evaluation/GAVI/hhs/uga/data_production/release/household_external.csv",
                           colClasses=col_classes)
summary()

# Set up plot_uga function used in homework 1
plot_uga <- function(plotvar, title_, nclr=8, brks=NULL){
    # plots a map of uganda counties assuimg the plotvar argument is of
    # length 88 where each value represents an Ohio county sorted by 
    # fips number.
    if(length(plotvar) != 113){
        stop("'plotvar' argument must be of length 88")
    }
    # next few lines set up the color scheme for plotting
    plotclr <- brewer.pal(nclr,"BuPu")
    if (is.null(brks)){
        brks <- round(quantile(plotvar,probs=seq(0,1,1/(nclr))),digits=1)
    }
    colornum <- findInterval(plotvar,brks,all.inside=T)
    colcode <- plotclr[colornum]
    # Note order of data in file is in terms of increasing FIPS codes, 
    # which is the same as in the map function (see county.fips)
    map("county", "ohio",col=colcode,fill=T)
    title(title_)
    leg.txt <- paste("[",brks[nclr],",",brks[nclr+1],"]",sep="")
    for(i in (nclr-1):1){
        leg.txt <- append(leg.txt,paste("[",brks[i],",",brks[i+1],")",sep=""))
    }
    leg.txt <- rev(leg.txt)
    legend("bottomright",legend=leg.txt,fill=plotclr,bty="n",cex=.8)
}
