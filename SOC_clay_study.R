
library(rgdal)
library(plyr)
library(foreign) #to read db files
library(raster)
library(sp)
library(sf)

library(MASS) #for density estimates with kde2d
library(rworldmap)


#define the palettes
LUC_palette_red=rev(c("#009E73", "#F0E442", "#CC79A7"))
köppen_palette=rev(magma(11))




#Read LUCAS geodata
LUCAS_geodata_2015 <- readOGR(dsn = file.path("../2015/LUCAS2015_topsoildata_20200323/LUCAS_Topsoil_2015_20200323-shapefile/LUCAS_Topsoil_2015_20200323.shp"),
                         stringsAsFactors = F)
LUCAS_geodata_2009 <- readOGR(dsn = file.path("../2009/SoilAttr_LUCAS2009/SoilAttr_LUCAS_2009.shp"),
                              stringsAsFactors = F)


#load UNFCCC datasets
load("../../../UNFCCC_data/UNFCCC.RData")
FL$MineralSoilsNetChaTot<-as.numeric(FL$MineralSoilsNetChaTot)
FL_agg <- ddply(FL[FL$Year>2010,], ~  Country, summarize, MinSoilNet = sum(MineralSoilsNetChange))
CL$MineralSoilsNetChaTot<-as.numeric(CL$MineralSoilsNetChaTot)
CL_agg <- ddply(CL[CL$Year>2010,], ~  Country, summarize, MinSoilNet = sum(MineralSoilsNetChange))
GL$`OrganicSoilsNetChange(tC/ha)`<-as.numeric(GL$`OrganicSoilsNetChange(tC/ha)`)
GL_agg <- ddply(GL[GL$Year>2010,], ~  Country, summarize, MinSoilNet = sum(`MineralSoilsNetChange(tC/ha)`))


# rename the forest class
LUCAS_geodata_2015@data$LC0_Desc[LUCAS_geodata_2015@data$LC0_Desc=="Woodland"]="Forest"


#read the soil map and associated files
Soilmap_Europe_shapefile <- readOGR(dsn = file.path("../../soilDB_shapefiles_and_attributes/sgdbe4_0.shp"),
                                    stringsAsFactors = F)
#reprojectiong to LUCAS 205
Soilmap_Europe_shapefile_WGS84 <- spTransform(Soilmap_Europe_shapefile,
                              crs(LUCAS_geodata_2015))
STU_sgdbe <- read.dbf("../../soilDB_shapefiles_and_attributes/stu_sgdbe.dbf")







#<*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*>*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*>*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*>
#### Soil classes 
#<*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*>*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*>*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*>

soil_types<-c()
soil_types_desc<-c()
for(i in 1:length(Soilmap_Europe_shapefile_WGS84@data$SMU)){
  if(length(SMU_sgdbe[Soilmap_Europe_shapefile_WGS84@data[i,]$SMU== SMU_sgdbe$SMU,]$WRBLV1)>0)
  { soil_types[i]<- SMU_sgdbe[Soilmap_Europe_shapefile_WGS84@data[i,]$SMU== SMU_sgdbe$SMU,]$WRBLV1
  soil_types_desc[i]<- as.character(SMU_sgdbe[Soilmap_Europe_shapefile_WGS84@data[i,]$SMU== SMU_sgdbe$SMU,]$WRBLV1)
  }else {
    soil_types[i]<-NA
    soil_types_desc[i]<-NA
  }
}

Soilmap_Europe_shapefile_WGS84@data<-cbind(Soilmap_Europe_shapefile_WGS84@data, soil_types, soil_types_desc)



## parser, defines three vectors with soil classifications and qualifiers
WRBFU <- c()
WRBLV1 <- c()
WRBFU_group <- c()
WRBFU_qual <- c()
for (i in 1:length(SMU_LUCAS)) {
  which_SMU <- which(SMU_sgdbe$SMU == SMU_LUCAS[i])
  # WRBFU[i] <- SMU_sgdbe[which_SMU,]$WRBFU
  # WRBLV1[i] <- SMU_sgdbe[which_SMU,]$WRBLV1
  which_STU <- which(STU_sgdbe$STU == STU_LUCAS[i])
  if(length(which_STU)!=0){
    WRBFU[i] <- as.character(STU_sgdbe[which_STU,]$WRBFU)
    WRBFU_group[i] <- str_sub(as.character(STU_sgdbe[which_STU,]$WRBFU), 1,2)
    WRBFU_qual[i] <- str_sub(as.character(STU_sgdbe[which_STU,]$WRBFU), 3,4)
  } else {
    WRBFU[i] <- NA
    WRBFU_group[i] <- NA
    WRBFU_qual[i] <- NA
  }
}







#<*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*>*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*>*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*>
#### work on the Koppen classification, 
#<*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*>*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*>*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*>

Koppen <- raster("../../../koppen_geiger_tif/1991_2020/koppen_geiger_0p01.tif")

KoppenValue=extract(Koppen, LUCAS_geodata_2009)
KoppenValue <- as.factor(KoppenValue)
Koppen_dictionary <- mat.or.vec(length(levels(KoppenValue)), 2)
Koppen_dictionary[,1] <- levels(KoppenValue)
Koppen_dictionary[,2] <- c("BWh  Arid, desert, hot",
                           "BWk  Arid, desert, cold",
                           "BSh  Arid, steppe, hot",
                           "BSk  Arid, steppe, cold",
                           "Csa  Temperate, dry summer, hot summer" ,
                           "Csb  Temperate, dry summer, warm summer" ,
                           "Cfa  Temperate, no dry season, hot summer" ,
                           "Cfb  Temperate, no dry season, warm summer",
                           "Dsb  Cold, dry summer, warm summer",
                           "Dfa  Cold, no dry season, hot summer",
                           "Dfb  Cold, no dry season, warm summer",
                           "Dfc  Cold, no dry season, cold summer")


Koppen_dictionary <- as.data.frame(Koppen_dictionary)
names(Koppen_dictionary) <- c("Value", "Climate")

e <- as(extent( -25, 40, 36, 71), 'SpatialPolygons')
crs(e) <- "+proj=longlat +datum=WGS84 +no_defs"
Koppen_crop <- crop(Koppen, e)


#<*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*>*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*>*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*>
#### Sampling and sampling density
#<*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*>*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*>*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*>

worldmap <- getMap(resolution="low")


LUCAS_geodata_2015_no_NA<-data.frame(LUCAS_geodata_2015[!is.na(LUCAS_geodata_2015$Clay),])
LUCAS_geodata_2009_no_NA<-data.frame(LUCAS_geodata_2009[!is.na(LUCAS_geodata_2009$clay),])

kde2d_est_2015 <- kde2d(x = LUCAS_geodata_2015_no_NA$coords.x1, y = LUCAS_geodata_2015_no_NA$coords.x2, h=4, n = 150, 
                        lims = c(range(LUCAS_geodata_2015_no_NA$coords.x1), range(LUCAS_geodata_2015_no_NA$coords.x2)))
filled.contour(kde2d_est_2015,color.palette=colorRampPalette(c('white','darkred')))
plot(worldmap, add=TRUE, lwd=0.5)

png("./Appendix/Sampling_density.png", height = 5000, width = 2500, res=300)
par(mar=c(5,5,1,1), mfrow=c(2,1))

smoothScatter(x = LUCAS_geodata_2009_no_NA$coords.x1, y = LUCAS_geodata_2009_no_NA$coords.x2, nbin = 240, bandwidth=0.6,
              colramp = colorRampPalette(c("white", blues9)),
              nrpoints = 5000, ret.selection = FALSE,
              pch = ".", cex = 1.2, col = "black",
              transformation = function(x) x^.25,
              postPlotHook = box,
              xlab = "Longitude", ylab = "Latitude",
              xaxs = par("xaxs"), yaxs = par("yaxs"), main="2009")
points(LUCAS_geodata_2009_no_NA$coords.x1, y = LUCAS_geodata_2009_no_NA$coords.x2, pch = ".", cex = 1.2, col = "black")
plot(worldmap, add=TRUE, lwd=0.5)

smoothScatter(x = LUCAS_geodata_2015_no_NA$coords.x1, y = LUCAS_geodata_2015_no_NA$coords.x2, nbin = 240, bandwidth=0.6,
              colramp = colorRampPalette(c("white", blues9)),
              nrpoints = 5000, ret.selection = FALSE,
              pch = ".", cex = 1.2, col = "black",
              transformation = function(x) x^.25,
              postPlotHook = box,
              xlab = "Longitude", ylab = "Latitude",
              xaxs = par("xaxs"), yaxs = par("yaxs"), main=2015)
plot(worldmap, add=TRUE, lwd=0.5)

dev.off()
