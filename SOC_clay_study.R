
library(viridis)
library(rgdal)
library(plyr)
library(foreign) #to read db files
library(raster)
library(sp)
library(sf)

library(modeest)

library(MASS) #for density estimates with kde2d
library(rworldmap)

library(dplyr) #for joining with the code lookup table

library(ggplot2)
library(ggExtra)
library(ggpubr)
library(paletteer)
library(ggokabeito)

library(caret)
library(parallel)


#parallel processing (here used in CARET)
library(doParallel)
cluster <- makeCluster(detectCores() - 1)
registerDoParallel(cluster)


# palette (colorblind) for plotting the extended land cover classes
palette_LC0=rep(NA, 11)
palette_LC0[1:2] = palette_okabe_ito(seq(1:2))
palette_LC0[3] = palette_okabe_ito(4)
palette_LC0[4] = palette_okabe_ito(3)
palette_LC0[5] = palette_okabe_ito(3)
palette_LC0[6] = palette_okabe_ito(3)
palette_LC0[7:11] = palette_okabe_ito(seq(4:8))


#define the palettes
LUC_palette_red=(c("#999999", "#0072B2", "#009E73", "#E69F00", "#CC79A7"))
LUC_palette=c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

köppen_palette=rev(magma(11))




#Read LUCAS geodata
LUCAS_geodata_2015 <- readOGR(dsn = file.path("../2015/LUCAS2015_topsoildata_20200323/LUCAS_Topsoil_2015_20200323-shapefile/LUCAS_Topsoil_2015_20200323.shp"),
                         stringsAsFactors = F)
LUCAS_geodata_2009 <- readOGR(dsn = file.path("../2009/SoilAttr_LUCAS2009/SoilAttr_LUCAS_2009.shp"),
                              stringsAsFactors = F)
LUCAS_geodata_2012_BG_RO <- readOGR(dsn = file.path("../2009/SoilAttr_BG_RO/SoilAttr_LUCAS_2012_BG_RO.shp"),
                              stringsAsFactors = F)
LUCAS_geodata_2009_CYP_MLT <- readOGR(dsn = file.path("../2009/SoilAttr_CYP_MLT/SoilAttr_LUCAS_2009_CYP_MLT.shp"),
                                    stringsAsFactors = F)
LUCAS_geodata_Iceland <- readOGR(dsn = file.path("../2009/SoilAttr_ICELAND/SoilAttr_ICELAND.shp"),
                                    stringsAsFactors = F)

LUCAS_geodata_2009_2012 <- rbind(LUCAS_geodata_2009, LUCAS_geodata_2012_BG_RO, LUCAS_geodata_2009_CYP_MLT, LUCAS_geodata_Iceland)




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
SMU_sgdbe <- read.dbf("../../soilDB_shapefiles_and_attributes/smu_sgdbe.dbf")

#spatial join
SMU_LUCAS<-over(LUCAS_geodata_2009_2012, Soilmap_Europe_shapefile_WGS84)$SMU
STU_LUCAS<-over(LUCAS_geodata_2009_2012, Soilmap_Europe_shapefile_WGS84)$STU







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
WRBFU_group <- c()
WRBFU_qual <- c()
WRBFU_SMU_LV1 <- c()
for (i in 1:length(SMU_LUCAS)) {
  which_SMU <- which(SMU_sgdbe$SMU == SMU_LUCAS[i])
  # WRBFU[i] <- SMU_sgdbe[which_SMU,]$WRBFU
  # WRBLV1[i] <- SMU_sgdbe[which_SMU,]$WRBLV1
  which_STU <- which(STU_sgdbe$STU == STU_LUCAS[i])
  if(length(which_STU)!=0){
    WRBFU[i] <- as.character(STU_sgdbe[which_STU,]$WRBFU)
    WRBFU_group[i] <- stringr::str_sub(as.character(STU_sgdbe[which_STU,]$WRBFU), 1,2)
    WRBFU_qual[i] <- stringr::str_sub(as.character(STU_sgdbe[which_STU,]$WRBFU), 3,4)
  } else {
    WRBFU[i] <- NA
    WRBFU_group[i] <- NA
    WRBFU_qual[i] <- NA
  }
  
  if(length(which_SMU)!=0){
    WRBFU_SMU_LV1[i] <- as.character(SMU_sgdbe[which_SMU,]$WRBLV1)
  } else {
    WRBFU_SMU_LV1[i] <- NA
  }
}

WRB_LV1_dictionary=data.frame(name=c(
"Albeluvisol",
"Acrisol",
"Alisol",
"Andosol",
"Arenosol",
"Anthrosol",
"Chernozem",
"Calcisol",
"Cambisol",
"Cryosol",
"Durisol",
"Fluvisol",
"Ferralsol",
"Gleysol",
"Gypsisol",
"Histosol",
"Kastanozem",
"Leptosol",
"Luvisol",
"Lixisol",
"Nitisol",
"Phaeozem",
"Planosol",
"Plinthosol",
"Podzol",
"Regosol",
"Solonchak",
"Solonetz",
"Umbrisol",
"Vertisol",
"Town",
"Soil disturbed by man",
"Water body",
"Marsh",
"Glacier",
"Rock outcrops", "Greyzem", "Podzoluvisol"),

code=c(
"AB",
"AC",
"AL",
"AN",
"AR",
"AT",
"CH",
"CL",
"CM",
"CR",
"DU",
"FL",
"FR",
"GL",
"GY",
"HS",
"KS",
"LP",
"LV",
"LX",
"NT",
"PH",
"PL",
"PT",
"PZ",
"RG",
"SC",
"SN",
"UM",
"VR",
"1",
"2",
"3",
"4",
"5",
"6", "GR", "PD"))



#<*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*>*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*>*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*>
#### work on the Köppen classification, 
#<*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*>*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*>*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*>

Koppen <- raster("../../../koppen_geiger_tif/1991_2020/koppen_geiger_0p01.tif")
#downscaled_raster <- aggregate(Koppen, fact = 5, fun = mean)


KoppenValue=extract(Koppen, LUCAS_geodata_2009_2012)
KoppenValue <- as.factor(KoppenValue)
Koppen_dictionary <- mat.or.vec(length(levels(KoppenValue)), 4)
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
                           "Dfc  Cold, no dry season, cold summer",
                           "ET  Polar, tundra")

#adding a simplified description
Koppen_dictionary[,3] <- c("BWh, Desert, hot",
                           "BWk, Desert, cold",
                           "BSh, Steppe, hot",
                           "BSk, Steppe, cold",
                           "Csa, Mediterranean (hot summer)" ,
                           "Csb, Mediterranean (warm summer)" ,
                           "Cfa, Oceanic subtropical" ,
                           "Cfb, Oceanic temperate",
                           "Dsb, Continental (mediterranean influenced)",
                           "Dfa, Continental humid (hot summer)",
                           "Dfb, Continental humid (warm summer)",
                           "Dfc, Continental (subartic)",
                           "ET, Polar tundra")

Koppen_dictionary[,4] <- c("BWh",
                           "BWk",
                           "BSh",
                           "BSk",
                           "Csa" ,
                           "Csb" ,
                           "Cfa" ,
                           "Cfb",
                           "Dsb",
                           "Dfa",
                           "Dfb",
                           "Dfc",
                           "ET")

Koppen_dictionary <- as.data.frame(Koppen_dictionary)
names(Koppen_dictionary) <- c("Value", "Climate", "Simplified", "Code")

KoppenClim <- Koppen_dictionary$Simplified[match(KoppenValue, Koppen_dictionary$Value)]
#KoppenClim <- merge(data.frame(Value = KoppenValue), Koppen_dictionary[, c("Value", "Simplified")], by = "Value", all.x = TRUE)$Simplified


# define the geographic area where we will work
e <- as(extent( -25, 40, 36, 71), 'SpatialPolygons')
crs(e) <- "+proj=longlat +datum=WGS84 +no_defs"
Koppen_crop <- crop(Koppen, e)

# 
# # Filter the raster to keep only pixels with a value of 7
# value_to_plot <- 7
# filtered_raster <- Koppen_crop
# filtered_raster[filtered_raster != value_to_plot] <- NA
# 
# # Plot the filtered raster using levelplot from rasterVis
# rasterVis::levelplot(filtered_raster, margin = FALSE)


#<*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*>*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*>*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*>
#### Sampling and sampling density
#<*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*>*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*>*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*>

worldmap <- getMap(resolution="low")


LUCAS_geodata_2015_no_NA<-data.frame(LUCAS_geodata_2015[!is.na(LUCAS_geodata_2015$Clay),])
LUCAS_geodata_2009_2012_no_NA<-data.frame(LUCAS_geodata_2009_2012[!is.na(LUCAS_geodata_2009_2012$clay),])

which(LUCAS_geodata_2009_2012$Country == "Croatia")



kde2d_est_2015 <- kde2d(x = LUCAS_geodata_2015_no_NA$coords.x1, y = LUCAS_geodata_2015_no_NA$coords.x2, h=4, n = 150, 
                        lims = c(range(LUCAS_geodata_2015_no_NA$coords.x1), range(LUCAS_geodata_2015_no_NA$coords.x2)))

png("./Appendix/Sampling_density.png", height = 4600, width = 2800, res=300)
par(mar=c(5,5,1,1), mfrow=c(2,1))

smoothScatter(x = LUCAS_geodata_2009_2012_no_NA$coords.x1, y = LUCAS_geodata_2009_2012_no_NA$coords.x2, nbin = 240, bandwidth=0.6,
              colramp = colorRampPalette(c("white", blues9)),
              nrpoints = 5000, ret.selection = FALSE,
              pch = ".", cex = 1.2, col = "black",
              transformation = function(x) x^.25,
              postPlotHook = box,
              xlab = "Longitude", ylab = "Latitude",
              xaxs = par("xaxs"), yaxs = par("yaxs"), main="2009", xlim=range(LUCAS_geodata_2009_2012_no_NA$coords.x1), ylim=range( LUCAS_geodata_2009_2012_no_NA$coords.x2))
points(LUCAS_geodata_2009_2012_no_NA$coords.x1, y = LUCAS_geodata_2009_2012_no_NA$coords.x2, pch = ".", cex = 1.2, col = "black")
plot(worldmap, add=TRUE, lwd=0.5)

smoothScatter(x = LUCAS_geodata_2015_no_NA$coords.x1, y = LUCAS_geodata_2015_no_NA$coords.x2, nbin = 240, bandwidth=0.6,
              colramp = colorRampPalette(c("white", blues9)),
              nrpoints = 5000, ret.selection = FALSE,
              pch = ".", cex = 1.2, col = "black",
              transformation = function(x) x^.25,
              postPlotHook = box,
              xlab = "Longitude", ylab = "Latitude",
              xaxs = par("xaxs"), yaxs = par("yaxs"), main="2015",range(LUCAS_geodata_2009_2012_no_NA$coords.x1), ylim=range( LUCAS_geodata_2009_2012_no_NA$coords.x2))
plot(worldmap, add=TRUE, lwd=0.5)

dev.off()

which(LUCAS_geodata_2009_2012_no_NA$Country == "Croatia")



#<*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*>*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*>*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*>
#### Combining the datasets (2009)
#<*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*>*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*>*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*>


# a bit of data massaging
LUCAS_geodata_WRB<-cbind(LUCAS_geodata_2009_2012@data, WRBFU=as.factor(WRBFU), WRBFU_group=as.factor(WRBFU_group),
                         WRBFU_qual=as.factor(WRBFU_qual),
                         WRBFU_SMU_LV1=as.factor(WRBFU_SMU_LV1), LUCAS_geodata_2009_2012@coords, KoppenValue, KoppenClim)


LUCAS_geodata_WRB[LUCAS_geodata_WRB$KoppenValue==7,]$coords.x2
range(LUCAS_geodata_WRB[LUCAS_geodata_WRB$KoppenClim=="BSk, Steppe, cold"   ,]$coords.x2, na.rm=T)
LUCAS_geodata_WRB[LUCAS_geodata_WRB$KoppenClim=="BSk, Steppe, hot"   ,]$coords.x2


LUCAS_geodata_WRB<-na.omit(LUCAS_geodata_WRB)
as.factor(LUCAS_geodata_WRB$Country)

#some soil points are not well defined
LUCAS_geodata_WRB<-LUCAS_geodata_WRB[!(LUCAS_geodata_WRB$WRBFU_group==levels(LUCAS_geodata_WRB$WRBFU_group)[1] | LUCAS_geodata_WRB$WRBFU_group==levels(LUCAS_geodata_WRB$WRBFU_group)[2] | LUCAS_geodata_WRB$WRBFU_group==levels(LUCAS_geodata_WRB$WRBFU_group)[3]),]
LUCAS_geodata_WRB$WRBFU_group<- droplevels(LUCAS_geodata_WRB$WRBFU_group)

LUCAS_geodata_WRB<-LUCAS_geodata_WRB[!(LUCAS_geodata_WRB$WRBFU_SMU_LV1==levels(LUCAS_geodata_WRB$WRBFU_SMU_LV1)[1] | LUCAS_geodata_WRB$WRBFU_SMU_LV1==levels(LUCAS_geodata_WRB$WRBFU_SMU_LV1)[2] | LUCAS_geodata_WRB$WRBFU_SMU_LV1==levels(LUCAS_geodata_WRB$WRBFU_SMU_LV1)[3]),]
LUCAS_geodata_WRB$WRBFU_SMU_LV1<- droplevels(LUCAS_geodata_WRB$WRBFU_SMU_LV1)


LUCAS_geodata_WRB$KoppenValue <- as.factor(LUCAS_geodata_WRB$KoppenValue)
LUCAS_geodata_WRB$KoppenClim <- as.factor(LUCAS_geodata_WRB$KoppenClim)



#<*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*>*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*>*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*>
#### Land use
#<*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*>*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*>*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*>

#loading the land use data and joining them to the dataset
LUCAS_2009_primary <- read.csv("../2009/LUCAS_2009_primary/EU_2009_20200213.CSV.csv")
LUCAS_LU_classes <- read.csv("../2009/LUCAS_2009_primary/LU_classes.csv") #loading the classes names lookup table
colnames(LUCAS_LU_classes)<-c("LU1", "Class")
merged_data <- merge(LUCAS_2009_primary[,c( "POINT_ID", "LU1" , "LU2", "LC1")], LUCAS_geodata_WRB, by = "POINT_ID")

LC0<-substr(merged_data$LC1, 1,1)

LC0[LC0=="A"]="Artificial"
LC0[LC0=="B"]="Cropland"
LC0[merged_data$LC1=="B70" | merged_data$LC1=="B81" | merged_data$LC1=="BP0"  | merged_data$LC1=="B82"]="Orchards/Wineyards"
LC0[merged_data$LC1=="C10"]="Forest (Broadleaves)"
LC0[merged_data$LC1=="C20"]="Forest (Conifer)"
LC0[merged_data$LC1=="C30"]="Forest (Mixed)"
LC0[LC0=="D"]="Shrubland"
LC0[LC0=="E"]="Grassland"
LC0[LC0=="F"]="Bare land"
LC0[LC0=="G"]="Water body"
LC0[LC0=="H"]="Wetland"

LC0_simpl<-substr(merged_data$LC1, 1,1)

LC0_simpl[LC0_simpl=="A"]="Artificial"
LC0_simpl[LC0_simpl=="B"]="Cropland"
LC0_simpl[LC0_simpl=="C"]="Forest"
LC0_simpl[LC0_simpl=="D"]="Shrubland"
LC0_simpl[LC0_simpl=="E"]="Grassland"
LC0_simpl[LC0_simpl=="F"]="Bare land"
LC0_simpl[LC0_simpl=="G"]="Water body"
LC0_simpl[LC0_simpl=="H"]="Wetland"

merged_data = cbind(merged_data, LC0, LC0_simpl)



# Perform a left join to substitute codes with names
LUCAS_geodata_2009_2012_WRB_merged <- left_join(merged_data, LUCAS_LU_classes, by = "LU1")


#calculate means and modes

# Function to calculate mode using mlv with a specified method
calculate_mode <- function(x, method) {
  return(mlv(x, method = "venter"))
}

LUCAS_geodata_2009_2012_WRB_merged_filtered <- LUCAS_geodata_2009_2012_WRB_merged[LUCAS_geodata_2009_2012_WRB_merged$OC<200,]

LUCAS_geodata_2009_2012_WRB_merged_filtered$OC<- as.numeric(LUCAS_geodata_2009_2012_WRB_merged_filtered$OC)
LUCAS_geodata_2009_2012_WRB_merged_filtered$clay <- as.numeric(LUCAS_geodata_2009_2012_WRB_merged_filtered$clay)*10
LUCAS_geodata_2009_2012_WRB_merged_filtered$LC0_simpl <- as.factor(LUCAS_geodata_2009_2012_WRB_merged_filtered$LC0_simpl
                                                                   )
png("./Appendix/OC_Clay_modes.png", width = 3000, height=5000, res=320)
par(mfrow=c(2,1))
#OC
means_OC <- tapply(LUCAS_geodata_2009_2012_WRB_merged_filtered$OC, LUCAS_geodata_2009_2012_WRB_merged_filtered$LC0_simpl, FUN = mean)
medians_OC <- tapply(LUCAS_geodata_2009_2012_WRB_merged_filtered$OC, LUCAS_geodata_2009_2012_WRB_merged_filtered$LC0_simpl, FUN = median)
modes_OC <- c()


plot(density(LUCAS_geodata_2009_2012_WRB_merged_filtered$OC), main = "SOC", xlab = "C (g kg-1)", 
     xlim = range(LUCAS_geodata_2009_2012_WRB_merged_filtered$OC), col=NA, ylim=c(0,0.1))
# Create density histograms and add vertical lines for modes
for (i in 1:length(levels(LUCAS_geodata_2009_2012_WRB_merged_filtered$LC0_simpl))) {
  level=levels(LUCAS_geodata_2009_2012_WRB_merged_filtered$LC0_simpl)[i]
  subset_data <- LUCAS_geodata_2009_2012_WRB_merged_filtered$OC[LUCAS_geodata_2009_2012_WRB_merged_filtered$LC0_simpl == level]
  polygon(density(subset_data), col=prettyGraphs::add.alpha(palette_LC0[i]))
  modes_OC[i] <-density(subset_data)$x[ which.max(density(subset_data)$y)]
}

for (i in 1:length(levels(LUCAS_geodata_2009_2012_WRB_merged_filtered$LC0_simpl))) {
  abline(v = modes_OC[i], col = palette_LC0[i], lty = 2)
}
  
#Clay
means_Clay <- tapply(LUCAS_geodata_2009_2012_WRB_merged_filtered$clay, LUCAS_geodata_2009_2012_WRB_merged_filtered$LC0_simpl, FUN = mean)
medians_Clay <- tapply(LUCAS_geodata_2009_2012_WRB_merged_filtered$clay, LUCAS_geodata_2009_2012_WRB_merged_filtered$LC0_simpl, FUN = median)
modes_Clay <- c()


plot(density( LUCAS_geodata_2009_2012_WRB_merged_filtered$clay), main = "Clay", xlab = "Clay (g kg-1)", 
     xlim = range(LUCAS_geodata_2009_2012_WRB_merged_filtered$clay), col=NA, ylim=c(0,0.01))
# Create density histograms and add vertical lines for modes
for (i in 1:length(levels(LUCAS_geodata_2009_2012_WRB_merged_filtered$LC0_simpl))) {
  level=levels(LUCAS_geodata_2009_2012_WRB_merged_filtered$LC0_simpl)[i]
  subset_data <- LUCAS_geodata_2009_2012_WRB_merged_filtered$clay[LUCAS_geodata_2009_2012_WRB_merged_filtered$LC0_simpl == level]
  polygon(density(subset_data), col=prettyGraphs::add.alpha(palette_LC0[i]))
  modes_Clay[i] <-density(subset_data)$x[ which.max(density(subset_data)$y)]
}

for (i in 1:length(levels(LUCAS_geodata_2009_2012_WRB_merged_filtered$LC0_simpl))) {
  abline(v = modes_Clay[i], col = palette_LC0[i], lty = 2)
}

dev.off()

write.csv(round(data.frame(means_OC, medians_OC, modes_OC, means_Clay, medians_Clay, modes_Clay),3), file="./Appendix/S2.csv")


#<*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*>*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*>*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*>
#### Degraded soils by group
#<*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*>*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*>*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*>

LUCAS_geodata_2009_2012_WRB_merged$clay<-as.numeric(LUCAS_geodata_2009_2012_WRB_merged$clay)
LUCAS_geodata_2009_2012_WRB_merged$OC<-as.numeric(LUCAS_geodata_2009_2012_WRB_merged$OC)
names(LUCAS_geodata_2009_2012_WRB_merged)

# Calculate the OC_clay_wrb values
OC_clay_wrb <- with(LUCAS_geodata_2009_2012_WRB_merged, OC / (clay * 10))

# Create a data frame with WRB and NUTS columns
df.WRB <- data.frame(
  WRB = LUCAS_geodata_2009_2012_WRB_merged$WRBFU_SMU_LV1,
  NUTS = LUCAS_geodata_2009_2012_WRB_merged$Country,
  LU = LUCAS_geodata_2009_2012_WRB_merged$Class,
  LC = LUCAS_geodata_2009_2012_WRB_merged$LC0,
  LC_simpl = LUCAS_geodata_2009_2012_WRB_merged$LC0_simpl, 
  Koppenclim = LUCAS_geodata_2009_2012_WRB_merged$KoppenClim)



# Count elements below a numerical criterion by WRB and NUTS
threshold <- 1 / 13
below_threshold <- OC_clay_wrb <= threshold

# Create a table of counts by NUTS and calculate proportions by NUTS
table_by_NUTS <- table(df.WRB$NUTS, below_threshold)
proportions_by_NUTS <- prop.table(table_by_NUTS, margin = 1)

# Create a table of counts by WRB and calculate proportions
table_by_WRB <- table(df.WRB$WRB, below_threshold)
table_by_WRB<-table_by_WRB[rowSums(table_by_WRB)>50,]
proportions_by_WRB <- prop.table(table_by_WRB, margin = 1)

# Create a table of counts by land cover and NUTS and calculate proportions by NUTS
table_by_LC_NUTS <- table(df.WRB$NUTS, df.WRB$LC_simpl, below_threshold)
proportions_by_LC_NUTS <- prop.table(table_by_LC_NUTS, margin = 1)


# Create a table of counts by NUTS and Koppen and calculate proportions 
table_by_LC_Koppen_NUTS_all <- table(df.WRB$Koppenclim, df.WRB$NUTS)
proportions_by_LC_Koppen_NUTS_all <- round(prop.table(table_by_LC_Koppen_NUTS_all, margin = 1)*100,1)


# Create a table of counts by land cover and Koppen and calculate proportions by NUTS
table_by_LC_Koppen <- table(df.WRB$Koppenclim, df.WRB$LC_simpl, below_threshold)
proportions_by_LC_Koppen <- prop.table(table_by_LC_Koppen, margin = 1)

table_by_LC_Koppen_all <- table(df.WRB$Koppenclim, below_threshold)
proportions_by_LC_Koppen_all <- round(prop.table(table_by_LC_Koppen_all, margin = 1)*100,1)

table_by_Koppen_LUC <- table(df.WRB$Koppenclim, df.WRB$NUTS)
proportions_by_LC_Koppen_LUC <- table_by_Koppen_LUC/sum(table_by_Koppen_LUC)

table_by_Koppen_LUC[9,]/sum(table_by_Koppen_LUC[9,])

# Create a table of counts by land use and calculate proportions
table_by_LU <- table(df.WRB$LU, below_threshold)
proportions_by_LU <- prop.table(table_by_LU, margin = 1)

# Create a table of counts by land cover and calculate proportions
table_by_LC <- table(df.WRB$LC, below_threshold)
proportions_by_LC <- prop.table(table_by_LC, margin = 1)

table_by_LC_simpl <- table(df.WRB$LC_simpl, below_threshold)
proportions_by_LC_simpl <- prop.table(table_by_LC_simpl, margin = 1)

table_by_LC_all <- table(df.WRB$LC)
proportions_by_LC_all <- round(table_by_LC_all/sum(table_by_LC_all)*100,2)



# writing the appendix tables
write.csv(cbind(n=rowSums(table_by_LC_simpl), 
                share=round(rowSums(table_by_LC_simpl)/sum(rowSums(table_by_LC_simpl))*100,3),
                degraded=round((table_by_LC_simpl[,2]/rowSums(table_by_LC_simpl))*100,3)), "./Appendix/S1.csv")
write.csv(round(proportions_by_LC,3), "./Appendix/S3.csv")
write.csv(round(proportions_by_WRB,3), "./Appendix/S4.csv")


proportions_by_LC*100



#forest proportion of total
sum(proportions_by_LC_all[4:6])
sum(proportions_by_LC_all[c(3,8)])


write.csv(round(proportions_by_LC*100, 0), "./Appendix/proportions_by_LC.csv")

# Sort prportions based on the number of negatives in descending order
proportions_by_WRB <- proportions_by_WRB[order(proportions_by_WRB[,"TRUE"], decreasing = TRUE),]
proportions_by_NUTS <- proportions_by_NUTS[order(proportions_by_NUTS[,"TRUE"], decreasing = TRUE),]
proportions_by_LU <- proportions_by_LU[order(proportions_by_LU[,"TRUE"], decreasing = TRUE),]
proportions_by_LC <- proportions_by_LC[order(proportions_by_LC[,"TRUE"], decreasing = TRUE),]


range(proportions_by_NUTS[,2])*100
mean(proportions_by_NUTS[,2])
sum(table_by_NUTS[,2])/sum(table_by_NUTS)
proportions_by_LC_simpl*100
proportions_by_LC*100


colnames(proportions_by_WRB)<- c("Healthy", "Degraded")
colnames(proportions_by_NUTS)<- c("Healthy", "Degraded")
colnames(proportions_by_LU)<- c("Healthy", "Degraded")
colnames(proportions_by_LC)<- c("Healthy", "Degraded")

# Create a named vector mapping WRB codes to denominations
WRB_denominations <- setNames(WRB_LV1_dictionary$name, WRB_LV1_dictionary$code)

# Access denomination by WRB code
denomination <- WRB_denominations[rownames(proportions_by_WRB)]


png("./appendix/degraded_by_soilgroup.png", width=2000, height=2000, res=300)
par(mar=c(6,5,4,1))
barpl<-barplot(t(proportions_by_WRB)*100, las=2, names.arg=denomination, col=c("cadetblue2", "firebrick1"),
               legend.text = TRUE, 
               args.legend = list(x = "topright",
                                  inset = c(0.35, -0.17)),
               ylim=c(0,100),  ylab="Proportion of total data points considered (%)", main="")
box()
dev.off()



png("./appendix/degraded_by_land_use.png", width=2000, height=2000, res=300)
par(mar=c(9,5,4,1))
barpl<-barplot(t(proportions_by_LU)*100, las=2, names.arg=rownames(proportions_by_LU), col=c("cadetblue2", "firebrick1"),
               legend.text = TRUE, 
               args.legend = list(x = "topright",
                                  inset = c(0.35, -0.17)),
               ylim=c(0,100),  ylab="Proportion of total data points considered (%)", main="")
box()
dev.off()

png("./appendix/degraded_by_land_cover.png", width=2000, height=2000, res=300)
par(mar=c(9,5,4,1))
excluded_classes=c("Artificial", "Water body")
which_excluded=rownames(proportions_by_LC) %in% excluded_classes
barpl<-barplot(t(proportions_by_LC[!which_excluded,])*100, las=2, names.arg=rownames(proportions_by_LC[!which_excluded]), col=c("cadetblue2", "firebrick1"),
               legend.text = TRUE, 
               args.legend = list(x = "topright",
                                  inset = c(0.35, -0.17)),
               ylim=c(0,100),  ylab="Proportion of total data points considered (%)", main="")
box()
dev.off()



png("./appendix/degraded_by_nation.png", width=2000, height=2000, res=300)
par(mar=c(9,5,4,1))
barpl<-barplot(t(proportions_by_NUTS)*100, las=2,  col=c("cadetblue2", "firebrick1"),
               legend.text = TRUE, 
               args.legend = list(x = "topright",
                                  inset = c(0.35, -0.17)),
               ylim=c(0,100),  ylab="Proportion of total data points considered (%)", main="")
box()
dev.off()



#<*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*>*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*>*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*>
#### Plotting the space by land use
#<*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*>*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*>*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*>


LUCAS_geodata_2009_2012_WRB_merged$LC0 <- as.factor(LUCAS_geodata_2009_2012_WRB_merged$LC0)
LUCAS_geodata_2009_2012_WRB_merged$LC0_simpl <- as.factor(LUCAS_geodata_2009_2012_WRB_merged$LC0_simpl)
LUCAS_geodata_2009_2012_WRB_merged$clay <- LUCAS_geodata_2009_2012_WRB_merged$clay *10


considered_LC<-c("Bare land", "Cropland", "Forest", "Grassland", "Shrubland")
considered_LC_reduced=c( "Cropland",  "Forest",    "Grassland")



png("LUCAS_clay_C_LUC_groups_ggplot.png", width=2000, height=1700, res=300)
# create scatter plot using ggplot() function
plot <- ggplot(LUCAS_geodata_2009_2012_WRB_merged[LUCAS_geodata_2009_2012_WRB_merged$LC0_simpl %in% considered_LC_reduced,], aes(x=clay, y=OC, color=LC0_simpl))+
  geom_point(aes(shape=LC0_simpl))+
  theme(legend.position="none")+
  theme_bw() +
  ylim(-10, 200)+
  theme(legend.position = "bottom") +
  xlab(expression(paste("Clay (g ", kg^-1, " dw)"))) +
  ylab(expression(paste("SOC (g ", kg^-1, " dw)")))+
  #scale_colour_paletteer_d("MetBrewer::Juarez")+
  #scale_shape_manual(values=c(8, 15, 16, 17, 9))+
  scale_colour_manual(values = alpha(LUC_palette_red[2:4], 0.4)) +
  geom_abline(intercept = 0, slope = 1/13, color="black", linetype="dashed", size=1)+
  labs(shape= "", colour="")+ guides(colour = guide_legend(override.aes = list(size=5)))
# use ggMarginal function to create
# marginal histogram, boxplot and density plot
marginal_plot<-ggMarginal(plot, type = "density", groupColour = TRUE, groupFill = TRUE)
marginal_plot
dev.off()


png("LUCAS_clay_C_LUC_groups_ggplot_density.png", width=2000, height=1700, res=300)
density<-ggplot(LUCAS_geodata_2009_2012_WRB_merged[LUCAS_geodata_2009_2012_WRB_merged$LC0_simpl %in% considered_LC_reduced,], aes(clay, OC)) +
  stat_density_2d(geom = "polygon", aes(alpha = (..level..)^1.5, fill = LC0_simpl),  bins = 23)+
  theme(legend.position="none")+
  theme_bw() +
  ylim(-10, 200)+
  theme(legend.position = "bottom") +
  xlab(expression(paste("Clay (g ", kg^-1, " dw)"))) +
  ylab(expression(paste("SOC (g ", kg^-1, " dw)")))+
  scale_fill_manual(values = alpha(LUC_palette_red[2:4], 0.6)) +
  geom_abline(intercept = 0, slope = 1/13, color="black", linetype="dashed", size=1)+
  labs(shape= "", colour="")+ theme(legend.position="none")
density
dev.off()



png("./Figures/Fig1.png", width=3400, height=1700, res=300)

figure <- ggarrange(marginal_plot,density,
                    labels = c("a)", "b)"),
                    ncol = 2, nrow = 1)
figure
dev.off()




#<*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*>*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*>*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*>
#### sink or source?
#<*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*>*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*>*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*>

#load UNFCCC datasets
load("../../../UNFCCC_data/UNFCCC.RData")
UNCFF_data_CL<-read.csv("../../../UNFCCC_data/losses_CL.csv")
UNCFF_data_GL<-read.csv("../../../UNFCCC_data/losses_GL.csv")
UNCFF_data_FL<-read.csv("../../../UNFCCC_data/losses_FL.csv")

colnames(UNCFF_data_CL)
UNCFF_data_CL$MineralSoilsNetChange.tC.ha. <- as.numeric(as.character(UNCFF_data_CL$MineralSoilsNetChange.tC.ha.))
UNCFF_data_GL$MineralSoilsNetChange.tC.ha. <- as.numeric(as.character(UNCFF_data_GL$MineralSoilsNetChange.tC.ha.))
UNCFF_data_FL$MineralSoilsNetChange.tC.ha. <- as.numeric(as.character(UNCFF_data_FL$MineralSoilsNetChange.tC.ha.))



year_list <- seq(from=2000, to=2020)




CL_change<-CL[CL$Year %in% year_list, c("Country", "Year", "MineralSoilsNetChange")]
GL_change<-GL[GL$Year %in% year_list, c("Country", "Year", "MineralSoilsNetChange(tC/ha)")]
colnames(GL_change) <- colnames(CL_change)
par(mfrow=c(2,2))
plot(CL_change[CL_change$Country=="NLD",]$Year, CL_change[CL_change$Country=="NLD",]$MineralSoilsNetChange, type="l", ylab="MineralSoilsNetChange", xlab="year", main="NLD Cl")
plot(CL_change[CL_change$Country=="EST",]$Year, CL_change[CL_change$Country=="EST",]$MineralSoilsNetChange, type="l", ylab="MineralSoilsNetChange", xlab="year", main="EST Cl")
plot(CL_change[CL_change$Country=="JPN",]$Year, CL_change[CL_change$Country=="JPN",]$MineralSoilsNetChange, type="l", ylab="MineralSoilsNetChange", xlab="year", main="JPN Cl")
plot(CL_change[CL_change$Country=="ROU",]$Year, CL_change[CL_change$Country=="ROU",]$MineralSoilsNetChange, type="l", ylab="MineralSoilsNetChange", xlab="year", main="ROU Cl")

plot(GL_change[GL_change$Country=="NLD",]$Year, GL_change[GL_change$Country=="NLD",]$MineralSoilsNetChange, type="l", ylab="MineralSoilsNetChange", xlab="year", main="NLD Gl")


# aggregated_CL <- aggregate(. ~ Country, data = CL[CL$Year %in% year_list, c("Country","MineralSoilsNetChange")], FUN = mean)
# aggregated_GL <- aggregate(. ~ Country, data = GL[GL$Year %in% year_list, c("Country","MineralSoilsNetChange(tC/ha)")], FUN = mean)
# aggregated_FL <- aggregate(. ~ Country, data = FL[FL$Year %in% year_list, c("Country","MineralSoilsNetChange")], FUN = mean)

colnames(UNCFF_data_CL)[1] = "Country"
colnames(UNCFF_data_GL)[1] = "Country"
colnames(UNCFF_data_FL)[1] = "Country"
aggregated_CL <- aggregate(. ~ Country, data = UNCFF_data_CL[UNCFF_data_CL$Year %in% year_list, c("Country","MineralSoilsNetChange.tC.ha.")], FUN = mean, na.rm=T)
aggregated_GL <- aggregate(. ~ Country, data = UNCFF_data_GL[UNCFF_data_GL$Year %in% year_list, c("Country","MineralSoilsNetChange.tC.ha.")], FUN = mean)
aggregated_FL <- aggregate(. ~ Country, data = UNCFF_data_FL[UNCFF_data_FL$Year %in% year_list, c("Country","MineralSoilsNetChange.tC.ha.")], FUN = mean)


unique(UNCFF_data_CL$Country)


colnames(aggregated_GL) <- colnames(aggregated_FL)

# Merge the first two dataframes based on 'Country'
merged_data <- merge(aggregated_CL, aggregated_GL, by = "Country")

# Merge the resulting dataframe with the third dataframe based on 'Country'
merged_data <- merge(merged_data, aggregated_FL, by = "Country")


png("./Appendix/UNFCC_data.png", width = 2500, height=2000, res=300)
# UNFCC <- data.frame(CL_agg$MinSoilNet, GL_agg$MinSoilNet, FL_agg$MinSoilNet)
# barpl<-barplot(as.matrix(t(UNFCC)), beside=T, names.arg=CL_agg$Country, las=2, col=LUC_palette_red[c(2,4,3)])

barpl<-barplot(as.matrix(t(merged_data[,2:4])), beside=T, names.arg=merged_data$Country, las=2, col=LUC_palette_red[c(2,4,3)])

abline(v=barpl[1,]-1, lty=2, col="darkgrey")
legend("topleft", c("Cropland", "Grassland", "Forest"), bty="n", fill=LUC_palette_red[c(2,4,3)])
box()
dev.off()



nation_codes<-unique(LUCAS_geodata_2009_2012_WRB_merged$NUTS_0)
nation_names<-unique(LUCAS_geodata_2009_2012_WRB_merged$Country)



merged_UNFCCC <- merge(aggregated_CL, aggregated_FL, by="Country", all=T)
merged_UNFCCC <- merge(merged_UNFCCC, aggregated_GL, by="Country", all=T)
names(merged_UNFCCC)<-c("Country", "CL", "FL", "GL")

sinksource<-mat.or.vec(3,29)

colnames(sinksource)=substr(merged_UNFCCC$Country, 1,2)
aggregated_FL$Country[9]
colnames(sinksource)[25]="SK"
colnames(sinksource)[26]="SI"
colnames(sinksource)[27]="SE"
colnames(sinksource)[9]="EE"


colnames(sinksource)[colnames(sinksource)=="GR"]="EL"
colnames(sinksource)[colnames(sinksource)=="DN"]="DK"
colnames(sinksource)[colnames(sinksource)=="IR"]="IE"
colnames(sinksource)[colnames(sinksource)=="PO"]="PL"
colnames(sinksource)[colnames(sinksource)=="PR"]="PT"
colnames(sinksource)[colnames(sinksource)=="AU"]="AT"
colnames(sinksource)[colnames(sinksource)=="GB"]="UK"

aggregated_FL$Country[!colnames(sinksource) %in% nation_codes]


sinksource[,"UK"]
merged_UNFCCC[merged_UNFCCC$Country=="GBR",]
aggregated_CL[aggregated_CL$Country=="GBR",]
aggregated_FL[aggregated_FL$Country=="GBR",]
aggregated_GL[aggregated_GL$Country=="GBR",]

#FL_agg$Country==GL_agg$Country==CL_agg$Country
rownames(sinksource)<-c("Cropland","Forest","Grassland")


sinksource[1,]<-merged_UNFCCC$CL
sinksource[2,]<-merged_UNFCCC$FL
sinksource[3,]<-merged_UNFCCC$GL


relative_nondegr_by_nation_prop=mat.or.vec(3, length(nation_codes))
sinksource_processed=mat.or.vec(3, length(nation_codes))
rownames(sinksource_processed)<-c("Cropland","Grassland","Woodland")

for(i in 1:length(nation_codes)){
  nation<-nation_codes[i]
  
  #sink or source since 2010?
  if(nation %in% colnames(sinksource)){
    
    if(is.na(sinksource[1,colnames(sinksource)==nation])){sinksource_processed[1,i]=NA
    } else if (sinksource[1,colnames(sinksource)==nation] < -0.05){sinksource_processed[1,i]="-"
    } else if (sinksource[1,colnames(sinksource)==nation] >= -0.05 & sinksource[1,colnames(sinksource)==nation] <= 0.05){sinksource_processed[1,i]="x"
    } else if (sinksource[1,colnames(sinksource)==nation] > 0.05){sinksource_processed[1,i]="+"}
    
    
    if(is.na(sinksource[2,colnames(sinksource)==nation])){sinksource_processed[2,i]=NA
    } else if (sinksource[2,colnames(sinksource)==nation] < -0.05){sinksource_processed[2,i]="-"
    } else if (sinksource[2,colnames(sinksource)==nation] >= -0.05 & sinksource[2,colnames(sinksource)==nation] <= 0.05){sinksource_processed[2,i]="x"
    } else if (sinksource[2,colnames(sinksource)==nation] > 0.05){sinksource_processed[2,i]="+"}
    
  
    if(is.na(sinksource[3,colnames(sinksource)==nation])){sinksource_processed[3,i]=NA
    } else if (sinksource[3,colnames(sinksource)==nation] < -0.05){sinksource_processed[3,i]="-"
    } else if (sinksource[3,colnames(sinksource)==nation] >= -0.05 & sinksource[3,colnames(sinksource)==nation] <= 0.05){sinksource_processed[3,i]="x"
    } else if (sinksource[3,colnames(sinksource)==nation] > 0.05){sinksource_processed[3,i]="+"}
    
  
    }else{
      
    sinksource_processed[1,i]<-NA
    sinksource_processed[2,i]<-NA
    sinksource_processed[3,i]<-NA
    
  }
}

colnames(sinksource_processed)<-nation_names

sinksource_processed[1,which((sinksource_processed[1,])=="-")]
sinksource_processed[2,which((sinksource_processed[2,])=="-")]
sinksource_processed[3,which((sinksource_processed[3,])=="-")]

CL_sources <- names(sinksource_processed[1,which((sinksource_processed[1,])=="-")])
CL_sources[order(CL_sources)]

write.csv(sinksource, "./Appendix/sinksource.csv")


# sinksource_processed <- rbind(sinksource_processed, rep(NA, dim(sinksource_processed)[2]))
# rownames(sinksource_processed)[4] = "Shrubland"


#<*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*>*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*>*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*>
#### Plotting degraded proportion by country and LC
#<*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*>*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*>*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*>

reorder<-rev(order(rowSums(proportions_by_LC_NUTS[,c(2,3,4,5,6),1])))
png("./Figures/Fig2_alt.png", width = 2500, height=2000, res=300)
par(mar=c(8,5,3,2))
barpl<-barplot(t(proportions_by_LC_NUTS[reorder[-c(1,2)],c(2,3,4,5,6),1])*100,  las=2, density=c(35) , angle=c(0,45,135),
               col=LUC_palette_red, ylim=c(0,107), ylab="Healthy proportion of total points considered (%)")
legend("topright", colnames(proportions_by_LC_NUTS[reorder,c(2,3,4,5,6),1]), density=c(35) , angle=c(0,45,135),
       fill=LUC_palette_red, inset=c(0.15,-0.14),xpd=TRUE, ncol=3,
       pch=NA, bty="n", cex=1.2)
box()
dev.off()



png("./Figures/Fig2.png", width = 3600, height=2000, res=350)

nf <- layout( matrix(c(1,2), ncol=2), 
              widths=c(10,1.6))

par(mar=c(7,4,1,0))
reorder<-rev(order(rowSums(proportions_by_LC_NUTS[,,1])))
barpl<-barplot(t(proportions_by_LC_NUTS[reorder[-c(1,2)],c(3,4,5,6),1])*100,  las=2, density=c(30) , angle=c(0,45,135,90),
               col=LUC_palette_red[-1], ylim=c(0,107), ylab="Healthy proportion of total points considered (%)", beside = T)
legend("topleft", colnames(proportions_by_LC_NUTS[reorder,c(3,4,5,6),1]), density=c(45) , angle=c(0,45,135, 90),
       fill=LUC_palette_red[-1], xpd=TRUE, ncol=4,
       pch=NA, bty="n", cex=1)
box()
legend("topright", "(a)", bty="n", cex=0.6)
colnames(barpl) <- colnames(t(proportions_by_LC_NUTS[reorder[-c(1,2)],c(3,4,5,6),1]))


col_points=mat.or.vec(3, length(nation_codes))
col_points[3,as.character(sinksource_processed[3,])=="+"]="firebrick"
col_points[3,as.character(sinksource_processed[3,])=="-"]="chartreuse2"
col_points[2,as.character(sinksource_processed[2,])=="+"]="firebrick"
col_points[2,as.character(sinksource_processed[2,])=="-"]="chartreuse2"
col_points[1,as.character(sinksource_processed[1,])=="+"]="firebrick"
col_points[1,as.character(sinksource_processed[1,])=="-"]="chartreuse2"
col_points<-as.data.frame(col_points)
col_points[col_points==0] = "NA"
colnames(col_points) <- nation_names

for(i in 1:dim(barpl)[2]){
text(barpl[,i], 1.2 + (t(proportions_by_LC_NUTS[reorder[-c(1,2)],c(3,4,5,6),1])*100)[,i], 
     sinksource_processed[,colnames(sinksource_processed)==colnames(barpl)[i]], col=col_points[,colnames(sinksource_processed)==colnames(barpl)[i]])
}

par(mar=c(7,0,1,1))
reorder2<-(order(proportions_by_NUTS[,2]))
barpl<-barplot(t(proportions_by_NUTS[reorder2[-c(1,2)],])*100, las=2,  col=c("lightgreen", "tomato1"),
               # legend.text = TRUE, 
               # args.legend = list(x = "topright",
               #                    inset = c(0.35, -0.17)),
               xlim=c(0,100), ylim=c(0,27), ylab="", main="", horiz=T, yaxt="n", xlab=" heal./degr.  (%)")
text(-5 , barpl-0.07, rownames(proportions_by_NUTS[reorder2[-c(1,2)],]), cex=0.6,  pos = 4)
box()
legend("topright", "(b)", bty="n", cex=0.6)
# axis(3, 50, "Healthy proportion of total points considered (%)", tick=F)

dev.off()
  


png("./Figures/Fig2_colorcoded.png", width = 3600, height=2000, res=350)

nf <- layout( matrix(c(1,2), ncol=2), 
              widths=c(10,1.6))

col_points=mat.or.vec(3, length(nation_codes))
col_points[3,as.character(sinksource_processed[3,])=="+"]="#56B4E9"
col_points[3,as.character(sinksource_processed[3,])=="-"]="#E69F00"
col_points[3,as.character(sinksource_processed[3,])=="x"]="#CC79A7"
col_points[2,as.character(sinksource_processed[2,])=="+"]="#56B4E9"
col_points[2,as.character(sinksource_processed[2,])=="-"]="#E69F00"
col_points[2,as.character(sinksource_processed[2,])=="x"]="#CC79A7"
col_points[1,as.character(sinksource_processed[1,])=="+"]="#56B4E9"
col_points[1,as.character(sinksource_processed[1,])=="-"]="#E69F00"
col_points[1,as.character(sinksource_processed[1,])=="x"]="#CC79A7"

col_points<-as.data.frame(col_points)
col_points[col_points==0] = "lightgray"
colnames(col_points) <- nation_names

color_names <- rownames(proportions_by_LC_NUTS[reorder[-c(1,2)],c(3,4,5,6),1])


new_xlim <- c(4, 85)

par(mar=c(7,4,1,0))
reorder<-rev(order(rowSums(proportions_by_LC_NUTS[,,1])))
barpl<-barplot(t(proportions_by_LC_NUTS[reorder[-c(1,2)],c(3,4,5),1])*100,  las=2,
               col=prettyGraphs::add.alpha(unlist(col_points[, color_names]), 0.6), ylim=c(0,90), ylab="Healthy proportion of total points considered (%)", 
               beside = T, xlim = new_xlim)

barpl<-barplot(t(proportions_by_LC_NUTS[reorder[-c(1,2)],c(3,4,5),1])*100,  las=2, density=c(17) , angle=c(0,45,135),
               col="black", ylim=c(0,90), ylab="Healthy proportion of total points considered (%)", 
               beside = T, add=T, xlim = new_xlim)

legend(1, 90, colnames(proportions_by_LC_NUTS[reorder,c(3,4,5),1]), 
       fill="lightgray", xpd=TRUE, ncol=4,
       pch=NA, bty="n", cex=1)
legend(1, 90, colnames(proportions_by_LC_NUTS[reorder,c(3,4,5),1]), density=c(20) , angle=c(0,45,135),
       fill="black", xpd=TRUE, ncol=4,
       pch=NA, bty="n", cex=1)
legend("topright", c("Sink (UNFCCC data)", "Source (UNFCCC data)",  expression(paste("Neutral, ±0.05 t ", ha^-1, " (UNFCCC data)"))),
       fill=prettyGraphs::add.alpha(c("#56B4E9", "#E69F00", "#CC79A7"), 0.6), 
       xpd=TRUE, ncol=1,
       pch=NA, bty="n", cex=1)
box()
legend(0, 85, "(a)", bty="n", cex=0.85)
colnames(barpl) <- colnames(t(proportions_by_LC_NUTS[reorder[-c(1,2)],c(3,4,5),1]))

#text(barpl, rep(c(4,3,4,4.5), 22) + t(proportions_by_LC_NUTS[reorder[-c(1,2)],c(3,4,5,6),1])*100, c("Cropland", "Forest", "Grassland", "Shrubland"), cex=0.55, srt = 90)
#text(barpl, -2, c("Cr.", "Fo.", "Gr.", "Sh."), cex=0.52,  xpd = NA, srt = 90)
text(barpl, -2, c("C", "F", "G"), cex=0.6,  xpd = NA)

par(mar=c(7,0,1,1))
reorder2<-(order(proportions_by_NUTS[,2]))
barpl<-barplot(t(proportions_by_NUTS[reorder2[-c(1,2)],])*100, las=2,  col=c("lightgreen", "tomato1"),
               xlim=c(0,100), ylim=c(0,27), ylab="", main="", horiz=T, yaxt="n", xlab=" heal./degr.  (%)")
barplot(t(proportions_by_NUTS[reorder2[-c(1,2)],])*100, las=2,  col=c("lightgreen", "tomato4"), density=20,
               xlim=c(0,100), ylim=c(0,27), ylab="", main="", horiz=T, yaxt="n", xlab=" heal./degr.  (%)", add=T)

text(-5 , barpl-0.07, rownames(proportions_by_NUTS[reorder2[-c(1,2)],]), cex=0.6,  pos = 4)
box()
legend("topright", "(b)", bty="n", cex=0.85)
# axis(3, 50, "Healthy proportion of total points considered (%)", tick=F)

dev.off()





#<*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*>*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*>*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*>
#### Plotting degraded proportion by Koppen and LC
#<*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*>*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*>*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*>


reorder_kop<-rev(order(rowSums(proportions_by_LC_Koppen[,,1])))
proportions_by_LC_Koppen <- proportions_by_LC_Koppen[reorder_kop,,]

KoppenClim<-as.factor(KoppenClim)


classes_to_plot <-names(table(KoppenClim)[which(table(KoppenClim)>20)])
classes_to_plot<-classes_to_plot[classes_to_plot !="ET, Polar tundra" & !is.na(classes_to_plot)]
which_to_plot <- rownames(proportions_by_LC_Koppen) %in% classes_to_plot


clim_codes <- substr(rownames(proportions_by_LC_Koppen[which_to_plot,c(2,3,4,5,6),1]),1,3)
clim_names <- substr(rownames(proportions_by_LC_Koppen[which_to_plot,c(2,3,4,5,6),1]),6,40)

png("./Figures/Fig3.png", width = 2500, height=2000, res=300)
par(mar=c(16,5,3,2))
barpl<-barplot(t(proportions_by_LC_Koppen[which_to_plot,c(2,3,4,5,6),1])*100,  las=2, density=c(35) , angle=c(0,45,135),
               col=LUC_palette_red, ylim=c(0,107), ylab="Healthy proportion of total points considered (%)", 
               names.arg=clim_names)
legend("topright", colnames(proportions_by_LC_Koppen[which_to_plot,c(2,3,4,5,6),1]), density=c(35) , angle=c(0,45,135),
       fill=LUC_palette_red, inset=c(0.15,-0.14),xpd=TRUE, ncol=5,
       pch=NA, bty="n", cex=1)
box()
text(barpl, colSums(t(proportions_by_LC_Koppen[which_to_plot,c(2,3,4,5,6),1])*100)+3, clim_codes, cex=0.9)

dev.off()


png("./Figures/Fig_3_alt.png", width=2000, height=1700, res=300)
# par(mfrow=c(1,2))
par(mar=c(4,1,1,1))
proportions_by_LC_Koppen_all_plot<-proportions_by_LC_Koppen_all[rev(order(proportions_by_LC_Koppen_all[,1])),][c(-1,-2),]
barpl <- barplot(t(proportions_by_LC_Koppen_all_plot),col=c("lightgreen", "tomato1"), yaxt="n",
        xlim=c(0,100), ylab="", main="", horiz=T,  xlab="Healthy/Degraded  (%)", names.arg=rownames(proportions_by_LC_Koppen_all_plot))
box()
barplot(t(proportions_by_LC_Koppen_all_plot),  col=c("lightgreen", "tomato4"), density=20,
        xlim=c(0,100), ylim=c(0,27), ylab="", main="", horiz=T, yaxt="n", xlab="Healthy/Degraded  (%)", add=T)
text(2 , barpl-0.07, rownames(proportions_by_LC_Koppen_all_plot), cex=1,  pos = 4)
#legend("bottomright", "(a)", bty="n", cex=0.85)
dev.off()



#load the barplot density function
source("barplot_density_function.R")


png("./Figures/Fig_3_alt_longlat.png", width=2500, height=1500, res=320)

nf <- layout( matrix(c(1,2,3), ncol=3), 
              widths=c(9,4,4))
layout.show(n=3)

koppen_palette <- rev(c("#D5BB21FF", 
                    "#F8B620FF", 
                    "#F89217FF", 
                    "#F06719FF", 
                    "#FC719EFF", 
                    "#F64971FF", 
                    "#A2B627FF", 
                    "#CE69BEFF", 
                    "#7873C0FF",
                    "#57A337FF", 
                    "#21B087FF"))

element_colors <- matrix(koppen_palette, nrow = length(koppen_palette), byrow = TRUE)

par(mar=c(4,1,1,0))
elements <- length(t(proportions_by_LC_Koppen_all_plot)[1,])
#proportions_by_LC_Koppen_all_plot<-proportions_by_LC_Koppen_all[rev(order(proportions_by_LC_Koppen_all[,1])),][c(-1,-2),]
proportions_by_LC_Koppen_all_plot<-proportions_by_LC_Koppen_all[(order(proportions_by_LC_Koppen_all[,1])),][c(-12,-13),] #### careful here, removing Tundra and dsb

barpl<-barplot(rep(100, elements), col="tomato1", yaxt="n", xaxt="n", #col=darken_colors(koppen_palette, 0.45), yaxt="n", xaxt="n",
                 xlim=c(0,100), ylab="", main="", horiz=T,  xlab="Healthy/Degraded  (%)", names.arg=rownames(proportions_by_LC_Koppen_all_plot))
barplot(rep(100, elements), col="tomato4", density=20, yaxt="n", xaxt="n", #col=darken_colors(koppen_palette, 0.45), yaxt="n", xaxt="n",
        xlim=c(0,100), ylab="", main="", horiz=T,  xlab="Healthy/Degraded  (%)", names.arg=rownames(proportions_by_LC_Koppen_all_plot), add=T)
barplot(t(proportions_by_LC_Koppen_all_plot)[1,],  col="lightgreen", xaxt="n", # col=lighten_colors(koppen_palette, 0.2),  xaxt="n",
        xlim=c(0,100), ylim=c(0,27), ylab="", main="", horiz=T, yaxt="n", xlab="Healthy/Degraded  (%)", add=T)

tick_positions <- seq(0,100, by = 25)  # Adjust by = value as needed
axis(side = 1, at = tick_positions[-length(tick_positions)], labels = tick_positions[-length(tick_positions)])
box()
text(2 , barpl-0.07, rownames(proportions_by_LC_Koppen_all_plot), cex=1,  pos = 4, col="black", font=2)
legend("bottomleft", "a)", bty="n", cex=0.85)

proportions_by_LC_Koppen_all_plot2<-proportions_by_LC_Koppen_all[rev(order(proportions_by_LC_Koppen_all[,1])),][c(-1,-2),] #### careful here, removing Tundra and dsb


#LATITUDE
par(mar=c(4,0,1,0))
#build the df to plot, latitude
lat_df <- data.frame(LUCAS_geodata_WRB$KoppenClim, LUCAS_geodata_WRB$coords.x2)
# Filter the dataframe based on the subset of levels in 'Category' column
filtered_lat_df <- lat_df[lat_df$LUCAS_geodata_WRB.KoppenClim %in% rownames(proportions_by_LC_Koppen_all_plot2), ]
filtered_lat_df$LUCAS_geodata_WRB.KoppenClim <- droplevels(filtered_lat_df$LUCAS_geodata_WRB.KoppenClim)
# Order of the elements in the first vector
order_plot <- match(rev(rownames(proportions_by_LC_Koppen_all_plot2)), levels(filtered_lat_df$LUCAS_geodata_WRB.KoppenClim))
#order_plot <- match( levels(filtered_lat_df$LUCAS_geodata_WRB.KoppenClim), rownames(proportions_by_LC_Koppen_all_plot2))
#barplot_density(df=filtered_lat_df, colors=koppen_palette, xlabel="Latitude", yaxt="n", order_vec = order_plot)
barplot_density(df=filtered_lat_df, colors=rep("cadetblue4",13), xlabel="Latitude", yaxt="n", order_vec = order_plot)
legend("bottomleft", "b)", bty="n", cex=0.85)

#LONGITUDE
par(mar=c(4,0,1,0))
#build the df to plot, longitude
long_df <- data.frame(LUCAS_geodata_WRB$KoppenClim, LUCAS_geodata_WRB$coords.x1)
# Filter the dataframe based on the subset of levels in 'Category' column
filtered_long_df <- long_df[long_df$LUCAS_geodata_WRB.KoppenClim %in% rownames(proportions_by_LC_Koppen_all_plot2), ]
filtered_long_df$LUCAS_geodata_WRB.KoppenClim <- droplevels(filtered_long_df$LUCAS_geodata_WRB.KoppenClim)
# Order of the elements in the first vector
order_plot <- match(rev(rownames(proportions_by_LC_Koppen_all_plot2)), levels(filtered_long_df$LUCAS_geodata_WRB.KoppenClim))
#barplot_density(df=filtered_long_df, colors=koppen_palette, xlabel="Longitude", yaxt="n", order_vec = order_plot)
barplot_density(df=filtered_long_df, colors=rep("darkorange3",13), xlabel="Longitude", yaxt="n", order_vec = order_plot)
legend("bottomleft", "c)", bty="n", cex=0.85)

dev.off()

range(LUCAS_geodata_WRB[LUCAS_geodata_WRB$KoppenClim=="BSk, Steppe, cold",]$coords.x2)
range(filtered_lat_df[filtered_lat_df$LUCAS_geodata_WRB.KoppenClim=="BSk, Steppe, cold",]$LUCAS_geodata_WRB.coords.x2)





png("./Appendix/Fig_3_1.png", width=2000, height=1700, res=300)
par(mar=c(4,1,1,1))

proportions_by_LC_plot<-proportions_by_LC[rowSums(table_by_LC)>100,]
proportions_by_LC_plot<-proportions_by_LC_plot[rev(order(proportions_by_LC_plot[,1])),]
rownames(proportions_by_LC_plot)[rownames(proportions_by_LC_plot)=="Orchards/Wineyards"]="Permanent cropland"
barpl <- barplot(t(proportions_by_LC_plot)*100,col=c("lightgreen", "tomato1"), yaxt="n",
                 xlim=c(0,100), ylab="", main="", horiz=T,  xlab="Healthy/Degraded  (%)")
box()
barplot(t(proportions_by_LC_plot)*100,  col=c("lightgreen", "tomato4"), density=20,
        xlim=c(0,100), ylim=c(0,27), ylab="", main="", horiz=T, yaxt="n", xlab="Healthy/Degraded  (%)", add=T)
text(2 , barpl-0.07, rownames(proportions_by_LC_plot), cex=1,  pos = 4)

box()
dev.off()






#<*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*>*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*>*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*>
#### Random forest predictive model (exploration of variable importance)
#<*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*>*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*>*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*>

#splitting training and validation (70%-30%)
set.seed(123)
LUCAS_geodata_WRB_rf<-LUCAS_geodata_2009_2012_WRB_merged[LUCAS_geodata_2009_2012_WRB_merged$OC<200,]
colnames(LUCAS_geodata_WRB_rf)[colnames(LUCAS_geodata_WRB_rf)=="coords.x2"]="Lat"
colnames(LUCAS_geodata_WRB_rf)[colnames(LUCAS_geodata_WRB_rf)=="coords.x1"]="Long"
ind <- sample(2, nrow(LUCAS_geodata_WRB_rf), replace = TRUE, prob = c(0.7, 0.3))
LUCAS_geodata_WRB_train<-LUCAS_geodata_WRB_rf[ind==1,]
LUCAS_geodata_WRB_valid<-LUCAS_geodata_WRB_rf[ind==2,]



#correlation matrices
str(LUCAS_geodata_WRB_rf)
LUCAS_geodata_WRB_rf[,"coarse"] <- as.numeric(LUCAS_geodata_WRB_rf[,"coarse"])
LUCAS_geodata_WRB_rf[,"silt"] <- as.numeric(LUCAS_geodata_WRB_rf[,"silt"])
LUCAS_geodata_WRB_rf[,"sand"] <- as.numeric(LUCAS_geodata_WRB_rf[,"sand"])
LUCAS_geodata_WRB_rf[,"CaCO3"] <- as.numeric(LUCAS_geodata_WRB_rf[,"CaCO3"])
LUCAS_geodata_WRB_rf[,"N"] <- as.numeric(LUCAS_geodata_WRB_rf[,"N"])
LUCAS_geodata_WRB_rf[,"P"] <- as.numeric(LUCAS_geodata_WRB_rf[,"P"])
LUCAS_geodata_WRB_rf[,"K"] <- as.numeric(LUCAS_geodata_WRB_rf[,"K"])
str(LUCAS_geodata_WRB_rf)


# Function to check if columns are numeric¨
is.numeric.mat <- function(mat) {
  vector<-c()
  for(i in 1:dim(mat)[2]){
    x=mat[,i]
     if (is.numeric(x)) {
       vector[i] = TRUE
     } else {
       vector[i] = FALSE
       }
  }
  
  return(vector)
}

#correlation matrix
corrmat <- round(cor(LUCAS_geodata_WRB_rf[,is.numeric.mat(LUCAS_geodata_WRB_rf)], method="pearson"),2)
corrmat_OC<-abs(corrmat[,"OC"])
unlist(corrmat[,"OC"][order(corrmat_OC)])
corrmat_OC[order(corrmat_OC)]

corrmat_data<-LUCAS_geodata_WRB_rf[,is.numeric.mat(LUCAS_geodata_WRB_rf)]

clay_r<-summary(lm(corrmat_data$OC ~ corrmat_data$clay))$r.squared
silt_r<-summary(lm(corrmat_data$OC ~ corrmat_data$silt))$r.squared
sand_r<-summary(lm(corrmat_data$OC ~ corrmat_data$sand))$r.squared
K_r<-summary(lm(corrmat_data$OC ~ corrmat_data$K))$r.squared
P_r<-summary(lm(corrmat_data$OC ~ corrmat_data$P))$r.squared
coarse_r<-summary(lm(corrmat_data$OC ~ corrmat_data$coarse))$r.squared
CaCO3_r<-summary(lm(corrmat_data$OC ~ corrmat_data$CaCO3))$r.squared
long_r<-summary(lm(corrmat_data$OC ~ corrmat_data$Long))$r.squared
CEC_r<-summary(lm(corrmat_data$OC ~ corrmat_data$CEC))$r.squared
Lat_r<-summary(lm(corrmat_data$OC ~ corrmat_data$Lat))$r.squared
pHinCaCl2_r<-summary(lm(corrmat_data$OC ~ corrmat_data$pHinCaCl2))$r.squared
N_r<-summary(lm(corrmat_data$OC ~ corrmat_data$N))$r.squared


round(N_r, 2)

#splitting the datasets
LUCAS_geodata_WRB_train_rf_full_LC<-LUCAS_geodata_WRB_train[,c("coarse", "clay", "silt", "sand", "pHinCaCl2", "OC", "CaCO3", "Country", "Long", "Lat", "LC0")]
LUCAS_geodata_WRB_valid_rf_full_LC<-LUCAS_geodata_WRB_valid[,c("coarse", "clay", "silt", "sand", "pHinCaCl2", "OC", "CaCO3",  "Country", "Long", "Lat", "LC0")]
LUCAS_geodata_WRB_train_rf_WRB<-LUCAS_geodata_WRB_train[,c("coarse", "clay", "silt", "sand", "pHinCaCl2", "OC", "CaCO3", "Country", "Long", "Lat", "LC0", "WRBFU_group")]
LUCAS_geodata_WRB_valid_rf_WRB<-LUCAS_geodata_WRB_valid[,c("coarse", "clay", "silt", "sand", "pHinCaCl2", "OC", "CaCO3", "Country", "Long", "Lat", "LC0", "WRBFU_group")]
LUCAS_geodata_WRB_train_rf_full_LC_CEC<-LUCAS_geodata_WRB_train[,c("coarse", "clay", "silt", "sand", "pHinCaCl2", "OC", "CaCO3", "Country", "Long", "Lat", "LC0", "CEC")]
LUCAS_geodata_WRB_valid_rf_full_LC_CEC<-LUCAS_geodata_WRB_valid[,c("coarse", "clay", "silt", "sand", "pHinCaCl2", "OC", "CaCO3",  "Country", "Long", "Lat", "LC0", "CEC")]

#constraining all the data frames to the same classes
type_list<-rep(NA, 11)
type_list[c(1:7,9,10)]<-"numeric"
type_list[c(8,11)]<-"factor"

# Set the column types based on the 'type_list'
for (i in seq_along(type_list)) {
  if (type_list[i] == "character") {
    LUCAS_geodata_WRB_train_rf_full_LC[, i] <- as.character(LUCAS_geodata_WRB_train_rf_full_LC[, i])
    LUCAS_geodata_WRB_valid_rf_full_LC[, i] <- as.character(LUCAS_geodata_WRB_valid_rf_full_LC[, i])
    LUCAS_geodata_WRB_train_rf_WRB[, i] <- as.character(LUCAS_geodata_WRB_train_rf_WRB[, i])
    LUCAS_geodata_WRB_valid_rf_WRB[, i] <- as.character(LUCAS_geodata_WRB_valid_rf_WRB[, i])
    LUCAS_geodata_WRB_train_rf_full_LC_CEC[, i] <- as.character(LUCAS_geodata_WRB_train_rf_full_LC_CEC[, i])
    LUCAS_geodata_WRB_valid_rf_full_LC_CEC[, i] <- as.character(LUCAS_geodata_WRB_valid_rf_full_LC_CEC[, i])
  } else if (type_list[i] == "numeric") {
    LUCAS_geodata_WRB_train_rf_full_LC[, i] <- as.numeric(LUCAS_geodata_WRB_train_rf_full_LC[, i])
    LUCAS_geodata_WRB_valid_rf_full_LC[, i] <- as.numeric(LUCAS_geodata_WRB_valid_rf_full_LC[, i])
    LUCAS_geodata_WRB_train_rf_WRB[, i] <- as.numeric(LUCAS_geodata_WRB_train_rf_WRB[, i])
    LUCAS_geodata_WRB_valid_rf_WRB[, i] <- as.numeric(LUCAS_geodata_WRB_valid_rf_WRB[, i])
    LUCAS_geodata_WRB_train_rf_full_LC_CEC[, i] <- as.numeric(LUCAS_geodata_WRB_train_rf_full_LC_CEC[, i])
    LUCAS_geodata_WRB_valid_rf_full_LC_CEC[, i] <- as.numeric(LUCAS_geodata_WRB_valid_rf_full_LC_CEC[, i])
  } else if (type_list[i] == "factor") {
    LUCAS_geodata_WRB_train_rf_full_LC[, i] <- as.factor(LUCAS_geodata_WRB_train_rf_full_LC[, i])
    LUCAS_geodata_WRB_valid_rf_full_LC[, i] <- as.factor(LUCAS_geodata_WRB_valid_rf_full_LC[, i])
    LUCAS_geodata_WRB_train_rf_WRB[, i] <- as.factor(LUCAS_geodata_WRB_train_rf_WRB[, i])
    LUCAS_geodata_WRB_valid_rf_WRB[, i] <- as.factor(LUCAS_geodata_WRB_valid_rf_WRB[, i])
    LUCAS_geodata_WRB_train_rf_full_LC_CEC[, i] <- as.factor(LUCAS_geodata_WRB_train_rf_full_LC_CEC[, i])
    LUCAS_geodata_WRB_valid_rf_full_LC_CEC[, i] <- as.factor(LUCAS_geodata_WRB_valid_rf_full_LC_CEC[, i])
  }
}


#check that the data types are correct
str(LUCAS_geodata_WRB_train_rf_full_LC)
str(LUCAS_geodata_WRB_valid_rf_WRB)
str(LUCAS_geodata_WRB_valid_rf_full_LC_CEC)

#training the RF model

#setting up parameters for the grid search with CARET
control <- trainControl(
                method = "cv",  # Cross-validation method
                number = 5,  # Number of folds
                verboseIter = TRUE )
tunegrid <- expand.grid(mtry = seq(from=11, to=13))
tunegrid <- expand.grid(mtry = seq(from=13, to=13))

#fitting the extended RF model
start<-Sys.time()
rf.full_LC <- train(OC~.,
                    data=LUCAS_geodata_WRB_train_rf_full_LC,
                    method='parRF', #cforest, #RRF
                    tuneGrid=tunegrid,
                    trControl=control, 
                    importance=T, allowParallel = T)
end<-Sys.time()
rf.full_LC.time<- end - start

#minor data processong (there's a NA)
NAs_note<-dim(LUCAS_geodata_WRB_train_rf_WRB)[1]-dim(na.omit(LUCAS_geodata_WRB_train_rf_WRB))[1]
LUCAS_geodata_WRB_train_rf_WRB <- na.omit(LUCAS_geodata_WRB_train_rf_WRB)


#fitting the WRB RF model
start<-Sys.time()
rf.WRB <- train(OC~.,
                    data=LUCAS_geodata_WRB_train_rf_WRB,
                    method='parRF', #cforest, #RRF
                    tuneGrid=tunegrid,
                    trControl=control, 
                    importance=T, allowParallel = T)
end<-Sys.time()
rf.WRB.time<- end - start


#fitting the CEC RF model
start<-Sys.time()
rf.CEC <- train(OC~.,
                data=LUCAS_geodata_WRB_train_rf_full_LC_CEC,
                method='parRF', #cforest, #RRF
                tuneGrid=tunegrid,
                trControl=control, 
                importance=T, allowParallel = T)
end<-Sys.time()
rf.CEC.time<- end - start

rf.full_LC
rf.WRB

#escluding fre minor classes not in the training dataset
which <- levels(LUCAS_geodata_WRB_valid_rf_WRB$WRBFU_group)[levels(LUCAS_geodata_WRB_valid_rf_WRB$WRBFU_group) %in% levels(LUCAS_geodata_WRB_train_rf_WRB$WRBFU_group)]
LUCAS_geodata_WRB_valid_rf_WRB<-LUCAS_geodata_WRB_valid_rf_WRB[LUCAS_geodata_WRB_valid_rf_WRB$WRBFU_group %in% which,]


#calculate the residuals just in case
residuals.full_LC=abs(predict(rf.full_LC, newdata = LUCAS_geodata_WRB_valid_rf_full_LC)  - LUCAS_geodata_WRB_valid_rf_full_LC$OC)
residuals.WRB=abs(predict(rf.WRB, newdata = LUCAS_geodata_WRB_valid_rf_WRB)  - LUCAS_geodata_WRB_valid_rf_WRB$OC)

rf.full_LC.fit<-summary(lm(predict(rf.full_LC, newdata = LUCAS_geodata_WRB_valid_rf_full_LC) ~ LUCAS_geodata_WRB_valid_rf_full_LC$OC))
rf.full_WRB.fit<-summary(lm(predict(rf.WRB, newdata = LUCAS_geodata_WRB_valid_rf_WRB) ~ LUCAS_geodata_WRB_valid_rf_WRB$OC))
rf.full_CEC.fit<-summary(lm(predict(rf.CEC, newdata = LUCAS_geodata_WRB_valid_rf_full_LC_CEC) ~ LUCAS_geodata_WRB_valid_rf_full_LC_CEC$OC))


png("./Appendix/RF_model_validation.png", width=2200, height=5200, res=340)
par(mfrow=c(3,1))

plot(predict(rf.full_LC, newdata = LUCAS_geodata_WRB_valid_rf_full_LC), LUCAS_geodata_WRB_valid_rf_full_LC$OC, 
     pch=as.numeric(LUCAS_geodata_WRB_valid_rf_full_LC$LC0), col=palette_LC0[as.numeric(LUCAS_geodata_WRB_valid_rf_full_LC$LC0)],
     ylab="Predicted OC (g kg-1)", xlab="Measured OC (g kg-1)", main="Land cover ", ylim=c(0,200), xlim=c(0,200))
legend("bottomright", levels(LUCAS_geodata_WRB_valid_rf_full_LC$LC0), pch=seq(1:13), col=palette_LC0)
r2 = bquote(italic(R)^2 == .(format(round(rf.full_LC.fit$r.squared,3), digits = 3)))
text(x = 13, y = 200, labels = r2)
abline(0,1, lty=2)


plot(predict(rf.WRB, newdata = LUCAS_geodata_WRB_valid_rf_WRB), LUCAS_geodata_WRB_valid_rf_WRB$OC, 
     pch=as.numeric(LUCAS_geodata_WRB_valid_rf_WRB$LC0), col=palette_LC0[as.numeric(LUCAS_geodata_WRB_valid_rf_WRB$LC0)],
     ylab="Predicted OC (g kg-1)", xlab="Measured OC (g kg-1)", main="Land cover + WRB ",  ylim=c(0,200), xlim=c(0,200))
#legend("bottomright", levels(LUCAS_geodata_WRB_valid_rf_WRB$LC0), pch=seq(1:9), col=LUC_palette_red)
abline(0,1, lty=2)
r2 = bquote(italic(R)^2 == .(format(round(rf.full_WRB.fit$r.squared,3), digits = 3)))
text(x = 13, y = 200, labels = r2)

plot(predict(rf.CEC, newdata = LUCAS_geodata_WRB_valid_rf_full_LC_CEC), LUCAS_geodata_WRB_valid_rf_full_LC_CEC$OC, 
     pch=as.numeric(LUCAS_geodata_WRB_valid_rf_full_LC$LC0), col=palette_LC0[as.numeric(LUCAS_geodata_WRB_valid_rf_full_LC$LC0)],
     ylab="Predicted OC (g kg-1)", xlab="Measured OC (g kg-1)", main="Land cover + CEC ", ylim=c(0,200), xlim=c(0,200))
abline(0,1, lty=2)
r2 = bquote(italic(R)^2 == .(format(round(rf.full_CEC.fit$r.squared,3), digits = 3)))
text(x = 13, y = 200, labels = r2)

dev.off()


boxplot(residuals.full_LC ~ LUCAS_geodata_WRB_valid_rf_full_LC$LC0, las=2)




#importance of variables
importance.full_LC<-varImp(rf.full_LC,
                   sort=T,
                   main="Variable Importance Plot")
importance.WRB<-varImp(rf.WRB,
                                sort=T,
                                main="Variable Importance Plot")

importance.CEC<-varImp(rf.CEC,
                       sort=T,
                       main="Variable Importance Plot")


imp_LC_df<-data.frame(names=rownames(importance.full_LC$importance), imp=importance.full_LC$importance, rank=rank(-importance.full_LC$importance$Overall))
imp_WRB_df<-data.frame(names=rownames(importance.WRB$importance), imp=importance.WRB$importance, rank=rank(-importance.WRB$importance$Overall))
imp_CEC_df<-data.frame(names=rownames(importance.CEC$importance), imp=importance.CEC$importance, rank=rank(-importance.CEC$importance$Overall))


imp_LC_df  <- imp_LC_df[rev(order(imp_LC_df$Overall)),][1:20,]
imp_WRB_df <- imp_WRB_df[rev(order(imp_WRB_df$Overall)),][1:20,]
imp_CEC_df <- imp_CEC_df[rev(order(imp_CEC_df$Overall)),][1:20,]


all_names<-unique(c(imp_LC_df$names, imp_WRB_df$names, imp_CEC_df$names))


# Create a new DataFrame with all_names and NAs where necessary
filtered_df <- data.frame(names = all_names)

# Merge imp_LC_df with filtered_df based on names and replace missing values with NA
filtered_df_LC <- merge(filtered_df, imp_LC_df, all.x = TRUE)
filtered_df_WRB <- merge(filtered_df, imp_WRB_df, all.x = TRUE)
filtered_df_CEC <- merge(filtered_df, imp_CEC_df, all.x = TRUE)

filtered_df_LC <- filtered_df_LC[order(filtered_df_LC$names),]
filtered_df_WRB <- filtered_df_WRB[order(filtered_df_WRB$names),]
filtered_df_CEC <- filtered_df_CEC[order(filtered_df_CEC$names),]

RF_ranking<- data.frame(variable=filtered_df_LC$names, only_LC=filtered_df_LC$rank, WRB=filtered_df_WRB$rank, CEC=filtered_df_CEC$rank)

write.csv(RF_ranking, file="./Appendix/varimp.csv")



png("./Appendix/RF_model_importance_LC.png", width=2000, height=2300, res=300)
plot(importance.full_LC)
dev.off()

png("./Appendix/RF_model_importance_WRB.png", width=2000, height=2300, res=300)
plot(importance.WRB)
dev.off()

png("./Appendix/RF_model_importance_CEC.png", width=2000, height=2300, res=300)
plot(importance.CEC)
dev.off()



png("./Figures/Fig5.png", width=2400, height=1500, res=300)

library(ggpubr)
library(viridis)

imp_LC_df_plot<-imp_LC_df[1:10,]
imp_LC_df_plot$rank<-as.factor(imp_LC_df_plot$rank)

imp_LC_df_plot$names= c("Clay fraction",
                            "pH",
                            "LC (if cropland)",
                            "Latitude",
                            "Longitude",
                            "Sand fraction",
                            "CaCO3",
                            "Coarse fraction",
                            "Silt fraction",
                            "LC (if grassland)")

labels_plot <- rev(imp_LC_df_plot$names)
labels_plot[labels_plot=="CaCO3"] = ""

dotchart(rev(imp_LC_df_plot$Overall), labels = labels_plot,
                cex = 1.2, pch = 19, color = rev(inferno(12)[1:10])[as.numeric(imp_LC_df_plot$rank)],
                main = "",
                xlab = "Relative variable importance")  # Customize y-axis label here
text(x = 27.7, y =  4 , expression(CaCO[3]), xpd = TRUE, cex=1.2, col=rev(inferno(12)[1:10])[4])


segments(min(rev(imp_LC_df_plot$Overall))-2, (seq(1:10)), rev(imp_LC_df_plot$Overall), (seq(1:10)), col = rev(inferno(12)[1:10])[1:10][as.numeric(imp_LC_df_plot$rank)], lwd=5)
points(rev(imp_LC_df_plot$Overall), seq(1:10),  cex = 2.3, pch = 19, col = rev(inferno(12)[1:10])[1:10][as.numeric(imp_LC_df_plot$rank)])
text(rev(imp_LC_df_plot$Overall), seq(1:10), rev(round(imp_LC_df_plot$Overall, 0)) , col = "white", cex=0.7, xpd = TRUE)


dev.off()



chart<-dotchart(rev(imp_LC_df_plot$Overall), labels = rev(imp_LC_df_plot$names),
         cex = 1.2, pch = 19, color = rev(inferno(12)[1:10])[as.numeric(imp_LC_df_plot$rank)],
         main = "",
         xlab = "Relative variable importance")  # Customize y-axis label here

segments(min(rev(imp_LC_df_plot$Overall))-2, (seq(1:10)), rev(imp_LC_df_plot$Overall), (seq(1:10)), col = rev(inferno(12)[1:10])[1:10][as.numeric(imp_LC_df_plot$rank)], lwd=3)
points(rev(imp_LC_df_plot$Overall), seq(1:10),  cex = 3, pch = 19, col = rev(inferno(12)[1:10])[1:10][as.numeric(imp_LC_df_plot$rank)])
text(rev(imp_LC_df_plot$Overall), seq(1:10), rev(round(imp_LC_df_plot$Overall, 0)) , col = "white", cex=0.9)

#<*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*>*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*>*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*>
#### Decision tree predictive model (exploration)
#<*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*>*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*>*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*>


trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

tree.full_LC = train(OC ~ .,
             data=LUCAS_geodata_WRB_train_rf_full_LC,
             method="rpart2",
             trControl=trctrl,
             tuneLength = 20)
tree.full_LC
rattle::fancyRpartPlot(tree.full_LC$finalModel)

tree.full_LC_lm<-summary(lm(predict(tree.full_LC, newdata = LUCAS_geodata_WRB_valid_rf_full_LC) ~ LUCAS_geodata_WRB_valid_rf_full_LC$OC))
plot(predict(tree.full_LC, newdata = LUCAS_geodata_WRB_valid_rf_full_LC), LUCAS_geodata_WRB_valid_rf_full_LC$OC, 
    pch=as.numeric(LUCAS_geodata_WRB_valid_rf_full_LC$LC0), col=palette_LC0[as.numeric(LUCAS_geodata_WRB_valid_rf_full_LC$LC0)],
    ylab = "Predicted OC (g kg-1)", xlab="Measured OC (g kg-1)", main="Land cover ", xlim=c(0,200), ylim=c(0,200))
legend("bottomright", levels(LUCAS_geodata_WRB_valid_rf_full_LC$LC0), pch=seq(1:9), col=palette_LC0)
r2 = bquote(italic(R)^2 == .(format(round(tree.full_LC_lm$r.squared,2), digits = 3)))
text(x = 13, y = 200, labels = r2)


# Create a grid of hyperparameters for rpart
hyper_grid <- expand.grid(
  cp = seq(0.001, 0.1, by = 0.001)  # Range for the complexity parameter (cp)
)

# Define the control parameters for cross-validation
trctrl <- trainControl(
  method = "cv", 
  number = 5, 
  verboseIter = TRUE
)

# Perform hyperparameter tuning using train function
tree_tuned <- train(
  OC ~ .,
  data = LUCAS_geodata_WRB_train_rf_full_LC,
  method = "rpart",
  trControl = trctrl,
  tuneGrid = hyper_grid
)

png("./Appendix/decison_tree_structure.png", width=3200, height=2200, res=300)
rattle::fancyRpartPlot(tree_tuned$finalModel)
dev.off()

png("./Appendix/decison_tree_model_validation.png", width=2200, height=2200, res=300)
tree.tuned_LC_lm<-summary(lm(predict(tree_tuned, newdata = LUCAS_geodata_WRB_valid_rf_full_LC) ~ LUCAS_geodata_WRB_valid_rf_full_LC$OC))
plot(predict(tree_tuned, newdata = LUCAS_geodata_WRB_valid_rf_full_LC), LUCAS_geodata_WRB_valid_rf_full_LC$OC, 
     pch=as.numeric(LUCAS_geodata_WRB_valid_rf_full_LC$LC0), col=palette_LC0[as.numeric(LUCAS_geodata_WRB_valid_rf_full_LC$LC0)],
     ylab = "Predicted OC (g kg-1)", xlab="Measured OC (g kg-1)", main="Land cover ", xlim=c(0,200), ylim=c(0,200))
legend("bottomright", levels(LUCAS_geodata_WRB_valid_rf_full_LC$LC0), pch=seq(1:9), col=palette_LC0)
r2 = bquote(italic(R)^2 == .(format(round(tree.tuned_LC_lm$r.squared,2), digits = 3)))
text(x = 13, y = 200, labels = r2)
dev.off()






mean(LUCAS_geodata_2009_2012_WRB_merged[LUCAS_geodata_2009_2012_WRB_merged$NUTS_0=="IE" & LUCAS_geodata_2009_2012_WRB_merged$LC0_simpl =="Grassland",]$clay)
mean(LUCAS_geodata_2009_2012_WRB_merged[LUCAS_geodata_2009_2012_WRB_merged$NUTS_0=="IE" & LUCAS_geodata_2009_2012_WRB_merged$LC0_simpl =="Grassland",]$OC)

table(LUCAS_geodata_2009_2012_WRB_merged[LUCAS_geodata_2009_2012_WRB_merged$NUTS_0=="IE",]$LC0_simpl)







library(sf)
library(ggplot2)
library(rnaturalearth)


# Define a custom bounding box for mainland Europe coordinates
custom_bbox <- c(xmin = -10, xmax = 40, ymin = 35, ymax = 70)

# Get world map data
world_map <- ne_countries(scale = "medium", returnclass = "sf")


eu_countries <- world_map[world_map$continent == "Europe" & world_map$name %in% c(
  "Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czechia", "Denmark",
  "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Ireland",
  "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta", "Netherlands",
  "Poland", "Portugal", "Romania", "Slovakia", "Slovenia", "Spain", "Sweden", "United Kingdom"
), ]

eu_countries <- world_map[world_map$continent == "Europe", ]
eu_countries$geounit


proportion_data<-data.frame(Country=rownames(proportions_by_NUTS), Proportion=proportions_by_NUTS[,"Degraded"]*100)

eu_countries <- merge(eu_countries, proportion_data[proportion_data$Country != "Croatia" & proportion_data$Country != "Luxembourg",], by.x = "geounit", by.y = "Country")



# Define limits for x and y axes to focus on desired coordinates
x_limits <- c(-10, 40)
y_limits <- c(35, 70)


# Download detailed coastline data
#ne_coastline <- ne_download(scale = 10, type = "coastline", category = "physical", returnclass = "sf")

plasma_palette <- plasma(5)

# Plot the map with sea layer and limited coordinates
graphabstr<-ggplot() +
  geom_sf(data = world_map, fill = "lightgrey") + 
  #geom_sf(data = ne_coastline, color = "black") + # Detailed sea layer
  geom_sf(data = eu_countries, aes(fill = Proportion)) +
  scale_fill_gradient(low = "dodgerblue", high = "red2", name = "degraded (%)") +
  #scale_fill_viridis() +
  labs(title = "Identified proportion of degraded soils based on LUCAS 2009 soil data") +
  theme_minimal() +
  xlim(x_limits) +
  ylim(y_limits)
graphabstr



ggsave("./Figures/graphical_abstract.png", graphabstr, width = 8, height = 8, dpi = 300, bg="white")

