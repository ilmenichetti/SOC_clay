
library(viridis)
library(rgdal)
library(plyr)
library(foreign) #to read db files
library(raster)
library(sp)
library(sf)

library(MASS) #for density estimates with kde2d
library(rworldmap)

library(dplyr) #for joining with the code lookup table

library(ggplot2)
library(ggExtra)
library(ggpubr)
library(paletteer)

#define the palettes
LUC_palette_red=(c("#999999", "#0072B2", "#009E73", "#E69F00", "#CC79A7"))
LUC_palette=c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

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
SMU_sgdbe <- read.dbf("../../soilDB_shapefiles_and_attributes/smu_sgdbe.dbf")

#spatial join
SMU_LUCAS<-over(LUCAS_geodata_2009, Soilmap_Europe_shapefile_WGS84)$SMU
STU_LUCAS<-over(LUCAS_geodata_2009, Soilmap_Europe_shapefile_WGS84)$STU







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

# create the vector of Köppen climate codes for 2009 data
KoppenClim<- c()
for(i in 1:length(KoppenValue)){
  if(!is.na(KoppenValue[i])){
    KoppenClim[i] <- Koppen_dictionary[Koppen_dictionary$Value==KoppenValue[i],]$Climate
  } else {
    KoppenClim[i] <- NA
  }
}


KoppenClim<-as.factor(KoppenClim)


# define the geographic area where we will work
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
              xaxs = par("xaxs"), yaxs = par("yaxs"), main="2015")
plot(worldmap, add=TRUE, lwd=0.5)

dev.off()



#<*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*>*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*>*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*>
#### Combining the datasets (2009)
#<*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*>*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*>*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*>


# a bit of data massaging
LUCAS_geodata_WRB<-cbind(LUCAS_geodata_2009@data, WRBFU=as.factor(WRBFU), WRBFU_group=as.factor(WRBFU_group), WRBFU_qual=as.factor(WRBFU_qual),
                         WRBFU_SMU_LV1=as.factor(WRBFU_SMU_LV1), LUCAS_geodata_2009@coords, KoppenValue, KoppenClim)
LUCAS_geodata_WRB<-na.omit(LUCAS_geodata_WRB)

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
LC0[merged_data$LC1=="B70" | merged_data$LC1=="B81" | merged_data$LC1=="BP0"  | merged_data$LC1=="B81"]="Orchards/Wineyards"
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
LUCAS_geodata_2009_WRB_merged <- left_join(merged_data, LUCAS_LU_classes, by = "LU1")





#<*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*>*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*>*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*>
#### Degraded soils by group
#<*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*>*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*>*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*>

LUCAS_geodata_2009_WRB_merged$clay<-as.numeric(LUCAS_geodata_2009_WRB_merged$clay)
LUCAS_geodata_2009_WRB_merged$OC<-as.numeric(LUCAS_geodata_2009_WRB_merged$OC)


# Calculate the OC_clay_wrb values
OC_clay_wrb <- with(LUCAS_geodata_2009_WRB_merged, OC / (clay * 10))

# Create a data frame with WRB and NUTS columns
df.WRB <- data.frame(
  WRB = LUCAS_geodata_2009_WRB_merged$WRBFU_SMU_LV1,
  NUTS = LUCAS_geodata_2009_WRB_merged$Country,
  LU = LUCAS_geodata_2009_WRB_merged$Class,
  LC = LUCAS_geodata_2009_WRB_merged$LC0
)

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

# Create a table of counts by country and NUTS and calculate proportions by NUTS
table_by_LU_NUTS <- table(df.WRB$NUTS, df.WRB$LU, below_threshold)
proportions_by_LU_NUTS <- prop.table(table_by_LU_NUTS, margin = 1)

table_by_LU <- table(df.WRB$LU, below_threshold)
proportions_by_LU <- prop.table(table_by_LU, margin = 1)

table_by_LC <- table(df.WRB$LC, below_threshold)
proportions_by_LC <- prop.table(table_by_LC, margin = 1)


# Sort prportions based on the number of negatives in descending order
proportions_by_WRB <- proportions_by_WRB[order(proportions_by_WRB[,"TRUE"], decreasing = TRUE),]
proportions_by_NUTS <- proportions_by_NUTS[order(proportions_by_NUTS[,"TRUE"], decreasing = TRUE),]
proportions_by_LU <- proportions_by_LU[order(proportions_by_LU[,"TRUE"], decreasing = TRUE),]
proportions_by_LC <- proportions_by_LC[order(proportions_by_LC[,"TRUE"], decreasing = TRUE),]

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


LUCAS_geodata_2009_WRB_merged$LC0 <- as.factor(LUCAS_geodata_2009_WRB_merged$LC0)
LUCAS_geodata_2009_WRB_merged$LC0_simpl <- as.factor(LUCAS_geodata_2009_WRB_merged$LC0_simpl)
LUCAS_geodata_2009_WRB_merged$clay <- LUCAS_geodata_2009_WRB_merged$clay *10


considered_LC<-c("Bare land", "Cropland", "Forest", "Grassland", "Shrubland")
considered_LC_reduced=c( "Cropland",  "Forest",    "Grassland")



png("LUCAS_clay_C_LUC_groups_ggplot.png", width=2000, height=1700, res=300)
# create scatter plot using ggplot() function
plot <- ggplot(LUCAS_geodata_2009_WRB_merged[LUCAS_geodata_2009_WRB_merged$LC0_simpl %in% considered_LC_reduced,], aes(x=clay, y=OC, color=LC0_simpl))+
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
density<-ggplot(LUCAS_geodata_2009_WRB_merged[LUCAS_geodata_2009_WRB_merged$LC0_simpl %in% considered_LC_reduced,], aes(clay, OC)) +
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
                    labels = c("A", "B"),
                    ncol = 2, nrow = 1)
figure
dev.off()
