########################################################################################################
#Globals: vars, funcs, codes that they needed to be executed just once at the beginning
########################################################################################################
#load libraries
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(leaflet)
library(dplyr)
library(leaflet.extras)
library(htmlwidgets)
library(sp)
library(sf)
library(mapview)
library(htmltools)

library(deldir)
library(spatstat)
library(tidyr)
library(tictoc)
library(DEoptim)
library(GenSA)
library(optimParallel)


library(rgdal)
library(tibble)
library(geosphere)
library(lwgeom)
#library(proj4)
########################################################################################################
library(parallel)
library(foreach)
library(doParallel)

numCores <- detectCores()


########################################################################################################
# Code to verify if PhantomJS is installed
########################################################################################################
phantomjs_path <- webshot:::find_phantom()
if (is.null(phantomjs_path)){
  webshot::install_phantomjs()
  FlgJS <- F
} else{
  FlgJS <- T
}
phantomjs_path2 <- webshot:::find_phantom()
(FlgJS2 <- !(is.null(phantomjs_path2)))

EtatInstallationJS <- ifelse(isTRUE(FlgJS),
                             "1-PhJS déja installé",
                             ifelse(isTRUE(FlgJS2),
                                    "2-PhJS vient d'être installé",
                                    "3-PhJS n'a pas été installé"))
cat("phantomjs status:", EtatInstallationJS,"\n")
########################################################################################################







########################################################################################################
#Functions
########################################################################################################

source("myfunctions.R")





########################################################################################################
#Global vars
########################################################################################################
#Boolean for marking the begin of selecting polygon vertices 
#Initially the poly is presented over Camerino anyway
polyBEGIN <- FALSE
#Global polygon for the demo region
regionPoly <-  st_polygon(list())
regionPolyBBox <- st_bbox(regionPoly)
regionPolyArea <- 0

polyIdx <- 0

#Boolean for marking the begin of selecting BS points
#Initially the bs selesction is open
bsBEGIN <- TRUE
nBS <- 0 
BSList <- list()
BSMtx <- NULL


#VBS variables
linesBS2VBS <- list()
VBSList <- list()
VBSMtx <- NULL


#For initial region
#Read KML that is created from Google Earth
regionVtxMtx <- maptools::getKMLcoordinates(kmlfile="Camerino.kml", ignoreAltitude=T)
regionPolyDF <- VtxMtx2SpatPolyDF(regionVtxMtx)
cat("Global length:", length(regionPolyDF@polygons[[1]]@Polygons[[1]]@coords[,1])-1,"\n")
#regionVtxMtx <<- rbind(regionVtxMtx, regionVtxMtx[1,])
regionPoly <-  st_polygon(regionVtxMtx)
regionPolyBBox <- st_bbox(regionPoly)
regionPolyArea <- st_area(regionPoly)


#Voronoi vars
#Number of polygons from  VT
nVoronoiPoly <- 0

#VT polys
VoronoiPolyList <- list()
VoronoiPolyBBoxList <- list()
furthestPtsinPoly <- list()
MaxDist3DinPoly <- list()



#For coloring
hue = c("red", "orange", "yellow","green", "blue", "purple", "pink")
luminosity = c(" ", "light", "bright", "dark")

########################################################################################################
#Global vars for drones
########################################################################################################
#For each BS different number of drones
dronesPerVBSList <- list()


totalDrones <- 25

#Distribute the total number of drones depending on the area?

#Drone min/max/medium altitude 5-300m
hmin <- 15
hmax <- 200
hmed <- floor((hmin + hmax) / 2)


#Drone beam cone angle in degree. Half of the cone infact!!!
halfTheta <- 35 

#Drone min max coverage radius
rmin <- ceiling(hmin * tan(halfTheta *pi / 180))
rmax <- ceiling(hmax * tan(halfTheta *pi / 180))
rmed <- ceiling(hmed * tan(halfTheta *pi / 180))


#Scores for debugging
xwCoverage <- 1.0
xwOverflow <- -1.0
xwOverlap <- -1.0
xwDist <- 1.00
xwScore <- 1.0
overlapThreshold <- 0.9


#List for initsoln
dronesInPolyList <- list()

#Data to hold the soln
solMtx <- list()
solCircles <- list()
#Data to hold the initial soln
initSolMtx <- list()

#Obviousy the grid size should not be smaller than the smallest radius of the drone circle
# and greater than the greatest radius.
gridSize <- 3 * rmin

#For the altitude of drones the following discrete stepping can be done
#This will be input for the GA
altStep <- 5
altValues <- seq(hmin,hmax,by=altStep)


#Get the r values associated to the altValues and give it to the algo
rValues <- ceiling(altValues * tan(halfTheta*pi/180))

########################################################################################################

totDroneReq <- round(areaPolygon(regionPoly[[1]])/ (pi * (hmed  * tan(halfTheta * pi / 180)) * (hmed * tan(halfTheta * pi / 180))))
cat("Region area is",areaPolygon(regionPoly[[1]]),"meter^2\n")
cat("ESTIMATED DRONES REQ FOR HMED AT",hmed,"meter is", totDroneReq,"\n")

########################################################################################################

# 
# myIcons <- iconList(
#   myBS = makeIcon("bs.png", "bs-18@2x.png", 18, 18),
#   myDrone = makeIcon("drone.png", "drone-18@2x.png", 18, 18)
# )

# greenLeafIcon <- makeIcon(
#   iconUrl = "http://leafletjs.com/examples/custom-icons/leaf-green.png",
#   iconWidth = 38, iconHeight = 95,
#   iconAnchorX = 22, iconAnchorY = 94,
#   shadowUrl = "http://leafletjs.com/examples/custom-icons/leaf-shadow.png",
#   shadowWidth = 50, shadowHeight = 64,
#   shadowAnchorX = 4, shadowAnchorY = 62
# )

myBSIcon <- makeIcon(
  iconUrl = "bs.png",
  iconWidth = 48, iconHeight = 48,
  iconAnchorX = 48, iconAnchorY = 48,
)


myDroneIcon <- makeIcon(
  iconUrl = "drone.png",
  iconWidth = 48, iconHeight = 48,
  iconAnchorX = 24, iconAnchorY = 48,
)

myVBSIcon <- makeIcon(
  iconUrl = "vbs-drone.png",
  iconWidth = 48, iconHeight = 48,
  iconAnchorX = 24, iconAnchorY = 48,
)

myDroneIconSmall <- makeIcon(
  iconUrl = "drone.png",
  iconWidth = 24, iconHeight = 24,
  iconAnchorX = 12, iconAnchorY = 12,
)



########################################################################################################

########################################################################################################

