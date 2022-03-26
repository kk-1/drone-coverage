

########################################################################################################
#lon lat  to meters conversion
#https://stackoverflow.com/questions/18639967/converting-latitude-and-longitude-points-to-utm
########################################################################################################
find_UTM_zone <- function(longitude, latitude) {
  
  # Special zones for Svalbard and Norway
  if (latitude >= 72.0 && latitude < 84.0 ) 
    if (longitude >= 0.0  && longitude <  9.0) 
      return(31);
  if (longitude >= 9.0  && longitude < 21.0)
    return(33)
  if (longitude >= 21.0 && longitude < 33.0)
    return(35)
  if (longitude >= 33.0 && longitude < 42.0) 
    return(37)
  
  (floor((longitude + 180) / 6) %% 60) + 1
}


find_UTM_hemisphere <- function(latitude) {
  
  ifelse(latitude > 0, "north", "south")
}

# returns a DF containing the UTM values, the zone and the hemisphere
longlat_to_UTM <- function(long, lat, units = 'm') {
  
  df <- data.frame(
    id = seq_along(long), 
    x = long, 
    y = lat
  )
  sp::coordinates(df) <- c("x", "y")
  
  hemisphere <- find_UTM_hemisphere(lat)
  zone <- find_UTM_zone(long, lat)
  
  sp::proj4string(df) <- sp::CRS("+init=epsg:4326") 
  CRSstring <- paste0(
    "+proj=utm +zone=", zone,
    " +ellps=WGS84",
    " +", hemisphere,
    " +units=", units)
  if (dplyr::n_distinct(CRSstring) > 1L) 
    stop("multiple zone/hemisphere detected")
  
  res <- sp::spTransform(df, sp::CRS(CRSstring[1L])) %>%
    tibble::as_data_frame() %>%
    dplyr::mutate(
      zone = zone,
      hemisphere = hemisphere
    )
  
  res
}

UTM_to_longlat <- function(utm_df, zone, hemisphere) {
  
  CRSstring <- paste0("+proj=utm +zone=", zone, " +", hemisphere)
  utmcoor <- sp::SpatialPoints(utm_df, proj4string = sp::CRS(CRSstring))
  longlatcoor <- sp::spTransform(utmcoor, sp::CRS("+init=epsg:4326"))
  tibble::as_data_frame(longlatcoor)
}
########################################################################################################


########################################################################################################
#Create circle as polygon
########################################################################################################
CreateCirclePolyPixel <- function(x0, y0, r){
  
  #make each 2 pixels one edge
  res <- floor(pi * r)
  theta = seq(0, 2*pi, length.out = res+1)
  
  x = r*cos(theta) + x0
  y = r*sin(theta) + y0
  
  #Close the polygon: first and last points should be same
  x[res +1] <- x[1]
  y[res +1] <- y[1]
  
  return(st_polygon(list(cbind(x, y))))
  
  
}
########################################################################################################

########################################################################################################
#Create circle as polygon
########################################################################################################
CreateCirclePolyMeter <- function(x0, y0, r){
  
  #give r as meter!!!
  #divide perimeter into 1 meter sticks 
  res <- floor(2 * pi * r)
  #res <- floor(pi * r)
  theta <-  seq(0, 2 * pi, length.out = res + 1)
  
  #1 degree is 111111 meters for lon and lat!!!!
  x <- r / 111111 * cos(theta) + x0
  y <- r / 111111 * sin(theta) + y0
  
  #Close the polygon: first and last points should be same
  x[res + 1] <- x[1]
  y[res + 1] <- y[1]
  
  return(st_polygon(list(cbind(x, y))))
  
  
}
########################################################################################################


########################################################################################################
#Create circle as spatial polygon data frame. Good for overlaying on maps
########################################################################################################
CreateCircleSPDF <- function(x0, y0, r){
  
  #make each 2 pixels one edge
  res <- floor(pi * r)
  theta = seq(0, 2*pi, length.out = res+1)
  
  x = r*cos(theta) + x0
  y = r*sin(theta) + y0
  
  #Close the polygon: first and last points should be same
  x[res +1] <- x[1]
  y[res +1] <- y[1]
  
  
  xy <- cbind(x,y)
  
  #make polygon from kml
  p1 = sp::Polygon(xy)
  #make Polygon class
  p2 = sp::Polygons(list(p1), ID = "drivetime")
  #make spatial polygons class
  p3 = sp::SpatialPolygons(list(p2),proj4string=sp::CRS("+init=epsg:4326")) # WGS 84 frpmat for GPS coord
  # Create a dataframe and display default rownames
  p3.df <- data.frame(ID=1:length(p3)) 
  #rownames(p3.df)
  # Extract polygon ID's
  pid <- sapply(slot(p3, "polygons"), function(x) slot(x, "ID")) 
  # Create dataframe with correct rownames
  p3.df <- data.frame( ID=1:length(p3), row.names = pid)    
  #Create the SpatialPolygonsDataFrame
  p4 = SpatialPolygonsDataFrame(p3, data=p3.df)
  
  return(p4)
}
########################################################################################################


########################################################################################################
#https://stackoverflow.com/questions/639695/how-to-convert-latitude-or-longitude-to-meters
#https://en.wikipedia.org/wiki/Haversine_formula
#Converts lon, lat to meters
########################################################################################################
lonlat2m <- function(lon1,lat1,lon2,lat2) {
  R <- 6378.137                                # radius of earth in Km
  dLat <- (lat2-lat1)*pi/180
  dLon <- (lon2-lon1)*pi/180
  a <- sin((dLat/2))^2 + cos(lat1*pi/180)*cos(lat2*pi/180)*(sin(dLon/2))^2
  c <- 2 * atan2(sqrt(a), sqrt(1-a))
  d <- R * c
  return (d * 1000)                            # distance in meters
}
########################################################################################################


########################################################################################################
#Convert the circle polygon list to drone solution matrix (x,y,r for each drone)
########################################################################################################
dronesInPolyList2DroneXYRVec <-  function(xdronesInPolyList,xDronesPerPolyVec,polyIdx){
  
  #Parameters
  #xdronesInPolyList: The related circle list for specific polygon not the polyList
  #xDronesPerPolyVec: Vector that contains numbers of circles for each poly it can be skipped if you use ndrones global vec
  #polyIdx: polyIdx of the kth polygon
  
  initDroneXYRVec <- matrix(rep(c(0,0,0),xDronesPerPolyVec[[polyIdx]]), xDronesPerPolyVec[[polyIdx]]*3, 1, byrow = TRUE)
  
  n <- 1
  for (i in 1:xDronesPerPolyVec[[polyIdx]]) {
    #The only way is to find the nearest point to the centroid and calculate distance
    #basically get the first point on the circle and find the distance between centroid (center)
    ptsOnCircle <- st_point(c(xdronesInPolyList[[i]][[1]][1,1], xdronesInPolyList[[i]][[1]][1,2]))
    cntrCircle <- st_centroid(xdronesInPolyList[[i]])
    st_nearest_points(st_centroid(xdronesInPolyList[[i]]), xdronesInPolyList[[i]])
    rad <- st_distance(ptsOnCircle,cntrCircle)
    cat("***Poly",polyIdx,"circle",i,"Radius",rad,"\n")
    # xi <- ceiling(initDroneXYRVec[i]) #+ tempBBX$xmin
    # yi <- ceiling(initDroneXYRVec[i+1]) # + tempBBX$ymin
    # ri <- ceiling(initDroneXYRVec[i+2])
    
    
    #The coordinates should be relative to the polygon or absolute?
    initDroneXYRVec[n] <- cntrCircle[1]
    initDroneXYRVec[n+1] <- cntrCircle[2]
    initDroneXYRVec[n+2] <-  rad
    n <- n + 3
    #draw.circle(xyr[i, 1], xyr[i, 2], xyr[i, 3], "gray")
  }
  
  #Return value
  initDroneXYRVec
}
########################################################################################################


########################################################################################################
#Convert the drone solution matrix (x,y,r for each drone) to circle polygon list
########################################################################################################
DroneXYRVec2dronesInPolyList <- function(dMtx){
  
  lx <- length(dMtx)
  xdronesInPolyList <- list()
  i <- 1
  for (k in seq(1,lx,3)) {
    x1 <- dMtx[k]
    y1 <- dMtx[k+1]
    r1 <- dMtx[k+2] * 111111
    xdronesInPolyList[[i]] <- CreateCirclePolyMeter(x1, y1, r1)
    i <- i + 1 
  }
  
  #Return value
  xdronesInPolyList
}
########################################################################################################


########################################################################################################
#Converts the initial solution to desired format by evolutionary algos
########################################################################################################
DroneXYRVec2Initsol <-  function(imtx,n){
  initSoln <- matrix(c(imtx), nrow = 1, ncol = 3*n)
  
  #Return value
  initSoln
}
########################################################################################################


########################################################################################################
#Show the statistics of the solution matrix (x,y,r for each drone)
########################################################################################################
my_show_DroneXYRVec_stat <- function(dMtx){
  
  
  lx <- length(dMtx)
  cat(lx/3, "circles r (in meter):\n")
  
  for (k in seq(1,lx,3)) {
    # Get values from the final solution fo the GA
    x1 <- dMtx[k]
    y1 <- dMtx[k+1]
    r1 <- dMtx[k+2] * 111111
    cat(r1," ")
  }
  cat("\n\n")
}
########################################################################################################


########################################################################################################
#Show the statistics of the circle list
########################################################################################################
my_show_dronesInPolyList_stat <- function(poly, xdronesInPolyList, xDronesPerPolyVec, polyIdx){
  
  
  ###############################################################
  #TODO
  ###############################################################
  #In/out polygon/region(all the polygons) overflow and overlaps
  ###############################################################
  
  ###############################################################
  #DEBUG for the kth poly
  # 
  # poly <- VoronoiPolyList[[k]] 
  # xdronesInPolyList <- solCircles[[k]] 
  # xDronesPerPolyVec <- dronesPerVBSList 
  # polyIdx <- k
  ###############################################################
  
  nCircle <- xDronesPerPolyVec[[polyIdx]]
  
  
  #Find the overflow (union of circles - (intersection of (union of circles, region)) color it
  ########################################################################################################
  circlePolyUnion <- st_polygon(list())
  
  
  
  for(i in 1:(nCircle-1)){
    for(j in (i+1):nCircle){
      unionij <- st_union(xdronesInPolyList[[i]], xdronesInPolyList[[j]])
      circlePolyUnion <- st_union(circlePolyUnion, unionij)
    }
  }
  
  
  regionIntersect <- st_intersection(poly, circlePolyUnion)
  plot(regionIntersect, add=TRUE, col="yellow")
  
  
  plot(circlePolyUnion, add=TRUE, col="green")
  
  regionOverflow <- st_difference(circlePolyUnion, regionIntersect)
  plot(regionOverflow, add=TRUE, col="red")
  
  
  #Find the intersection (overlap) of all circles and color it
  ########################################################################################################
  
  #Find the intersection (overlap) of all circles and color it
  ########################################################################################################
  circlePolyIntersect <- st_polygon(list())
  circleOverlap <- 0
  
  for(i in 1:(nCircle-1)){
    for(j in (i+1):nCircle){
      circleAreai <- st_area(xdronesInPolyList[[i]])
      circleAreaj <- st_area(xdronesInPolyList[[j]])
      interij <- st_intersection(xdronesInPolyList[[i]], xdronesInPolyList[[j]])
      interArea <- st_area(interij)
      #If we have overlap more than the 90% of the smaller circle
      #in other words one of them is almost under the other, overlap situation
      #Then give zero score and exit evaluation
      if (interArea >= overlapThreshold * min(circleAreai, circleAreaj)){
        #DEBUG
        cat("Threshold exceeded! Circles",i,j,"of polygon", polyIdx,"\n")
        # circleOverlap <- 1
        # break
      }
      # if (circleOverlap == 1) break
      circlePolyIntersect <- st_union(circlePolyIntersect, interij)
    }
  }
  
  #if (circleOverlap == 1) return(0)
  
  #DEBUG
  plot(circlePolyIntersect, add=TRUE, col="blue")
  # ########################################################################################################
  # 
  # 
  # circlePolyIntersect <- st_polygon(list())
  # 
  # for(i in 1:(nCircle-1)){
  #   for(j in (i+1):nCircle){
  #     interij <- st_intersection(xdronesInPolyList[[i]], xdronesInPolyList[[j]])
  #     circlePolyIntersect <- st_union(circlePolyIntersect, interij)
  #   }
  # }
  # plot(circlePolyIntersect, add=TRUE, col="blue")
  ########################################################################################################
  
  
  #Find total dist from VBS to centroids
  ########################################################################################################
  
  totDistInPoly <- 0 
  vbsCntr <- VBSList[[polyIdx]]
  
  for(i in 1:nCircle){
    centroXY <- st_centroid(xdronesInPolyList[[i]])
    
    segments(vbsCntr[1], vbsCntr[2], centroXY[1], centroXY[2])
    d2D <- st_distance(vbsCntr,centroXY)
    
    
    
    #The only way is to find the nearest point to the centroid and calculate distance
    #basically get the first point on the circle and find the distance between centroid (center)
    ptsOnCircle <- st_point(c(xdronesInPolyList[[i]][[1]][1,1], xdronesInPolyList[[i]][[1]][1,2]))
    cntrCircle <- st_centroid(xdronesInPolyList[[i]])
    st_nearest_points(st_centroid(xdronesInPolyList[[i]]), xdronesInPolyList[[i]])
    rad <- st_distance(ptsOnCircle,cntrCircle)
    
    
    #VBSs are AT ALTITUDE of HMED!!!
    droneHeightDiffFromVBS <- abs( (rad / (tan(halfTheta * pi / 180))) - (hmed) )
    
    d3D <- sqrt((d2D * d2D) + (droneHeightDiffFromVBS * droneHeightDiffFromVBS))
    cat("Poly",polyIdx, "circle",i,"dist", d3D,"\n")
    totDistInPoly <- totDistInPoly + d3D
  }
  
  
  
  #Print some statistics
  ########################################################################################################
  #pCovered <- st_difference(circlePolyUnion, regionOverflow)
  pCovered <- regionIntersect
  plot(pCovered, add=TRUE, col="green")
  plot(circlePolyIntersect, add=TRUE, col="blue")
  
  #DEBUG the areas are in km^2
  areaRegion <- st_area(poly)
  areaCovered <- st_area(pCovered)
  areaOverlap <- st_area(circlePolyIntersect)
  areaOverflow <- st_area(regionOverflow)
  
  cat("Total area for poly",polyIdx,":",areaRegion,
      "\nTotal area for poly",polyIdx,":",areaPolygon(poly[[1]]), areaPolygon(VoronoiPolyList[[polyIdx]][[1]]), "meter^2",
      "\nCovered:",areaCovered,"---",round(100*areaCovered/areaRegion, 2),"%",
      "\nTotal Overlap:",areaOverlap,"---",round(100*areaOverlap/areaRegion, 2),"%",
      "\nTotal Overflow:",areaOverflow,"---",round(100*areaOverflow/areaRegion, 2),"%",
      "\nDistance Diff Percent:",round((100 * ((MaxDist3DinPoly[[polyIdx]] * nCircle) - totDistInPoly) / (MaxDist3DinPoly[[polyIdx]] * nCircle)), 2),"%",
      "\nMax 3D Distance:",MaxDist3DinPoly[[polyIdx]],
      "\nTotal 3D Distance:",totDistInPoly,"\n"
  )
  ########################################################################################################
  
  
  
  
  
}
########################################################################################################






########################################################################################################
#Show the statistics of the region
########################################################################################################
my_show_overall_stat <- function(xdronesInPolyList, xDronesPerPolyVec){
  
  #DEBUG
  #xdronesInPolyList <- solCircles
  #xDronesPerPolyVec <- DronesPerPolyVec
  
  
  
  allCircles <- list()
  for (i in 1:nVoronoiPoly) allCircles <- c(allCircles, xdronesInPolyList[[i]])
  
  nCircle <- length(allCircles)
  
  
  
  #Find the overflow (union of circles - (intersection of (union of circles, region)) color it
  ########################################################################################################
  circlePolyUnion <- st_polygon(list())
  
  
  
  for(i in 1:(nCircle-1)){
    for(j in (i+1):nCircle){
      unionij <- st_union(allCircles[[i]], allCircles[[j]])
      circlePolyUnion <- st_union(circlePolyUnion, unionij)
    }
  }
  
  
  regionIntersect <- st_intersection(regionPoly, circlePolyUnion)
  plot(regionIntersect, add=TRUE, col="yellow")
  
  
  plot(circlePolyUnion, add=TRUE, col="green")
  
  regionOverflow <- st_difference(circlePolyUnion, regionIntersect)
  plot(regionOverflow, add=TRUE, col="red")
  
  
  #Find the intersection (overlap) of all circles and color it
  ########################################################################################################
  
  #Find the intersection (overlap) of all circles and color it
  ########################################################################################################
  circlePolyIntersect <- st_polygon(list())
  circleOverlap <- 0
  
  for(i in 1:(nCircle-1)){
    for(j in (i+1):nCircle){
      circleAreai <- st_area(allCircles[[i]])
      circleAreaj <- st_area(allCircles[[j]])
      interij <- st_intersection(allCircles[[i]], allCircles[[j]])
      interArea <- st_area(interij)
      #If we have overlap more than the 90% of the smaller circle
      #in other words one of them is almost under the other, overlap situation
      #Then give zero score and exit evaluation
      if (interArea >= overlapThreshold * min(circleAreai, circleAreaj)){
        #DEBUG
        cat("Threshold exceeded! Circles",i,j,"\n")
        # circleOverlap <- 1
        # break
      }
      # if (circleOverlap == 1) break
      circlePolyIntersect <- st_union(circlePolyIntersect, interij)
    }
  }
  
  #if (circleOverlap == 1) return(0)
  
  #DEBUG
  plot(circlePolyIntersect, add=TRUE, col="blue")
  # ########################################################################################################
  # 
  # 
  # circlePolyIntersect <- st_polygon(list())
  # 
  # for(i in 1:(nCircle-1)){
  #   for(j in (i+1):nCircle){
  #     interij <- st_intersection(xdronesInPolyList[[i]], xdronesInPolyList[[j]])
  #     circlePolyIntersect <- st_union(circlePolyIntersect, interij)
  #   }
  # }
  # plot(circlePolyIntersect, add=TRUE, col="blue")
  ########################################################################################################
  
  
  #Find total dist from VBS to centroids
  ########################################################################################################
  
  totDistInRegion <- 0 
  totMaxDist3DinRegion <- 0
  distDiffPerInPoly <- vector()
  
  #For each polygon find the total dist diff
  
  for (polyNum in 1:nVoronoiPoly) {
    vbsCntr <- VBSList[[polyNum]]
    totDistInPoly <- 0 
    
    
    #For each circle in the polygon find the individual dist diff
    for(circleNum in 1:xDronesPerPolyVec[polyNum]){
      centroXY <- st_centroid(xdronesInPolyList[[polyNum]][[circleNum]])
      
      segments(vbsCntr[1], vbsCntr[2], centroXY[1], centroXY[2])
      d2D <- st_distance(vbsCntr,centroXY)
      
      
      
      #The only way is to find the nearest point to the centroid and calculate distance
      #basically get the first point on the circle and find the distance between centroid (center)
      ptsOnCircle <- st_point(c(xdronesInPolyList[[polyNum]][[circleNum]][[1]][1,1], xdronesInPolyList[[polyNum]][[circleNum]][[1]][1,2]))
      cntrCircle <- st_centroid(xdronesInPolyList[[polyNum]][[circleNum]])
      st_nearest_points(st_centroid(xdronesInPolyList[[polyNum]][[circleNum]]), xdronesInPolyList[[polyNum]][[circleNum]])
      rad <- st_distance(ptsOnCircle,cntrCircle)
      
      
      #VBSs ARE AT ALT OF HMED!!!
      droneHeightDiffFromVBS <- abs( (rad / (tan(halfTheta * pi / 180))) - (hmed) )
      
      d3D <- sqrt((d2D * d2D) + (droneHeightDiffFromVBS * droneHeightDiffFromVBS))
      cat("Poly",polyNum, "circle",circleNum,"dist", d3D,"\n")
      totDistInPoly <- totDistInPoly + d3D
    }
    
    #totDistPer <- 100 * (maxDist*ndrones - sumDist) / (maxDist*ndrones)
    totMaxDist3DinPoly <- (MaxDist3DinPoly[[polyNum]] * xDronesPerPolyVec[polyNum])
    
    distDiffPerInPoly <- c (distDiffPerInPoly , (100 * (totMaxDist3DinPoly - totDistInPoly) / totMaxDist3DinPoly) )
    
    cat("Total Distance:",totDistInPoly,
        "\nDistance Difference Percent:",round((100 * (totMaxDist3DinPoly - totDistInPoly) / totMaxDist3DinPoly), 2),"%",
        "\nMax Distance:",MaxDist3DinPoly[[polyNum]],"\n\n"
    )
    totMaxDist3DinRegion <- totMaxDist3DinRegion + (MaxDist3DinPoly[[polyNum]] * xDronesPerPolyVec[polyNum])
    
    totDistInRegion <- totDistInRegion + totDistInPoly
  }
  
  #TODO Ratios for the overall total dist measure?
  
  #Print some statistics
  ########################################################################################################
  #pCovered <- st_difference(circlePolyUnion, regionOverflow)
  pCovered <- regionIntersect
  plot(pCovered, add=TRUE, col="green")
  plot(circlePolyIntersect, add=TRUE, col="blue")
  
  areaRegion <- st_area(regionPoly)
  areaCovered <- st_area(pCovered)
  areaOverlap <- st_area(circlePolyIntersect)
  areaOverflow <- st_area(regionOverflow)
  
  distDiffPerInRegion <- sum(distDiffPerInPoly * xDronesPerPolyVec) / sum(xDronesPerPolyVec)
  
  cat("Total area for region:",areaRegion,
      "\nCovered:",areaCovered,"---",round(100 * areaCovered / areaRegion, 2),"%",
      "\nTotal Overlap:",areaOverlap,"---",round(100 * areaOverlap / areaRegion, 2),"%",
      "\nTotal Overflow:",areaOverflow,"---",round(100 * areaOverflow / areaRegion, 2),"%",
      "\nTotal Distance:",totDistInRegion,
      "\nOverall Distance Difference Percent:",round(distDiffPerInRegion, 2),"%",
      "\n"
  )
  ########################################################################################################
  
  
  
  
  
}
########################################################################################################























########################################################################################################
#Get the list of points in the polygon as input to GA algorithm
########################################################################################################
getPtsInPoly <-  function(polyX, gSize){
  
  
  ########################################################################################################
  #Choose the gridsize and you will get the points in the polygon
  ########################################################################################################
  #Obviousy the grid size should not be smaller than the smallest radius of the drone circle
  # and greater than the greatest radius.
  gridSize <- gSize
  
  
  
  
  # Below is the part to get the list of points in the polygon
  ################################################################################
  tempBBX <- st_bbox(polyX)
  
  
  #Check the grid
  xPts <- seq(tempBBX$xmin, tempBBX$xmax, by=gridSize)
  yPts <- seq(tempBBX$ymin, tempBBX$ymax, by=gridSize)
  
  pointXY <- crossing(xPts, yPts)
  
  
  #Consider the polygons with multiple parts
  nPartsinPoly <- length(polyX)
  inTable <- rep(0, length(xPts))
  if (nPartsinPoly > 1){
    for (p in (1:nPartsinPoly)){
      inTable <- inTable + point.in.polygon(pointXY$xPts, pointXY$yPts, polyX[[p]][[1]][,1],polyX[[p]][[1]][,2])
    }
  } else {
    #get the vector of points for a single part
    inTable <- point.in.polygon(pointXY$xPts, pointXY$yPts, polyX[[1]][,1],polyX[[1]][,2])
  }
  
  
  #Find the indexes of points inside
  logicalIdx <- as.logical(inTable)
  
  
  #Subsetting for the inside points
  inX <- pointXY$xPts[logicalIdx]
  inY <- pointXY$yPts[logicalIdx]
  
  #points(pointXY$xPts, pointXY$yPts, add = TRUE, col = "black")
  
  #points(inX, inY, add = TRUE, col = "black",cex=0.2)
  
  #Give inX and inY as parameters for the GA!!!
  ########################################################################################################
  
  
  return(as.data.frame(cbind(inX,inY)))
  
  
  
}

########################################################################################################




########################################################################################################
#Fitness function that rules them all others!!!
########################################################################################################
fit_with_weights <-  function(solVector, wCoverage, wOverflow, wOverlap, wDist, wScore, polyNo){
  #This function should be fast. So avoid calling simple functions inside of it. Just put the code!
  
  # #DEBUG
  # 
  # solVector <- solMtx
  # wCoverage=xwCoverage
  # wOverflow=xwOverflow
  # 
  # wOverlap=xwOverlap
  # wDist=xwDist
  # wScore=xwScore
  # polyNo <- polyIdx 
  
  #First convert the solVector to circlepoly list
  
  lx <- length(solVector)
  xcircleList <- list()
  i <- 1
  for (k in seq(1,lx,3)) {
    x1 <- solVector[k]
    y1 <- solVector[k+1]
    
    
    
    #Check if the point is outside poly reject the soln!
    #########################################################
    lenList <- length(VoronoiPolyList[[polyNo]]) 
    inPoly <- 0
    if (lenList > 1) {
      #If the polygon has multiple pieces, being in one piece is enough for the "inness" of the point
      for (z in 1:lenList){
        inPoly <- inPoly + point.in.polygon(x1, y1, VoronoiPolyList[[polyNo]][[z]][[1]][,1],VoronoiPolyList[[polyNo]][[z]][[1]][,2])
      }
    }else{
      inPoly <- inPoly + point.in.polygon(x1, y1, VoronoiPolyList[[polyNo]][[1]][,1],VoronoiPolyList[[polyNo]][[1]][,2])
    }
    
    #if the point is outside reject the soln
    if (inPoly == 0) {
      
      #DEBUG cat("--- For poly", polyNo, "circle number",i,"x y:",x1,y1,"not in poly:",inPoly,"\n")
      break
    }
    #########################################################
    
    
    
    
    r1 <- solVector[k+2] * 111111
    xcircleList[[i]] <- CreateCirclePolyMeter(x1, y1, r1)
    i <- i + 1 
  }
  
  #if the center of any circle is outside of the polygon reject the soln!
  if (inPoly == 0) return(0)
  
  
  
  
  #How many circles?
  nCircle <- i-1
  
  
  #TODO if there is one circle crashes!!!!
  
  #Find the intersection (overlap) of all circles and color it
  ########################################################################################################
  xcirclePolyIntersect <- st_polygon(list())
  circleOverlap <- 0
  
  for(i in 1:(nCircle-1)){
    for(j in (i+1):nCircle){
      circleAreai <- st_area(xcircleList[[i]])
      circleAreaj <- st_area(xcircleList[[j]])
      interij <- st_intersection(xcircleList[[i]], xcircleList[[j]])
      interArea <- st_area(interij)
      #If we have overlap more than the 90% of the smaller circle
      #in other words one of them is almost under the other, overlap situation
      #Then give zero score and exit evaluation
      if (interArea >= overlapThreshold * min(circleAreai, circleAreaj)){
        #DEBUG
        #cat("Threshold exceeded! Circles",i,j,"of polygon", polyNo,"\n")
        circleOverlap <- 1
        break
      }
      if (circleOverlap == 1) break
      xcirclePolyIntersect <- st_union(xcirclePolyIntersect, interij)
    }
  }
  
  if (circleOverlap == 1) return(0)
  
  #DEBUG
  #plot(circlePolyIntersect, add=TRUE, col="blue")
  ########################################################################################################
  
  
  
  #Find the overflow (union of circles - (intersection of (union of circles, region)) color it
  ########################################################################################################
  xcirclePolyUnion <- st_polygon(list())
  
  for(i in 1:(nCircle-1)){
    for(j in (i+1):nCircle){
      unionij <- st_union(xcircleList[[i]], xcircleList[[j]])
      xcirclePolyUnion <- st_union(xcirclePolyUnion, unionij)
    }
  }
  
  regionIntersect <- st_intersection(VoronoiPolyList[[polyNo]], xcirclePolyUnion)
  
  #DEBUG
  #plot(regionIntersect, add=TRUE, col="yellow")
  #plot(circlePolyUnion, add=TRUE, col="green")
  
  regionOverflow <- st_difference(xcirclePolyUnion, regionIntersect)
  
  #DEBUG
  #plot(regionOverflow, add=TRUE, col="red")
  
  
  
  
  
  #Find total dist from VBS to centroids
  ########################################################################################################
  
  totDistInPoly <- 0 
  vbsCntr <- VBSList[[polyNo]]
  distVec <- vector()
  for(i in 1:nCircle){
    centroXY <- st_centroid(xcircleList[[i]])
    #DEBUG
    #segments(vbsCntr[1], vbsCntr[2], centroXY[1], centroXY[2], add = TRUE)
    #Find the 3D distance between VBS and the drone
    d2D <- st_distance(vbsCntr,centroXY)
   
    # droneHeightDiffFromVBS <-  solVector[i * 3] / (tan(halfTheta * pi / 180))
    
    droneHeightDiffFromVBS <- abs( (solVector[i * 3]  / (tan(halfTheta * pi / 180))) - (hmed) )
    
    d3D <- sqrt((d2D * d2D) + (droneHeightDiffFromVBS * droneHeightDiffFromVBS))
    
    #DEBUG
    #cat("Poly",k, "circle",i,"dist", d,"\n")
    totDistInPoly <- totDistInPoly + d3D
    distVec <- c(distVec, d3D)
  }
  
  
  
  #Print some statistics
  ########################################################################################################
  #pCovered <- st_difference(xcirclePolyUnion, regionOverflow)
  
  pCovered <- regionIntersect
  
  #DEBUG
  #plot(pCovered, add=TRUE, col="green")
  #plot(circlePolyIntersect, add=TRUE, col="blue")
  
  areaRegion <- st_area(VoronoiPolyList[[polyNo]])
  areaCovered <- st_area(pCovered)
  areaOverlap <- st_area(xcirclePolyIntersect)
  areaOverflow <- st_area(regionOverflow)
  
  
  
  # As drones comes towards "Virtual Base Station" distPer becomes larger
  xavgDist <- totDistInPoly / nCircle
  #DEBUG
  #cat("avg dist from my calc:", xavgDist, "from vector:",mean(distVec),"\n")
  
  xmedDist <- median(distVec)
  maxDist <- MaxDist3DinPoly[[polyNo]]
  distPer <- 100 * (maxDist - xavgDist) / maxDist
  medDistPer <- 100 * (maxDist - xmedDist) / maxDist
  #The dist per is the deviance from the maxdist
  #So closer the drones higher the per better results
  totDistPer <- 100 * (maxDist * nCircle - totDistInPoly) / (maxDist * nCircle)
  
  coveredPer <- (areaCovered * 100 / areaRegion)
  overflowPer  <- (areaOverflow * 100 / areaCovered)
  overlapPer  <- (areaOverlap * 100 / areaCovered)
  
  score <- wCoverage * coveredPer + wOverflow * overflowPer + wOverlap * overlapPer + wDist * totDistPer
  
  #DEBUG
  # cat("Total area for poly:",areaRegion,
  #     "\nCovered:",areaCovered,"---",round(100*areaCovered/areaRegion, 2),"%",
  #     "\nTotal Overlap:",areaOverlap,"---",round(100*areaOverlap/areaRegion, 2),"%",
  #     "\nTotal Overflow:",areaOverflow,"---",round(100*areaOverflow/areaRegion, 2),"%",
  #     "\nDistance Percent:",round((100*totDistInPoly/(maxDist * nCircle)), 2),"%",
  #     "\nMax Distance:",maxDist,
  #     "\nTotal Distance:",totDistInPoly,"\n"
  # )
  # 
  return(wScore * score)
  
  
  
}

########################################################################################################






########################################################################################################
#Draw demo region
########################################################################################################
draw_demo_region <-  function(){
  
  vRegionClosed <- rbind(c(0,0), c(0,200), c(200,350), c(400,200), c(400,0), c(250, 0), c(70, 100) , c(75,0), c(0,0))
  spRegion <- sp::Polygon(vRegionClosed)
  
  regionPoly <<-  st_polygon(list(vRegionClosed))
  regionPolyBBox <<- st_bbox(regionPoly)
  regionPolyArea <<- st_area(regionPoly)
  
  plot(regionPoly, main="Drone Coverage Demo", sub="subtitle",xaxt="n", yaxt="n",
       xlab="width", ylab="height",
       xlim=c(-xMargin, regionPolyBBox$xmax+xMargin), ylim=c(-yMargin, regionPolyBBox$ymax+yMargin),
       col.axis="blue", font.axis=4, cex.axis=1.5,las=1)
  
  
  # Changing x axis
  xtick<-seq(-xMargin, regionPolyBBox$xmax+xMargin, by=50)
  axis(side=1, at=xtick)
  
  ytick<-seq(-yMargin, regionPolyBBox$ymax+yMargin, by=50)
  axis(side=2, at=ytick)
  
  
  #polygon(p[[1]], border = "black", col = NA)
  #sf::st_area(regionPoly)
  
  plot(st_as_sfc(regionPolyBBox),add=TRUE)
}
########################################################################################################






########################################################################################################
#Draw shortest distance line from BSs to the perimeter of the region
#where VBS will fly to
########################################################################################################
draw_vbs_lines <-  function(){
  
  
  for (i in 1:nBS){
    #Find the point nearest to the BS on the edge of the polygon which will be VBS loc
    linesBS2VBS[[i]] <<- st_nearest_points(BSList[[i]],regionPoly)
    
    VBSList[[i]] <<- st_point(c(linesBS2VBS[[i]][[1]][2,1], linesBS2VBS[[i]][[1]][2,2]))
    
    VBSMtx[i,] <<- c(linesBS2VBS[[i]][[1]][2,1], linesBS2VBS[[i]][[1]][2,2])
    plot(linesBS2VBS[[i]], add = TRUE, col ="blue")
    plot(BSList[[i]], add = TRUE, col="red")
    plot(VBSList[[i]], add = TRUE, col="green")
  }
}
########################################################################################################




########################################################################################################
#Find VBS positions for Voronoi Tess.
########################################################################################################
draw_voronoi_tess <-  function(){
  
  #Voronoi stuff
  dxy <- deldir(x = VBSMtx[,1], y = VBSMtx[,2], 
                rw = c(regionPolyBBox$xmin,regionPolyBBox$xmax, regionPolyBBox$ymin,regionPolyBBox$ymax))
  txy <- tile.list(dxy)
  
  #Number of polygons from  VT
  nVoronoiPoly <<- length(txy)
  for (i in 1:nVoronoiPoly){
    pLen <- length(txy[[i]][["x"]])
    plyX <- txy[[i]][["x"]]
    plyY <- txy[[i]][["y"]]
    plyX[pLen+1] <- plyX[1]
    plyY[pLen+1] <- plyY[1]
    
    
    colHue <- sample(hue,1)
    colLum <- sample(luminosity,1)
    myCol <- paste(colLum,colHue)
    
    VoronoiPolyList[[i]] <<- st_intersection(regionPoly, st_polygon(list(cbind(plyX,plyY))))
    VoronoiPolyBBoxList[[i]] <<- st_as_sfc(st_bbox(VoronoiPolyList[[i]]))
    plot(VoronoiPolyList[[i]], add=TRUE, col = hue[i])
    
    
    
    areaPolyX <- st_area(VoronoiPolyList[[i]])
    DronesPerPolyVec[i] <<- floor(totalDrones * areaPolyX / regionPolyArea)
    #TODO Handle the case of single drone!!!
    cat(DronesPerPolyVec[i], "drones necessary for poly",i,"\n")
    
    #Put label of the polygon with opposite color on the centroid
    centroXY <- st_centroid(VoronoiPolyList[[i]])
    opCol <- paste0("#" , toupper(sprintf("%x", (0xFFFFFF - as.numeric(stringr::str_replace(gplots::col2hex(hue[i]), "#", "0x"))))))
    l <- stringr::str_length(opCol)
    zerosAtEnd <- 7 - l
    opCol <- paste0(opCol, paste(rep("0", zerosAtEnd), sep="", collapse=""))
    #DEBUG cat(opCol,"\n")              
    text(centroXY[[1]], centroXY[[2]], sprintf("%d", i), col = opCol)
    
    
    #Find the furthest distance to that VBS point
    #We will use it to estimate the distance percentages
    #Convert sf to spatial
    y <- as(VoronoiPolyList[[i]], "Spatial")
    #Convert spatial to owin object so that we can use distances func
    yowin <- polyCub::as.owin.SpatialPolygons(y)
    #Get vertices of the polygon
    vtx <- vertices(yowin) # Polygon vertices
    #Find the distances from the VBS to all the vertices of the poly
    #Here fudging for simplicity?
    #Any point on the edge can be at max distance?
    d2D <- crossdist(vtx$x, vtx$y, VBSList[[i]][1],  VBSList[[i]][2]) # n-column matrix of cross distances
    i1 <- which.max(d2D[,1]) # Index of max dist point
    
    #DEBUG points(vtx$x[i1], vtx$y[i1], col = "red", cex = 1.5)
    
    #For drawing in 2D
    furthestPtsinPoly[[i]] <<- st_point(c(vtx$x[i1], vtx$y[i1]))
    #For calculating in 3D
    MaxDist3DinPoly[[i]] <<- sqrt ((d2D[i1] * d2D[i1]) + (hmax * hmax)) 
    
    #DEBUG segments(VBSList[[i]][1],VBSList[[i]][2],furthestPtsinPoly[[i]][1],furthestPtsinPoly[[i]][2], add = TRUE, col = opCol )
    
    
    #DEBUG points(furthestPtsinPoly[[i]][1], furthestPtsinPoly[[i]][2], col = opCol, pch=9+i, cex = 1.5)
  }
}
########################################################################################################




########################################################################################################
#Put grid points on each bbx of voronoi poly
########################################################################################################
draw_voronoi_bbx_pts <-  function(){
  
  
  #Add the bounding boxes of the Voronoi polygons
  for (i in 1:nVoronoiPoly){
    plot(VoronoiPolyBBoxList[[i]], add=TRUE, col = NA)
    dfPts <- getPtsInPoly(VoronoiPolyList[[i]], gridSize)
    #points(df$xPts, pointXY$yPts, add = TRUE, col = "black")
    points(dfPts$inX, dfPts$inY, add = TRUE, col = "black",cex=0.2)
  }
}
########################################################################################################





########################################################################################################
# Initial soln stuff
# Sample hexagonal points (for circles centers) in the polygon bbox and 
# move the ones outside of the polygons to the nearest polygon edge
########################################################################################################
create_initial_soln <-  function(){
  
  #Somehow sampling large number of points is easier
  #So sample max number of points (circle centers) but pick required number of them
  cntMaxReqPts <- as.integer(max(unlist(dronesPerVBSList))) + 1
  
  for (k in 1:nVoronoiPoly) {
    
    #DEBUG k = 3 
    
    frameBBX <- NULL
    frameBBX <- st_bbox(VoronoiPolyList[[k]])
    framePoly <- Polygon(rbind(c(frameBBX$xmin,frameBBX$ymin),c(frameBBX$xmin,frameBBX$ymax),
                               c(frameBBX$xmax,frameBBX$ymax), c(frameBBX$xmax,frameBBX$ymin),
                               c(frameBBX$xmin,frameBBX$ymin)))
    # framePolys <- Polygons(list(framePoly),1)
    # myBBox <- SpatialPolygons(list(framePolys))
    
    #myBBox <- NULL
    #ERROR when poly has 2 or more component!!!!
    
   
    #initDroneXYRVec <- pack.circles(DronesPerPolyVec, Polygon(VoronoiPolyList[[k]][[1]]),st_area(VoronoiPolyList[[k]]), c((tempBBX$xmax - tempBBX$xmin + 1), (tempBBX$ymax - tempBBX$ymin + 1)), 500, 0)
    
    
    #Here we need to find area in square meter
    
    #areaPerDrn <- st_area(VoronoiPolyList[[k]]) / dronesPerVBSList[[k]]
    
    totAreaPolygon <- 0
    numberOfParts <- length(VoronoiPolyList[[k]])
    if (numberOfParts > 1) {
      for (p in 1:numberOfParts){
        totAreaPolygon <- totAreaPolygon + geosphere::areaPolygon(VoronoiPolyList[[k]][[p]])
      }
    }else{
      totAreaPolygon <- geosphere::areaPolygon(VoronoiPolyList[[k]][[1]])
      
    }
    cat("Poly",k,"has area",totAreaPolygon,"square meters\n")
    areaPerDrn <- totAreaPolygon / dronesPerVBSList[[k]]
    rRequiredToCoverRegion <- ceiling(sqrt(areaPerDrn / pi)) #as meter
    
    
    # rRequiredToCoverRegion is calculated and can be outside of the given range
    # When that happens just set it to a valid value!!!
    
    #Too few drones
    if (rRequiredToCoverRegion > rmax) {
      rRequiredToCoverRegion <- rmax - 1
      cat("For poly",k,dronesPerVBSList[[k]],"drones is too few! rRequiredToCoverRegion:",rRequiredToCoverRegion,"> rmax:",rmax,"\n")
    }
    
    #Too many drones
    if (rRequiredToCoverRegion < rmin) {
      rRequiredToCoverRegion <- rmin + 1
      cat("For poly",k,dronesPerVBSList[[k]],"drones is too many! rRequiredToCoverRegion:",rRequiredToCoverRegion,"< rmin:",rmin,"\n")
    }
    
    
    ###DEBUG
    #You need to convert radius to geo coordinates
    #roughly divide it by 111.111 m/km
    #CreateCirclePolyMeter does it!!!
    #rRequiredToCoverRegion <- rRequiredToCoverRegion / 111111
    ###DEBUG
    
    
    #Note that it is easier to sample hexagonal points in rectangles
    
    #framePoly@coords <- as.numeric(as.character(framePoly@coords))
    
    rndPts <- spsample(framePoly, n=cntMaxReqPts, type="hexagonal", offset=c(0,0))
    
    # spsample par type can be:
    # "random" for completely spatial random; 
    # "regular" for regular (systematically aligned) sampling; 
    # "stratified" for stratified random (one single random location in each "cell"); 
    # "nonaligned" for nonaligned systematic sampling (nx random y coordinates, ny random x coordinates); 
    # "hexagonal" for sampling on a hexagonal lattice; 
    # "clustered" for clustered sampling; 
    # "Fibonacci" for Fibonacci sampling on the sphere (see references).
    
    
    cntRndPts <- as.numeric(length(as.numeric(rndPts@coords[,1])))
    
    #Sometimes you can not sample as many as cntMaxReqPts!!!
    #What to do:
    #Enlarge the bbox?
    #Pick some edge points and place initial drones?
    
    
    
    
    # #We need to repeat till we get enough points
    # #DEBUG cat("****1\n")
    # iterNo <- 1
    # while ((cntRndPts < cntMaxReqPts)){
    #   rndPts <- NULL
    #   #DEBUG cat(k,iterNo,cntMaxReqPts,"x****\n")
    #   if (is.null(rndPts <- spsample(framePoly, n=cntMaxReqPts, type="hexagonal", offset=c(0,0)))) cat("Bingo!\n")
    #   #DEBUG cat(k,iterNo,cntMaxReqPts,"y****\n")
    #   coordPts <- coordinates(rndPts)
    #   cntRndPts <- length(coordPts[,1])
    #   if (!is.numeric(coordPts)) cat("non numeric coords!\n")
    #   if (is.null(coordPts)) cat("Bingo!\n")
    #   #DEBUG cat(k,iterNo,"****cnt=",cntRndPts,"coords:",coordPts,"\n\n")
    #   #DEBUG print(summary(rndPts))
    #   iterNo <- iterNo + 1
    #   if ((iterNo == 2000) || (cntRndPts > cntMaxReqPts)) break
    # }
    # 
    # #DEBUG cat("****2 - coords:",rndPts@coords,"\n\n")
    # #Now move the points outside of the poly (but inside bbox) to the closest polygon edge point
    
    #Here we need to check points that are number of drones
    
    
    
    cat(dronesPerVBSList[[k]], "drones asked with r =", rRequiredToCoverRegion,"meter height =",(rRequiredToCoverRegion/tan((halfTheta*pi/180))),"meter\n")
    cat(cntMaxReqPts,"points asked. Points sampled:",cntRndPts,"\n")
    
    
    #Find the centroid of the polygon
    
    centroXY <- st_centroid(VoronoiPolyList[[k]])
    
    #Filter the points that are in the polygon
    ptsInVoronoiPoly <- rndPts[which(point.in.polygon(rndPts@coords[,1], rndPts@coords[,2], VoronoiPolyList[[k]][[1]][,1],VoronoiPolyList[[k]][[1]][,2]) != 0)]
    #Count them
    cntPtsInVoronoiPoly <- as.numeric(length(as.numeric(ptsInVoronoiPoly@coords[,1])))
    
    #The list for drones only for the kth polygon
    dronesInPoly <<- list()
    
    #Now add the points in the polygon to the initial soln
    #If more required place the rest of the required drones on the centroid
    
    
    
    
    
    if (cntPtsInVoronoiPoly < dronesPerVBSList[[k]]) {
      #we sampled less pts in the poly
      #take all the cntPtsInVoronoiPoly pts
      #and add cntPtsToCentroid pts to the center
      
      #You can sample rnd points in the poly and put the rest there!!!!
      #Otherwise at the center all the circles will overlap 100%
      #Diagonal thin ploygons are problem for this
      #Most drone centers fall onto center!!!!
      
      for (px in 1:cntPtsInVoronoiPoly){
        #Create the circle and add it to list
        
        #Create projections of the drones as circles on the polygons
        #The radius will be lost by the way!!!
        #rRequiredToCoverRegion should be given as meter!!!
        dronesInPoly[[px]] <- CreateCirclePolyMeter(ptsInVoronoiPoly@coords[px,1], ptsInVoronoiPoly@coords[px,2], rRequiredToCoverRegion)
        
        ##############################################################################################
        #DEBUG BEGIN
        #The only way is to find the nearest point to the centroid and calculate distance
        #basically get the first point on the circle and find the distance between centroid (center)
        ptsOnCircle <- st_point(c(dronesInPoly[[px]][[1]][1,1], dronesInPoly[[px]][[1]][1,2]))
        cntrCircle <- st_centroid(dronesInPoly[[px]])
        st_nearest_points(st_centroid(dronesInPoly[[px]]), dronesInPoly[[px]])
        d <- st_distance(ptsOnCircle,cntrCircle)
        cat("Poly",k, "circle",px,"radius given:", rRequiredToCoverRegion, "radius est:",d,"\n")
        #DEBUG END
        ##############################################################################################
        
      }
      
      #This many to center: cntPtsToCentroid <- dronesPerVBSList[[k]] - cntPtsInVoronoiPoly
      #Fill the rest of the drone circles
      #Find the centroid of the first part of the Voronoi poly if its multipart
      
      lenList <- length(VoronoiPolyList[[k]]) 
      if (lenList > 1) {
        cntrPoly <- st_centroid(VoronoiPolyList[[k]][[1]])
        thePoly <- SpatialPolygons(list(Polygons(list(Polygon(VoronoiPolyList[[k]][[1]][[1]])), "x")))
      }else{
        cntrPoly <- st_centroid(VoronoiPolyList[[k]])
        thePoly <- SpatialPolygons(list(Polygons(list(Polygon(VoronoiPolyList[[k]][[1]])), "x")))
      }
      
      for (px in ((cntPtsInVoronoiPoly+1):dronesPerVBSList[[k]])){
        #Create the circle and add it to list
        
        #Pick randome point and place the extra drones there
        
       
          rndPtsForRest <- spsample(thePoly, n=1, type="random", offset=c(0,0))
       
       
        
        # spsample par type can be:
        # "random" for completely spatial random; 
        # "regular" for regular (systematically aligned) sampling; 
        # "stratified" for stratified random (one single random location in each "cell"); 
        # "nonaligned" for nonaligned systematic sampling (nx random y coordinates, ny random x coordinates); 
        # "hexagonal" for sampling on a hexagonal lattice; 
        # "clustered" for clustered sampling; 
        # "Fibonacci" for Fibonacci sampling on the sphere (see references).
        
        
    
        
        #Create projections of the drones as circles on the polygons
        #The radius will be lost by the way!!!
        
        dronesInPoly[[px]] <- CreateCirclePolyMeter(rndPtsForRest@coords[,1], rndPtsForRest@coords[,2], rRequiredToCoverRegion)
        #To put extra non aligned hexagonal points at the center of poly uncomment the next
        #dronesInPoly[[px]] <- CreateCirclePolyMeter(cntrPoly[1], cntrPoly[2], rRequiredToCoverRegion)
        
        
        ##############################################################################################
        #DEBUG BEGIN
        #The only way is to find the nearest point to the centroid and calculate distance
        #basically get the first point on the circle and find the distance between centroid (center)
        ptsOnCircle <- st_point(c(dronesInPoly[[px]][[1]][1,1], dronesInPoly[[px]][[1]][1,2]))
        cntrCircle <- st_centroid(dronesInPoly[[px]])
        st_nearest_points(st_centroid(dronesInPoly[[px]]), dronesInPoly[[px]])
        d <- st_distance(ptsOnCircle,cntrCircle)
        cat("Poly",k, "circle",px,"radius given:", rRequiredToCoverRegion, "radius est:",d,"\n")
        #DEBUG END
        ##############################################################################################
      }
      
    }else if(cntPtsInVoronoiPoly == dronesPerVBSList[[k]]){
      #we sampled enough pts in the poly
      #take all the cntPtsInVoronoiPoly pts
      
      for (px in 1:cntPtsInVoronoiPoly){
        #Create the circle and add it to list
        
        #Create projections of the drones as circles on the polygons
        #The radius will be lost by the way!!!
        #rRequiredToCoverRegion should be given as meter!!!
        dronesInPoly[[px]] <- CreateCirclePolyMeter(ptsInVoronoiPoly@coords[px,1], ptsInVoronoiPoly@coords[px,2], rRequiredToCoverRegion)
        
        ##############################################################################################
        #DEBUG BEGIN
        #The only way is to find the nearest point to the centroid and calculate distance
        #basically get the first point on the circle and find the distance between centroid (center)
        ptsOnCircle <- st_point(c(dronesInPoly[[px]][[1]][1,1], dronesInPoly[[px]][[1]][1,2]))
        cntrCircle <- st_centroid(dronesInPoly[[px]])
        st_nearest_points(st_centroid(dronesInPoly[[px]]), dronesInPoly[[px]])
        d <- st_distance(ptsOnCircle,cntrCircle)
        cat("Poly",k, "circle",px,"radius given:", rRequiredToCoverRegion, "radius est:",d,"\n")
        #DEBUG END
        ##############################################################################################
        
      }
      
      
    }else{
      #we sampled more pts in the poly
      #take the first dronesPerVBSList[[k]] pts
      
      for (px in 1:dronesPerVBSList[[k]] ){
        #Create the circle and add it to list
        
        #Create projections of the drones as circles on the polygons
        #The radius will be lost by the way!!!
        #rRequiredToCoverRegion should be given as meter!!!
        dronesInPoly[[px]] <- CreateCirclePolyMeter(ptsInVoronoiPoly@coords[px,1], ptsInVoronoiPoly@coords[px,2], rRequiredToCoverRegion)
        
        ##############################################################################################
        #DEBUG BEGIN
        #The only way is to find the nearest point to the centroid and calculate distance
        #basically get the first point on the circle and find the distance between centroid (center)
        ptsOnCircle <- st_point(c(dronesInPoly[[px]][[1]][1,1], dronesInPoly[[px]][[1]][1,2]))
        cntrCircle <- st_centroid(dronesInPoly[[px]])
        st_nearest_points(st_centroid(dronesInPoly[[px]]), dronesInPoly[[px]])
        d <- st_distance(ptsOnCircle,cntrCircle)
        cat("Poly",k, "circle",px,"radius given:", rRequiredToCoverRegion, "radius est:",d,"\n")
        #DEBUG END
        ##############################################################################################
        
      }
      
    }
    
    # 
    # ##############################################################################################
    # #The old code BEGIN
    # ##############################################################################################
    # 
    # 
    # if (cntRndPts < dronesPerVBSList[[k]]) {
    #   #We sampled less points
    #   #First handle the sampled pts then place the rest of the drones to the centroid of the polygon!!!
    #   for (px in 1:cntRndPts){
    #     lenList <- length(VoronoiPolyList[[k]]) 
    #     inPoly <- 0
    #     if (lenList > 1) {
    #       #If the polygon has multiple pieces, being in one piece is enough for the "inness" of the point
    #       for (z in 1:lenList){
    #         inPoly <- inPoly + point.in.polygon(rndPts@coords[px,1], rndPts@coords[px,2], VoronoiPolyList[[k]][[z]][[1]][,1],VoronoiPolyList[[k]][[z]][[1]][,2])
    #       }
    #     }else{
    #       
    #       inPoly <- inPoly + point.in.polygon(rndPts@coords[px,1], rndPts@coords[px,2], VoronoiPolyList[[k]][[1]][,1],VoronoiPolyList[[k]][[1]][,2])
    #       cat("---k",k,"px",px,"inpoly=",inPoly,"\n")
    #     }
    #     
    #     #if the point is outside move it to the edge or to the center. 
    #     #You can also move it to the mid point between edge point and the centroid.
    #     if (inPoly == 0){
    #       nearPt <- st_nearest_points(st_point(rndPts@coords[px,]), VoronoiPolyList[[k]])
    #       #To put it on the edge:
    #       #rndPts@coords[px,] <- nearPt[[1]][2,]
    #       #To put it on the midpoint between edge and the centroid:
    #       midX  <- (centroXY[[1]] + nearPt[[1]][2,1])/2
    #       midY  <- (centroXY[[2]] + nearPt[[1]][2,2])/2
    #       rndPts@coords[px,] <- centroXY
    #       #DEBUG
    #       inPoly <- inPoly + point.in.polygon(rndPts@coords[px,1], rndPts@coords[px,2], VoronoiPolyList[[k]][[1]][,1],VoronoiPolyList[[k]][[1]][,2])
    #       cat("DEBUG After Moving ---k",k,"px",px,"inpoly=",inPoly,"\n")
    #     }
    #     
    #     #Create the circle and add it to list
    #     
    #     #Create projections of the drones as circles on the polygons
    #     #The radius will be lost by the way!!!
    #     dronesInPoly[[px]] <- CreateCirclePolyMeter(rndPts@coords[px,1], rndPts@coords[px,2], rRequiredToCoverRegion)
    #     
    #     #DEBUG points(rndPts@coords[i,1], rndPts@coords[i,2], add = TRUE, col = "black",cex=0.3)
    #     
    #     #The only way is to find the nearest point to the centroid and calculate distance
    #     #basically get the first point on the circle and find the distance between centroid (center)
    #     ptsOnCircle <- st_point(c(dronesInPoly[[px]][[1]][1,1], dronesInPoly[[px]][[1]][1,2]))
    #     cntrCircle <- st_centroid(dronesInPoly[[px]])
    #     st_nearest_points(st_centroid(dronesInPoly[[px]]), dronesInPoly[[px]])
    #     d <- st_distance(ptsOnCircle,cntrCircle)
    #     cat("Poly",k, "circle",px,"radius given:", rRequiredToCoverRegion, "radius est:",d,"\n")
    #   }
    #   
    #   #Find the centroid of the first part of the Voronoi poly if its multipart
    #   lenList <- length(VoronoiPolyList[[k]]) 
    #   if (lenList > 1) {
    #     cntrPoly <- st_centroid(VoronoiPolyList[[k]][[1]])
    #   }else{
    #     cntrPoly <- st_centroid(VoronoiPolyList[[k]])
    #   }
    #   
    #   #Fill the rest of the drone circles
    #   
    #   for (px in ((cntRndPts+1):dronesPerVBSList[[k]])){
    #     #Create the circle and add it to list
    #     
    #     #Create projections of the drones as circles on the polygons
    #     #The radius will be lost by the way!!!
    #     dronesInPoly[[px]] <- CreateCirclePolyMeter(cntrPoly[1], cntrPoly[2], rRequiredToCoverRegion)
    #     
    #     #DEBUG points(rndPts@coords[i,1], rndPts@coords[i,2], add = TRUE, col = "black",cex=0.3)
    #     
    #     #The only way is to find the nearest point to the centroid and calculate distance
    #     #basically get the first point on the circle and find the distance between centroid (center)
    #     ptsOnCircle <- st_point(c(dronesInPoly[[px]][[1]][1,1], dronesInPoly[[px]][[1]][1,2]))
    #     cntrCircle <- st_centroid(dronesInPoly[[px]])
    #     st_nearest_points(st_centroid(dronesInPoly[[px]]), dronesInPoly[[px]])
    #     d <- st_distance(ptsOnCircle,cntrCircle)
    #     cat("Poly",k, "circle",px,"radius given:", rRequiredToCoverRegion, "radius est:",d,"\n")
    #   }
    #   
    #   
    # }else if (cntRndPts == dronesPerVBSList[[k]]){
    #   #We sampled enough points
    #   #Just place them
    #   for (px in 1:cntRndPts){
    #     lenList <- length(VoronoiPolyList[[k]]) 
    #     inPoly <- 0
    #     if (lenList > 1) {
    #       #If the polygon has multiple pieces, being in one piece is enough for the "inness" of the point
    #       for (z in 1:lenList){
    #         inPoly <- inPoly + point.in.polygon(rndPts@coords[px,1], rndPts@coords[px,2], VoronoiPolyList[[k]][[z]][[1]][,1],VoronoiPolyList[[k]][[z]][[1]][,2])
    #       }
    #     }else{
    #      
    #       inPoly <- inPoly + point.in.polygon(rndPts@coords[px,1], rndPts@coords[px,2], VoronoiPolyList[[k]][[1]][,1],VoronoiPolyList[[k]][[1]][,2])
    #       cat("---k",k,"px",px,"inpoly=",inPoly,"\n")
    #     }
    #     
    #     #if the point is outside move it to the edge
    #     if (inPoly == 0){
    #       nearPt <- st_nearest_points(st_point(rndPts@coords[px,]), VoronoiPolyList[[k]])
    #       #To put it on the edge:
    #       #rndPts@coords[px,] <- nearPt[[1]][2,]
    #       #To put it on the midpoint between edge and the centroid:
    #       midX  <- (centroXY[[1]] + nearPt[[1]][2,1])/2
    #       midY  <- (centroXY[[2]] + nearPt[[1]][2,2])/2
    #       rndPts@coords[px,] <- centroXY
    #       #DEBUG
    #       inPoly <- inPoly + point.in.polygon(rndPts@coords[px,1], rndPts@coords[px,2], VoronoiPolyList[[k]][[1]][,1],VoronoiPolyList[[k]][[1]][,2])
    #       cat("DEBUG After Moving ---k",k,"px",px,"inpoly=",inPoly,"\n")
    #     }
    #     
    #     #Create the circle and add it to list
    #     
    #     #Create projections of the drones as circles on the polygons
    #     #The radius will be lost by the way!!!
    #     dronesInPoly[[px]] <- CreateCirclePolyMeter(rndPts@coords[px,1], rndPts@coords[px,2], rRequiredToCoverRegion)
    #     
    #     #DEBUG points(rndPts@coords[i,1], rndPts@coords[i,2], add = TRUE, col = "black",cex=0.3)
    #     
    #     #The only way is to find the nearest point to the centroid and calculate distance
    #     #basically get the first point on the circle and find the distance between centroid (center)
    #     ptsOnCircle <- st_point(c(dronesInPoly[[px]][[1]][1,1], dronesInPoly[[px]][[1]][1,2]))
    #     cntrCircle <- st_centroid(dronesInPoly[[px]])
    #     st_nearest_points(st_centroid(dronesInPoly[[px]]), dronesInPoly[[px]])
    #     d <- st_distance(ptsOnCircle,cntrCircle)
    #     cat("Poly",k, "circle",px,"radius given:", rRequiredToCoverRegion, "radius est:",d,"\n")
    #   }
    #   
    # }else {
    #   #We sampled more points
    #   #Place first dronesPerVBSList[[k]] pts
    #   
    #   for (px in 1:dronesPerVBSList[[k]]){
    #     lenList <- length(VoronoiPolyList[[k]]) 
    #     inPoly <- 0
    #     if (lenList > 1) {
    #       #If the polygon has multiple pieces, being in one piece is enough for the "inness" of the point
    #       for (z in 1:lenList){
    #         inPoly <- inPoly + point.in.polygon(rndPts@coords[px,1], rndPts@coords[px,2], VoronoiPolyList[[k]][[z]][[1]][,1],VoronoiPolyList[[k]][[z]][[1]][,2])
    #       }
    #     }else{
    #    
    #       inPoly <- inPoly + point.in.polygon(rndPts@coords[px,1], rndPts@coords[px,2], VoronoiPolyList[[k]][[1]][,1],VoronoiPolyList[[k]][[1]][,2])
    #       cat("---k",k,"px",px,"inpoly=",inPoly,"\n")
    #     }
    #     
    #     #if the point is outside move it to the edge
    #     if (inPoly == 0){
    #       nearPt <- st_nearest_points(st_point(rndPts@coords[px,]), VoronoiPolyList[[k]])
    #       
    #       #To put it on the edge:
    #       #rndPts@coords[px,] <- nearPt[[1]][2,]
    #       #To put it on the midpoint between edge and the centroid:
    #       midX  <- (centroXY[[1]] + nearPt[[1]][2,1])/2
    #       midY  <- (centroXY[[2]] + nearPt[[1]][2,2])/2
    #       rndPts@coords[px,] <- centroXY
    #       #DEBUG
    #       inPoly <- inPoly + point.in.polygon(rndPts@coords[px,1], rndPts@coords[px,2], VoronoiPolyList[[k]][[1]][,1],VoronoiPolyList[[k]][[1]][,2])
    #       cat("DEBUG After Moving ---k",k,"px",px,"inpoly=",inPoly,"\n")
    #     }
    #     
    #     #Create the circle and add it to list
    #     
    #     #Create projections of the drones as circles on the polygons
    #     #The radius will be lost by the way!!!
    #     dronesInPoly[[px]] <- CreateCirclePolyMeter(rndPts@coords[px,1], rndPts@coords[px,2], rRequiredToCoverRegion)
    #     
    #     #DEBUG points(rndPts@coords[i,1], rndPts@coords[i,2], add = TRUE, col = "black",cex=0.3)
    #     
    #     #The only way is to find the nearest point to the centroid and calculate distance
    #     #basically get the first point on the circle and find the distance between centroid (center)
    #     ptsOnCircle <- st_point(c(dronesInPoly[[px]][[1]][1,1], dronesInPoly[[px]][[1]][1,2]))
    #     cntrCircle <- st_centroid(dronesInPoly[[px]])
    #     st_nearest_points(st_centroid(dronesInPoly[[px]]), dronesInPoly[[px]])
    #     d <- st_distance(ptsOnCircle,cntrCircle)
    #     cat("Poly",k, "circle",px,"radius given:", rRequiredToCoverRegion, "radius est:",d,"\n")
    #     
    #   }
    #   
    # }
    # ##############################################################################################
    # #The old code END
    # ##############################################################################################
    # 
    
    
    
    #size of the circle poly object in Kb
    #DEBUG print(object.size(dronesInPolyList), units="Kb")
    #DEBUG cat("For poly",k,"drones:",length(dronesInPolyList),"\n\n")
    dronesInPolyList[[k]] <<- dronesInPoly
    #size of the circle poly object in Kb
    cat("Size of circle polygons in poly",k,"is ")
    print(object.size(dronesInPolyList[[k]]), units="Kb")
    cat("\n")
    #DEBUG cat("**For poly",k,"drones:",length(dronesInPolyList[[k]]),"\n")
    
  } #For loop on k
}

########################################################################################################





########################################################################################################
#Put circles on the polygons and print some statistics
########################################################################################################
draw_initial_soln <-  function(){
  for (k in 1:nVoronoiPoly) {
    
    #draw drone circles
    dronesInPolyNo <- length(dronesInPolyList[[k]])
    
    cat("*******DronesPerPolyVec [",DronesPerPolyVec,"] --- Poly", k,"has",dronesInPolyNo,"circles \n")
    imtx <- dronesInPolyList2DroneXYRVec(dronesInPolyList[[k]], DronesPerPolyVec, k)
    cat("\n******** STATISTICS for poly",k,"********\n")
    my_show_DroneXYRVec_stat(imtx)
    my_show_dronesInPolyList_stat (VoronoiPolyList[[k]], dronesInPolyList[[k]], DronesPerPolyVec, k)
    #xsolVector <- dronesInPolyList2DroneXYRVec(VoronoiPolyList[[k]], DronesPerPolyVec,k)
    polyScore <- fit_with_weights(imtx, xwCoverage, xwOverflow, xwOverlap, xwDist, xwScore, k)
    cat ("Poly score is:", polyScore,"\n")
    cat("******************************\n\n")
    #DEBUG cat("Drones in poly",k,":",dronesInPolyNo,"\n")
    #Draw the circles on each polygon
    for(i in 1:dronesInPolyNo){
      plot(dronesInPolyList[[k]][[i]], add=TRUE)
      st_area(dronesInPolyList[[k]][[i]])
    }
    
    
  }
  # 
  # #Add the bounding boxes of the Voronoi polygons
  # for (i in 1:nVoronoiPoly){
  #   plot(VoronoiPolyBBoxList[[i]], add=TRUE, col = NA)
  # }
}
########################################################################################################




########################################################################################################
#Given lon lat in a 2d list creates spatial polygons data frame
########################################################################################################
VtxMtx2SpatPolyDF <- function(xVtxMtx){
  
  #make polygon from kml
  p1 <- sp::Polygon(xVtxMtx)
  #make Polygon class
  p2 <- sp::Polygons(list(p1), ID = "drivetime")
  #make spatial polygons class
  p3 <- sp::SpatialPolygons(list(p2),proj4string=sp::CRS("+init=epsg:4326"))
  # Create a dataframe and display default rownames
  p3.df <- data.frame(ID=1:length(p3)) 
  rownames(p3.df)
  # Extract polygon ID's
  pid <- sapply(slot(p3, "polygons"), function(x) slot(x, "ID")) 
  # Create dataframe with correct rownames
  p3.df <- data.frame( ID=1:length(p3), row.names = pid)    
  #Create the SpatialPolygonsDataFrame
  p4 <- SpatialPolygonsDataFrame(p3, data=p3.df)
  
  return(p4)
}
########################################################################################################







########################################################################################################


