


########################################################################################################
#Functions
########################################################################################################





###########################################################################################
#Packs the circles hexagonally on o the region
#as an initial solution for evolutinary algorithms
#https://lastresortsoftware.blogspot.com/2010/07/circle-packing-with-r.html
###########################################################################################
pack.circles <- function(ndrn, size=c(100, 100), max.iter=1000, overlap=0 ) {
  #
  # Simple circle packing algorithm based on inverse size weighted repulsion
  #
  # config   - matrix with two cols: radius, N
  # size     - width and height of bounding rectangle
  # max.iter - maximum number of iterations to try
  # overlap  - allowable overlap expressed as proportion of joint radii
  
  # ============================================================================
  #  Global constants
  # ============================================================================
  # round-off tolerance
  
  
  TOL <- 0.0001
  
  xSize <- size[1]
  ySize <- size[2]
  
  maxArea <- xSize * ySize
  
  areaPerDrn <- maxArea / ndrn
  
  rmed <- ceiling(sqrt(areaPerDrn / pi))
  
  # rmed is calculated and can be outside of the given range
  # When that happens just set it to a valid value!!!
  if (rmed > rmax) rmed <- rmax - 1
  if (rmed < rmin) rmed <- rmin + 1
  
  ndrones <- ndrn
  
  
  
  config <- matrix(c(rmed,ndrones),nrow = 1, ncol = 2,byrow = TRUE)
  
  
  
  initDroneMtx<-matrix(rep(c(0,0,rmed),ndrones), ndrones*3, 1, byrow = TRUE)
   
#   
#   if (ndrn == 1) {
#     cat("Single Drone!!!\n")
#     initDroneMtx <- matrix(rep(c(0,0,rmed),1), 3, 1, byrow = TRUE)
#     initDroneMtx[1] <- floor(xSize/2)
#     initDroneMtx[2] <-  floor(ySize/2)
#     initDroneMtx[3] <-  rmed 
#     return (initDroneMtx)
#   }
#   
  
  
  
  # convert overlap to proportion of radius
  if (overlap < 0 | overlap >= 1) {
    stop("overlap should be in the range [0, 1)")
  }
  PRADIUS <- 1 - overlap
  
  NCIRCLES <- sum(config[,2])
  
  # ============================================================================
  #  Helper function - Draw a circle
  # ============================================================================
  # draw.circle <- function(x, y, r, col) { 
  #   lines( cos(seq(0, 2*pi, pi/180)) * r + x, sin(seq(0, 2*pi, pi/180)) * r + y , col=col )
  # }
  
  
  # ============================================================================
  #  Helper function - Move two circles apart. The proportion of the required
  #  distance moved by each circle is proportional to the size of the other 
  #  circle. For example, If a c1 with radius r1 overlaps c2 with radius r2,
  #  and the movement distance required to separate them is ds, then c1 will
  #  move ds * r2 / (r1 + r2) while c2 will move ds * r1 / (r1 + r2). Thus,
  #  when a big circle overlaps a little one, the little one moves a lot while
  #  the big one moves a little.
  # ============================================================================
  repel <- function(xyr, c0, c1) {
    dx <- xyr[c1, 1] - xyr[c0, 1]
    dy <- xyr[c1, 2] - xyr[c0, 2]
    d <- sqrt(dx*dx + dy*dy)
    r <- xyr[c1, 3] + xyr[c0, 3]
    w0 <- xyr[c1, 3] / r
    w1 <- xyr[c0, 3] / r
    
    if (d < r - TOL) {
      p <- (r - d) / d
      xyr[c1, 1] <<- toroid(xyr[c1, 1] + p*dx*w1, 1)
      xyr[c1, 2] <<- toroid(xyr[c1, 2] + p*dy*w1, 2)
      xyr[c0, 1] <<- toroid(xyr[c0, 1] - p*dx*w0, 1)
      xyr[c0, 2] <<- toroid(xyr[c0, 2] - p*dy*w0, 2)
      
      return(TRUE)
    }
    
    return(FALSE)
  }
  
  
  # ============================================================================
  #  Helper function - Adjust a coordinate such that if it is distance d beyond
  #  an edge (ie. outside the area) it is moved to be distance d inside the 
  #  opposite edge. This has the effect of treating the area as a toroid.
  # ============================================================================
  toroid <- function(coord, axis) {
    tcoord <- coord
    
    if (coord < 0) {
      tcoord <- coord + size[axis]
    } else if (coord >= size[axis]) {
      tcoord <- coord - size[axis]
    }
    
    tcoord
  }
  
  
  # ============================================================================
  #  Main program
  # ============================================================================
  
  # ------------------------------------------
  # create a random initial layout
  # ------------------------------------------
  xyr <- matrix(0, NCIRCLES, 3)
  
  pos0 <- 1
  for (i in 1:nrow(config)) {
    pos1 <- pos0 + config[i,2] - 1
    xyr[pos0:pos1, 1] <- runif(config[i, 2], 0, size[1])
    xyr[pos0:pos1, 2] <- runif(config[i, 2], 0, size[2])
    xyr[pos0:pos1, 3] <- config[i, 1] * PRADIUS
    pos0 <- pos1 + 1
  }
  
  # ------------------------------------------
  # iteratively adjust the layout
  # ------------------------------------------
  for (iter in 1:max.iter) {
    moved <- FALSE
    for (i in 1:(NCIRCLES-1)) {
      for (j in (i+1):NCIRCLES) {
        if (repel(xyr, i, j)) {
          moved <- TRUE
        }
      }
    }
    if (!moved) break
  }
  
  cat("Packed circles in",paste(iter, "iterations\n"));
  
  # ------------------------------------------
  # draw the layout
  # ------------------------------------------
  #plot(0, type="n", xlab="x", xlim=c(0,size[1]), ylab="y", ylim=c(0, size[2]))
  
  xyr[, 3] <- xyr[, 3] / PRADIUS
  
  k <- 1
  
  for (i in 1:nrow(xyr)) {
    
    initDroneMtx[k] <- floor(xyr[i, 1])
    initDroneMtx[k+1] <-  floor(xyr[i, 2])
    initDroneMtx[k+2] <-  floor(xyr[i, 3])
    k <- k + 3
    #draw.circle(xyr[i, 1], xyr[i, 2], xyr[i, 3], "gray")
  }
  
  # ------------------------------------------
  # return the layout
  # ------------------------------------------
  colnames(xyr) <- c("x", "y", "radius")
  invisible(xyr)
  
  initDroneMtx
}
###########################################################################################





###########################################################################################
#Show the statistics of the solution matrix (x,y,r for each drone)
###########################################################################################
my_show_droneMtx_stat <-  function(dMtx){
# init loop vars
totCircleArea <- 0
lx <- length(dMtx)

xtemplateOverlap <- matrix(0,nrow=w, ncol=h)
xtemplateOverflow <- matrix(0,nrow=w, ncol=h)
xtemplateForCircle <- matrix(0,nrow=w, ncol=h)
xtemplateOverlapSum <- matrix(0,nrow=w, ncol=h)

xcoveredArea <- 0
xprecoveredArea <- 0
xtotOverflow <- 0
xtotOverlap <- 0
xtotBigOverlap <- 0
xcntBigOverlap1 <- 0


#Region as matrix
regionx <- matrix(0,nrow=w, ncol=h)

#Auxilary region matrices
#NOTE THE COORDINATE INCOMPATIBLITY BETWEEN MATRICES AND THE EBIMAGE FUNCTIONS!!!
#Just when you define matrix for row use size in x-horizontal-width and col size in y-vertical-height!!!!
#Use row as x and col as y
tempRegion1x <- matrix(0,nrow=w, ncol=h)
tempRegion2x <- matrix(0,nrow=w, ncol=h)
showRegionx <- matrix(0,nrow=w, ncol=h)




sumCircleArea <- 0 #Do not care for overflow or overlap




for (k in seq(1,lx,3)) {
  
  # Get values from the final solution fo the GA
  x1 <- ceiling(dMtx[k])
  y1 <- ceiling(dMtx[k+1])
  r1 <- ceiling(dMtx[k+2])

  
  # Same code from the fitness function
  #####################################################
  # Draw the circle on empty region on the original position to see how many pixels
  # overflowed
  xnonOverflowedRegion <- EBImage::drawCircle(xtemplateOverflow, x1, y1, r1, col=1, fill = TRUE)
  
  # Draw the circle in the middle, (assuming that r will be always less than the half width and half height)
  # then count pixels to find out area in pixels
  xcircleAreaRegion <- EBImage::drawCircle(xtemplateForCircle, ceiling(w/2), ceiling(h/2), r1, col=1, fill = TRUE)
  
  xcirclepixelArea <- length(xcircleAreaRegion[xcircleAreaRegion==1])
  sumCircleArea <- sumCircleArea + xcirclepixelArea
  xnonOverflowedArea <- length(xnonOverflowedRegion[xnonOverflowedRegion==1])
  xcircleOverflow <- xcirclepixelArea - xnonOverflowedArea
  
  
  xtotOverflow <- xtotOverflow + xcircleOverflow
  
  xtemplateOverlap <- EBImage::drawCircle(xtemplateOverlap, x1, y1, r1, col=1, fill = TRUE)
  xcoveredArea <- length(xtemplateOverlap[xtemplateOverlap==1])
  
  #Sum the template for overlap over and over again 
  #In the final template pixels with value more than 1 will give you the total overlap
  xtemplateOverlapSum <- xtemplateOverlapSum + xnonOverflowedRegion
  
  
  # To calculate overlap that comes from that circle:
  # First findout the theoretical contribution of the circle = How many pixels would be added
  # Count actually added pixels and
  # then subtract the overflow if the center is close to edges of the region
  # HOW ABOUT MORE THAN 2 CIRCLES ON TOP OF EACH OTHER?
  # This line is for one circle only!!!!!
  
  xcircleOverlap <- (xprecoveredArea + xcirclepixelArea) - xcoveredArea - xcircleOverflow
  
  
  
  
  
  # If the circle is overlapped more than a threshold, give high penalty!!!
  # But when the bigger circle on top of  small circle the overlap will be less than its overlapThreshold
  # On the other hand poor small circle will be covered totally more than threshold probably
  # So order is important then!
  # Sorting helps
  # or two pass: first to last and then last to first passes, like scanning!
  ###################################################################################
  # DOES THAT WORK FOR MORE THAN 2 CIRCLE ORDERINGS
  # IMAGINE A CASE WHERE OTHER CIRCLES EATS AWAY LITTLE BIT OF A CERTAIN CIRCLE
  # AMOUNTING MORE THAN THRESHOLD OVERLAP
  # WILL TWO PASS ALGO FIND THIS?
  ###################################################################################
  if (xcirclepixelArea <= (100 / overlapThreshold * xcircleOverlap) ) {
    #DEBUG
    cat("FW Pass - Circle:",(k+2)/3,"x=",x1,"y=",y1,"r",r1,"- more than", overlapThreshold,"percent overlap!",xcircleOverlap, xcirclepixelArea,
        "=",round((100 *xcircleOverlap / xcirclepixelArea),1),"\n",file=textFileName,append=TRUE)
    
    xtotBigOverlap <- xtotBigOverlap + xcircleOverlap
    xcntBigOverlap1 <- xcntBigOverlap1 + 1
  }
  
  
  
  xprecoveredArea <- xcoveredArea
  
  
  #This line overcounts!!!!
  #xtotOverlap <- xtotOverlap + xcircleOverlap
  
  xtotOverlap <-  length(xtemplateOverlapSum[xtemplateOverlapSum>1])
  
  
  # In ga (parallel) this trace output is not possible!!
  # #BEGIN DEBUG
  #cat(round,"Circle",(k-1)/3+1, "at",x1,y1,"r=",r1, "pixel area",circlepixelArea, "areacalc",circleArea,"has overflow area of",circleOverflow, "and overlapped",circleOverlap,"covered so far",coveredArea,"\n")
  #cat("Circle",(k-1)/3+1, "at",x1,y1,"r=",r1, "pixel area",circlepixelArea, "areacalc",circleArea,"has overflow area of",circleOverflow, "and overlapped",circleOverlap,"covered so far",coveredArea,"\n")
  # #END DEBUG
  ###################################################
  # # Drawing stuff
  

   regionx <- EBImage::drawCircle(regionx, x1, y1, r1, col=1, fill = TRUE)
   showRegionx <- EBImage::drawCircle(showRegionx, x1, y1, r1, col=1, fill = FALSE)
   tempRegion2x <- EBImage::drawCircle(tempRegion1x, x1, y1, r1, col=1, fill = TRUE)
   totCircleArea <- totCircleArea + length(tempRegion2x[tempRegion2x==1])
   #EBImage::display(showRegion, method = "raster", all = TRUE)
  
}
###########################################################################################
#Misc statistics calculated
totCoveredArea <- length(regionx[regionx==1])

#This line overcounts!!!!
#totOverlapArea <- totCircleArea - totCoveredArea
totOverlapArea <- xtotOverlap

CoveredPercentage <- 100 * totCoveredArea / areaEmptyRegion
totEmptyArea <- areaEmptyRegion - totCoveredArea
EmptyPercentage <- 100 * totEmptyArea / areaEmptyRegion



#Calculate mean radius/height of drones
xavgr <- mean(dMtx[c(FALSE, FALSE, TRUE)])
xavgh <- ceiling(xavgr/tan((halfTheta*pi/180)))

#Calculate mean distance from the Base Station for drones
# Z for base station is 0!
xavgDist  <- mapply(function(x){mean(sqrt((x[c(TRUE, FALSE, FALSE)]-bsx)^2 +  
                                          (x[c(FALSE, TRUE, FALSE)]-bsy)^2 + 
                                          ((x[c(FALSE, FALSE, TRUE)]/tan(halfTheta*pi/180)) ^2)))}, 
                     as.data.frame(dMtx))

xmedDist  <- mapply(function(x){median(sqrt((x[c(TRUE, FALSE, FALSE)]-bsx)^2 +  
                                            (x[c(FALSE, TRUE, FALSE)]-bsy)^2 + 
                                            ((x[c(FALSE, FALSE, TRUE)]/tan(halfTheta*pi/180)) ^2)))}, 
                    as.data.frame(dMtx))

xsumDist  <- mapply(function(x){sum(sqrt((x[c(TRUE, FALSE, FALSE)]-bsx)^2 +  
                                              (x[c(FALSE, TRUE, FALSE)]-bsy)^2 + 
                                              ((x[c(FALSE, FALSE, TRUE)]/tan(halfTheta*pi/180)) ^2)))}, 
                    as.data.frame(dMtx))

#scorePixel <- func_with_PixelCount(dMtx)
scorePixelPercent <-fit_with_weights(dMtx, wCoverage=xwCoverage, wOverflow=xwOverflow, wOverlap=xwOverlap, wDist=xwDist, wScore=xwScore)
  
cat("RegionArea:(", w,",",h,"):",areaEmptyRegion,"m^2",
    "\nCoveredArea:",totCoveredArea,"m^2 (",round(CoveredPercentage,2),"% )",
    "\nxCoveredArea:",xcoveredArea,"m^2 (",round(xcoveredArea/areaEmptyRegion*100,2),"% )",
    "\nTotCircleArea(with overlap):",totCircleArea,"m^2 (",round(totCircleArea/areaEmptyRegion*100,2),"% )",
    "\nMean coverage of drones(overlap and overflow not considered):",sumCircleArea/ndrones,"m^2",
    "\nOverlap:",totOverlapArea,"m^2 (",round((totOverlapArea*100/totCoveredArea),2),"% of covered,"
                                       ,round((totOverlapArea*100/areaEmptyRegion),2),"% of region )",
    "\nMean Overlap of drones:",totOverlapArea/ndrones,"m^2",
    "\nOverflow:",xtotOverflow,"m^2 (",round((xtotOverflow*100/totCoveredArea),2),"% of covered,"
                                      ,round((xtotOverflow*100/areaEmptyRegion),2),"% of region )",
    "\nMean Overflow of drones:",xtotOverflow/ndrones,"m^2",
    "\nEmpty:",totEmptyArea ,"m^2 (",round(EmptyPercentage,1),"% of region )",
    "\nMean Radius of drones:",xavgr,"m",
    "\nMean Height of drones:",xavgh,"m",
    "\nTotal Distance of drones from BS(", bsx,",",bsy,", 0 ):",xsumDist,"m",
    "\nMax Distance a drone from BS(", bsx,",",bsy,", 0 ):",maxDist,"m",
    "\nMean Distance of drones from BS(", bsx,",",bsy,", 0 ):",xavgDist,"m",
    "\nMedian Distance of drones from BS(", bsx,",",bsy,", 0 ):",xmedDist,"m",
    #"\nPixel Score:", scorePixel,
    "\nPercent Score:", scorePixelPercent,
    "\n",file=textFileName,append=TRUE
    )
cat("**********************************************************************************\n",file=textFileName,append=TRUE) 
  

#DEBUG
# cat("xRegionArea:", areaEmptyRegion,
#     "- xCoveredArea:",xcoveredArea,"( %",round(CoveredPercentage,1),
#     ") - xOverlap:",xtotOverlap,"( %",round((xtotOverlap*100/xcoveredArea),1),
#     ") - xOverflow:",xtotOverflow,"( %",round((xtotOverflow*100/xcoveredArea),1),
#     ") - xEmpty:",totEmptyArea ,"( %",round(EmptyPercentage,1),
#     "\n\n")
coveredPer <- round(CoveredPercentage,2)
return(coveredPer)
}
######################################################################################################





########################################################################################################
# Rotate the input matrix clockwise 90
########################################################################################################
rotate <- function(x) t(apply(x, 2, rev))
######################################################################################################





########################################################################################################
#https://www.xarg.org/2016/07/calculate-the-intersection-area-of-two-circles/
########################################################################################################
my_2circle_intersection_area <- function(cx1, cy1, cr1, cx2, cy2, cr2){
  
  d = learningr::hypotenuse(cx2 - cx1, cy2 - cy1)
  #cat("d=",d,cx1,cx2,cr1,cy1,cy2,cr2,"\n")
  if (d < (cr1 + cr2)) {
    
    a = cr1 * cr1
    b = cr2 * cr2
    
    x = (a - b + d * d) / (2 * d)
    z = x * x
    y = sqrt(a - z)
    
    if (d < abs(cr2 - cr1)) {
      #one is in another completely so the instersection is the smallest circles area
      return (pi * min(a, b))
    }
    #otherwise use some trigo to find the area
    return (a * asin(y / cr1) + b * asin(y / cr2) - y * (x + sqrt(z + b - a)))
  }
  return (0)
}
######################################################################################################






########################################################################################################
#Plots the solution
########################################################################################################
my_show_droneMtx <-  function(dMtx, labelsOn){
  #Give the matrix with center pos and r and this function will plot the config
  #y coords first as row
  #x coords thatn as cols
  #r is the third 
  
  lx <- length(dMtx)
  
  #You have to somehow swap coordinate for matrices if you want to use EBImage stuff
  #x and y coord as usual 
  showRegionx  <- matrix(0,nrow=w, ncol=h)
  
  for (i in seq(1,lx,3)) {
    
    xi <- ceiling(dMtx[i])
    yi <- ceiling(dMtx[i+1])
    ri <- ceiling(dMtx[i+2])
    
    # Drawing stuff
    showRegionx <- EBImage::drawCircle(showRegionx, xi, yi, ri, col=1, fill = FALSE)
    EBImage::display(showRegionx, method = "raster", all = TRUE) 
  }
  
  
  # #DEBUG
  # readline(prompt="Press [enter] to see rectangle around the image")
  # showRegionx[1,1:h] = 1
  # showRegionx[w,1:h] = 1
  # showRegionx[1:w,1] = 1
  # showRegionx[1:w,h] = 1
  # EBImage::display(showRegionx, method = "raster", all = TRUE) 
  
  
  
  if (labelsOn == 1) {
    cat("\n**********************************************************************************\n",file=textFileName,append=TRUE)
    cat("*Initial Drone positions\n",file=textFileName,append=TRUE)
    cat("**********************************************************************************\n",file=textFileName,append=TRUE)
    for (i in seq(1,lx,3)) {
     xi <- ceiling(dMtx[i])
     yi <- ceiling(dMtx[i+1])
     ri <- ceiling(dMtx[i+2])
     
     text(xi,yi,sprintf("%d", (((i-1)/3) + 1)), col = 0)
     hi <- ceiling(ri/tan((halfTheta*pi/180)))
     distix <- sqrt((xi-bsx)^2+(yi-bsy)^2+(hi)^2)
     cat(
       sprintf("Drone %3d at (x, y) = (%3d, %3d)  r = %3d  height = %3d  dist = %6.3f\n",(((i-1)/3) + 1),xi,yi,ri,hi,distix),
       file=textFileName,append=TRUE)
    }
    cat("**********************************************************************************\n",file=textFileName,append=TRUE)
  }
  
}
######################################################################################################







########################################################################################################
#Fitness function that rules them all others!!!
########################################################################################################
fit_with_weights <-  function(solVector, wCoverage, wOverflow, wOverlap, wDist, wScore){
  #GA provides the solution/population as a vector of length 3  
  
  # We can sort circles in the sol according to the radius for detecting more "big overlaps" here
  # Sth like x <- sol[with(sol, order(x)),]
  # But two pass method implemented in this function
  
  x <- solVector
  
  lx <- length(x)
  #n <- lx / ndrones
  
  
  templateOverlap <- matrix(0,nrow=w, ncol=h)
  templateOverflow <- matrix(0,nrow=w, ncol=h)
  templateForCircle <- matrix(0,nrow=w, ncol=h)
  
  
  coveredArea <- 0
  precoveredArea <- 0
  totOverflow <- 0
  totOverlap <- 0
  totBigOverlap <- 0
  cntBigOverlap1 <- 0
  
  ###############################################################################
  #Lets find the distance between each circle centers
  #Will be helpful for the nested overlaps
  #centerDistances will be upper triangular matrix by the way
  #We will sqaunder the half of the matrix anyway for the sake of speed
  #insideMtx is just a data structure for future debugging or use
  ###############################################################################
  
  
  centerDistances <- matrix(0,nrow=ndrones, ncol=ndrones)
  insideMtx <- matrix(0,nrow=ndrones, ncol=ndrones)
  DistMtx <- matrix(0, ndrones, 1)
  
  
  #Our flag!
  anyCircleOverlap <- 0
  src <- 1
  
  for (i in seq(1,lx-3,3)) {
    
    xi <- ceiling(x[i])
    yi <- ceiling(x[i+1])
    ri <- ceiling(x[i+2])
    
    if (anyCircleOverlap == 1) break
    
    dst <- src + 1
    for (j in seq(i+3,lx,3)) {
      
      #cat (i,j,"\n")
      
      xj <- ceiling(x[j])
      yj <- ceiling(x[j+1])
      rj <- ceiling(x[j+2])
      
      # Use Pitagor to calculate dist between ith circle and jth circle
      distij <- ceiling(sqrt((xi-xj)*(xi-xj) + (yi-yj)*(yi-yj)))
      centerDistances[src,dst] <- distij
      
      # if the distance between two centers is less than the sum of their raidii
      # then there is an overlap
      
      # When  the small circle is inside the bigger one,
      # the centers are closer than their (radii) difference
      
      #Check if one circle is inside of other
      if (ri <= rj){  #rj is bigger or they are equal size
        # i is inside j
        if (distij <= (rj-ri)) {
          insideMtx[src,dst] <- 1 
          anyCircleOverlap <- 1
        } else insideMtx[src,dst] <- 0
      } else {# ri is bigger
        # j is inside i
        if (distij <= (ri-rj)) { 
          insideMtx[dst,src] <- 1 
          anyCircleOverlap <- 1
        } else insideMtx[dst,src] <- 0
      }
      if (anyCircleOverlap == 1) break
      dst <- dst + 1
    }
    src <- src + 1
  }
  
  # We penalize for the nested circles, simply we do not want any of them
  # Later we just consider that as an unnecessary drone if it is not possible to
  # place it anywhere else
  if (anyCircleOverlap == 1)  { 
    score <- 0 
    return(score)
  } else {
    
    ###############################################################################
    #Now we are sure that we do not have any nested (fully overlapped) circle
    #Lets go and evaluate the current drone config
    ############################################################################### 
    
    sumDist <- 0
    ix <- 1
    
    for (k in seq(1,lx,3)) {
      
      x1 <- ceiling(x[k])
      y1 <- ceiling(x[k+1])
      r1 <- ceiling(x[k+2])
      
      #Sum the distance from BS while we are going on the way!!!
      # Z for base station is 0!
      distix <- sqrt((x1-bsx)^2+(y1-bsy)^2+(r1/tan(halfTheta*pi/180))^2)
      sumDist <- sumDist + distix
      DistMtx[ix] <- distix
      ix <- ix + 1
      
      
      # #BEGIN DEBUG
      # circleArea <- pi * r1 * r1
      # #END DEBUG
      
      # Draw the circle on empty region on the original position to see how many pixels
      # overflowed
      nonOverflowedRegion <- EBImage::drawCircle(templateOverflow, x1, y1, r1, col=1, fill = TRUE)
      
      # Draw the circle in the middle, (assuming that r will be always less than the half width and half height)
      # then count pixels to find out area in pixels
      circleAreaRegion <- EBImage::drawCircle(templateForCircle, ceiling(w/2), ceiling(h/2), r1, col=1, fill = TRUE)
      
      circlepixelArea <- length(circleAreaRegion[circleAreaRegion==1])
      nonOverflowedArea <- length(nonOverflowedRegion[nonOverflowedRegion==1])
      circleOverflow <- circlepixelArea - nonOverflowedArea
      
      
      totOverflow <- totOverflow + circleOverflow
      
      templateOverlap <- EBImage::drawCircle(templateOverlap, x1, y1, r1, col=1, fill = TRUE)
      coveredArea <- length(templateOverlap[templateOverlap==1])
      
      
      # To calculate overlap that comes from that circle:
      # First findout the theoretical contribution of the circle = How many pixels would be added
      # Count actually added pixels and
      # then subtract the overflow if the center is close to edges of the region
      
      circleOverlap <- (precoveredArea + circlepixelArea) - coveredArea - circleOverflow
      
      
      
      
      
      # If the circle is overlapped more than a threshold, give high penalty!!!
      # But when the bigger circle on top of  small circle the overlap will be less than its overlapThreshold
      # On the other hand poor small circle will be covered totally more than threshold probably
      # So order is important then!
      # Sorting helps
      # or two pass: first to last and then last to first passes, like scanning!
      if (circlepixelArea <= (100 / overlapThreshold * circleOverlap) ) {
        #DEBUG
        #cat("More than", overlapThreshold,"percent overlap!",circleOverlap, circlepixelArea,"\n")
        
        totBigOverlap <- totBigOverlap + circleOverlap
        cntBigOverlap1 <- cntBigOverlap1 + 1
        
      }
      
      
      
      precoveredArea <- coveredArea
      
      totOverlap <- totOverlap + circleOverlap
      
      # In ga (parallel) this trace output is not possible!!
      # #BEGIN DEBUG
      #cat(round,"Circle",(k-1)/3+1, "at",x1,y1,"r=",r1, "pixel area",circlepixelArea, "areacalc",circleArea,"has overflow area of",circleOverflow, "and overlapped",circleOverlap,"covered so far",coveredArea,"\n")
      #cat("Circle",(k-1)/3+1, "at",x1,y1,"r=",r1, "pixel area",circlepixelArea, "areacalc",circleArea,"has overflow area of",circleOverflow, "and overlapped",circleOverlap,"covered so far",coveredArea,"\n")
      # #END DEBUG
      
      
      
    }
    
    
    
    templateOverlap <- matrix(0,nrow=w, ncol=h)
    templateOverflow <- matrix(0,nrow=w, ncol=h)
    templateForCircle <- matrix(0,nrow=w, ncol=h)
    
    
    coveredArea <- 0
    precoveredArea <- 0
    totOverflow <- 0
    totOverlap <- 0
    totBigOverlap <- 0
    cntBigOverlap2 <- 0
    
    
    # Do the reverse pass to see if any small circle goes into bigger circle
    # "over Overlap"
    # Drone info is already filled only overlap direction needs to be found
    for (k in seq(lx,1,-3)) {
      
      x1 <- ceiling(x[k-2])
      y1 <- ceiling(x[k-1])
      r1 <- ceiling(x[k])
      
      
      # #BEGIN DEBUG
      # circleArea <- pi * r1 * r1
      # #END DEBUG
      
      # Draw the circle on empty region on the original position to see how many pixels
      # overflowed
      nonOverflowedRegion <- EBImage::drawCircle(templateOverflow, x1, y1, r1, col=1, fill = TRUE)
      
      # Draw the circle in the middle, (assuming that r will be always less than the half width and half height)
      # then count pixels to find out area in pixels
      circleAreaRegion <- EBImage::drawCircle(templateForCircle, ceiling(w/2), ceiling(h/2), r1, col=1, fill = TRUE)
      
      circlepixelArea <- length(circleAreaRegion[circleAreaRegion==1])
      nonOverflowedArea <- length(nonOverflowedRegion[nonOverflowedRegion==1])
      circleOverflow <- circlepixelArea - nonOverflowedArea
      
      
      totOverflow <- totOverflow + circleOverflow
      
      templateOverlap <- EBImage::drawCircle(templateOverlap, x1, y1, r1, col=1, fill = TRUE)
      coveredArea <- length(templateOverlap[templateOverlap==1])
      
      
      # To calculate overlap that comes from that circle:
      # First findout the theoretical contribution of the circle = How many pixels would be added
      # Count actually added pixels and
      # then subtract the overflow if the center is close to edges of the region
      
      circleOverlap <- (precoveredArea + circlepixelArea) - coveredArea - circleOverflow
      
      
      # If the circle is overlapped more than a threshold, give high penalty!!!
      # But when the bigger circle on top of  small circle the overlap will be less than its overlapThreshold
      # On the other hand poor small circle will be covered totally more than threshold probably
      # So order is important then!
      # Sorting helps
      # or two pass: first to last and then last to first passes, like scanning!
      if (circlepixelArea <= (100 / overlapThreshold * circleOverlap) ) {
        #DEBUG
        #cat("More than", overlapThreshold,"percent overlap!",circleOverlap, circlepixelArea,"\n")
        
        totBigOverlap <- totBigOverlap + circleOverlap
        cntBigOverlap2 <- cntBigOverlap2 + 1
        
      }
      
      precoveredArea <- coveredArea
      
      totOverlap <- totOverlap + circleOverlap
      
      # In ga (parallel) this trace output is not possible!!
      # #BEGIN DEBUG
      #cat(round,"Circle",(k-1)/3+1, "at",x1,y1,"r=",r1, "pixel area",circlepixelArea, "areacalc",circleArea,"has overflow area of",circleOverflow, "and overlapped",circleOverlap,"covered so far",coveredArea,"\n")
      #cat("Circle",(k-1)/3+1, "at",x1,y1,"r=",r1, "pixel area",circlepixelArea, "areacalc",circleArea,"has overflow area of",circleOverflow, "and overlapped",circleOverlap,"covered so far",coveredArea,"\n")
      # #END DEBUG
      
      
    }
    
    
    
    
    
    
    ##BEGIN DEBUG
    ## 1 is for the forward pass and 2 is for the backward pass
    ## Slows down execution!!!
    # cat("1-",cntBigOverlap1, "2-",cntBigOverlap2,"\n")
    ##END DEBUG
    
    
    
    # In ga (parallel) this trace output is not possible!!
    # #BEGIN DEBUG
    # 
    # EBImage::display(templateOverlap, method = "raster", all = TRUE)
    # for (k in seq(1,lx,3)) {
    #   y1 <- x[k]
    #   x1 <- x[k+1]
    #   r1 <- x[k+2]
    #   text(x1,y1,sprintf("%d", (((k-1)/3) + 1)))
    #   }
    # 
    # #readline(prompt = "Pause. Press <Enter> to continue...")
    # 
    # #END DEBUG
    
    # round <- round + 1
    
    
    #May be instead of total absolute value of overlapp/overflow 
    #consider the ratio of them to the realted circles
    
    if ((cntBigOverlap1 + cntBigOverlap2) > 1) score <- 0
    
    else{
      # HERE consider height and distances!!!!
      xmedDist <- median(DistMtx)
      
      
      #Calculate mean distance from the Base Station for drones
      xavgDist  <- sumDist / ndrones
      
      # As drones comes towards "Virtual Base Station" distPer becomes larger
      distPer <- 100 * (maxDist-xavgDist) / maxDist
      medDistPer <- 100 * (maxDist-xmedDist) / maxDist
      
      totDistPer <- 100 * (maxDist*ndrones - sumDist) / (maxDist*ndrones)
      
      coveredPer <- (coveredArea*100/areaEmptyRegion)
      overflowPer  <- (totOverflow*100/coveredArea)
      overlapPer  <- (totOverlap*100/coveredArea)
      score <- wCoverage * coveredPer + wOverflow * overflowPer + wOverlap * overlapPer + wDist * totDistPer
      
      
    }
    
    
    
    return(wScore * score)
  }
}

########################################################################################################










