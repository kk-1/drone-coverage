

########################################################################################################
# Define server logic
########################################################################################################
server <- function(input, output) {
  
  
  
  ########################################################################################################
  # Begin Functions
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
            "=",round((100 *xcircleOverlap / xcirclepixelArea),1),"\n")
        
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
    scorePixelPercent <-fit_with_weights(dMtx, wCoverage=input$xwCoverage, wOverflow=input$xwOverflow, wOverlap=input$xwOverlap, wDist=input$xwDist, wScore=xwScore)
    
    txtStat <- paste("\nRegionArea:(", w,",",h,"):",areaEmptyRegion,"m^2",
                     "\nCoveredArea:",totCoveredArea,"m^2 (",round(CoveredPercentage,2),"% )",
                     "\nxCoveredArea:",xcoveredArea,"m^2 (",round(xcoveredArea/areaEmptyRegion*100,2),"% )",
                     "\nTotCircleArea(with overlap):",totCircleArea,"m^2 (",round(totCircleArea/areaEmptyRegion*100,2),"% )",
                     "\nMean coverage of drones(overlap and overflow not considered):",sumCircleArea/input$ndrones,"m^2",
                     "\nOverlap:",totOverlapArea,"m^2 (",round((totOverlapArea*100/totCoveredArea),2),"% of covered,"
                     ,round((totOverlapArea*100/areaEmptyRegion),2),"% of region )",
                     "\nMean Overlap of drones:",totOverlapArea/input$ndrones,"m^2",
                     "\nOverflow:",xtotOverflow,"m^2 (",round((xtotOverflow*100/totCoveredArea),2),"% of covered,"
                     ,round((xtotOverflow*100/areaEmptyRegion),2),"% of region )",
                     "\nMean Overflow of drones:",xtotOverflow/input$ndrones,"m^2",
                     "\nEmpty:",totEmptyArea ,"m^2 (",round(EmptyPercentage,1),"% of region )",
                     "\nMean Radius of drones:",xavgr,"m",
                     "\nMean Height of drones:",xavgh,"m",
                     "\nTotal Distance of drones from BS(", bsx,",",bsy,", 0 ):",xsumDist,"m",
                     "\nMax Distance a drone from BS(", bsx,",",bsy,", 0 ):",maxDist,"m",
                     "\nMean Distance of drones from BS(", bsx,",",bsy,", 0 ):",xavgDist,"m",
                     "\nMedian Distance of drones from BS(", bsx,",",bsy,", 0 ):",xmedDist,"m",
                     #"\nPixel Score:", scorePixel,
                     "\nPercent Score:", scorePixelPercent,
                     "\n" 
    )
    txtStat <- paste(txtStat,"\n\n\n**********************************************************************************\n") 
    
    
    #DEBUG
    # cat("xRegionArea:", areaEmptyRegion,
    #     "- xCoveredArea:",xcoveredArea,"( %",round(CoveredPercentage,1),
    #     ") - xOverlap:",xtotOverlap,"( %",round((xtotOverlap*100/xcoveredArea),1),
    #     ") - xOverflow:",xtotOverflow,"( %",round((xtotOverflow*100/xcoveredArea),1),
    #     ") - xEmpty:",totEmptyArea ,"( %",round(EmptyPercentage,1),
    #     "\n\n")
    coveredPer <- round(CoveredPercentage,2)
    return(c(coveredPer,txtStat))
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
      txtForInitial <- paste(txtForInitial,"\n**********************************************************************************")
      txtForInitial <- paste(txtForInitial,"\n*Initial Drone positions")
      txtForInitial <- paste(txtForInitial,"\n**********************************************************************************\n")
      # cat("\n**********************************************************************************\n")
      # cat("*Initial Drone positions")
      # cat("\n**********************************************************************************\n")
      for (i in seq(1,lx,3)) {
        xi <- ceiling(dMtx[i])
        yi <- ceiling(dMtx[i+1])
        ri <- ceiling(dMtx[i+2])
        
        text(xi,yi,sprintf("%d", (((i-1)/3) + 1)), col = 0)
        hi <- ceiling(ri/tan((halfTheta*pi/180)))
        distix <- sqrt((xi-bsx)^2+(yi-bsy)^2+(hi)^2)
        # cat(
        #   sprintf("Drone %3d at (x, y) = (%3d, %3d)  r = %3d  height = %3d  dist = %6.3f\n",(((i-1)/3) + 1),xi,yi,ri,hi,distix)
        #    )
        
        txtForInitial <- paste(txtForInitial,sprintf("Drone %3d at (x, y) = (%3d, %3d)  r = %3d  height = %3d  dist = %6.3f\n",(((i-1)/3) + 1),xi,yi,ri,hi,distix)) 
      }
      #cat("**********************************************************************************\n")
      txtForInitial <- paste(txtForInitial,"\n**********************************************************************************\n")
      #output$msgInit<- renderText({ txtForInitial })
      return(txtForInitial)
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
    
    #DEBUG cat ("*****ndronesVal=", ndronesVal,"\n")
    centerDistances <- matrix(0,nrow=ndronesVal, ncol=ndronesVal)
    insideMtx <- matrix(0,nrow=ndronesVal, ncol=ndronesVal)
    DistMtx <- matrix(0, ndronesVal, 1)
    
    
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
        xavgDist  <- sumDist / ndronesVal
        
        # As drones comes towards "Virtual Base Station" distPer becomes larger
        distPer <- 100 * (maxDist-xavgDist) / maxDist
        medDistPer <- 100 * (maxDist-xmedDist) / maxDist
        
        totDistPer <- 100 * (maxDist*ndronesVal - sumDist) / (maxDist*ndronesVal)
        
        coveredPer <- (coveredArea*100/areaEmptyRegion)
        overflowPer  <- (totOverflow*100/coveredArea)
        overlapPer  <- (totOverlap*100/coveredArea)
        score <- wCoverage * coveredPer + wOverflow * overflowPer + wOverlap * overlapPer + wDist * totDistPer
        
        
      }
      
      
      
      return(wScore * score)
    }
  }
  
  ########################################################################################################
  
  
  
  
  ########################################################################################################
  # End Functions
  ########################################################################################################
  
  
  infoTxt <- paste("Hello, set parameters and click \"RunSim\"!\n")
  output$info<- renderUI(HTML(paste(c("<pre>", capture.output(cat(infoTxt)), "</pre>"), collapse = "<br>")))
  
  
  my_clear_msg <- function(){
    my_text<-" "
    output$msg1<- renderText({ my_text })
  }
  
  
  
  
  
  ########################################################################################################
  observeEvent(input$runSim,{
    
    
    ndronesVal <<- force(input$ndrones)
    #DEBUG cat ("-----ndronesVal=", ndronesVal,"\n")
    
    
    
    
    infoTxt <- paste("Running Sim with", input$ndrones,"drones.\n")
    output$info<- renderUI(HTML(paste(c("<pre>", capture.output(cat(infoTxt)), "</pre>"), collapse = "<br>")))
    
    
    
    
    #Set some variables for GA
    populationSize <- 3*input$ndrones
    if (populationSize < 10) populationSize <- 20
    
    
    ########################################################################################################
    # Create data frame for debugging purposes that can hold info for each drone
    # Pos, height or Radius, overlap, overflow, area
    ########################################################################################################
    
    N <- input$ndrones
    drones <- data.frame(x   = integer(N),
                         y   = integer(N),
                         
                         height= integer(N),
                         r   = integer(N),
                         area   = integer(N),
                         overlap   = integer(N),
                         overflow   = integer(N),
                         # 1 threshold passed
                         # 0 There was no significant overlap from this drone
                         forwardOverlap = integer(N),
                         backwardOverlap= integer(N),
                         regionX =  integer(N),
                         regionY =  integer(N),
                         regionArea = integer(N),
                         regionCoveredArea = integer(N),
                         regionCoveredPercentage = integer(N),
                         regionOverlap = integer(N),
                         regionOverflow = integer(N)
    ) 
    
    
    #Set values for region just to save them to a file
    drones$regionX <- w
    drones$regionY <- h
    drones$regionArea <- w * h
    
    
    #Drone matrix with size of 3*2*number_of_drones
    #xmin, xmax, ymin, ymax, rmin, rmax for each
    #1,800,1,600,5,300
    #rows, columns
    dronemtx<-matrix(rep(c(1,w,1,h,rmin,rmax),input$ndrones), input$ndrones*3, 2, byrow = TRUE)
    
    
    # #Set the text output file name for various statistics
    # textFileName <- paste0(algoName,"drones=",input$ndrones,"_",input$xwCoverage,"_",input$xwOverflow,"_",input$xwOverlap,"_",input$xwDist,"_iter=",input$MaxIteration,"_Out")
    # ext<-"txt"
    # textFileName <- paste(textFileName, ext, sep = ".")
    
    
    
    # 
    # cat("\n**********************************************************************************\n")
    # cat(algotitle, )
    # cat("\n**********************************************************************************\n")
    # #Command line
    # # cat("Command line with arguments:\n")
    # # args2 <- commandArgs()
    # # cat(args2, sep =" ", )
    # cat("\n")
    
    # 
    # txtForInitial <- ""
    # txtForInitial <- paste("\n**********************************************************************************\n")
    # txtForInitial <- paste(txtForInitial, "*Drone Configuration")
    # txtForInitial <- paste(txtForInitial,"\n**********************************************************************************\n")                       
    # txtForInitial <- paste(txtForInitial, "Area size:",areaEmptyRegion,"\n")
    # txtForInitial <- paste(txtForInitial, "Min drones necessary (at max h) for 100% coverage:",minNdrones,"\n")
    # txtForInitial <- paste(txtForInitial, "Max drones necessary (at min h) for 100% coverage:",maxNdrones,"\n")
    # txtForInitial <- paste(txtForInitial, "Region width=",w,"m height=",h,"m\n")
    # txtForInitial <- paste(txtForInitial, "Using",input$ndrones,"drones!\n")
    # txtForInitial <- paste(txtForInitial, nCluster,"cores detected for parallel processing!\n")
    # txtForInitial <- paste(txtForInitial, "Max",input$MaxIteration,"iterations for evolutionary algorithms\n")
    # txtForInitial <- paste(txtForInitial, "Drone beam angle is",2*halfTheta,"degrees\n")
    # txtForInitial <- paste(txtForInitial, "Drone height range as",hmin,"-",hmax,"meters\n")
    # txtForInitial <- paste(txtForInitial, "Drone radius range as",rmin,"-",rmax,"meters\n")
    # txtForInitial <- paste(txtForInitial, "Max distance for drones in the region (furthest diagonal and highest)",round(maxDist,2),"meters\n")
    # txtForInitial <- paste(txtForInitial, "Overlap threshold set as",overlapThreshold,"%\n")
    # txtForInitial <- paste(txtForInitial, "Percentage Weights: Coverage =",input$xwCoverage,"Overlap =",input$xwOverlap,"Overflow =",input$xwOverflow,"Dist =",input$xwDist,"Score =",xwScore,"\n")
    # txtForInitial <- paste(txtForInitial, "\n**********************************************************************************\n")
    
    infoTxt <- paste("\n**********************************************************************************\n")
    infoTxt <- paste(infoTxt, "Sim with", input$ndrones,"drones.\n")
    infoTxt <- paste(infoTxt, "\n**********************************************************************************\n")
    infoTxt <- paste(infoTxt, "\n**********************************************************************************\n")
    infoTxt <- paste(infoTxt, "*Drone Configuration")
    infoTxt <- paste(infoTxt,"\n**********************************************************************************\n")                       
    infoTxt <- paste(infoTxt, "Area size:",areaEmptyRegion,"\n")
    infoTxt <- paste(infoTxt, "Min drones necessary (at max h) for 100% coverage:",minNdrones,"\n")
    infoTxt <- paste(infoTxt, "Max drones necessary (at min h) for 100% coverage:",maxNdrones,"\n")
    infoTxt <- paste(infoTxt, "Region width=",w,"m height=",h,"m\n")
    infoTxt <- paste(infoTxt, "Using",input$ndrones,"drones!\n")
    infoTxt <- paste(infoTxt, nCluster,"cores detected for parallel processing!\n")
    infoTxt <- paste(infoTxt, "Max",input$MaxIteration,"iterations for evolutionary algorithms\n")
    infoTxt <- paste(infoTxt, "Drone beam angle is",2*halfTheta,"degrees\n")
    infoTxt <- paste(infoTxt, "Drone height range as",hmin,"-",hmax,"meters\n")
    infoTxt <- paste(infoTxt, "Drone radius range as",rmin,"-",rmax,"meters\n")
    infoTxt <- paste(infoTxt, "Max distance for drones in the region (furthest diagonal and highest)",round(maxDist,2),"meters\n")
    infoTxt <- paste(infoTxt, "Overlap threshold set as",overlapThreshold,"%\n")
    infoTxt <- paste(infoTxt, "Percentage Weights: Coverage =",input$xwCoverage,"Overlap =",input$xwOverlap,"Overflow =",input$xwOverflow,"Dist =",input$xwDist,"Score =",xwScore,"\n")
    infoTxt <- paste(infoTxt, "\n**********************************************************************************\n")
    output$info<- renderUI(HTML(paste(c("<pre>", capture.output(cat(infoTxt)), "</pre>"), collapse = "<br>")))
    
    
    
    #output$msgInit<- renderText({ txtForInitial })
    
    #DEBUG output$msgFinal<- renderText({ txtForInitial })
    ########################################################################################################
    
    
    
    
    ########################################################################################################
    output$initSolnPlot <- renderPlot({ 
      ####################################################################
      #BEGIN initSolnPlot stuff
      ####################################################################
      
      input$runSim
      
      ########################################################################################################
      isolate({
        
        wCoveragex <<- force(input$xwCoverage)
        wOverflowx <<- force(input$xwOverflow)
        wOverlapx <<- force(input$xwOverlap)
        wDistx <<- force(input$xwDist)
        MaxIterationx <<- force(input$MaxIteration)
        
        if (force(input$useInitSolSW) == TRUE) {
          useInitSol = 1
        }else{
          useInitSol = 0
        }
        algoIdx <- match(force(input$algo),  list("GA", "genoud", "DEoptim", "GenSA","optimParallel")) - 1
        
        # Set file name to save the drawing to png file
        if (algoIdx == 0){
          #algoName="GA_"
          cat("Using ga algortihm - Maximizing - Accepts initial solution\n")
          #algotitle <- paste("Using ga algortihm - Maximizing - Accepts initial solution")
          xwScore=1.0
          acceptsInitSol = 1
        } else if (algoIdx == 1) {
          #algoName="genoud_"
          cat("Using genoud algortihm - Maximizing - Accepts initial solution\n")
          #algotitle <- paste("Using genoud algortihm - Maximizing - Accepts initial solution")
          xwScore=1.0
          acceptsInitSol = 1
        } else if (algoIdx == 2) {
          #algoName="deoptim_"
          cat("Using deoptim algortihm - Minimizing - Does not accept initial solution - Tolerance is 0.0001\n")
          #algotitle <- paste("Using deoptim algortihm - Minimizing - Does not accept initial solution - Tolerance is 0.0001")
          xwScore=-1.0
          acceptsInitSol = 1
          #acceptsInitSol = 0
        } else if (algoIdx == 3) {
          #algoName="GenSA_"
          cat("Using GenSA algortihm - Minimizing - Accepts initial solution\n")
          #algotitle <- paste("Using GenSA algortihm - Minimizing - Accepts initial solution")
          xwScore=-1.0
          acceptsInitSol = 1
        } else if (algoIdx == 4) {
          #algoName="optimPar_"
          cat("Using optimParallel algortihm - Maximizing - Accepts initial solution\n")
          #algotitle <- paste("Using  optimParallel algortihm - Maximizing- Accepts initial solution")
          cl <- parallel::makeCluster(parallel::detectCores()); 
          parallel::setDefaultCluster(cl = cl)
          xwScore=1.0
          acceptsInitSol = 1
        } else {
          # This should not happen as previously checked!
          cat("Unlisted algo!\n")
        }
        
        
        
        
        
        
        
        
        if (acceptsInitSol == 1) {
          cat("Algo accepts initial sol\n")
          if (readInitDroneMtxfromFile == 0) initDroneMtx <- pack.circles(ndronesVal, c(w,h), 500, 0)
          else {
            fname<-paste0("my_initDroneMtx-",ndronesVal,"drones.rds")
            cat("Reading initial sol from:",fname,"\n")
            initDroneMtx <- readRDS(fname)
          }
          
          
          # cat("\n**********************************************************************************\n")
          # cat("*Initial Solution Statistics\n")
          # cat("**********************************************************************************\n")
          
          returnVector <- my_show_droneMtx_stat(initDroneMtx)
          percent <- as.numeric(returnVector[1])
          txtStat <- returnVector[2]
          
          
          
          txtForInitial <- ""
          txtForInitial <- paste("**********************************************************************************")
          txtForInitial <- paste(txtForInitial, "\n*Initial Solution Statistics")
          txtForInitial <- paste(txtForInitial,"\n**********************************************************************************")   
          
          txtPos <- my_show_droneMtx(initDroneMtx, labelsOn = 1)
          txtForInitial <- paste(txtForInitial, txtStat,txtPos )
          output$msgInit<- renderText({ txtForInitial })    
        }
        ########################################################################################################
        
        
        
        
        ########################################################################################################
        output$CovPlot <- renderPlot({ 
          ####################################################################
          #BEGIN CovPlot stuff
          ####################################################################
          
          
          
          
          
          
          ########################################################################################################
          #Run the desired GA algo
          ########################################################################################################
          
          
          
          if (algoIdx == 0) {
            
            
            if (useInitSol == 1) {
              
              #Convert the initial solution to desired format by ga algo
              InitialsolSuggestion <- matrix(c(initDroneMtx), nrow = 1, ncol = 3*ndronesVal)
              
              
              
              tic.clearlog();
              tic("GA Algorithm")
              GA <- ga(type = "real-valued",
                       fitness = function(xxx) fit_with_weights(xxx, wCoverage=wCoveragex, wOverflow=wOverflowx, wOverlap=wOverlapx, wDist=wDistx, wScore=xwScore),
                       lower = rep(c(1,1,rmin),ndronesVal), upper = rep(c(w,h,rmax),ndronesVal), parallel = TRUE,
                       # popSize = populationSize, maxiter = input$MaxIteration, optim = TRUE,
                       popSize = populationSize, run = 20, maxiter = MaxIterationx, optim = TRUE,
                       suggestions = InitialsolSuggestion)
              
              # GA <- ga(type = "real-valued", 
              #          fitness = function(xxx) fit_with_weights(xxx, wCoverage=1, wOverflow=0, wOverlap=0, wDist=1, wScore=1),
              #          lower = rep(c(1,1,rmin),4), upper = rep(c(w,h,rmax),4), parallel = TRUE,
              #          # popSize = populationSize, maxiter = input$MaxIteration, optim = TRUE,
              #          popSize = populationSize, run = MaxIterationx, maxiter = 500, optim = TRUE,
              #          suggestions = InitialsolSuggestion)
              
              timing <- toc(log = TRUE, quiet = TRUE)
            } else {
              
              
              tic.clearlog();
              tic("GA Algorithm")
              GA <- ga(type = "real-valued", 
                       fitness = function(xxx) fit_with_weights(xxx, wCoverage=wCoveragex, wOverflow=wOverflowx, wOverlap=wOverlapx, wDist=wDistx, wScore=xwScore),
                       lower = rep(c(1,1,rmin),ndronesVal), upper = rep(c(w,h,rmax),ndronesVal), parallel = TRUE,
                       #popSize = populationSize, maxiter = input$MaxIteration, optim = TRUE)
                       popSize = populationSize, run = 20, maxiter = MaxIterationx, optim = TRUE)
              
              timing <- toc(log = TRUE, quiet = TRUE)
            }
            
            
            log.txt <- tic.log(format = TRUE)
            #cat("\n",log.txt[[1]],"\n",file=textFileName,append=TRUE )
            dstr <- timing$toc - timing$tic
            secs <- as.numeric(sprintf("%6.3f",dstr))
            summary(GA)
            solxyr<-summary(GA)
            
            # #Lets save the progress plot of GA to png
            # fname<-paste0(algoName,"drones=",ndronesVal,"_",input$xwCoverage,"_",input$xwOverflow,"_",input$xwOverlap,"_",input$xwDist,"_pop=",populationSize,"_iter=",solxyr$iter,"_secs=",secs,"_ProgPlot")
            # ext<-"png"
            # fname<-paste(fname, ext, sep = ".")
            # png(filename = fname, width = 800, height = 600)
            # plot(GA)
            # dev.off()
            
            lx <- length(solxyr$solution[1,])
            solpars <- solxyr$solution[1,]
            
            
            
          } else if (algoIdx == 1) {
            
            
            if (useInitSol == 1) {
              #Convert the initial solution to desired format by ga algo
              InitialsolSuggestion <- matrix(c(initDroneMtx), nrow = 1, ncol = 3*ndronesVal)
              
              
              #fork 4 threads depending on the CPU
              cl<-parallel::makeForkCluster(nCluster)
              
              # Run the genoud Algorithm
              tic.clearlog();
              tic("genoud Algorithm")
              GA <- genoud(function(xxx) fit_with_weights(xxx, wCoverage=wCoveragex, wOverflow=wOverflowx, wOverlap=wOverlapx, wDist=wDistx, wScore=xwScore),
                           nvars = (ndronesVal * 3),max = TRUE,
                           starting.values = initDroneMtx,
                           pop.size = populationSize,max.generations = MaxIterationx,
                           wait.generations = ceiling(MaxIterationx/3),Domains = dronemtx,boundary.enforcement = 2,
                           print.level = 2, MemoryMatrix = TRUE, cluster = cl)
              
              
              timing <- toc(log = TRUE, quiet = TRUE)
            }else{
              
              #fork 4 threads depending on the CPU
              cl<-parallel::makeForkCluster(nCluster)
              
              # Run the genoud Algorithm
              tic.clearlog();
              tic("genoud Algorithm")
              GA <- genoud(function(xxx) fit_with_weights(xxx, wCoverage=wCoveragex, wOverflow=wOverflowx, wOverlap=wOverlapx, wDist=wDistx, wScore=xwScore),
                           nvars = (ndronesVal * 3),max = TRUE,
                           pop.size = populationSize,max.generations = MaxIterationx,
                           wait.generations = ceiling(MaxIterationx/3),Domains = dronemtx,boundary.enforcement = 2,
                           print.level = 2, MemoryMatrix = TRUE, cluster = cl)
              
              
              timing <- toc(log = TRUE, quiet = TRUE)
            }
            
            
            log.txt <- tic.log(format = TRUE)
            #cat("\n",log.txt[[1]],"\n",file=textFileName,append=TRUE )
            dstr <- timing$toc - timing$tic
            
            secs <- as.numeric(sprintf("%6.3f",dstr))
            
            #Stop the threads
            parallel::stopCluster(cl)
            
            # Maximum possible solution of the equations 
            GA$value
            # The parameters of the solutions
            GA$par
            
            
            lx <- length(GA$par)
            solpars <- GA$par
            
            
            
          } else if (algoIdx == 2) {
            
            
            
            # opt <- DEoptim(func_with_PixelCountNeg,
            #                rep(c(1,1,rmin),ndronesVal), rep(c(600,800,rmax),ndronesVal),
            #                DEoptim.control(NP=20, itermax=3, F=1.2, CR=0.7))
            
            # opt <- DEoptim(func_with_PixelCountNeg,
            #                rep(c(1,1,rmin),ndronesVal), rep(c(600,800,rmax),ndronesVal),
            #                DEoptim.control(NP=populationSize, itermax=input$MaxIteration, parallelType = 1) )
            # 
            
            
            # For many problems it is best to set 'NP' (in 'control') 
            # to be at least ten times the length of the parameter vector.
            # So we leave to the algo to decide optimum number of population
            
            # Run the deoptimum Algorithm
            tic.clearlog();
            tic("DEoptimum Algorithm")
            opt <- DEoptim(fn = function(xxx) fit_with_weights(xxx,wCoverage=wCoveragex, wOverflow=wOverflowx, wOverlap=wOverlapx, wDist=wDistx, wScore=xwScore),
                           rep(c(1,1,rmin),ndronesVal), rep(c(w,h,rmax),ndronesVal),
                           DEoptim.control(
                             itermax=MaxIterationx,
                             reltol=0.0001,
                             parallelType = 1, 
                             parVar = c("ndronesVal","h","w","overlapThreshold",
                                        "rmin","bsx","bsy","halfTheta","maxDist",
                                        "areaEmptyRegion",#"fit_with_weights",
                                        "wCoveragex","wOverflowx","wOverlapx",
                                        "wDistx","xwScore")) 
            )
            
            timing <- toc(log = TRUE, quiet = TRUE)
            log.txt <- tic.log(format = TRUE)
            #cat("\n",log.txt[[1]],"\n",file=textFileName,append=TRUE )
            dstr <- timing$toc - timing$tic
            
            secs <- as.numeric(sprintf("%6.3f",dstr))
            
            #plot(opt)
            #summary(opt)
            
            
            lx <- length(opt$optim$bestmem)
            solpars <- opt$optim$bestmem
            populationSize <- length(opt$member$pop)
            
            
            
          } else if (algoIdx == 3) {
            
            
            
            set.seed(1234) # The user can use any seed.
            
            #In case you can guess the global min and can set the tolerance level
            global.min <- 0
            tol <- 1e-13
            
            
            lowerVec <- rep(c(1,1,rmin),ndronesVal)
            upperVec <- rep(c(w,h,rmax),ndronesVal)
            
            #Here we can pass non-optimizng params like weights!!!
            #fit_with_weights <-  function(solVector, wCoverage, wOverflow, wOverlap, wDist, wScore){
            #https://stackoverflow.com/questions/45920796/how-to-pass-non-optimizing-arguments-into-fitness-function-in-ga-package
            
            
            
            if (useInitSol == 1) {
              # Run the GenSA Algorithm with time limit = max.time!
              tic.clearlog();
              tic("GenSA Algorithm")  
              
              out <- GenSA(par = initDroneMtx, lower = lowerVec, upper = upperVec,
                           fn = function(xxx) fit_with_weights(xxx, wCoverage=wCoveragex, wOverflow=wOverflowx, wOverlap=wOverlapx, wDist=wDistx, wScore=xwScore),
                           control=list(verbose=TRUE, nb.stop.improvement = MaxIterationx))
              #control=list(verbose=TRUE, max.time = input$MaxIteration))
              #control=list(verbose=TRUE, maxit = input$MaxIteration))
              
              timing <- toc(log = TRUE, quiet = TRUE)
            }else{
              # Run the GenSA Algorithm with time limit = max.time!
              tic.clearlog();
              tic("GenSA Algorithm")  
              
              out <- GenSA(lower = lowerVec, upper = upperVec,
                           fn = function(xxx) fit_with_weights(xxx, wCoverage=wCoveragex, wOverflow=wOverflowx, wOverlap=wOverlapx, wDist=wDistx, wScore=xwScore),
                           control=list(verbose=TRUE, nb.stop.improvement = MaxIterationx))
              #control=list(verbose=TRUE, max.time = input$MaxIteration))
              #control=list(verbose=TRUE, maxit = input$MaxIteration))
              
              timing <- toc(log = TRUE, quiet = TRUE)
            }
            
            
            
            
            
            log.txt <- tic.log(format = TRUE)
            #cat("\n",log.txt[[1]],"\n",file=textFileName,append=TRUE )
            dstr <- timing$toc - timing$tic
            
            secs <- as.numeric(sprintf("%6.3f",dstr)) 
            
            
            out[c("value","par","counts")]
            
            lx <- length(out$par)
            solpars <- out$par
            
            MaxIterationx <<- out$counts
            populationSize <- 0
            
            
            
            
          }  else if (algoIdx == 4) {
            
            
            
            
            lowerVec <- rep(c(1,1,rmin),ndronesVal)
            upperVec <- rep(c(w,h,rmax),ndronesVal)
            
            
            clusterExport(cl,"ndronesVal")
            clusterExport(cl,"w")
            clusterExport(cl,"h")
            clusterExport(cl,"overlapThreshold")
            clusterExport(cl,"rmin")
            clusterExport(cl,"bsx")
            clusterExport(cl,"bsy")
            clusterExport(cl,"halfTheta")
            clusterExport(cl,"maxDist")
            clusterExport(cl,"areaEmptyRegion")
            
            
            if (useInitSol == 1) {
              #Convert the initial solution to desired format by gensa algo
              InitialsolSuggestion <- matrix(c(initDroneMtx), nrow = 1, ncol = 3*ndronesVal)
              
              tic.clearlog();
              tic("optimParallel Algorithm")  
              
              out <- optimParallel(par = as.vector(initDroneMtx), gr=NULL,
                                   fn = fit_with_weights, wCoverage=wCoveragex, wOverflow=wOverflowx, wOverlap=wOverlapx, wDist=wDistx, wScore=xwScore,
                                   method = "L-BFGS-B",
                                   lower = lowerVec, 
                                   upper = upperVec,
                                   parallel=list(loginfo=TRUE),
                                   control=list(fnscale = -1 ,maxit = MaxIterationx))
              
              timing <- toc(log = TRUE, quiet = TRUE)
              
            }else{
              
              tic.clearlog();
              tic("optimParallel Algorithm")  
              
              out <- optimParallel(gr=NULL,
                                   fn = fit_with_weights, wCoverage=wCoveragex, wOverflow=wOverflowx, wOverlap=wOverlapx, wDist=wDistx, wScore=xwScore,
                                   method = "L-BFGS-B",
                                   lower = lowerVec, 
                                   upper = upperVec,
                                   parallel=list(loginfo=TRUE),
                                   control=list(fnscale = -1 ,maxit = MaxIterationx))
              
              timing <- toc(log = TRUE, quiet = TRUE)
            }
            
            
            log.txt <- tic.log(format = TRUE)
            #cat("\n",log.txt[[1]],"\n",file=textFileName,append=TRUE )
            dstr <- timing$toc - timing$tic
            
            setDefaultCluster(cl=NULL); stopCluster(cl)
            
            secs <- as.numeric(sprintf("%6.3f",dstr)) 
            
            
            #out[c("value","par","counts")]
            
            lx <- length(out$par)
            solpars <- out$par
            
            #input$MaxIteration <- out$counts
            populationSize <- 0
            
            
            
          } else {
            cat("Unlisted algo!\n")
            stop("exit")
          }
          
          
          
          
          
          
          ########################################################################################################
          
          
          
          
          ########################################################################################################
          # Supply the output of one algo to another
          # Like SA is fast then give it to GA
          
          # 
          # library(GA)
          # 
          # #Convert the initial solution to desired format by ga algo
          # 
          # InitialsolSuggestion <- matrix(c(round(solpars)), nrow = 1, ncol = 3*ndronesVal)
          # 
          # 
          # tic.clearlog();
          # tic("GA Algorithm")
          # GA <- ga(type = "real-valued", 
          #          fitness =  func_with_PixelCount,
          #          lower = rep(c(1,1,rmin),ndronesVal), upper = rep(c(w,h,rmax),ndronesVal), parallel = TRUE,
          #          popSize = populationSize, maxiter = input$MaxIteration, optim = TRUE,
          #          suggestions = InitialsolSuggestion)
          # 
          # timing <- toc()
          # 
          # dstr <- timing$toc - timing$tic
          # 
          # secs <- as.numeric(sprintf("%6.3f",dstr))
          # 
          # 
          # 
          # summary(GA)
          # plot(GA)
          # solxyr<-summary(GA)
          # 
          # 
          # lx <- length(solxyr$solution)
          # solpars <- solxyr$solution
          # 
          # 
          
          
          ########################################################################################################
          
          
          
          ###########################################################################################
          #Forward pass for detecting if overlap threshold surpassed
          ###########################################################################################
          # init loop vars
          totCircleArea <- 0
          didx<-1
          
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
          sumCircleArea <- 0
          
          
          for (k in seq(1,lx,3)) {
            
            # Get values from the final solution fo the GA
            x1 <- ceiling(solpars[k])
            y1 <- ceiling(solpars[k+1])
            r1 <- ceiling(solpars[k+2])
            
            
            #Store statistics
            drones$x[didx] <- x1
            drones$y[didx] <- y1
            drones$r[didx] <- r1
            drones$height[didx] <- ceiling(r1/tan((halfTheta*pi/180)))
            drones$forwardOverlap[didx] <-0
            
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
              #cat("More than", overlapThreshold,"percent overlap!",circleOverlap, circlepixelArea,"\n")
              cat("\n*** FW Pass - Circle:",(k+2)/3,"x=",x1,"y=",y1,"r=",r1,"- more than", 
                  overlapThreshold,"percent overlap!",xcircleOverlap, "out of",xcirclepixelArea,"=",round((100 *xcircleOverlap / xcirclepixelArea),1),
                  "\n\n",file=textFileName,append=TRUE)
              xtotBigOverlap <- xtotBigOverlap + xcircleOverlap
              xcntBigOverlap1 <- xcntBigOverlap1 + 1
              drones$forwardOverlap[didx] <- 1
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
            
            
            drones$area[didx] <- xcirclepixelArea
            drones$overlap[didx] <- xcircleOverlap
            drones$overflow[didx] <- xcircleOverflow
            
            
            didx <- didx + 1
            
            
            ###################################################
            
            
            
            
            # Drawing stuff
            region <- EBImage::drawCircle(region, x1, y1, r1, col=1, fill = TRUE)
            showRegion <- EBImage::drawCircle(showRegion, x1, y1, r1, col=1, fill = FALSE)
            tempRegion2 <- EBImage::drawCircle(tempRegion1, x1, y1, r1, col=1, fill = TRUE)
            totCircleArea <- totCircleArea + length(tempRegion2[tempRegion2==1])
            
            
            #The following is for the interactive mode
            #EBImage::display(showRegion, method = "raster", all = TRUE) 
            
          }
          # ###########################################################################################
          # #Misc statistics calculated
          # totCoveredArea <- length(region[region==1])
          # totOverlapArea <- totCircleArea - totCoveredArea
          # CoveredPercentage <- 100 * totCoveredArea / areaEmptyRegion
          # cat("Area:", areaEmptyRegion,"Covered Area:",totCoveredArea,"%",CoveredPercentage,"Overlap:",totOverlapArea,"\n")
          # 
          # drones$regionCoveredArea <- totCoveredArea
          # drones$regionCoveredPercentage <- CoveredPercentage
          # drones$regionOverlap <- totOverlapArea
          # drones$regionOverflow <- sum(drones$overflow)
          # ###########################################################################################
          
          
          ###########################################################################################
          #Misc statistics calculated
          totCoveredArea <- length(region[region==1])
          
          
          #This line overcounts!!!!
          #totOverlapArea <- totCircleArea - totCoveredArea
          totOverlapArea <- xtotOverlap
          
          
          CoveredPercentage <- 100 * totCoveredArea / areaEmptyRegion
          totEmptyArea <- areaEmptyRegion - totCoveredArea
          EmptyPercentage <- 100 * totEmptyArea / areaEmptyRegion
          
          #Convert solpars (1,3*ndronesVal) to dMtx2(ndronesVal*3, 1): 1 row mtx to 1 column mtx!!!
          dMtx2 <- matrix(c(solpars), nrow = ndronesVal*3, ncol = 1)
          
          #Calculate mean radius/height of drones
          xavgr <- mean(dMtx2[c(FALSE, FALSE, TRUE)])
          xavgh <- ceiling(xavgr/tan((halfTheta*pi/180)))
          
          #Calculate mean distance from the Base Station for drones
          # Z for base station is 0!
          xavgDist  <- mapply(function(x){mean(sqrt((x[c(TRUE, FALSE, FALSE)]-bsx)^2 +  
                                                      (x[c(FALSE, TRUE, FALSE)]-bsy)^2 + 
                                                      ((x[c(FALSE, FALSE, TRUE)]/tan(halfTheta*pi/180)) ^2)))}, 
                              as.data.frame(dMtx2))
          
          
          xmedDist  <- mapply(function(x){median(sqrt((x[c(TRUE, FALSE, FALSE)]-bsx)^2 +  
                                                        (x[c(FALSE, TRUE, FALSE)]-bsy)^2 + 
                                                        ((x[c(FALSE, FALSE, TRUE)]/tan(halfTheta*pi/180)) ^2)))}, 
                              as.data.frame(dMtx2))
          
          xsumDist  <- mapply(function(x){sum(sqrt((x[c(TRUE, FALSE, FALSE)]-bsx)^2 +  
                                                     (x[c(FALSE, TRUE, FALSE)]-bsy)^2 + 
                                                     ((x[c(FALSE, FALSE, TRUE)]/tan(halfTheta*pi/180)) ^2)))}, 
                              as.data.frame(dMtx2))
          
          
          #scorePixel <- func_with_PixelCount(dMtx2)
          scorePixelPercent <-fit_with_weights(dMtx2, wCoverage=wCoveragex, wOverflow=wOverflowx, wOverlap=wOverlapx, wDist=wDistx, wScore=xwScore)
          
          
          # cat("\n**********************************************************************************\n",file=textFileName,append=TRUE)
          # cat("*Final Solution Statistics\n",file=textFileName,append=TRUE)
          # cat("**********************************************************************************\n",file=textFileName,append=TRUE)
          
          txtForFinal <- ""
          txtForFinal <- paste("**********************************************************************************")
          txtForFinal <- paste(txtForFinal, "\n*Final Solution Statistics")
          txtForFinal <- paste(txtForFinal,"\n**********************************************************************************")
          txtForFinal <- paste(txtForFinal,"\nRegionArea:(", w,",",h,"):",areaEmptyRegion,"m^2",
                               "\nCoveredArea:",totCoveredArea,"m^2 (",round(CoveredPercentage,2),"% )",
                               "\nxCoveredArea:",xcoveredArea,"m^2 (",round(xcoveredArea/areaEmptyRegion*100,2),"% )",
                               "\nTotCircleArea(with overlap):",totCircleArea,"m^2 (",round(totCircleArea/areaEmptyRegion*100,2),"% )",
                               "\nMean coverage of drones(overlap and overflow not considered):",sumCircleArea/ndronesVal,"m^2",
                               "\nOverlap:",totOverlapArea,"m^2 (",round((totOverlapArea*100/totCoveredArea),2),"% of covered,"
                               ,round((totOverlapArea*100/areaEmptyRegion),2),"% of region )",
                               "\nMean Overlap of drones:",totOverlapArea/ndronesVal,"m^2",
                               "\nOverflow:",xtotOverflow,"m^2 (",round((xtotOverflow*100/totCoveredArea),2),"% of covered,"
                               ,round((xtotOverflow*100/areaEmptyRegion),2),"% of region )",
                               "\nMean Overflow of drones:",xtotOverflow/ndronesVal,"m^2",
                               "\nEmpty:",totEmptyArea ,"m^2 (",round(EmptyPercentage,2),"% of region )",
                               "\nMean Radius of drones:",xavgr,"m",
                               "\nMean Height of drones:",xavgh,"m",
                               "\nTotal Distance of drones from BS(", bsx,",",bsy,", 0 ):",xsumDist,"m",
                               "\nMax Distance a drone from BS(", bsx,",",bsy,", 0 ):",maxDist,"m",
                               "\nMean Distance of drones from BS(", bsx,",",bsy,", 0 ):",xavgDist,"m",
                               "\nMedian Distance of drones from BS(", bsx,",",bsy,", 0 ):",xmedDist,"m",
                               #"\nPixel Score:", scorePixel,
                               "\nPercent Score:", scorePixelPercent,
                               "\nWeights  (C, Olap, Oflow, Dist, Score) = (",wCoveragex, wOverlapx, wOverflowx, wDistx, xwScore,")",
                               "\nPercents (C, Olap, Oflow, Dist) = (",round(CoveredPercentage,2), round((totOverlapArea*100/totCoveredArea),2), round((xtotOverflow*100/totCoveredArea),2), round((100 * (maxDist*ndronesVal - xsumDist) / (maxDist*ndronesVal)), 2),")",
                               "\n"
          )
          
          txtForFinal <- paste(txtForFinal,"\n**********************************************************************************\n")
          
          
          drones$regionCoveredArea <- totCoveredArea
          drones$regionCoveredPercentage <- CoveredPercentage
          drones$regionOverlap <- totOverlapArea
          drones$regionOverflow <- sum(drones$overflow)
          
          ###########################################################################################
          
          
          
          
          ###########################################################################################
          #Backward pass for detecting if overlap threshold surpassed
          ###########################################################################################
          # init loop vars
          totCircleArea <- 0
          didx<-ndronesVal
          
          xtemplateOverlap <- matrix(0,nrow=w, ncol=h)
          xtemplateOverflow <- matrix(0,nrow=w, ncol=h)
          xtemplateForCircle <- matrix(0,nrow=w, ncol=h)
          
          
          xcoveredArea <- 0
          xprecoveredArea <- 0
          xtotOverflow <- 0
          xtotOverlap <- 0
          xtotBigOverlap <- 0
          xcntBigOverlap2 <- 0
          
          for (k in seq(lx,1,-3)) {
            
            # Get values from the final solution fo the GA
            x1 <- ceiling(solpars[k-2])
            y1 <- ceiling(solpars[k-1])
            r1 <- ceiling(solpars[k])
            
            
            #Drone statistics is already saved in the forward pass before
            #Only update is necessary to overlap threshold
            drones$backwardOverlap[didx] <-0
            
            # Same code from the fitness function
            #####################################################
            # Draw the circle on empty region on the original position to see how many pixels
            # overflowed
            xnonOverflowedRegion <- EBImage::drawCircle(xtemplateOverflow, x1, y1, r1, col=1, fill = TRUE)
            
            # Draw the circle in the middle, (assuming that r will be always less than the half width and half height)
            # then count pixels to find out area in pixels
            xcircleAreaRegion <- EBImage::drawCircle(xtemplateForCircle, ceiling(w/2), ceiling(h/2), r1, col=1, fill = TRUE)
            
            xcirclepixelArea <- length(xcircleAreaRegion[xcircleAreaRegion==1])
            xnonOverflowedArea <- length(xnonOverflowedRegion[xnonOverflowedRegion==1])
            xcircleOverflow <- xcirclepixelArea - xnonOverflowedArea
            
            
            xtotOverflow <- xtotOverflow + xcircleOverflow
            
            xtemplateOverlap <- EBImage::drawCircle(xtemplateOverlap, x1, y1, r1, col=1, fill = TRUE)
            xcoveredArea <- length(xtemplateOverlap[xtemplateOverlap==1])
            
            
            # To calculate overlap that comes from that circle:
            # First findout the theoretical contribution of the circle = How many pixels would be added
            # Count actually added pixels and
            # then subtract the overflow if the center is close to edges of the region
            
            xcircleOverlap <- (xprecoveredArea + xcirclepixelArea) - xcoveredArea - xcircleOverflow
            
            
            
            
            
            # If the circle is overlapped more than a threshold, give high penalty!!!
            # But when the bigger circle on top of  small circle the overlap will be less than its overlapThreshold
            # On the other hand poor small circle will be covered totally more than threshold probably
            # So order is important then!
            # Sorting helps
            # or two pass: first to last and then last to first passes, like scanning!
            if (xcirclepixelArea <= (100 / overlapThreshold * xcircleOverlap) ) {
              #DEBUG
              #cat("More than", overlapThreshold,"percent overlap!",circleOverlap, circlepixelArea,"\n")
              cat("\n *** BW Pass - Circle:",k/3,"x=",x1,"y=",y1,"r=",r1,"- more than", 
                  overlapThreshold,"percent overlap!",xcircleOverlap, "out of",xcirclepixelArea,"=",round((100 *xcircleOverlap / xcirclepixelArea),1),
                  "\n\n",file=textFileName,append=TRUE)
              xtotBigOverlap <- xtotBigOverlap + xcircleOverlap
              xcntBigOverlap2 <- xcntBigOverlap2 + 1
              drones$forwardOverlap[didx] <- 1
            }
            
            
            
            xprecoveredArea <- xcoveredArea
            
            xtotOverlap <- xtotOverlap + xcircleOverlap
            
            # In ga (parallel) this trace output is not possible!!
            # #BEGIN DEBUG
            #cat(round,"Circle",(k-1)/3+1, "at",x1,y1,"r=",r1, "pixel area",circlepixelArea, "areacalc",circleArea,"has overflow area of",circleOverflow, "and overlapped",circleOverlap,"covered so far",coveredArea,"\n")
            #cat("Circle",(k-1)/3+1, "at",x1,y1,"r=",r1, "pixel area",circlepixelArea, "areacalc",circleArea,"has overflow area of",circleOverflow, "and overlapped",circleOverlap,"covered so far",coveredArea,"\n")
            # #END DEBUG
            
            
            
            
            didx <- didx - 1
            
            
            ###################################################
            
            
          }
          
          ###########################################################################################
          
          
          
          
          
          ###########################################################################################
          #Results for saving
          ###########################################################################################
          
          percent <- round(CoveredPercentage,2)
          #Save to png the drawing with drone numbers at centers
          # fname<-paste0(algoName,"drones=",ndronesVal,"_",input$xwCoverage,"_",input$xwOverflow,"_",input$xwOverlap,"_",input$xwDist,"_covered=",percent,"_pop=",populationSize,"_iter=",input$MaxIteration,"_secs=",secs)
          # ext<-"png"
          # fname<-paste(fname, ext, sep = ".")
          # png(filename = fname, width = w, height = h)
          
          
          
          EBImage::display(showRegion, method = "raster", all = TRUE) 
          
          
          # #Save to png the drawing with only circles if you like
          # fname<-paste0(algoName,ndronesVal,"drones-",percent,"covered-",populationSize,"pop-",input$MaxIteration,"iter")
          # ext<-"png"
          # fname<-paste(fname, ext, sep = ".")
          # dev.print(png, filename = fname , width = dim(region)[1], height = dim(showRegion)[2])
          
          
          
          txtForFinal <- paste(txtForFinal,"\n**********************************************************************************")
          txtForFinal <- paste(txtForFinal,"\n*Final Drone Positions")
          txtForFinal <- paste(txtForFinal,"\n**********************************************************************************\n")
          #Put the drone numbers onto centers on the drawing
          for (k in seq(1,lx,3)) {
            x1 <- ceiling(solpars[k])
            y1 <- ceiling(solpars[k+1])
            r1 <- ceiling(solpars[k+2])
            text(x1,y1,sprintf("%d", (((k-1)/3) + 1)), col = 0)
            h1= ceiling(r1/tan((halfTheta*pi/180)))
            distix <- sqrt((x1-bsx)^2+(y1-bsy)^2+(h1)^2)
            
            txtForFinal <- paste(txtForFinal,sprintf("Drone %3d at (x, y) = (%3d, %3d)  r = %3d  height = %3d  dist = %6.3f\n",(((k-1)/3) + 1),x1,y1,r1,h1,distix))
            
          }
          txtForFinal <- paste(txtForFinal,"\n**********************************************************************************\n")
          output$msgFinal<- renderText({ txtForFinal })
          # dev.off()
          # 
          # 
          # #Save the drone info to a excel or cvs file
          # fname<-paste0(algoName,"drones=",ndronesVal,"_",input$xwCoverage,"_",input$xwOverflow,"_",input$xwOverlap,"_",input$xwDist,"_covered=",percent,"_pop=",populationSize,"_iter=",input$MaxIteration,"_secs=",secs)
          # #fname<-paste0(algoName,ndronesVal,"drones_(",input$xwCoverage,")_(",input$xwOverflow,")_(",input$xwOverlap,")_(",input$xwDist,")-",percent,"covered-",populationSize,"pop-",input$MaxIteration,"iter-",secs,"secs")
          # ext<-"csv"
          # fname<-paste(fname, ext, sep = ".")
          # write.csv(drones, file = fname )
          ########################################################################################################
          
          
          
          ####################################################################
          #END CovPlot stuff
          ####################################################################
          
        })#END of output$CovPlot <- renderPlot
        ########################################################################################################
        
        
      })#END of isolate
      ########################################################################################################
      
      
      ####################################################################
      #END initSolnPlot stuff
      ####################################################################
      
    })#END of output$initSolnPlot
    ########################################################################################################
    
    
  })#END of observeEvent-input$runSim
  ########################################################################################################
  
  
}#ENDof server
########################################################################################################
