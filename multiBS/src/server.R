########################################################################################################

server <- function(input, output, session) {
  
  
  #hmed <- floor (hmin + input$hPercentx / 100 * (hmax - hmin))
  
  output$DroneNumberMsg <- renderText({ paste0(totDroneReq," drones are required at ",input$DroneAlt, " meter.")})
  
  ########################################################################################################
  #Initially disable these
  ########################################################################################################
  #The initial polygon is already there so disable startPoly
  updateSwitchInput(
    session = session,
    inputId = "startPoly",
    disabled = TRUE,
    value = FALSE,
  )
  
  updateSwitchInput(
    session = session,
    inputId = "startBS",
    disabled = FALSE,
    value = TRUE
  )
  
  disable("closePolygon")
  disable("showVoronoi")
  
  #DEBUG for the time being disable these
  disable("dl")
  disable("saveMap")
  disable("findCov")
  disable("showInitSoln")
 
  
  
  
  #Reactive values
  mapToSave <- reactiveValues()
  inputDroneAlt <- reactiveValues()
  inputDroneNum <- reactiveValues()
  ########################################################################################################
  
  
  
  ########################################################################################################
  observeEvent(input$DroneAlt, {
    totDroneReq <<- round(areaPolygon(regionPoly[[1]])/ (pi * (input$DroneAlt  * tan(halfTheta * pi / 180)) * (input$DroneAlt * tan(halfTheta * pi / 180))))
    
    output$DroneNumberMsg <- renderText({ paste0(totDroneReq," drones are required at ",input$DroneAlt, " meter.")})
    cat("Region area is",areaPolygon(regionPoly[[1]]),"meter^2\n")
    cat("ESTIMATED DRONES REQ AT",input$DroneAlt,"meter is", totDroneReq,"\n")
    updateNumericInput(
      session,
      "DroneNumber",
      value = totDroneReq
    )
    
  }) 
  ########################################################################################################
  
  ########################################################################################################
  observeEvent(input$DroneNumber, {
    
    rPerDrone <- sqrt((areaPolygon(regionPoly[[1]]) / input$DroneNumber) / pi )
    
    altPerDrone <- ceiling(rPerDrone / tan(halfTheta *pi / 180))
    
    updateSliderInput(
      session,
      "DroneAlt",
      value = altPerDrone
    )
    totDroneReq <<- round(areaPolygon(regionPoly[[1]]) / (pi * (input$DroneAlt  * tan(halfTheta * pi / 180)) * (input$DroneAlt * tan(halfTheta * pi / 180))))
    
    output$DroneNumberMsg <- renderText({ paste0(totDroneReq," drones are required at ",input$DroneAlt, " meter.")})
    cat("Region area is",areaPolygon(regionPoly[[1]]),"meter^2\n")
    cat("ESTIMATED DRONES REQ AT",input$DroneAlt,"meter is", totDroneReq,"\n")
    
  }) 
  ########################################################################################################
  
  
  ########################################################################################################
  observeEvent(input$saveMap,{
    
    cat("Saving map.\n")
    mapshot(mapToSave$current, file = paste0(getwd(), "/map.png"))
    #output$myMsg <- renderText({ paste("map.png saved to:",getwd(),"\n") })
    # 2. specify size of map based on div size
    #vwidth = input$dimension[1], vheight = input$dimension[2])
  })
  ########################################################################################################
  
  
  
  ########################################################################################################
  # create download
  output$dl <- downloadHandler(
    filename = "map.png",
    
    content = function(file) {
      mapshot(mapToSave$current, file = file)
      # 2. specify size of map based on div size
      #vwidth = input$dimension[1], vheight = input$dimension[2])
    },
    output$myMsg <- renderText({ paste("map.png downloaded to:",getwd(),"\n") })
  )
  ########################################################################################################
  
  
  
  ########################################################################################################
  output$mouseCoord <- renderText({
    if(is.null(input$hover_coordinates)) {
      "Mouse outside of map"
    } else {
      paste0("Lat: ", round(input$hover_coordinates[1],3), 
             "\nLng: ", round(input$hover_coordinates[2],3))
    }
  })
  ########################################################################################################
  
  
  
  ########################################################################################################
  observeEvent(input$clearSelectionRegion,{
    cat("Reset select\n")
    #Disable startBS
    updateSwitchInput(
      session = session,
      inputId = "startBS",
      disabled = TRUE,
      value = FALSE,
    )
    bsBEGIN <<- FALSE
    
    #Enable startPoly for new region but let user to switch it on
    updateSwitchInput(
      session = session,
      inputId = "startPoly",
      disabled = FALSE, 
      value = FALSE,
    )
    polyBEGIN <<- FALSE
    
    disable("closePolygon")
    disable("showVoronoi")
    disable("findCov")
    disable("showInitSoln")
    disable("DroneAlt")
    
    updateNumericInput(
      session,
      "DroneNumber",
      min = 1,
      value = 1
    )
    #output$OutDroneNumber <- renderText({ input$DroneNumber })
    output$DroneNumberMsg <- renderText({""})
    disable("DroneNumber")
    
    
    isolate({
      new_zoom <- 9
      if(!is.null(input$mymap_zoom)) new_zoom <- input$mymap_zoom
      output$myMsg <- renderText({paste("Cleared Selection: Lat:", round(input$mymap_center$lat,2),"Lng:", round(input$mymap_center$lng,2),"Zoom:",new_zoom, "\n")})
      output$clickMsg<-renderText({ paste("Cleared Selection.\n") })
      leafletProxy("mymap") %>%  clearMarkers() %>% clearShapes()
      
      #Like this update also the map for saving
      mapToSave$current <- mapToSave$base %>%  clearMarkers() %>% clearShapes()
      
      #Init all data
      regionPolyDF <<- NULL
      regionVtxMtx <<- NULL
      BSMtx <<- NULL
      BSList <<- list()
      
      nBS <<- 0 
      
      regionPoly <<-  st_polygon(list())
      regionPolyBBox <<- st_bbox(regionPoly)
      regionPolyArea <<- 0
      
      linesBS2VBS <<- list()
      VBSList <<- list()
      VBSMtx <<- NULL
      
      nVoronoiPoly <<- 0
      
      
      VoronoiPolyList <<- list()
      VoronoiPolyBBoxList <<- list()
      furthestPtsinPoly <<- list()
      MaxDist3DinPoly <<- list()
      
      
    })
    
    
  })
  ########################################################################################################
  
  
  
  #######################################################################################################
  observeEvent(input$showVoronoi,{
    cat("Voronoi\n")
    enable("DroneAlt")
    if (input$startBS) {
      
      #if it was startBS mode that you need to check how many BS selected
      
      if (nBS >= 1) {
        
        
        if (nBS == 1){
          output$myMsg <- renderText({paste("Single BS, no Voronoi Tess. is necessary\n")})
        }else{  
          output$myMsg <- renderText({paste(nBS,"BS, Voronoi Tess. is necessary\n")})
        }
        updatedValue = !input$startBS
        
        updateSwitchInput(
          session = session,
          inputId = "startBS",
          value = updatedValue,
          disabled = TRUE
        )
        
        
        ##################################
        #Here do the voronoi stuff
        ##################################
        
        #Lines from BS to VBS
        ##################################
        for (i in 1:nBS){
          #Find the point nearest to the BS on the edge of the polygon which will be VBS loc
          linesBS2VBS[[i]] <<- st_nearest_points(BSList[[i]],regionPoly)
          VBSList[[i]] <<- st_point(c(linesBS2VBS[[i]][[1]][2,1], linesBS2VBS[[i]][[1]][2,2]))
          VBSMtx <<- rbind(VBSMtx, c(linesBS2VBS[[i]][[1]][2,1], linesBS2VBS[[i]][[1]][2,2]))
          
          
          #we need a line from BSMtx[i] to VBSMtx[i]
          df <- as.data.frame(rbind(BSMtx[i,], VBSMtx[i,]))
          
          
          leafletProxy("mymap") %>% 
            addPolylines(data = df,
                         lng = ~ V1, 
                         lat = ~ V2,
                         color = "green",
                         weight = 3)
          
          
          mapToSave$current <- mapToSave$base %>% 
            addPolylines(data = df,
                         lng = ~ V1, 
                         lat = ~ V2,
                         color = "green",
                         weight = 3)
        }
        
        #Place Drones as VBS
        ##################################
        leafletProxy("mymap") %>% 
          addMarkers(data = as.data.frame(VBSMtx),
                     lng = ~ V1, 
                     lat = ~ V2,
                     icon = myVBSIcon)  
        
        
        mapToSave$current <- mapToSave$base %>% 
          addMarkers(data = as.data.frame(VBSMtx),
                     lng = ~ V1, 
                     lat = ~ V2,
                     icon = myVBSIcon)  
        
        
        
        
        #Voronoi Tess. stuff
        ##################################
        if (nBS == 1){
          nVoronoiPoly <<- 1
          VoronoiPolyList[[1]] <<- regionPoly
          VoronoiPolyBBoxList[[1]] <<- st_as_sfc(st_bbox(VoronoiPolyList[[1]]))
          #Convert to  polylines mtx and put on map
          #plot(VoronoiPolyList[[i]], add=TRUE, col = hue[i])
          leafletProxy("mymap") %>% 
            addPolylines(data = as.data.frame(VoronoiPolyList[[1]][1]),
                         lng = ~ X1, 
                         lat = ~ X2,
                         color = "red",
                         weight = 3)
          
          
          
          areaPolyX <- st_area(VoronoiPolyList[[1]])
          dronesPerVBSList[[1]] <<- round(input$DroneNumber * areaPolyX / regionPolyArea)
          #TODO Handle the case of single drone!!!
          cat("DRONE ASSIGNMENT ACCORDING TO GIVEN NUMBER:",dronesPerVBSList[[1]], "drones given to poly",1,"out of",input$DroneNumber,"\n")
          
          #hmed <- floor (hmin + input$hPercent / 100 * (hmax - hmin))
          totDroneReq <<- round(regionPolyArea / (pi * (input$DroneAlt * tan(halfTheta * pi / 180)) * (input$DroneAlt * tan(halfTheta * pi / 180))))
          estDroneReq <- round(areaPolyX / (pi * (input$DroneAlt * tan(halfTheta * pi / 180)) * (input$DroneAlt * tan(halfTheta * pi / 180))))
          cat("ESTIMATED DRONE ASSIGNMENT ACCORDING TO ALT",input$DroneAlt,"meter:",estDroneReq, "drones required to poly",i,"out of",totDroneReq,"\n")
          
          #Put label of the polygon with opposite color on the centroid
          centroXY <- st_centroid(VoronoiPolyList[[1]])
          opCol <- paste0("#" , toupper(sprintf("%x", (0xFFFFFF - as.numeric(stringr::str_replace(gplots::col2hex(hue[1]), "#", "0x"))))))
          l <- stringr::str_length(opCol)
          zerosAtEnd <- 7 - l
          opCol <- paste0(opCol, paste(rep("0", zerosAtEnd), sep="", collapse=""))
          #DEBUG cat(opCol,"\n")              
          #text(centroXY[[1]], centroXY[[2]], sprintf("%d", i), col = opCol)
          
          
          #Find the furthest distance to that VBS point
          #We will use it to estimate the distance percentages
          #Convert sf to spatial
          y <- as(VoronoiPolyList[[1]], "Spatial")
          #Convert spatial to owin object so that we can use distances func
          yowin <- polyCub::as.owin.SpatialPolygons(y)
          #Get vertices of the polygon
          vtx <- vertices(yowin) # Polygon vertices
          #Find the distances from the VBS to all the vertices of the poly
          #Here fudging for simplicity?
          #Any point on the edge can be at max distance?
          d2D <- crossdist(vtx$x, vtx$y, VBSList[[1]][1],  VBSList[[1]][2]) # n-column matrix of cross distances
          i1 <- which.max(d2D[,1]) # Index of max dist point
          
          #DEBUG points(vtx$x[i1], vtx$y[i1], col = "red", cex = 1.5)
          
          #For drawing in 2D
          furthestPtsinPoly[[1]] <<- st_point(c(vtx$x[i1], vtx$y[i1]))
          #For calculating in 3D
          MaxDist3DinPoly[[1]] <<- sqrt ((d2D[i1] * d2D[i1]) + (hmax * hmax)) 
          
          #DEBUG segments(VBSList[[i]][1],VBSList[[i]][2],furthestPtsinPoly[[i]][1],furthestPtsinPoly[[i]][2], add = TRUE, col = opCol )
          
          
          #DEBUG points(furthestPtsinPoly[[i]][1], furthestPtsinPoly[[i]][2], col = opCol, pch=9+i, cex = 1.5)
          
          
        }else {
          
          dxy <- deldir(x = VBSMtx[,1], y = VBSMtx[,2], 
                        rw = c(regionPolyBBox$xmin, regionPolyBBox$xmax, regionPolyBBox$ymin, regionPolyBBox$ymax))
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
            
            
            
            #Convert to  polylines mtx and put on map
            #plot(VoronoiPolyList[[i]], add=TRUE, col = hue[i])
            leafletProxy("mymap") %>% 
              addPolylines(data = as.data.frame(VoronoiPolyList[[i]][1]),
                           lng = ~ X1, 
                           lat = ~ X2,
                           color = "red",
                           weight = 3)
            
            
            
            areaPolyX <- st_area(VoronoiPolyList[[i]])
            dronesPerVBSList[[i]] <<- round(input$DroneNumber * areaPolyX / regionPolyArea)
            #TODO Handle the case of single drone!!!
            cat("DRONE ASSIGNMENT ACCORDING TO GIVEN NUMBER:",dronesPerVBSList[[i]], "drones given to poly",i,"out of",input$DroneNumber,"\n")
            
            
            totDroneReq <<- round(regionPolyArea / (pi * (input$DroneAlt * tan(halfTheta * pi / 180)) * (input$DroneAlt * tan(halfTheta * pi / 180))))
            estDroneReq <- round(areaPolyX / (pi * (input$DroneAlt * tan(halfTheta * pi / 180)) * (input$DroneAlt * tan(halfTheta * pi / 180))))
            cat("ESTIMATED DRONE ASSIGNMENT ACCORDING TO ALT",input$DroneAlt,"meter:",estDroneReq, "drones required to poly",i,"out of",totDroneReq,"\n")
            
          
            
            #Put label of the polygon with opposite color on the centroid
            centroXY <- st_centroid(VoronoiPolyList[[i]])
            opCol <- paste0("#" , toupper(sprintf("%x", (0xFFFFFF - as.numeric(stringr::str_replace(gplots::col2hex(hue[i]), "#", "0x"))))))
            l <- stringr::str_length(opCol)
            zerosAtEnd <- 7 - l
            opCol <- paste0(opCol, paste(rep("0", zerosAtEnd), sep="", collapse=""))
            #DEBUG cat(opCol,"\n")              
            #text(centroXY[[1]], centroXY[[2]], sprintf("%d", i), col = opCol)
            
            
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
        
        
        
        
        
        
        
        
        
        
        
        
        ##################################
        
        #TODO
        enable("showInitSoln")
        #for more than one BS
      }else{
        output$myMsg <- renderText({paste("Select at least one BS pos!!!\n")})
      }
      
      
    }else {
      output$myMsg <- renderText({paste("Not in start bs mode!!!\n")})
    }
  })
  #######################################################################################################
  
  
  
  #######################################################################################################
  observeEvent(input$showInitSoln,{
    #Find initial coverage and print stats
    
    #TODO
    #Crashes when there is a single BS and no need for Voronoi!!!
    #Handle the case for a single BS!!!
    
    for (i in 1:nVoronoiPoly) {
      cat(dronesPerVBSList[[i]],"drones for BS no",i,"\n")
    }
    
    initSolMtx <- list()
    create_initial_soln()
    
    cat("*****************************Initial soln created!\n")
    
    
    
    ##################################################
    #Put drone config from all polygons to a single list, "dronePosMtx"
    ##################################################
    idxDf <- NULL
    dronePosMtx <- NULL
    for (pIdx in 1:nVoronoiPoly) {
      initSolMtx[[pIdx]] <- matrix(0, ncol = 3 * dronesPerVBSList[[pIdx]], nrow = 1)
      for (dIdx in 1:dronesPerVBSList[[pIdx]]) {
        #The only way is to find the nearest point to the centroid and calculate distance
        #basically get the first point on the circle and find the distance between centroid (center)
        ptsOnCircle <- st_point(c(dronesInPolyList[[pIdx]][[dIdx]][[1]][1,1], dronesInPolyList[[pIdx]][[dIdx]][[1]][1,2]))
        cntrCircle <- st_centroid(dronesInPolyList[[pIdx]][[dIdx]])
        st_nearest_points(st_centroid(dronesInPolyList[[pIdx]][[dIdx]]), dronesInPolyList[[pIdx]][[dIdx]])
        rad <- st_distance(ptsOnCircle,cntrCircle)
        cat("--- Poly",pIdx,"drone",dIdx,"radius:",rad,"meter \n")
        
        dronePosMtx <- rbind(dronePosMtx,c(cntrCircle[1], cntrCircle[2], rad))
        initSolMtx[[pIdx]][(3 * (dIdx - 1)) : (3 * dIdx)] <- c(cntrCircle[1], cntrCircle[2], rad)
        idxDf <- rbind(idxDf, c(pIdx, dIdx))
        
      }
    }
    ##################################################
    
    
    ##################################################
    cat("*****************************Drawing initial soln\n")
    
    
    
    ##################################################
    dfd <- as.data.frame(dronePosMtx)
    dfd <- cbind(dfd,idxDf)
    colnames(dfd) <- c("lon","lat","rad","bidx","didx")
    
    
    dfb <- as.data.frame(BSMtx)
    dfb <- cbind(dfb,seq(1:length(dfb$V1)),as.matrix(unlist(dronesPerVBSList)))
    colnames(dfb)<-c("lon","lat","idx","ndrones")
    
    dfv <- as.data.frame(VBSMtx)
    dfv  <- cbind(dfv,seq(1:length(dfv$V1)),as.matrix(unlist(dronesPerVBSList)))
    colnames(dfv)<-c("lon","lat","idx","ndrones")
    
    #hmed <- floor (hmin + input$hPercent / 100 * (hmax - hmin))
    
    leafletProxy("mymap") %>% 
      addCircles(data = dfd,
                 lng = ~ lon, 
                 lat = ~ lat,
                 color = "blue",
                 radius = ~ rad*111111,
                 stroke = TRUE, 
                 opacity = 5,
                 weight = 1,
                 fillColor = "blue",
                 fillOpacity = 0.5,
                 group = "initSoln"
                 #layerId = "initCircles",
      ) %>%
      
      addMarkers(data = dfb,
                 lng = ~ lon, 
                 lat = ~ lat,
                 icon = myBSIcon,
                 #layerId = "initBSIcons", 
                 group = "initSoln",
                 popup = paste0("BS",dfb$idx,"-",dfb$ndrones,"drones<br>","Long:", dfb$lon, "<br>", "Lat:", dfb$lat, "<br>")
      ) %>%
      
      addMarkers(data = dfd,
                 lng = ~ lon, 
                 lat = ~ lat,
                 icon = myDroneIconSmall, 
                 #layerId = "initDroneIcons",
                 group = "initSoln",
                 #popup = ~htmlEscape("lat=",~V2)) %>%
                 popup = paste0("BS",dfd$bidx,"-Drone", dfd$didx, "<br>","Long:", dfd$lon, "<br>", 
                                "Lat:", dfd$lat, "<br>", "Alt:",
                                ceiling((dfd$rad * 111111) / tan(halfTheta * pi / 180) ), "m<br>")
      ) %>%
      
      addMarkers(data = dfv,
                 lng = ~ lon, 
                 lat = ~ lat,
                 icon = myVBSIcon, 
                 group = "initSoln",
                 #layerId = "initVBSIcons",
                 popup = paste0("VBS",dfv$idx,"-",dfv$ndrones,"drones<br>","Long:", dfv$lon, "<br>", "Lat:", dfv$lat, "<br>", "Alt:", input$DroneAlt, "m<br>")
      )
    
    #Lines from VBS to drone centroid
    ##################################
    for (vbsIdx in 1:nBS){
      for (droneIdx in 1:dronesPerVBSList[[vbsIdx]]) {
        #Find the centriod of the drone
        cntrCircle <- st_centroid(dronesInPolyList[[vbsIdx]][[droneIdx]])
        #put the end points to df
        df <- as.data.frame(rbind(VBSMtx[vbsIdx,], c(cntrCircle[1], cntrCircle[2])))
        #draw the line on the map
        leafletProxy("mymap") %>% 
          addPolylines(data = df,
                       lng = ~ V1, 
                       lat = ~ V2,
                       color = "green",
                       group = "initSoln",
                       #layerId = "initVBS2DroneLines",
                       weight = 3)
        
        
        mapToSave$current <- mapToSave$base %>% 
          addPolylines(data = df,
                       lng = ~ V1, 
                       lat = ~ V2,
                       color = "green",
                       weight = 3)
      }
      
    }
    ##################################################
    
    
    
    
    
    ##################################################
    #DEBUG Solution
    plot(regionPoly)
    for (pIdx in 1:nVoronoiPoly) {
      initSolMtx <- dronesInPolyList2DroneXYRVec(dronesInPolyList[[pIdx]], dronesPerVBSList,pIdx)
      initScore <- fit_with_weights(initSolMtx,wCoverage=xwCoverage, wOverflow=xwOverflow,
                                    wOverlap=xwOverlap, wDist=xwDist, wScore=xwScore, pIdx)
      
      cat ("InitSoln: The score for poly",pIdx,"is:",initScore,"\n")
      plot(VoronoiPolyList[[pIdx]], add=TRUE, col = hue[pIdx])
      #BUG
      #my_show_dronesInPolyList_stat(regionPoly, dronesInPolyList[[pIdx]], unlist(dronesPerVBSList),pIdx)
      my_show_dronesInPolyList_stat(VoronoiPolyList[[pIdx]], dronesInPolyList[[pIdx]], unlist(dronesPerVBSList),pIdx)
    }
    #DEBUG Solution
    ##################################################
    
    enable("findCov")
  })
  
  
  
  
  #######################################################################################################
  
  
  
  
  
  #######################################################################################################
  observeEvent(input$findCov,{
    #Find coverage and print stats
    #TODO: For the algo later let the user choose it and the paramaters like running time, maxiter, etc...
    #disable this and enable it after showInitSoln
    
    #Data to hold the soln
    solMtx <<- list()
    solCircles <<- list()
    
    
    ##################################################
    #Clear Initial soln and start plotting the soln from algo
    ##################################################
    
    leafletProxy("mymap")   %>%  hideGroup("initSoln")
    
    
    
    
    
    ######################################## DEoptim BEGIN ########################################
    
    
    cat("Coverage running\n")
    cat("Using deoptim algortihm - Minimizing - Does not accept initial solution - Tolerance is 0.0001\n")
    #DEoptim minimizes
    xwScore=-1.0
    MaxIteration <- 100 #The maximum iteration (population generation) allowed. Default is 200.
    
    
    
    
    # ######################################################################
    # # Can we make parallel execution for each region with %dopar%?
    # # One cluster per region?
    # numCores <- nVoronoiPoly
    # # Example registering clusters
    # myCluster <- parallel::makeCluster(numCores)
    # clusterExport(cl=myCluster, envir=.GlobalEnv)
    # doParallel::registerDoParallel(myCluster)   
    # ######################################################################
    # 
    # ##################################### BEGIN Parallel Loop ##############################################
    # deployment <- foreach(i = 1:numCores, .combine=c) %dopar% {
    #   #Do here the optimization loop
    #   #After that output the resulting optimum deployment drone array
    #   #Do not forget to prefix it with the polygon index
    # }
    # parallel::stopCluster(myCluster)
    # ####################################### END Parallel Loop ##############################################
    # 
    
    
    tic.clearlog();
    tic("Coverage")
    
    ##################################################
    #DEBUG Solution
    plot(regionPoly)
    for (pIdx in 1:nVoronoiPoly) {
       plot(VoronoiPolyList[[pIdx]], add=TRUE, col = hue[pIdx])
     }
    #DEBUG Solution
    ##################################################
    
    
    for (k in 1:nVoronoiPoly) {
      
      ndrones <- dronesPerVBSList[[k]]
      
      
      if (ndrones > 1){
        
        
        dfPts <- getPtsInPoly(VoronoiPolyList[[k]], gridSize / 111111)
        
        lowerVec <- rep(c(min(dfPts$inX), min(dfPts$inY), min(rValues / 111111)),ndrones)
        upperVec <- rep(c(max(dfPts$inX), max(dfPts$inY), max(rValues / 111111)),ndrones)
        
        polyIdx <<- k
        cat("\n\n***********************************************************\n")
        cat("Solving for poly",k,"with",ndrones,"drones:\n")
        # Run the  Algorithm
        
        
        
        # ######################################## optimParallel BEGIN ######################################## 
        # 
        # imtx <- dronesInPolyList2DroneXYRVec(dronesInPolyList[[k]], dronesPerVBSList, k)
        # #     
        # #     #Convert the initial solution to desired format by ga algo
        # InitialsolSuggestion <- as.vector(imtx)
        # 
        # 
        # #create cluster with 6 cores
        # cl <- parallel::makeCluster(12)
        # setDefaultCluster(cl=cl) # set 'cl' as default cluster
        # 
        # parallel::clusterExport(cl,varlist=list("VoronoiPolyList",
        #                                         "overlapThreshold",
        #                                         "VBSList",
        #                                         "halfTheta",
        #                                         "hmed",
        #                                         "MaxDist3DinPoly",
        #                                         "fit_with_weights",
        #                                         "CreateCirclePolyMeter",
        #                                         "st_polygon",
        #                                         "st_area",
        #                                         "st_intersection",
        #                                         "st_union",
        #                                         "st_difference",
        #                                         "st_distance",
        #                                         "st_centroid",
        #                                         "point.in.polygon",
        #                                         "polyIdx",
        #                                         "xwCoverage","xwOverflow","xwOverlap","xwDist","xwScore"),
        #                         envir=environment()         )
        # 
        # 
        # tic.clearlog();
        # tic("optimParallel Algorithm")
        # 
        # opt <- optimParallel(par =  InitialsolSuggestion,
        #                      fn = function(xxx) fit_with_weights(xxx, wCoverage=xwCoverage, wOverflow=0,
        #                                                          wOverlap=0, wDist=1, wScore=xwScore, polyIdx),
        #                      gr = NULL,
        #                      
        #                      method = "L-BFGS-B", 
        #                      lower = lowerVec,
        #                      upper = upperVec,
        #                      control = list(maxit = MaxIteration,
        #                                     trace = TRUE)
        # )
        # 
        # 
        # 
        # 
        # timing <- toc(log = TRUE, quiet = TRUE)
        # dstr <- timing$toc - timing$tic
        # secs <- as.numeric(sprintf("%6.3f",dstr))
        # cat("Poly",k,"soln in", secs, "secs\n")
        # 
        # parallel::stopCluster(cl)
        # 
        # lx <- length(opt$par)
        # solMtx[[k]] <<- opt$par
        # optScore <- opt$value
        # 
        # solCircles[[k]] <<- DroneXYRVec2dronesInPolyList(solMtx[[k]])
        # my_show_DroneXYRVec_stat(solMtx[[k]])
        # my_show_dronesInPolyList_stat(VoronoiPolyList[[k]], solCircles[[k]], dronesPerVBSList, k)
        # score <- fit_with_weights(solMtx[[k]],wCoverage=xwCoverage, wOverflow=xwOverflow,
        #                           wOverlap=xwOverlap, wDist=xwDist, wScore=xwScore, k )
        # 
        # cat ("The score is:",score,"score from optimParallel:",optScore,"\n")
        # vbsCntr <- VBSList[[k]]
        # for(i in 1:ndrones){
        #   plot(solCircles[[k]][[i]], add=TRUE)
        #   centroXY <- st_centroid(solCircles[[k]][[i]])
        #   segments(vbsCntr[1], vbsCntr[2], centroXY[1], centroXY[2])
        # }
        # 
        # ######################################## optimParallel END ########################################
        
        #################################################################################################
        
        
        ######################################## DEoptim BEGIN ########################################
        
        # parallel::clusterExport(cl,varlist=list("VoronoiPolyList",
        #                                      "overlapThreshold",
        #                                      "VBSList",
        #                                      "halfTheta",
        #                                      "hmed",
        #                                      "MaxDist3DinPoly",
        #                                      "fit_with_weights",
        #                                      "CreateCirclePolyMeter",
        #                                      "st_polygon",
        #                                      "st_area",
        #                                      "st_intersection",
        #                                      "st_union",
        #                                      "st_difference",
        #                                      "st_distance",
        #                                      "st_centroid",
        #                                      "point.in.polygon",
        #                                      "polyIdx",
        #                                      "DEoptim",
        #                                      "xwCoverage","xwOverflow","xwOverlap","xwDist","xwScore"),
        #      envir=environment()         )
        
        #doParallel::registerDoParallel(cl)
        
        
        
        
         # cl <- parallel::makeCluster(12)
         # setDefaultCluster(cl=cl) # set 'cl' as default cluster
        
        
        tic.clearlog();
        tic("DEoptimum Algorithm")


        #hmed <- floor (hmin + input$hPercent / 100 * (hmax - hmin))
        #droneAlt <- input$DroneAlt

        opt <- DEoptim(fn = function(xxx) fit_with_weights(xxx, wCoverage=xwCoverage, wOverflow=0,
                                                           wOverlap=0, wDist=1, wScore=xwScore, polyIdx),
                       lowerVec,
                       upperVec,
                       DEoptim.control(
                         trace = FALSE,
                         itermax=MaxIteration,
                         reltol=0.0001,
                         #initialpop = initSolMtx[[polyIdx]],
                         parallelType = 1,
                         #cluster = cl,
                         parVar = c("VoronoiPolyList",
                                    "overlapThreshold",
                                    "VBSList",
                                    "halfTheta",
                                    "hmed",
                                    "MaxDist3DinPoly",
                                    "fit_with_weights",
                                    "CreateCirclePolyMeter",
                                    "st_polygon",
                                    "st_area",
                                    "st_intersection",
                                    "st_union",
                                    "st_difference",
                                    "st_distance",
                                    "st_centroid",
                                    "point.in.polygon",
                                    "polyIdx",
                                    "xwCoverage","xwOverflow","xwOverlap","xwDist","xwScore"))
        )

        timing <- toc(log = TRUE, quiet = TRUE)
        dstr <- timing$toc - timing$tic
        secs <- as.numeric(sprintf("%6.3f",dstr))
        cat("Poly",k,"soln in", secs, "secs\n")

        #stop the cluster
        #setDefaultCluster(cl=NULL);
        #parallel::stopCluster(cl)

        # lx <- length(opt$optim$bestmem)
        # solpars <- opt$optim$bestmem
        # populationSize <- length(opt$member$pop)

        lx <- length(opt$optim$bestmem)
        solMtx[[k]] <<- opt$optim$bestmem
        solCircles[[k]] <<- DroneXYRVec2dronesInPolyList(solMtx[[k]])
        my_show_DroneXYRVec_stat(solMtx[[k]])
        my_show_dronesInPolyList_stat(VoronoiPolyList[[k]], solCircles[[k]], dronesPerVBSList, k)
        score <- fit_with_weights(solMtx[[k]],wCoverage=xwCoverage, wOverflow=xwOverflow,
                                  wOverlap=xwOverlap, wDist=xwDist, wScore=xwScore, k )

        cat ("The score is:",score,"\n")
        vbsCntr <- VBSList[[k]]
        for(i in 1:ndrones){
          plot(solCircles[[k]][[i]], add=TRUE)
          centroXY <- st_centroid(solCircles[[k]][[i]])
          segments(vbsCntr[1], vbsCntr[2], centroXY[1], centroXY[2])
        }

        
        
        ######################################## DEoptim END ########################################
        
      } else {
        #Empty
      }
      
      
      
      cat("***********************************************************\n")
      
      ##################################################
      cat("Drawing soln\n")
      ##################################################
      
      #Convert x,y,r list to a matrix with 3 columns (x,y,r) ndrones (dronesPerVBSList[[k]]) rows
      dfd <- as.data.frame(matrix(unlist(solMtx[[k]]), nrow = dronesPerVBSList[[k]], ncol = 3, byrow = TRUE))
      #BS,VBS ids and drone numbers
      idxDf <- cbind(rep(k,dronesPerVBSList[[k]]),seq(1:dronesPerVBSList[[k]]))
      dfd <- cbind(dfd,idxDf)
      colnames(dfd) <- c("lon","lat","rad","bidx","didx")
      
      
      dfb <- as.data.frame(BSMtx)
      dfb <- cbind(dfb,seq(1:length(dfb$V1)),as.matrix(unlist(dronesPerVBSList)))
      colnames(dfb)<-c("lon","lat","idx","ndrones")
      
      dfv <- as.data.frame(VBSMtx)
      dfv  <- cbind(dfv,seq(1:length(dfv$V1)),as.matrix(unlist(dronesPerVBSList)))
      colnames(dfv)<-c("lon","lat","idx","ndrones")
      
      #hmed <- floor (hmin + input$hPercent / 100 * (hmax - hmin))
      ##################################################
      
      
      
      
      
      leafletProxy("mymap") %>% 
        addCircles(data = dfd,
                   lng = ~ lon, 
                   lat = ~ lat,
                   color = "blue",
                   radius = ~ rad*111111,
                   stroke = TRUE, 
                   opacity = 5,
                   weight = 1,
                   fillColor = "blue",
                   fillOpacity = 0.5) %>%
        
        addMarkers(data = dfd,
                   lng = ~ lon, 
                   lat = ~ lat,
                   icon = myDroneIconSmall,
                   #popup = ~htmlEscape("lat=",~V2)) %>%
                   popup = paste0("BS",dfd$bidx,"-Drone", dfd$didx, "<br>","Long:", dfd$lon, "<br>", 
                                  "Lat:", dfd$lat, "<br>", "Alt:",
                                  ceiling((dfd$rad * 111111) / tan(halfTheta * pi / 180) ), "m<br>")
        ) %>%
        
        addMarkers(data = dfb,
                   lng = ~ lon, 
                   lat = ~ lat,
                   icon = myBSIcon,
                   popup = paste0("BS",dfb$idx,"-",dfb$ndrones,"drones<br>","Long:", dfb$lon, "<br>", "Lat:", dfb$lat, "<br>")
        ) %>%
        
        addMarkers(data = dfv,
                   lng = ~ lon, 
                   lat = ~ lat,
                   icon = myVBSIcon, 
                   popup = paste0("VBS",dfv$idx,"-",dfv$ndrones,"drones<br>","Long:", dfv$lon, "<br>", "Lat:", dfv$lat, "<br>", "Alt:", input$DroneAlt, "m<br>")
        )
      
      
      
      #Lines from VBS to drone centroid
      ##################################
      
      for (droneIdx in 1:dronesPerVBSList[[k]]) {
        #line from VBS center to drone coverage circle center
        df <- as.data.frame(rbind(VBSMtx[k,], c(dfd[droneIdx,]$lon, dfd[droneIdx,]$lat)))
        #draw the line on the map
        leafletProxy("mymap") %>% 
          addPolylines(data = df,
                       lng = ~ V1, 
                       lat = ~ V2,
                       color = "green",
                       weight = 3)
        
        
        mapToSave$current <- mapToSave$base %>% 
          addPolylines(data = df,
                       lng = ~ V1, 
                       lat = ~ V2,
                       color = "green",
                       weight = 3)
      }
      
      
      ##################################################
      
      
      
      
      
      
      
    } #For loop (k in 1:nVoronoiPoly)
    
    
    timing <- toc(log = TRUE, quiet = TRUE)
    dstr <- timing$toc - timing$tic
    secs <- as.numeric(sprintf("%6.3f",dstr))
    cat("Coverage solution took", secs, "secs\n")
    
    
    
    ##isolate({
    #TODO
    # For coverage lon-lat should be converted to meters!!!
    # or vice versa
    
    # For many problems it is best to set 'NP' (in 'control') 
    # to be at least ten times the length of the parameter vector.
    # So we leave to the algo to decide optimum number of population
    # 
    
    # 
    # 
    # ######################################## GenSA BEGIN ########################################
    # cat("Coverage running\n")
    # cat("Using GenSA algortihm - Minimizing - Accepts initial solution\n")
    # set.seed(1234) # The user can use any seed.
    # #In case you can guess the global min and can set the tolerance level
    # #global.min <- 0
    # #tol <- 1e-13
    # #GenSA minimizes
    # xwScore=-1.0
    # 
    # 
    # 
    # for (k in 1:nVoronoiPoly) {
    #   
    #   ndrones <- dronesPerVBSList[[k]]
    #   
    #   
    #   if (ndrones > 1){
    #     
    #     dfPts <- getPtsInPoly(VoronoiPolyList[[k]], gridSize / 111111)
    #     
    #     
    #     
    #     #Set some variables for GA
    #     populationSize <- 3 * ndrones
    #     if (populationSize < 10) populationSize <- 20
    #     
    #     imtx <- dronesInPolyList2DroneXYRVec(dronesInPolyList[[k]], dronesPerVBSList, k)
    #     
    #     #Convert the initial solution to desired format by ga algo
    #     InitialsolSuggestion <- as.vector(imtx)
    #     #DEBUG InitialsolSuggestion <- matrix(c(imtx), nrow = 1, ncol = 3*ndrones)
    #     
    #     lowerVec <- rep(c(min(dfPts$inX), min(dfPts$inY), min(rValues / 111111)),ndrones)
    #     upperVec <- rep(c(max(dfPts$inX), max(dfPts$inY), max(rValues / 111111)),ndrones)
    #     
    #     
    #     
    #     # Run the GenSA Algorithm with time limit = max.time!
    #     tic.clearlog();
    #     tic("GenSA Algorithm")
    #     
    #     out <- GenSA(par = imtx, lower = lowerVec, upper = upperVec,
    #                  fn = function(xxx) fit_with_weights(xxx, wCoverage=xwCoverage,
    #                                                      wOverflow=xwOverflow, wOverlap=xwOverlap,
    #                                                      wDist=xwDist, wScore=xwScore,k),
    #                  #control=list(verbose=TRUE, nb.stop.improvement = 10))
    #                  control=list(verbose=TRUE, max.time = 60))
    #     #control=list(verbose=TRUE, maxit = MaxIteration))
    #     
    #     timing <- toc(log = TRUE, quiet = TRUE)
    #     dstr <- timing$toc - timing$tic
    #     secs <- as.numeric(sprintf("%6.3f",dstr))
    #     cat("Poly",k,"soln in", secs, "secs\n")
    #     
    #     
    #     #out[c("value","par","counts")]
    #     #MaxIteration <- out$counts
    #     
    #     lx <- length(out$par)
    #     solMtx[[k]] <<- out$par
    #     solCircles[[k]] <<- DroneXYRVec2dronesInPolyList(solMtx[[k]])
    #     my_show_DroneXYRVec_stat(solMtx[[k]])
    #     #plot(VoronoiPolyList[[k]], add=TRUE)
    #     my_show_dronesInPolyList_stat(VoronoiPolyList[[k]], solCircles[[k]], dronesPerVBSList, k)
    #     score <- fit_with_weights(solMtx[[k]],wCoverage=xwCoverage, wOverflow=xwOverflow,
    #                               wOverlap=xwOverlap, wDist=xwDist, wScore=xwScore, k )
    #     
    #     cat ("The score is:",score,"\n")
    #     vbsCntr <- VBSList[[k]]
    #     for(i in 1:ndrones){
    #       plot(solCircles[[k]][[i]], add=TRUE)
    #       centroXY <- st_centroid(solCircles[[k]][[i]])
    #       segments(vbsCntr[1], vbsCntr[2], centroXY[1], centroXY[2])
    #     }
    #     
    #     ######################################## GenSA END ########################################
    #     
    #     
    #     
    #     
    #     
    
    
    
    
    
    
    
    
    
    
    
    #     #TODO single drone at the centroid of the polygon covering all the polygon
    #     centroXY <- st_centroid(VoronoiPolyList[[k]])
    #     
    #     #Find the furthest distance to that VBS point
    #     #We will use it to estimate the distance percentages
    #     #Convert sf to spatial
    #     y <- as(VoronoiPolyList[[k]], "Spatial")
    #     #Convert spatial to owin object so that we can use distances func
    #     yowin <- polyCub::as.owin.SpatialPolygons(y)
    #     #Get vertices of the polygon
    #     vtx <- vertices(yowin) # Polygon vertices
    #     #Find the distances from the centroid to all the vertices of the poly
    #     #Here fudging for simplicity?
    #     #Any point on the edge can be at max distance?
    #     d2D <- crossdist(vtx$x, vtx$y, centroXY[1], centroXY[2]) # n-column matrix of cross distances
    #     i1 <- which.max(d2D[,1]) # Index of max dist point
    #     maxRadius <- d2D[i1]
    #     
    #     if (maxRadius > rmax) maxRadius <- rmax
    #     
    #     solMtx[[k]] <<- c(centroXY[1], centroXY[2], maxRadius)
    #     
    #     
    #     solCircles[[k]] <<- DroneXYRVec2dronesInPolyList(solMtx[[k]])
    #     my_show_DroneXYRVec_stat(solMtx[[k]])
    #     
    #     #These they do not work for a single circle!!!
    #     #################################################################################
    #     #my_show_dronesInPolyList_stat(VoronoiPolyList[[k]], solCircles[[k]], dronesPerVBSList, k)
    #     #score <- fit_with_weights(solMtx[[k]],wCoverage=xwCoverage, wOverflow=xwOverflow, 
    #     #                          wOverlap=xwOverlap, wDist=xwDist, wScore=xwScore, k )
    #     #cat ("The score is:",score,"\n")
    #     #################################################################################
    #     
    #     vbsCntr <- VBSList[[k]]
    #     plot(solCircles[[k]][[1]], add=TRUE)
    #     centroXY <- st_centroid(solCircles[[k]][[1]])
    #     segments(vbsCntr[1], vbsCntr[2], centroXY[1], centroXY[2])
    #     
    #     
    #   }
    #   
    #   
    # }
    # 
    # 
    # #Overall statistics for the whole region
    # 
    # my_show_overall_stat(solCircles, dronesPerVBSList)
    # 
    ## }) # isolate
    
    
    
    
    
    
    
    
    
    
    
  })
  
  
  
  
  #######################################################################################################
  
  
  
  
  # 
  # 
  # #DEBUG
  # #######################################################################################################
  # observeEvent(input$xshowBS,{
  #   #plot all the BS
  #   cat("Clearing shapes, markers\n")
  #   leafletProxy("mymap")  %>% removeShape("myBSEdges") %>% clearMarkers()
  #   
  #   df <- as.data.frame(BSMtx)
  #   df <- cbind(df,seq(1:length(df$V1)))
  #   colnames(df)<-c("lon","lat","idx")
  #   
  #   leafletProxy("mymap") %>%
  #     
  #     addMarkers(data = df,
  #                lng = ~ lon, 
  #                lat = ~ lat,
  #                icon = myBSIcon,
  #                #popup = ~htmlEscape("lat=",~V2)) %>%
  #                #popup = paste0("Long:", df$V1, "<br>", "Lat:", df$V2, "<br>")
  #     )%>%
  #     
  #     addPopups(data = df,
  #               lng = ~ lon, 
  #               lat = ~ lat,
  #               paste0("BS",df$idx,"<br>","Long:", df$lon, "<br>", "Lat:", df$lat, "<br>"),
  #               options = popupOptions(closeButton = TRUE))
  #   
  #   # %>% addPolylines(data = as.data.frame(BSMtx),
  #   #                lng = ~ V1, lat = ~ V2,
  #   #                color = "red",
  #   #                weight = 3)
  #   
  # })
  # 
  # 
  # #######################################################################################################
  # 
  # 
  # 
  
  
  
  ########################################################################################################
  observeEvent(input$closePolygon,{
    #clear the polygons
    #p4 <- VtxMtx2SpatPolyDF(a <- list())
    if (input$startPoly) {
      
      updatedValue = !input$startPoly
      
      updateSwitchInput(
        session = session,
        inputId = "startPoly",
        value = updatedValue,
        disabled = TRUE
      )
      
      
     enable("DroneAlt")
      
      
      polyBEGIN <<- FALSE
      
      if ( !is.null(regionVtxMtx) && (length(regionVtxMtx[,1])>=3) ){
        
        
        enable("DroneNumber")
        
        
        
        regionVtxMtx <<- rbind(regionVtxMtx, regionVtxMtx[1,])
        
        regionPoly <<-  st_polygon(list(regionVtxMtx))
        regionPolyBBox <<- st_bbox(regionPoly)
        regionPolyArea <<- st_area(regionPoly)
        
        #hmed <- floor (hmin + input$hPercent / 100 * (hmax - hmin))
        
        totDroneReq <<- round(areaPolygon(regionPoly[[1]]) / 
                               (pi * (input$DroneAlt  * tan(halfTheta * pi / 180)) * 
                              (input$DroneAlt * tan(halfTheta * pi / 180))))
        
        cat("Region area is",areaPolygon(regionPoly[[1]]),"meter^2\n")
        cat("ESTIMATED DRONES REQ AT",input$DroneAlt,"meter is", totDroneReq,"\n")
        
        updateNumericInput(
          session,
          "DroneNumber",
          #DEBUG  min = 2 * nBS,
          value = totDroneReq
        )
        output$DroneNumberMsg <- renderText({ paste0(totDroneReq," drones are required at ",input$DroneAlt, " meter.")})
        
        
        regionPolyDF <<- VtxMtx2SpatPolyDF(regionVtxMtx)
        isolate({
          new_zoom <- 9
          if(!is.null(input$mymap_zoom)) new_zoom <- input$mymap_zoom
          #cat("Clear Selection:", input$mymap_center$lng, input$mymap_center$lat, new_zoom, "\n")
          
          #Find the bbox and overlay the map
          ####################################################################################
          frameBBX <- st_bbox(regionPolyDF)
          
          mapH <- lonlat2m(frameBBX$xmin,frameBBX$ymin,frameBBX$xmin,frameBBX$ymax)
          mapW <- lonlat2m(frameBBX$xmin,frameBBX$ymin,frameBBX$xmax,frameBBX$ymin)
          
          framePoly <- Polygon(rbind(c(frameBBX$xmin,frameBBX$ymin),c(frameBBX$xmin,frameBBX$ymax),
                                     c(frameBBX$xmax,frameBBX$ymax), c(frameBBX$xmax,frameBBX$ymin),
                                     c(frameBBX$xmin,frameBBX$ymin)))
          framePolys <- Polygons(list(framePoly),1)
          myBBox <- SpatialPolygons(list(framePolys), proj4string=sp::CRS("+init=epsg:4326"))
          
          #length(regionVtxMtx[,1])-1, 
          output$myMsg <- renderText({paste0("Closing poly with ",length(regionPolyDF@polygons[[1]]@Polygons[[1]]@coords[,1])-1,
                                             " points. BBox width:",round(mapW,2),
                                             " - height:",round(mapH,2) ," meters\n") })
          ####################################################################################
          
          
          leafletProxy("mymap") %>% clearShapes()
          mapToSave$current <- mapToSave$base %>% clearShapes()
          
          leafletProxy("mymap") %>% 
            addPolygons(data=regionPolyDF, weight = 2, fillColor = "red",fillOpacity = 0.2, layerId ="myPolyLayer")%>% 
            addPolygons(data=myBBox, weight = 2, fill=FALSE, layerId ="myBBoxLayer")
          
          
          mapToSave$current <- mapToSave$base %>%
            addPolygons(data=regionPolyDF, weight = 2, fillColor = "red",fillOpacity = 0.2, layerId ="myPolyLayer")%>% 
            addPolygons(data=myBBox, weight = 2, fill=FALSE, layerId ="myBBoxLayer")
          #setView(lng = input$lng, lat = input$lat, zoom = new_zoom)
          
          updateSwitchInput(
            session = session,
            inputId = "startBS",
            disabled = FALSE
          )
        })
      }else{
        output$myMsg <- renderText({paste("Not enough polygon points: You need at least 3 points!!\n") })
        if (!is.null(regionVtxMtx)) {
          mapToSave$current <- mapToSave$base %>% clearShapes()
          leafletProxy("mymap") %>% clearShapes()
          regionVtxMtx <<- NULL
          
        }
      }
    }else {
      output$myMsg <- renderText({paste("Not in start poly mode!!!\n")})
    }
    
    
  })
  ########################################################################################################
  
  
  
  ########################################################################################################
  #create the map
  output$mymap <- renderLeaflet({
    
    #Find the bbox and overlay the map
    ####################################################################################
    frameBBX <- st_bbox(regionPolyDF)
    nPts <- length(regionPolyDF@polygons[[1]]@Polygons[[1]]@coords[,1]) - 1
    mapH <- lonlat2m(frameBBX$xmin,frameBBX$ymin,frameBBX$xmin,frameBBX$ymax)
    mapW <- lonlat2m(frameBBX$xmin,frameBBX$ymin,frameBBX$xmax,frameBBX$ymin)
    
    framePoly <- Polygon(rbind(c(frameBBX$xmin,frameBBX$ymin),c(frameBBX$xmin,frameBBX$ymax),
                               c(frameBBX$xmax,frameBBX$ymax), c(frameBBX$xmax,frameBBX$ymin),
                               c(frameBBX$xmin,frameBBX$ymin)))
    framePolys <- Polygons(list(framePoly),1)
    myBBox <- SpatialPolygons(list(framePolys), proj4string=sp::CRS("+init=epsg:4326"))
    
    
    output$myMsg <- renderText({paste0("Poly with ",nPts, 
                                       " points. BBox width:",round(mapW,2),
                                       " - height:",round(mapH,2) ," meters\n") })
    ####################################################################################
    
    
    mapToSave$current <- mapToSave$base <- leaflet("mymap") %>% 
      setView(lng = 13.07133 , lat = 43.13554, zoom = 15)  %>% #setting the view over Camerino
      #addTiles(options = providerTileOptions(minZoom = 1, maxZoom = 30)) %>% 
      addTiles()  %>%
      # addCircles(data = data, lat = ~ latitude, lng = ~ longitude, weight = 1, 
      #            radius = ~sqrt(mag)*25000, popup = ~as.character(mag), 
      #            label = ~as.character(paste0("Magnitude: ", sep = " ", mag)), 
      #            color = ~pal(mag), fillOpacity = 0.5) %>% 
      addPolygons(data=regionPolyDF, weight = 2, fillColor = "red",fillOpacity = 0.2, layerId ="myPolyLayer")%>%
      addPolygons(data=myBBox, weight = 2, fill=FALSE, layerId ="myBBoxLayer")%>%
      onRender(
        "function(el,x){
                    this.on('mousemove', function(e) {
                        var lat = e.latlng.lat;
                        var lng = e.latlng.lng;
                        var coord = [lat, lng];
                        Shiny.onInputChange('hover_coordinates', coord)
                    });
                    this.on('mouseout', function(e) {
                        Shiny.onInputChange('hover_coordinates', null)
                    })
                }"
      )
    
    
    
    
  })
  ########################################################################################################
  
  
  
  ########################################################################################################
  #next we use the observe function to make the checkboxes dynamic. 
  #If you leave this part out you will see that the checkboxes, when clicked on the 
  #first time, display our filters...But if you then uncheck them they stay on. 
  #So we need to tell the server to update the map when the checkboxes are unchecked.
  ########################################################################################################
  #startPoly button
  observe({
    proxy <- leafletProxy("mymap")
    #proxy %>% clearMarkers()
    if (input$startPoly) {
      polyBEGIN <<- TRUE
      bsBEGIN <<- FALSE
      enable("closePolygon")
      output$myMsg <- renderText({paste("Select at least 3 pts for the region\n")})
      regionPolyDF <<- NULL
      regionVtxMtx <<- NULL
      BSList <<- list()
      BSMtx <<- NULL
      nBS <<- 0
    }else {
      polyBEGIN <<- FALSE
      #output$myMsg <- renderText({paste("Toggle start poly:FALSE\n")})
      mapToSave$current <- mapToSave$base %>% clearControls()
      # proxy %>% clearMarkers() %>% clearControls()
      proxy %>% clearControls()
      regionPolyDF <<- NULL
      regionVtxMtx <<- NULL
      BSList <<- list()
      BSMtx <<- NULL
      nBS <<- 0
    }
  })
  ########################################################################################################
  
  
  
  ########################################################################################################
  #startBS button
  observe({
    proxy <- leafletProxy("mymap")
    #proxy %>% clearMarkers()
    if (input$startBS) {
      bsBEGIN <<- TRUE
      polyBEGIN <<- FALSE
      disable("closePolygon")
      
      #TODO
      #enable("findCov")
      enable("showVoronoi")
      
      output$myMsg <- renderText({paste("Select BS pos outside polygon\n")})
      regionPolyDF <<- NULL
      regionVtxMtx <<- NULL
      BSList <<- list()
      BSMtx <<- NULL
      nBS <<- 0
    }else {
      bsBEGIN <<- FALSE
    }
  })
  ########################################################################################################
  
  
  
  ######################################################################################################## 
  observe({
    #Mouse click
    
    click <- input$mymap_click
    if(is.null(click)) return()
    text<-paste("Lat:", round(click$lat,3), "Lng:", round(click$lng,3))
    
    #Check the mode
    if (polyBEGIN == TRUE){
      regionVtxMtx <<- rbind(regionVtxMtx, c(click$lng, click$lat))
      text2<-paste0("Polygon point[", length(regionVtxMtx[,1]),"] ", click$id, "at: ",text)
      cat("Add poly point - Total points:", length(regionVtxMtx[,1]),"\n")
      isolate({
        new_zoom <- 9
        if(!is.null(input$mymap_zoom)) new_zoom <- input$mymap_zoom
        #paste("Cleared Selection: Lat:", round(input$mymap_center$lat,2),"Lng:", round(input$mymap_center$lng,2),"Zoom:",new_zoom, "\n")
        output$myMsg <- renderText({paste("Click Selection: Lat:", round(click$lat,3),"Lng:", round(click$lng,3), "Zoom:",new_zoom, "\n") })
        #Remove the previous circle layer and add the new update one
        
        #leafletProxy("mymap") %>% removeShape(c("regionVtxMarkers","regionEdgeMarkers")) %>% clearMarkers()
        #mapToSave$current <- mapToSave$base %>% removeShape(c("regionVtxMarkers","regionEdgeMarkers")) %>% clearMarkers()
        
        leafletProxy("mymap") %>% 
          addCircles(data = as.data.frame(regionVtxMtx),
                     lng = ~ V1, lat = ~ V2,
                     color = "red",
                     radius = 10,
                     stroke = TRUE, 
                     opacity = 5,
                     weight = 1,
                     fillColor = "red",
                     fillOpacity = 0.5) %>% 
          addPolylines(data = as.data.frame(regionVtxMtx),
                       lng = ~ V1, 
                       lat = ~ V2,
                       color = "blue",
                       weight = 3)
        
        mapToSave$current <- mapToSave$base %>% 
          addCircles(data = as.data.frame(regionVtxMtx),
                     lng = ~ V1, 
                     lat = ~ V2,
                     color = "red",
                     radius = 10,
                     stroke = TRUE, 
                     opacity = 5,
                     weight = 1,
                     fillColor = "red",
                     fillOpacity = 0.5) %>% 
          addPolylines(data = as.data.frame(regionVtxMtx),
                       lng = ~ V1, 
                       lat = ~ V2,
                       color = "blue",
                       weight = 3)
        
        #addPolygons(data=regionPolyDF, weight = 2, fillColor = "red", popup=popup,fillOpacity = 0.2, layerId ="myPolyLayer")
        #setView(lng = input$lng, lat = input$lat, zoom = new_zoom)
      })
      
      #mymap$clearPopups()
      #mymap$showPopup( click$lat, click$lng, text)
    }#else
    
    if (bsBEGIN == TRUE) {
      #TODO check if the point is outside of the poly HERE
      
      nBS <<- nBS + 1
      # updateNumericInput(
      #   session,
      #   "DroneNumber",
      #   min = 2*nBS,
      #   value = 2*nBS
      # )
      # output$OutDroneNumber <- renderText({ input$DroneNumber })
      
      text2<-paste0("BS point[", nBS,"] ", click$id, "at: ",text)
      cat("Add BS point - Total points:", nBS,"\n")
      BSList[[nBS]] <<- st_point(c(click$lng, click$lat))
      BSMtx <<- rbind(BSMtx, c(click$lng, click$lat))
      
      isolate({
        new_zoom <- 9
        if(!is.null(input$mymap_zoom)) new_zoom <- input$mymap_zoom
        output$myMsg <- renderText({paste("Click Selection: Lat:", round(click$lat,3),"Lng:", round(click$lng,3), "Zoom:",new_zoom, "\n") })
        
        #Remove the previous circle layer and add the new update one
        #leafletProxy("mymap") %>% removeShape("myBSEdges") %>% clearMarkers()
        #mapToSave$current <- mapToSave$base %>% removeShape("myBSEdges") %>% clearMarkers("myBSPts")
        
        
        leafletProxy("mymap") %>% 
          addMarkers(data = as.data.frame(BSMtx),
                     lng = ~ V1, 
                     lat = ~ V2,
                     icon = myBSIcon)  
        
        # %>% 
        #   addPolylines(data = as.data.frame(BSMtx),
        #                lng = ~ V1, lat = ~ V2,
        #                color = "green",
        #                weight = 3)
        
        
        
        
        mapToSave$current <- mapToSave$base %>% 
          addMarkers(data = as.data.frame(BSMtx),
                     lng = ~ V1, 
                     lat = ~ V2,
                     icon = myBSIcon)  
        
        
        
        
        # %>% 
        #   addPolylines(data = as.data.frame(BSMtx),
        #                lng = ~ V1, lat = ~ V2,
        #                color = "green",
        #                weight = 3)
        
        #addPolygons(data=regionPolyDF, weight = 2, fillColor = "red", popup=popup,fillOpacity = 0.2, layerId ="myPolyLayer")
        #setView(lng = input$lng, lat = input$lat, zoom = new_zoom)
      })
      
    }else{
      text2<-paste("Clicked point", click$id,text)
      #mymap$clearPopups()
      #mymap$showPopup( click$lat, click$lng, text)
    }
    output$clickMsg<-renderText({ text2 })
  })
  
  ########################################################################################################
  
  
  
  
}
########################################################################################################

