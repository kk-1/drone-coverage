
########################################################################################################
#Drone coverage with GA - ga algorithm
########################################################################################################

########################################################################################################
#Set the directory for the code and output
########################################################################################################
#setwd("/my_ssd2/my_dir/my_prg/Camerino/R")
#setwd("/my_hd1/my_dir/my_download/ga-drone-code-buffer/")
#setwd("/my_home_ssd/kemal/my_dir/my_prog/Camerino/drone/sims/")

########################################################################################################

#Clean the environment
remove(list = ls())

#For timing
library(tictoc)


#Load the functions form other file
source("ga-drone-funcs-V47.R")



########################################################################################################
#Caveats
########################################################################################################
# #######################################################################
# # This is the best way to deal with the coordinate incompatibility!!!
# #######################################################################
# h=300
# w=600
# #Just when you define matrix for row use size in x-horizontal 
# #and col size in y-vertical!!!!
# #Use row as x and col as y
# x = matrix(0, nrow=w, ncol=h)
# 
# #For all the rest x and y as normal
# 
# #So the first coordinate is as usual x the xecond is y!!!
# #Rectangle from (x1=20, y1=100) to (x2=140, y2=150)
# x[20:140, 100:150] <- 1
# 
# 
# #Horizontal line of thickness 2 pixels from x1=1 to x2=100
# x[1:100, 2:3] <- 1
# x = drawCircle(x, 100, 20, 20, col=1,fill = TRUE)
# x = drawCircle(x, 200, 200, 20, col=1,fill = TRUE)
# display(x,method="raster")
# #######################################################################
########################################################################################################

#Print the arguments for the Rscript
cat("Here is the usage:\n")
cat("Rscript --vanilla ga-drone-parallel-V47.R <algo> <readInitSolflag> <useInitSolflag> <wCov> <wOverflow> <wOverlap> <xDistdiff> <maxIter> <w> <h> <bsx> <bsy> <halftheta> <dhmin> <dhmax> <overlapTh> <ndrones>\n")
#Rscript --vanilla ga-drone-parallel-V40.R 0 1 1 1.0 -1.0 -1.0 4.0 100 400 300 0 0 30 5 150 60 12
cat("Args:
     <algo>: Algo: 0 1 2 3
     <readInitSolflag> 1 - Read initsol from file 0 - Save it to file. Only for algos that accepts it
     <useInitSolflag> 1 - Use initsol. Only for algos that accepts it
     <wCov> Coverage weight +
     <wOverflow> Overflaw weight -
     <wOverlap> Overlap weight -
     <xDistdiff> Dist diff weight +
     <maxIter> For evolutionary algo number of iters without improvement
     <w> Region Width
     <h> Region Height
     <bsx> Base Stat. x coor
     <bsy> Base Stat. y coor
     <halftheta> Half of the drone beam angle 
     <dhmin> Drone min height
     <dhmax> Drone max height
     <overlapTh> Overlap threshold
     <ndrones> Drone number if -1 program will choose\n")


args <- commandArgs()
cat("Args:\n")
print(args)


# #DEBUG
# #Arguments comes as text!!!
# xxxx <- as.numeric(args[10])
# yyyy <- xxxx + 1.5
# cat ("1+ first is : ",yyyy,"\n")
# quit()
# #DEBUG


##Set the values for interactive run here (uncomment the following 2 lines!):
#args=matrix(seq(1:23),nrow=1,ncol=23)
#args[7:23] <- c(3, 1, 1, 1.5, -1.0, -1.0, 0.0, 100, 400, 300, 0, 0, 30, 5, 150, 60, 2)


########################################################################################################
# Here set variables
########################################################################################################
#CRUCIAL VARS:
########################################################################################################
#Select the GA R-function
#0 for the GA - Genetic Algorithms
#1 for the genoud - GENetic Optimization Using Derivatives 
#2 for the DEoptim - Global Optimization by Differential Evolution
#3 for the GenSA - Generalized Simulated Annealing Function
#4 for the optimParallel - parallel version of the L-BFGS-B method of General-purpose Optimization
#algo <- 0
algo <- as.numeric(args[7])
# The following variable is meaningful if your algo accepts initial solution!!!
# If you want to read from file previously generated initail solution set to 1
# 0 means to regenerate the initial solution matrix for drone positions and then save it to file
#readInitDroneMtxfromFile = 1
readInitDroneMtxfromFile <-as.numeric(args[8])


#useInitSol = 1
useInitSol <- as.numeric(args[9])


#Set the weights for scoring (fitness)
# xwCoverage=1.0
# xwOverflow=-1.0
# xwOverlap=-1.0
# xwDist=4.00

xwCoverage <- as.numeric(args[10])
xwOverflow <- as.numeric(args[11])
xwOverlap <- as.numeric(args[12])
xwDist <- as.numeric(args[13])


#Set the number of iterations for evolutionary algorithms
#MaxIteration <- 100

MaxIteration <- as.numeric(args[14])
########################################################################################################


# Set file name to save the drawing to png file
if (algo == 0){
  algoName="GA_"
  cat("Using ga algortihm - Maximizing - Accepts initial solution - Maxiter set as 500\n")
  algotitle <- paste("Using ga algortihm - Maximizing - Accepts initial solution - Maxiter set as 500")
  xwScore=1.0
  acceptsInitSol = 1
} else if (algo == 1) {
  algoName="genoud_"
  cat("Using genoud algortihm - Maximizing - Accepts initial solution\n")
  algotitle <- paste("Using genoud algortihm - Maximizing - Accepts initial solution")
  xwScore=1.0
  acceptsInitSol = 1
} else if (algo == 2) {
  algoName="deoptim_"
  cat("Using deoptim algortihm - Minimizing - Does not accept initial solution - Tolerance is 0.0001\n")
  algotitle <- paste("Using deoptim algortihm - Minimizing - Does not accept initial solution - Tolerance is 0.0001")
  xwScore=-1.0
  acceptsInitSol = 1
  #acceptsInitSol = 0
} else if (algo == 3) {
  algoName="GenSA_"
  cat("Using GenSA algortihm - Minimizing - Accepts initial solution\n")
  algotitle <- paste("Using GenSA algortihm - Minimizing - Accepts initial solution")
  xwScore=-1.0
  acceptsInitSol = 1
} else if (algo == 4) {
  algoName="optimPar_"
  cat("Using optimParallel algortihm - Maximizing - Accepts initial solution\n")
  algotitle <- paste("Using  optimParallel algortihm - Maximizing- Accepts initial solution")
  cl <- parallel::makeCluster(parallel::detectCores()); 
  parallel::setDefaultCluster(cl = cl)
  xwScore=1.0
  acceptsInitSol = 1
} else {
  # This should not happen as previously checked!
  cat("Unlisted algo!\n")
}




#Region size
#Remember normally (height ~ row ~ y), (width ~ col ~ x)
#But EBImage functions flip-flip-trans the matrix stuff!!!
#w<-400 # This will be x - horizontal dim
#h<-300 # This will be y - vertical dim

w  <- as.numeric(args[15])
h  <- as.numeric(args[16])

#"Virtual" BS position. In fact the entry point of the networking to the region.
#"Real" BS is always will be far away from the region
#But upto that point somehow with other drones the signal is coming
#This part is simple: You just put drones in linear config
#Just to bring the signal to that point!
#bsx <- 0
#bsy <- 0

bsx <- as.numeric(args[17])
bsy <- as.numeric(args[18])

#Drone beam cone angle in degree. Half of the cone infact!!!
#halfTheta <- 30 

halfTheta  <- as.numeric(args[19])


#Drone min/max altitude 5-300m
#hmin<-5
#hmax<-150


hmin <- as.numeric(args[20])
hmax <- as.numeric(args[21])

#Fudgy way of representing max distance for a drone on this psecific region
#Like going to opposite diagonal in other words longest distance and highest altitude
maxDist <- sqrt(hmax^2+w^2+h^2)

#Overlap threshold as percentage: 50 means high penalty for overlapping 50% of the area of the circle
#overlapThreshold <- 60
overlapThreshold <- as.numeric(args[22])

# For genoud algo number of threads in parallel. Depends on your CPU!!!
nCluster <- parallel::detectCores()


########################################################################################################





########################################################################################################
# Here calculate other variables
########################################################################################################

#Area of the region to be covered
areaEmptyRegion <- w * h


#Drone min max coverage radius
rmin <- ceiling(hmin * tan(halfTheta*pi/180))
rmax <- ceiling(hmax * tan(halfTheta*pi/180))


#Drone min max coverage area
amin <- ceiling(pi*rmin*rmin)
amax <- ceiling(pi*rmax*rmax)


#Min number of drones necessary to cover region with each max area
minNdrones <- ceiling(areaEmptyRegion / amax)


#Max number of drones necessary to cover region with each min area
maxNdrones <- ceiling(areaEmptyRegion / amin)


#Lets set number of drones to 2 * ndrones
tempArg <- as.numeric(args[23])
if (tempArg == -1) {
  ndrones <- 2*minNdrones
} else {
  ndrones <- as.numeric(args[23])
}


#Set the text output file name for various statistics
textFileName <- paste0(algoName,"drones=",ndrones,"_",xwCoverage,"_",xwOverflow,"_",xwOverlap,"_",xwDist,"_iter=",MaxIteration,"_Out")
ext<-"txt"
textFileName <- paste(textFileName, ext, sep = ".")

cat("\n**********************************************************************************\n*",file=textFileName,append=TRUE)
cat(algotitle,file=textFileName,append=TRUE)
cat("\n**********************************************************************************\n",file=textFileName,append=TRUE)
#Command line
cat("Command line with arguments:\n",file=textFileName,append=TRUE)
args2 <- commandArgs()
cat(args2, sep =" ",file=textFileName,append=TRUE)
cat("\n",file=textFileName,append=TRUE)
cat("Area size:",areaEmptyRegion,"\n",file=textFileName,append=TRUE)
cat("Min drones necessary (at max h) for 100% coverage:",minNdrones,"\n",file=textFileName,append=TRUE)
cat("Max drones necessary (at min h) for 100% coverage:",maxNdrones,"\n",file=textFileName,append=TRUE)
cat("Region width=",w,"m height=",h,"m\n",file=textFileName,append=TRUE)
cat("Using",ndrones,"drones!\n",file=textFileName,append=TRUE)
cat(nCluster,"cores detected for parallel processing!\n",file=textFileName,append=TRUE)
cat("Max",MaxIteration,"iterations for evolutionary algorithms\n",file=textFileName,append=TRUE)
cat("Drone beam angle is",2*halfTheta,"degrees\n",file=textFileName,append=TRUE)
cat("Drone height range as",hmin,"-",hmax,"meters\n",file=textFileName,append=TRUE)
cat("Drone radius range as",rmin,"-",rmax,"meters\n",file=textFileName,append=TRUE)
cat("Max distance for drones in the region (furthest diagonal and highest)",maxDist,"meters\n",file=textFileName,append=TRUE)
cat("Overlap threshold set as",overlapThreshold,"%\n",file=textFileName,append=TRUE)
cat("Percentage Weights: Coverage =",xwCoverage,"Overlap =",xwOverlap,"Overflow =",xwOverflow,"Dist =",xwDist,"Score =",xwScore,"\n",file=textFileName,append=TRUE)
cat("\n**********************************************************************************\n",file=textFileName,append=TRUE)



#Set some variables for GA
populationSize <- 3*ndrones
if (populationSize < 10) populationSize <- 20


########################################################################################################



########################################################################################################
#Setting useful data structures
########################################################################################################

# Create data frame for debugging purposes that can hold info for each drone
# Pos, height or Radius, overlap, overflow, area

N <- ndrones
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
                     regionCıııoveredPercentage = integer(N),
                     regionOverlap = integer(N),
                     regionOverflow = integer(N)
) 


#Set values for region just to save them to a file
drones$regionX <- w
drones$regionY <- h
drones$regionArea <- w * h


#Region as matrix
region <- matrix(0,nrow=w, ncol=h)

#Auxilary region matricesn
#NOTE THE COORDINATE INCOMPATIBLITY BETWEEN MATRICES AND THE EBIMAGE FUNCTIONS!!!
#Just when you define matrix for row use size in x-horizonta-width and col size in y-vertical-height!!!!
#Use row as x and col as y
tempRegion1 <- matrix(0,nrow=w, ncol=h)
tempRegion2 <- matrix(0,nrow=w, ncol=h)
showRegion  <- matrix(0,nrow=w, ncol=h)


#Drone matrix with size of 3*2*number_of_drones
#xmin, xmax, ymin, ymax, rmin, rmax for each
#1,800,1,600,5,300
#rows, columns
dronemtx<-matrix(rep(c(1,w,1,h,rmin,rmax),ndrones), ndrones*3, 2, byrow = TRUE)


########################################################################################################

if (acceptsInitSol == 1) {
cat("Algo accepts initial sol\n")
if (readInitDroneMtxfromFile == 0) initDroneMtx <- pack.circles(ndrones, c(w,h), 500, 0)
else {
  fname<-paste0("my_initDroneMtx-",ndrones,"drones.rds")
  cat("Reading initial sol from:",fname,"\n")
  initDroneMtx <- readRDS(fname)
}


cat("\n**********************************************************************************\n",file=textFileName,append=TRUE)
cat("*Initial Solution Statistics\n",file=textFileName,append=TRUE)
cat("**********************************************************************************\n",file=textFileName,append=TRUE)
percent <- my_show_droneMtx_stat(initDroneMtx)


fname<-paste0(algoName,ndrones,"drones-",percent,"covered-InitConfig")
ext<-"png"
fname<-paste(fname, ext, sep = ".")
png(filename = fname, width = w, height = h)
#DEBUG
my_show_droneMtx(initDroneMtx, labelsOn = 1)
dev.off()
#stop("Exit")

if (readInitDroneMtxfromFile == 0) { 
  #fname<-paste0("my_initDroneMtx-",ndrones,"drones-",round(percent,1),"covered.rds")
  fname<-paste0("my_initDroneMtx-",ndrones,"drones.rds")
  cat("Saving initial sol to:",fname,"\n")
  saveRDS(initDroneMtx, file = fname)
}

}
########################################################################################################



##DEBUG
#quit()



########################################################################################################
#Run the desired GA algo
########################################################################################################

if (algo == 0) {
  library(GA)
  
  if (useInitSol == 1) {
    
    #Convert the initial solution to desired format by ga algo
    InitialsolSuggestion <- matrix(c(initDroneMtx), nrow = 1, ncol = 3*ndrones)
    
    tic.clearlog();
    tic("GA Algorithm")
    GA <- ga(type = "real-valued", 
             fitness = function(xxx) fit_with_weights(xxx, wCoverage=xwCoverage, wOverflow=xwOverflow, wOverlap=xwOverlap, wDist=xwDist, wScore=xwScore),
             lower = rep(c(1,1,rmin),ndrones), upper = rep(c(w,h,rmax),ndrones), parallel = TRUE,
            # popSize = populationSize, maxiter = MaxIteration, optim = TRUE,
            popSize = populationSize, run = MaxIteration, maxiter = 500, optim = TRUE,
             suggestions = InitialsolSuggestion)
    
    
    timing <- toc(log = TRUE, quiet = TRUE)
  } else {
    
    
    tic.clearlog();
    tic("GA Algorithm")
    GA <- ga(type = "real-valued", 
             fitness = function(xxx) fit_with_weights(xxx, wCoverage=xwCoverage, wOverflow=xwOverflow, wOverlap=xwOverlap, wDist=xwDist, wScore=xwScore),
             lower = rep(c(1,1,rmin),ndrones), upper = rep(c(w,h,rmax),ndrones), parallel = TRUE,
             #popSize = populationSize, maxiter = MaxIteration, optim = TRUE)
             popSize = populationSize, run = MaxIteration, maxiter = 500, optim = TRUE)
    
    timing <- toc(log = TRUE, quiet = TRUE)
  }
    
 
  log.txt <- tic.log(format = TRUE)
  cat("\n",log.txt[[1]],"\n",file=textFileName,append=TRUE )
  dstr <- timing$toc - timing$tic
  
  secs <- as.numeric(sprintf("%6.3f",dstr))
  
  
  
  summary(GA)
  
  solxyr<-summary(GA)
  
  #Lets save the progress plot of GA to png
  fname<-paste0(algoName,"drones=",ndrones,"_",xwCoverage,"_",xwOverflow,"_",xwOverlap,"_",xwDist,"_pop=",populationSize,"_iter=",solxyr$iter,"_secs=",secs,"_ProgPlot")
  ext<-"png"
  fname<-paste(fname, ext, sep = ".")
  png(filename = fname, width = 800, height = 600)
  plot(GA)
  dev.off()
  
  
  
  
  
  
  lx <- length(solxyr$solution[1,])
  solpars <- solxyr$solution[1,]
  
} else if (algo == 1) {
  library(rgenoud)
  
  
  if (useInitSol == 1) {
    #Convert the initial solution to desired format by ga algo
    InitialsolSuggestion <- matrix(c(initDroneMtx), nrow = 1, ncol = 3*ndrones)
    
    
    #fork 4 threads depending on the CPU
    cl<-parallel::makeForkCluster(nCluster)
    
    # Run the genoud Algorithm
    tic.clearlog();
    tic("genoud Algorithm")
    GA <- genoud(function(xxx) fit_with_weights(xxx, wCoverage=xwCoverage, wOverflow=xwOverflow, wOverlap=xwOverlap, wDist=xwDist, wScore=xwScore),
                 nvars = (ndrones * 3),max = TRUE,
                 starting.values = initDroneMtx,
                 pop.size = populationSize,max.generations = MaxIteration,
                 wait.generations = ceiling(MaxIteration/3),Domains = dronemtx,boundary.enforcement = 2,
                 print.level = 2, MemoryMatrix = TRUE, cluster = cl)
    
    
    timing <- toc(log = TRUE, quiet = TRUE)
  }else{
  
    #fork 4 threads depending on the CPU
    cl<-parallel::makeForkCluster(nCluster)
    
    # Run the genoud Algorithm
    tic.clearlog();
    tic("genoud Algorithm")
    GA <- genoud(function(xxx) fit_with_weights(xxx, wCoverage=xwCoverage, wOverflow=xwOverflow, wOverlap=xwOverlap, wDist=xwDist, wScore=xwScore),
                 nvars = (ndrones * 3),max = TRUE,
                 pop.size = populationSize,max.generations = MaxIteration,
                 wait.generations = ceiling(MaxIteration/3),Domains = dronemtx,boundary.enforcement = 2,
                 print.level = 2, MemoryMatrix = TRUE, cluster = cl)
    
    
    timing <- toc(log = TRUE, quiet = TRUE)
  }
  

  log.txt <- tic.log(format = TRUE)
  cat("\n",log.txt[[1]],"\n",file=textFileName,append=TRUE )
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
  
  
} else if (algo == 2) {
  
  library(DEoptim)
  
  # opt <- DEoptim(func_with_PixelCountNeg,
  #                rep(c(1,1,rmin),ndrones), rep(c(600,800,rmax),ndrones),
  #                DEoptim.control(NP=20, itermax=3, F=1.2, CR=0.7))
  
  # opt <- DEoptim(func_with_PixelCountNeg,
  #                rep(c(1,1,rmin),ndrones), rep(c(600,800,rmax),ndrones),
  #                DEoptim.control(NP=populationSize, itermax=MaxIteration, parallelType = 1) )
  # 
  
  
  # For many problems it is best to set 'NP' (in 'control') 
  # to be at least ten times the length of the parameter vector.
  # So we leave to the algo to decide optimum number of population
  
  # Run the deoptimum Algorithm
  tic.clearlog();
  tic("DEoptimum Algorithm")
  opt <- DEoptim(fn = function(xxx) fit_with_weights(xxx, wCoverage=xwCoverage, wOverflow=xwOverflow, wOverlap=xwOverlap, wDist=xwDist, wScore=xwScore),
                 rep(c(1,1,rmin),ndrones), rep(c(w,h,rmax),ndrones),
                 DEoptim.control(
                                 #itermax=MaxIteration,
                                 reltol=0.0001,
                                 parallelType = 1, 
                                 parVar = c("ndrones","h","w","overlapThreshold",
                                            "rmin","bsx","bsy","halfTheta","maxDist",
                                            "areaEmptyRegion","fit_with_weights",
                                            "xwCoverage","xwOverflow","xwOverlap",
                                            "xwDist","xwScore")) 
  )
 
  timing <- toc(log = TRUE, quiet = TRUE)
  log.txt <- tic.log(format = TRUE)
  cat("\n",log.txt[[1]],"\n",file=textFileName,append=TRUE )
  dstr <- timing$toc - timing$tic
  
  secs <- as.numeric(sprintf("%6.3f",dstr))
  
  #plot(opt)
  summary(opt)
  
  
  lx <- length(opt$optim$bestmem)
  solpars <- opt$optim$bestmem
  populationSize <- length(opt$member$pop)
  
  
} else if (algo == 3) {
  
  library(GenSA)
  
  set.seed(1234) # The user can use any seed.
  
  #In case you can guess the global min and can set the tolerance level
  global.min <- 0
  tol <- 1e-13
  
  
  lowerVec <- rep(c(1,1,rmin),ndrones)
  upperVec <- rep(c(w,h,rmax),ndrones)
  
  #Here we can pass non-optimizng params like weights!!!
  #fit_with_weights <-  function(solVector, wCoverage, wOverflow, wOverlap, wDist, wScore){
  #https://stackoverflow.com/questions/45920796/how-to-pass-non-optimizing-arguments-into-fitness-function-in-ga-package
  
  
  
  if (useInitSol == 1) {
   # Run the GenSA Algorithm with time limit = max.time!
   tic.clearlog();
   tic("GenSA Algorithm")  
  
   out <- GenSA(par = initDroneMtx, lower = lowerVec, upper = upperVec,
               fn = function(xxx) fit_with_weights(xxx, wCoverage=xwCoverage, wOverflow=xwOverflow, wOverlap=xwOverlap, wDist=xwDist, wScore=xwScore),
               control=list(verbose=TRUE, nb.stop.improvement = MaxIteration))
               #control=list(verbose=TRUE, max.time = MaxIteration))
               #control=list(verbose=TRUE, maxit = MaxIteration))
  
   timing <- toc(log = TRUE, quiet = TRUE)
  }else{
    # Run the GenSA Algorithm with time limit = max.time!
    tic.clearlog();
    tic("GenSA Algorithm")  
    
    out <- GenSA(lower = lowerVec, upper = upperVec,
                 fn = function(xxx) fit_with_weights(xxx, wCoverage=xwCoverage, wOverflow=xwOverflow, wOverlap=xwOverlap, wDist=xwDist, wScore=xwScore),
                 control=list(verbose=TRUE, nb.stop.improvement = MaxIteration))
                 #control=list(verbose=TRUE, max.time = MaxIteration))
                 #control=list(verbose=TRUE, maxit = MaxIteration))
     
    timing <- toc(log = TRUE, quiet = TRUE)
  }
  
  
  
  
  
  log.txt <- tic.log(format = TRUE)
  cat("\n",log.txt[[1]],"\n",file=textFileName,append=TRUE )
  dstr <- timing$toc - timing$tic
  
  secs <- as.numeric(sprintf("%6.3f",dstr)) 
  
  
  out[c("value","par","counts")]
  
  lx <- length(out$par)
  solpars <- out$par
  
  MaxIteration <- out$counts
  populationSize <- 0
  
}  else if (algo == 4) {
  
  library(optimParallel)
  
  
  lowerVec <- rep(c(1,1,rmin),ndrones)
  upperVec <- rep(c(w,h,rmax),ndrones)
  
  
  clusterExport(cl,"ndrones")
  clusterExport(cl,"w")
  clusterExport(cl,"h")
  clusterExport(cl,"h")
  clusterExport(cl,"h")
  clusterExport(cl,"h")
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
    InitialsolSuggestion <- matrix(c(initDroneMtx), nrow = 1, ncol = 3*ndrones)
    
    tic.clearlog();
    tic("optimParallel Algorithm")  
    
    out <- optimParallel(par = as.vector(initDroneMtx), gr=NULL,
                         fn = fit_with_weights, wCoverage=xwCoverage, wOverflow=xwOverflow, wOverlap=xwOverlap, wDist=xwDist, wScore=xwScore,
                         method = "L-BFGS-B",
                         lower = lowerVec, 
                         upper = upperVec,
                         parallel=list(loginfo=TRUE),
                         control=list(fnscale = -1 ,maxit = MaxIteration))
    
    timing <- toc(log = TRUE, quiet = TRUE)
    
  }else{
    
    tic.clearlog();
    tic("optimParallel Algorithm")  
    
    out <- optimParallel(gr=NULL,
                         fn = fit_with_weights, wCoverage=xwCoverage, wOverflow=xwOverflow, wOverlap=xwOverlap, wDist=xwDist, wScore=xwScore,
                         method = "L-BFGS-B",
                         lower = lowerVec, 
                         upper = upperVec,
                         parallel=list(loginfo=TRUE),
                         control=list(fnscale = -1 ,maxit = MaxIteration))
    
    timing <- toc(log = TRUE, quiet = TRUE)
  }
  

  log.txt <- tic.log(format = TRUE)
  cat("\n",log.txt[[1]],"\n",file=textFileName,append=TRUE )
  dstr <- timing$toc - timing$tic
  
  setDefaultCluster(cl=NULL); stopCluster(cl)
  
  secs <- as.numeric(sprintf("%6.3f",dstr)) 
  
  
  #out[c("value","par","counts")]
  
  lx <- length(out$par)
  solpars <- out$par
  
  #MaxIteration <- out$counts
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
# InitialsolSuggestion <- matrix(c(round(solpars)), nrow = 1, ncol = 3*ndrones)
# 
# 
# tic.clearlog();
# tic("GA Algorithm")
# GA <- ga(type = "real-valued", 
#          fitness =  func_with_PixelCount,
#          lower = rep(c(1,1,rmin),ndrones), upper = rep(c(w,h,rmax),ndrones), parallel = TRUE,
#          popSize = populationSize, maxiter = MaxIteration, optim = TRUE,
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

#Convert solpars (1,3*ndrones) to dMtx2(ndrones*3, 1): 1 row mtx to 1 column mtx!!!
dMtx2 <- matrix(c(solpars), nrow = ndrones*3, ncol = 1)

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
scorePixelPercent <-fit_with_weights(dMtx2, wCoverage=xwCoverage, wOverflow=xwOverflow, wOverlap=xwOverlap, wDist=xwDist, wScore=xwScore)


cat("\n**********************************************************************************\n",file=textFileName,append=TRUE)
cat("*Final Solution Statistics\n",file=textFileName,append=TRUE)
cat("**********************************************************************************\n",file=textFileName,append=TRUE)



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
    "\nEmpty:",totEmptyArea ,"m^2 (",round(EmptyPercentage,2),"% of region )",
    "\nMean Radius of drones:",xavgr,"m",
    "\nMean Height of drones:",xavgh,"m",
    "\nTotal Distance of drones from BS(", bsx,",",bsy,", 0 ):",xsumDist,"m",
    "\nMax Distance a drone from BS(", bsx,",",bsy,", 0 ):",maxDist,"m",
    "\nMean Distance of drones from BS(", bsx,",",bsy,", 0 ):",xavgDist,"m",
    "\nMedian Distance of drones from BS(", bsx,",",bsy,", 0 ):",xmedDist,"m",
    #"\nPixel Score:", scorePixel,
    "\nPercent Score:", scorePixelPercent,
    "\nWeights  (C, Olap, Oflow, Dist, Score) = (",xwCoverage, xwOverlap, xwOverflow, xwDist, xwScore,")",
    "\nPercents (C, Olap, Oflow, Dist) = (",round(CoveredPercentage,2), round((totOverlapArea*100/totCoveredArea),2), round((xtotOverflow*100/totCoveredArea),2), round((100 * (maxDist*ndrones - xsumDist) / (maxDist*ndrones)), 2),")",
    "\n",file=textFileName,append=TRUE
)

cat("**********************************************************************************\n\n",file=textFileName,append=TRUE)


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
didx<-ndrones

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
fname<-paste0(algoName,"drones=",ndrones,"_",xwCoverage,"_",xwOverflow,"_",xwOverlap,"_",xwDist,"_covered=",percent,"_pop=",populationSize,"_iter=",MaxIteration,"_secs=",secs)
ext<-"png"
fname<-paste(fname, ext, sep = ".")
png(filename = fname, width = w, height = h)



EBImage::display(showRegion, method = "raster", all = TRUE) 


# #Save to png the drawing with only circles if you like
# fname<-paste0(algoName,ndrones,"drones-",percent,"covered-",populationSize,"pop-",MaxIteration,"iter")
# ext<-"png"
# fname<-paste(fname, ext, sep = ".")
# dev.print(png, filename = fname , width = dim(region)[1], height = dim(showRegion)[2])



cat("\n**********************************************************************************\n",file=textFileName,append=TRUE)
cat("*Final Drone Positions\n",file=textFileName,append=TRUE)
cat("**********************************************************************************\n",file=textFileName,append=TRUE)
#Put the drone numbers onto centers on the drawing
for (k in seq(1,lx,3)) {
  x1 <- ceiling(solpars[k])
  y1 <- ceiling(solpars[k+1])
  r1 <- ceiling(solpars[k+2])
  text(x1,y1,sprintf("%d", (((k-1)/3) + 1)), col = 0)
  h1= ceiling(r1/tan((halfTheta*pi/180)))
  distix <- sqrt((x1-bsx)^2+(y1-bsy)^2+(h1)^2)
  cat(
    sprintf("Drone %3d at (x, y) = (%3d, %3d)  r = %3d  height = %3d  dist = %6.3f\n",(((k-1)/3) + 1),x1,y1,r1,h1,distix),
    file=textFileName,append=TRUE)
}
cat("**********************************************************************************\n",file=textFileName,append=TRUE)

dev.off()


#Save the drone info to a excel or cvs file
fname<-paste0(algoName,"drones=",ndrones,"_",xwCoverage,"_",xwOverflow,"_",xwOverlap,"_",xwDist,"_covered=",percent,"_pop=",populationSize,"_iter=",MaxIteration,"_secs=",secs)
#fname<-paste0(algoName,ndrones,"drones_(",xwCoverage,")_(",xwOverflow,")_(",xwOverlap,")_(",xwDist,")-",percent,"covered-",populationSize,"pop-",MaxIteration,"iter-",secs,"secs")
ext<-"csv"
fname<-paste(fname, ext, sep = ".")
write.csv(drones, file = fname )
########################################################################################################



