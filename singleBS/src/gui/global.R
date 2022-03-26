
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

#setwd("/my_home_ssd/kemal/my_dir/my_prog/Camerino/drone/shiny/droneCoverage")

########################################################################################################
library(EBImage)
library(shiny)
library(shinyWidgets)
library(parallel)
library(doParallel)
#For timing
library(tictoc)
library(GA)
library(rgenoud)
library(DEoptim)
library(GenSA)
library(optimParallel)
library(benchmarkme)

# library(promises)
# library(future)

# 
# plan(multiprocess)
# Warning: [ONE-TIME WARNING] Forked processing ('multicore') is disabled in future (>= 1.13.0) when running R from RStudio, 
# because it is considered unstable. Because of this, plan("multicore") will fall back to plan("sequential"), 
# and plan("multiprocess") will fall back to plan("multisession") - not plan("multicore") as in the past. 
# For more details, how to control forked processing or not, and how to silence this warning in future 
# R sessions, see ?future::supportsMulticore

########################################################################################################
# Here set variables
########################################################################################################
#CRUCIAL VARS:
########################################################################################################

########################################################################################################

#Print the arguments for the Rscript
#cat("Here is the usage:\n")
#cat("Rscript --vanilla ga-drone-parallel-V47.R <algo> <readInitSolflag> <useInitSolflag> <wCov> <wOverflow> <wOverlap> <xDistdiff> <maxIter> <w> <h> <bsx> <bsy> <halftheta> <dhmin> <dhmax> <overlapTh> <ndrones>\n")
#Rscript --vanilla ga-drone-parallel-V40.R 0 1 1 1.0 -1.0 -1.0 4.0 100 400 300 0 0 30 5 150 60 12
#cat("Args:
#      <algo>: Algo: 0 1 2 3
#      <readInitSolflag> 1 - Read initsol from file 0 - Save it to file. Only for algos that accepts it
#      <useInitSolflag> 1 - Use initsol. Only for algos that accepts it
#      <wCov> Coverage weight +
#      <wOverflow> Overflaw weight -
#      <wOverlap> Overlap weight -
#      <xDistdiff> Dist diff weight +
#      <maxIter> For evolutionary algo number of iters without improvement
#      <w> Region Width
#      <h> Region Height
#      <bsx> Base Stat. x coor
#      <bsy> Base Stat. y coor
#      <halftheta> Half of the drone beam angle 
#      <dhmin> Drone min height
#      <dhmax> Drone max height
#      <overlapTh> Overlap threshold
#      <ndrones> Drone number if -1 program will choose\n")
# 
# 
# args <- commandArgs()
# cat("Args:\n")
# print(args)
########################################################################################################



#Select the GA R-function
#0 for the GA - Genetic Algorithms
#1 for the genoud - GENetic Optimization Using Derivatives 
#2 for the DEoptim - Global Optimization by Differential Evolution
#3 for the GenSA - Generalized Simulated Annealing Function
#4 for the optimParallel - parallel version of the L-BFGS-B method of General-purpose Optimization
algo <- 0
#algo <- as.numeric(args[7])
# The following variable is meaningful if your algo accepts initial solution!!!
# If you want to read from file previously generated initail solution set to 1
# 0 means to regenerate the initial solution matrix for drone positions and then save it to file

readInitDroneMtxfromFile = 1
#readInitDroneMtxfromFile <-as.numeric(args[8])


useInitSol <- 0
#useInitSol <- as.numeric(args[9])


#Set the weights for scoring (fitness)
# xwCoverage=1.0
# xwOverflow=-1.0
# xwOverlap=-1.0
# xwDist=4.00
# 
# xwCoverage <- as.numeric(args[10])
# xwOverflow <- as.numeric(args[11])
# xwOverlap <- as.numeric(args[12])
# xwDist <- as.numeric(args[13])


#Set the number of iterations for evolutionary algorithms
#MaxIteration <- 100

#MaxIteration <- as.numeric(args[14])
########################################################################################################


#Region size
#Remember normally (height ~ row ~ y), (width ~ col ~ x)
#But EBImage functions flip-flip-trans the matrix stuff!!!
w<-400 # This will be x - horizontal dim
h<-300 # This will be y - vertical dim

# w  <- as.numeric(args[15])
# h  <- as.numeric(args[16])


#"Virtual" BS position. In fact the entry point of the networking to the region.
#"Real" BS is always will be far away from the region
#But upto that point somehow with other drones the signal is coming
#This part is simple: You just put drones in linear config
#Just to bring the signal to that point!
bsx <- 0
bsy <- 0

# bsx <- as.numeric(args[17])
# bsy <- as.numeric(args[18])

#Drone beam cone angle in degree. Half of the cone infact!!!
halfTheta <- 30 

#halfTheta  <- as.numeric(args[19])


#Drone min/max altitude 5-300m
hmin<-5
hmax<-150

# 
# hmin <- as.numeric(args[20])
# hmax <- as.numeric(args[21])

#Fudgy way of representing max distance for a drone on this psecific region
#Like going to opposite diagonal in other words longest distance and highest altitude
maxDist <- sqrt(hmax^2+w^2+h^2)

#Overlap threshold as percentage: 50 means high penalty for overlapping 50% of the area of the circle
overlapThreshold <- 60
#overlapThreshold <- as.numeric(args[22])

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


# #Lets set number of drones to 2 * ndrones
# tempArg <- as.numeric(args[23])
# if (tempArg == -1) {
#   ndrones <- 2*minNdrones
# } else {
#   ndrones <- as.numeric(args[23])
# }


#Just put a value later it will change
xwScore=1.0
textFileName <- ""

txtForInitial <- ""
txtForFinal <- ""

ndronesVal <- 0
wCoveragex <- 0
wOverflowx <- 0
wOverlapx <- 0
wDistx <- 0
MaxIterationx <- 0

########################################################################################################



########################################################################################################
#Setting useful data structures
########################################################################################################
#Region as matrix
region <- matrix(0,nrow=w, ncol=h)

#Auxilary region matricesn
#NOTE THE COORDINATE INCOMPATIBLITY BETWEEN MATRICES AND THE EBIMAGE FUNCTIONS!!!
#Just when you define matrix for row use size in x-horizonta-width and col size in y-vertical-height!!!!
#Use row as x and col as y
tempRegion1 <- matrix(0,nrow=w, ncol=h)
tempRegion2 <- matrix(0,nrow=w, ncol=h)
showRegion  <- matrix(0,nrow=w, ncol=h)
initDroneMtx <- matrix(0,nrow=w, ncol=h)
########################################################################################################
########################################################################################################
# End variables
########################################################################################################

## Return the machine CPU
cat("Machine:     "); print(get_cpu()$model_name)

## Return number of true cores
cat("Num cores:   "); print(detectCores(logical = FALSE))

## Return number of threads
cat("Num threads: "); print(detectCores(logical = TRUE))

## Return the machine RAM
cat("RAM:         "); print (get_ram()); cat("\n")


