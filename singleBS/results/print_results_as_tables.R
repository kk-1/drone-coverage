
#Clean the environment
remove(list = ls())

#Set the results output dir
setwd("out")
library(ggplot2)
library(dplyr)
library(tidyr)


#NOTICE THE SPACE AT THE END!!!!
#Function to add extra level as factor to df
addInitSol <- function(x){
  if(is.factor(x)) return(factor(x, levels=c(levels(x), "InitSol ")))
  return(x)
}


# args <- commandArgs()
# cat("Args:\n")
# print(args)
# filename <- args[7]
# 
# Init_Oflow_Olap_Ignored.csv
# Init_Oflow_Olap_Penalty.csv
# noInit_Oflow_Olap_Ignored.csv
# noInit_Oflow_Olap_Penalty.csv
# AllScn.csv

####################################################################################################
####################################################################################################
# Read the meta results file with all scenarios
####################################################################################################
filename="AllScn.csv"
cat("Reading CSV File:",filename,"\n")
ftitle <- tools::file_path_sans_ext(filename)

precision=2

# Read in csv files
df <- read.csv(filename, header = TRUE, sep = ",")

####################################################################################################
#Loop to output values for latex table
####################################################################################################
#Set vars for table output
#For S1 2 drones
for (i in seq(2,12,2)){
  cat("\n %",i,"Drones:\n")
  cat ("\\multirow{4}{*}{",i,"} ")
for (s in levels(df$Scn)) {
 
  #Time
  isTime <- 1
  doTimeNoIS <- df[df$Algo==" DEoptim " & df$Ndrones == i & df$Scn==s & df$IS==0, ]$Time
  gaTimeIS <- df[df$Algo==" GA " & df$Ndrones == i & df$Scn==s & df$IS==1, ]$Time
  gaTimeNoIS <- df[df$Algo==" GA " & df$Ndrones == i & df$Scn==s & df$IS==0, ]$Time
  saTimeIS <- df[df$Algo==" GenSA " & df$Ndrones == i & df$Scn==s & df$IS==1, ]$Time
  saTimeNoIS <- df[df$Algo==" GenSA " & df$Ndrones == i & df$Scn==s & df$IS==0, ]$Time

  #Cov
  isCov <- df[df$Algo==" GA " & df$Ndrones == i & df$Scn==s & df$IS==1, ]$InitCov
  doCovNoIS <- df[df$Algo==" DEoptim " & df$Ndrones == i & df$Scn==s & df$IS==0, ]$FinCov
  gaCovIS <- df[df$Algo==" GA " & df$Ndrones == i & df$Scn==s & df$IS==1, ]$FinCov
  gaCovNoIS <- df[df$Algo==" GA " & df$Ndrones == i & df$Scn==s & df$IS==0, ]$FinCov
  saCovIS <- df[df$Algo==" GenSA " & df$Ndrones == i & df$Scn==s & df$IS==1, ]$FinCov
  saCovNoIS <- df[df$Algo==" GenSA " & df$Ndrones == i & df$Scn==s & df$IS==0, ]$FinCov

  #TotDist
  isTotDist <- df[df$Algo==" GA " & df$Ndrones == i & df$Scn==s & df$IS==1, ]$InitTotDist
  doTotDistNoIS <- df[df$Algo==" DEoptim " & df$Ndrones == i & df$Scn==s & df$IS==0, ]$FinTotDist
  gaTotDistIS <- df[df$Algo==" GA " & df$Ndrones == i & df$Scn==s & df$IS==1, ]$FinTotDist
  gaTotDistNoIS <- df[df$Algo==" GA " & df$Ndrones == i & df$Scn==s & df$IS==0, ]$FinTotDist 
  saTotDistIS <- df[df$Algo==" GenSA " & df$Ndrones == i & df$Scn==s & df$IS==1, ]$FinTotDist
  saTotDistNoIS <- df[df$Algo==" GenSA " & df$Ndrones == i & df$Scn==s & df$IS==0, ]$FinTotDist

  
  #S1 & \multicolumn{2}{c|}{d1} & \multicolumn{2}{c|}{d2} & d3 & d4 & d5 & d6 & \multicolumn{2}{c|}{d7} & \multicolumn{2}{c|}{d8} & d9 & d10 & d11 & d12 & \multicolumn{2}{c|}{d13} & \multicolumn{2}{c|}{d14} & d15 & d16 & d17 & d18 \tabularnewline \hline
  cat ("&" ,s , "& \\multicolumn{2}{c|}{", round(isTime ,precision),"} &", "\\multicolumn{2}{c|}{",round(doTimeNoIS,precision),"} &",round(gaTimeIS,precision),"&",round( gaTimeNoIS,precision), "&",round(saTimeIS,precision), "&",round(saTimeNoIS,precision),
           "& \\multicolumn{2}{c|}{", round(isCov,precision),"} &", "\\multicolumn{2}{c|}{",round(doCovNoIS,precision),"} &", round(gaCovIS,precision),"&", round(gaCovNoIS,precision),"&",round(saCovIS,precision), "&",round(saCovNoIS,precision),
           "& \\multicolumn{2}{c|}{", round(isTotDist,precision),"} &", "\\multicolumn{2}{c|}{",round(doTotDistNoIS,precision),"} &", round(gaTotDistIS,precision),"&", round(gaTotDistNoIS,precision), "&",round(saTotDistIS,precision), "&",round(saTotDistNoIS,precision),
           "\\tabularnewline \\cline{2-26}\n"
       )
  
 }
}

####################################################################################################
