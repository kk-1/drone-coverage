
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

# Read in csv files
df <- read.csv(filename, header = TRUE, sep = ",")

#Check the statistics by hand with the following lines
#Get the mean time for all algortihms at 10 drones
#mean(df$Time[df$Ndrones==10])
#Get the mean time for GA algortihm for 10 drones
#NOTICE THE SPACE AT THE END!!!!
#mean(df$Time[df$Algo=="GA " & df$Ndrones==10])

####################################################################################################
#Plots for running time
####################################################################################################
df %>%  group_by(Scn, Algo, IS, Ndrones) %>% summarize(average=mean(Time)) -> dfTime
colnames(dfTime) <- c("Scn","Algo","IS","Ndrones","Time")


dfTime$Algo <- trimws(dfTime$Algo)
dfTimeUnited <- dfTime %>% unite(Algo, Algo, IS, sep = ".")
dfTimeUnited$Algo <- factor(dfTimeUnited$Algo)
dfTime <- dfTimeUnited

#Grouped bar plot for S1
####################################################################################################

dfTimeS1 <- dfTime[dfTime$Scn=="s1 ",]
ggplot(dfTimeS1, aes(fill=Algo, y=Time, x=Ndrones)) + 
  geom_bar(position="dodge", stat="identity", width=1.5) + 
  labs(title = "S1 (1 0 0 0): Running Time - 1/0::With/No InitSoln", x = "N Drones", y = "Time(sec)") +
  scale_x_continuous(breaks=seq(0,12,2))+
  theme(axis.text=element_text(size=24),
        axis.title=element_text(size=26,face="bold"),
        plot.title = element_text(size=28, face="bold"),
        legend.title=element_text(face = "bold", size=26), 
        legend.text=element_text(size=24),
        legend.box.background = element_rect(color="red", size=1),
        legend.position = "bottom"
        ) 

pngFname <- paste0("RunningTime-S1-1_0_0_0",".png")
ggsave(pngFname, width = 40, height = 20, units = "cm")

cat("Plot saved in",pngFname,"\n\n")





#Grouped bar plot for S2
####################################################################################################

dfTimeS2 <- dfTime[dfTime$Scn=="s2 ",]
ggplot(dfTimeS2, aes(fill=Algo, y=Time, x=Ndrones)) + 
  geom_bar(position="dodge", stat="identity", width=1.5) + 
  labs(title = "S2 (1 -1 -1 0): Running Time - 1/0::With/No InitSoln", x = "N Drones", y = "Time(sec)") +
  scale_x_continuous(breaks=seq(0,12,2))+
  theme(axis.text=element_text(size=24),
        axis.title=element_text(size=26,face="bold"),
        plot.title = element_text(size=28, face="bold"),
        legend.title=element_text(size=26), 
        legend.text=element_text(size=24),
        legend.box.background = element_rect(color="red", size=1),
        legend.position = "bottom"
  )

pngFname <- paste0("RunningTime-S2-1_-1_-1_0",".png")
ggsave(pngFname, width = 40, height = 20, units = "cm")

cat("Plot saved in",pngFname,"\n\n")




####################################################################################################




#Grouped bar plot for S3
####################################################################################################

dfTimeS3 <- dfTime[dfTime$Scn=="s3 ",]
ggplot(dfTimeS3, aes(fill=Algo, y=Time, x=Ndrones)) + 
  geom_bar(position="dodge", stat="identity", width=1.5) + 
  labs(title = "S3 (1 -1 -1 0.5): Running Time - 1/0::With/No InitSoln", x = "N Drones", y = "Time(sec)") +
  scale_x_continuous(breaks=seq(0,12,2))+
  theme(axis.text=element_text(size=24),
        axis.title=element_text(size=26,face="bold"),
        plot.title = element_text(size=28, face="bold"),
        legend.title=element_text(size=26), 
        legend.text=element_text(size=24),
        legend.box.background = element_rect(color="red", size=1),
        legend.position = "bottom"
  )

pngFname <- paste0("RunningTime-S3-1_-1_-1_0.5",".png")
ggsave(pngFname, width = 40, height = 20, units = "cm")

cat("Plot saved in",pngFname,"\n\n")

####################################################################################################












#Grouped bar plot for S4
####################################################################################################

dfTimeS4 <- dfTime[dfTime$Scn=="s4 ",]
ggplot(dfTimeS4, aes(fill=Algo, y=Time, x=Ndrones)) + 
  geom_bar(position="dodge", stat="identity", width=1.5) + 
  labs(title = "S4 (1 0 0 0.5): Running Time - 1/0::With/No InitSoln", x = "N Drones", y = "Time(sec)") +
  scale_x_continuous(breaks=seq(0,12,2))+
  theme(axis.text=element_text(size=24),
        axis.title=element_text(size=26,face="bold"),
        plot.title = element_text(size=28, face="bold"),
        legend.title=element_text(size=26), 
        legend.text=element_text(size=24),
        legend.box.background = element_rect(color="red", size=1),
        legend.position = "bottom"
  )

pngFname <- paste0("RunningTime-S4-1_0_0_0.5",".png")
ggsave(pngFname, width = 40, height = 20, units = "cm")

cat("Plot saved in",pngFname,"\n\n")



####################################################################################################














####################################################################################################
#Plots for coverage
####################################################################################################
df %>% group_by(Scn, Algo, IS, Ndrones) %>% summarize(average=mean(FinCov)) -> dfCov
colnames(dfCov) <- c("Scn","Algo","IS","Ndrones","Cov")

#Adding the init soln to dataframe

dfCov <- as.data.frame(lapply(dfCov, addInitSol))
nd <- unique(dfCov$Ndrones)
n = length(nd) 


 for(i in 1:n)
 {
  
  number_of_drones <- as.numeric(nd[i])
  cov1 <- as.numeric(df$InitCov[df$Ndrones==number_of_drones][1])
  cat ("data:", number_of_drones, cov1,"\n")
  dfCov <- rbind(dfCov,c("s1 ","InitSol ",1,number_of_drones,cov1))
  dfCov <- rbind(dfCov,c("s2 ","InitSol ",1,number_of_drones,cov1))
  dfCov <- rbind(dfCov,c("s3 ","InitSol ",1,number_of_drones,cov1))
  dfCov <- rbind(dfCov,c("s4 ","InitSol ",1,number_of_drones,cov1))
 }


dfCov$Ndrones <- as.numeric(dfCov$Ndrones)
dfCov$Cov <- as.numeric(dfCov$Cov)


dfCov$Algo <- trimws(dfCov$Algo)
dfCovUnited <- dfCov %>% unite(Algo, Algo, IS, sep = ".")
dfCovUnited$Algo <- factor(dfCovUnited$Algo)
dfCov <- dfCovUnited


#Grouped bar plot for S1
####################################################################################################

dfCovS1 <- dfCov[dfCov$Scn=="s1 ",]
ggplot(dfCovS1, aes(fill=Algo, y=Cov, x=Ndrones)) + 
  geom_bar(position="dodge", stat="identity", width=1.5) + 
  labs(title = "S1 (1 0 0 0): Coverage Percent - 1/0::With/No InitSoln", x = "N Drones", y = "Percent") +
  scale_x_continuous(breaks=seq(0,12,2))+
  theme(axis.text=element_text(size=24),
        axis.title=element_text(size=26,face="bold"),
        plot.title = element_text(size=28, face="bold"),
        legend.title=element_text(size=26), 
        legend.text=element_text(size=24),
        legend.box.background = element_rect(color="red", size=1),
        legend.position = "bottom"
  )


pngFname <- paste0("CoveragePercent-S1-1_0_0_0",".png")
ggsave(pngFname, width = 40, height = 20, units = "cm")

cat("Plot saved in",pngFname,"\n\n")





#Grouped bar plot for S2
####################################################################################################

dfCovS2 <- dfCov[dfCov$Scn=="s2 ",]
ggplot(dfCovS2, aes(fill=Algo, y=Cov, x=Ndrones)) + 
  geom_bar(position="dodge", stat="identity", width=1.5) + 
  labs(title = "S2 (1 -1 -1 0): Coverage Percent - 1/0::With/No InitSoln", x = "N Drones", y = "Percent") +
  scale_x_continuous(breaks=seq(0,12,2))+
  theme(axis.text=element_text(size=24),
        axis.title=element_text(size=26,face="bold"),
        plot.title = element_text(size=28, face="bold"),
        legend.title=element_text(size=26), 
        legend.text=element_text(size=24),
        legend.box.background = element_rect(color="red", size=1),
        legend.position = "bottom"
  )

pngFname <- paste0("CoveragePercent-S2-1_-1_-1_0",".png")
ggsave(pngFname, width = 40, height = 20, units = "cm")

cat("Plot saved in",pngFname,"\n\n")



####################################################################################################




#Grouped bar plot for S3
####################################################################################################

dfCovS3 <- dfCov[dfCov$Scn=="s3 ",]
ggplot(dfCovS3, aes(fill=Algo, y=Cov, x=Ndrones)) + 
  geom_bar(position="dodge", stat="identity", width=1.5) + 
  labs(title = "S3 (1 -1 -1 0.5): Coverage Percent - 1/0::With/No InitSoln", x = "N Drones", y = "Percent") +
  scale_x_continuous(breaks=seq(0,12,2))+
  theme(axis.text=element_text(size=24),
        axis.title=element_text(size=26,face="bold"),
        plot.title = element_text(size=28, face="bold"),
        legend.title=element_text(size=26), 
        legend.text=element_text(size=24),
        legend.box.background = element_rect(color="red", size=1),
        legend.position = "bottom"
  )

pngFname <- paste0("CoveragePercent-S3-1_-1_-1_0.5",".png")
ggsave(pngFname, width = 40, height = 20, units = "cm")

cat("Plot saved in",pngFname,"\n\n")





####################################################################################################












#Grouped bar plot for S4
####################################################################################################

dfCovS4 <- dfCov[dfCov$Scn=="s4 ",]
ggplot(dfCovS4, aes(fill=Algo, y=Cov, x=Ndrones)) + 
  geom_bar(position="dodge", stat="identity", width=1.5) + 
  labs(title = "S4 (1 0 0 0.5): Coverage Percent - 1/0::With/No InitSoln", x = "N Drones", y = "Percent") +
  scale_x_continuous(breaks=seq(0,12,2))+
  theme(axis.text=element_text(size=24),
        axis.title=element_text(size=26,face="bold"),
        plot.title = element_text(size=28, face="bold"),
        legend.title=element_text(size=26), 
        legend.text=element_text(size=24),
        legend.box.background = element_rect(color="red", size=1),
        legend.position = "bottom"
  )

pngFname <- paste0("CoveragePercent-S4-1_0_0_0.5",".png")
ggsave(pngFname, width = 40, height = 20, units = "cm")

cat("Plot saved in",pngFname,"\n\n")



####################################################################################################










####################################################################################################
#Plots for TotDist
####################################################################################################
df %>% group_by(Scn, Algo, IS, Ndrones) %>% summarize(average=mean(FinTotDist)) -> dfTotDist
colnames(dfTotDist) <- c("Scn","Algo","IS","Ndrones","TotDist")


#Adding the init soln to dataframe

dfTotDist <- as.data.frame(lapply(dfTotDist, addInitSol))
nd <- unique(dfTotDist$Ndrones)
n = length(nd)
for(i in 1:n)
{
  number_of_drones <- as.numeric(nd[i])
  dist1 <- as.numeric(df$InitTotDist[df$Ndrones==number_of_drones][1])
  cat ("data:",number_of_drones, dist1,"\n")
  dfTotDist  <- rbind(dfTotDist,c("s1 ","InitSol ",1,number_of_drones,dist1))
  dfTotDist  <- rbind(dfTotDist,c("s2 ","InitSol ",1,number_of_drones,dist1))
  dfTotDist  <- rbind(dfTotDist,c("s3 ","InitSol ",1,number_of_drones,dist1))
  dfTotDist  <- rbind(dfTotDist,c("s4 ","InitSol ",1,number_of_drones,dist1))
}

dfTotDist$Ndrones <- as.numeric(dfTotDist$Ndrones)
dfTotDist$TotDist <- as.numeric(dfTotDist$TotDist)



dfTotDist$Algo <- trimws(dfTotDist$Algo)
dfTotDistUnited <- dfTotDist %>% unite(Algo, Algo, IS, sep = ".")
dfTotDistUnited$Algo <- factor(dfTotDistUnited$Algo)
dfTotDist <- dfTotDistUnited




#Grouped bar plot for S1
####################################################################################################

dfTotDistS1 <- dfTotDist[dfTotDist$Scn=="s1 ",]
ggplot(dfTotDistS1, aes(fill=Algo, y=TotDist, x=Ndrones)) + 
  geom_bar(position="dodge", stat="identity", width=1.5) + 
  labs(title = "S1 (1 0 0 0): Total Distance (m) - 1/0::With/No InitSoln", x = "N Drones", y = "Total Drone Distance(m)") +
  scale_x_continuous(breaks=seq(0,12,2))+
  theme(axis.text=element_text(size=24),
        axis.title=element_text(size=26,face="bold"),
        plot.title = element_text(size=28, face="bold"),
        legend.title=element_text(size=26), 
        legend.text=element_text(size=24),
        legend.box.background = element_rect(color="red", size=1),
        legend.position = "bottom"
  )



pngFname <- paste0("TotDist-S1-1_0_0_0",".png")
ggsave(pngFname, width = 40, height = 20, units = "cm")

cat("Plot saved in",pngFname,"\n\n")








#Grouped bar plot for S2
####################################################################################################

dfTotDistS2 <- dfTotDist[dfTotDist$Scn=="s2 ",]
ggplot(dfTotDistS2, aes(fill=Algo, y=TotDist, x=Ndrones)) + 
  geom_bar(position="dodge", stat="identity", width=1.5) + 
  labs(title = "S2 (1 -1 -1 0): Total Distance (m) - 1/0::With/No InitSoln", x = "N Drones", y = "Total Drone Distance(m)") +
  scale_x_continuous(breaks=seq(0,12,2))+
  theme(axis.text=element_text(size=24),
        axis.title=element_text(size=26,face="bold"),
        plot.title = element_text(size=28, face="bold"),
        legend.title=element_text(size=26), 
        legend.text=element_text(size=24),
        legend.box.background = element_rect(color="red", size=1),
        legend.position = "bottom"
  )

pngFname <- paste0("TotDist-S2-1_-1_-1_0",".png")
ggsave(pngFname, width = 40, height = 20, units = "cm")

cat("Plot saved in",pngFname,"\n\n")



####################################################################################################




#Grouped bar plot for S3
####################################################################################################

dfTotDistS3 <- dfTotDist[dfTotDist$Scn=="s3 ",]
ggplot(dfTotDistS3, aes(fill=Algo, y=TotDist, x=Ndrones)) + 
  geom_bar(position="dodge", stat="identity", width=1.5) + 
  labs(title = "S3 (1 -1 -1 0.5): Total Distance (m) - 1/0::With/No InitSoln", x = "N Drones", y = "Total Drone Distance(m)") +
  scale_x_continuous(breaks=seq(0,12,2))+
  theme(axis.text=element_text(size=24),
        axis.title=element_text(size=26,face="bold"),
        plot.title = element_text(size=28, face="bold"),
        legend.title=element_text(size=26), 
        legend.text=element_text(size=24),
        legend.box.background = element_rect(color="red", size=1),
        legend.position = "bottom"
  )

pngFname <- paste0("TotDist-S3-1_-1_-1_0.5",".png")
ggsave(pngFname, width = 40, height = 20, units = "cm")

cat("Plot saved in",pngFname,"\n\n")



####################################################################################################












#Grouped bar plot for S4
####################################################################################################

dfTotDistS4 <- dfTotDist[dfTotDist$Scn=="s4 ",]
ggplot(dfTotDistS4, aes(fill=Algo, y=TotDist, x=Ndrones)) + 
  geom_bar(position="dodge", stat="identity", width=1.5) + 
  labs(title = "S4 (1 0 0 0.5): Total Distance (m) - 1/0::With/No InitSoln", x = "N Drones", y = "Total Drone Distance(m)") +
  scale_x_continuous(breaks=seq(0,12,2))+
  theme(axis.text=element_text(size=24),
        axis.title=element_text(size=26,face="bold"),
        plot.title = element_text(size=28, face="bold"),
        legend.title=element_text(size=26), 
        legend.text=element_text(size=24),
        legend.box.background = element_rect(color="red", size=1),
        legend.position = "bottom"
  )

pngFname <- paste0("TotDist-S4-1_0_0_0.5",".png")
ggsave(pngFname, width = 40, height = 20, units = "cm")

cat("Plot saved in",pngFname,"\n\n")




####################################################################################################






