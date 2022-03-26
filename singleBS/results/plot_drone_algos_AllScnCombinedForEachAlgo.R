
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


#args <- commandArgs()
# cat("Args:\n")
# print(args)
# filename <- args[7]
# 
# Init_Oflow_Olap_Ignored.csv
# Init_Oflow_Olap_Penalty.csv
# noInit_Oflow_Olap_Ignored.csv
# noInit_Oflow_Olap_Penalty.csv

####################################################################################################
####################################################################################################
# Init_Oflow_Olap_Ignored.csv
####################################################################################################
filename="Init_Oflow_Olap_Ignored.csv"
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

#Plots for running time
####################################################################################################
df %>%  group_by(Algo, Ndrones) %>% summarize(average=mean(Time)) -> dfTime
colnames(dfTime) <- c("Algo","Ndrones","Time")

ggplot(dfTime, aes(y=dfTime$Time, x=dfTime$Ndrones)) + 
  #geom_line(aes(color = df$Algo, linetype = df$Algo)) + 
  geom_point() +
  geom_line(aes(color = dfTime$Algo)) + 
  labs(title = "Running Time - With init soln, overflow, overlap ignored", x = "N Drones", y = "Time(sec)", color='Algo')+
  scale_x_continuous(breaks=seq(0,12,2))
 

pngFname <- paste0(ftitle,"-time.png")
ggsave(pngFname, width = 40, height = 20, units = "cm")

cat("Plot saved in",pngFname,"\n\n")
####################################################################################################


#Plots for coverage
####################################################################################################
df %>% group_by(Algo, Ndrones) %>% summarize(average=mean(FinCov)) -> dfCov
colnames(dfCov) <- c("Algo","Ndrones","Cov")

#Adding the init soln to dataframe

dfCov <- as.data.frame(lapply(dfCov, addInitSol))
nd <- unique(dfCov$Ndrones)
n = length(nd)
for(i in 1:n)
{
  number_of_drones <- as.numeric(nd[i])
  cov1 <- as.numeric(df$InitCov[df$Ndrones==number_of_drones][1])
  cat ("data:",number_of_drones, cov1,"\n")
  dfCov <- rbind(dfCov,c("InitSol ",number_of_drones,cov1))
}

dfCov$Ndrones <- as.numeric(dfCov$Ndrones)
dfCov$Cov <- as.numeric(dfCov$Cov)

ggplot(dfCov, aes(y=dfCov$Cov, x=dfCov$Ndrones)) + 
  geom_point() +
  geom_line(aes(color = dfCov$Algo)) + 
  labs(title = "Coverage Percent - With init soln, overflow, overlap ignored", x = "N Drones", y = "Coverage Percent", color='Algo') +
  scale_y_continuous(breaks=seq(0,110.0,10.0)) +
  scale_x_continuous(breaks=seq(0,12,2))
  
  
pngFname <- paste0(ftitle,"-coverage.png")
ggsave(pngFname, width = 40, height = 20, units = "cm")
  
cat("Plot saved in",pngFname,"\n\n")
  
#Plots for TotDist
####################################################################################################
df %>% group_by(Algo, Ndrones) %>% summarize(average=mean(FinTotDist)) -> dfTotDist
colnames(dfTotDist) <- c("Algo","Ndrones","TotDist")


#Adding the init soln to dataframe

dfTotDist <- as.data.frame(lapply(dfTotDist, addInitSol))
nd <- unique(dfTotDist$Ndrones)
n = length(nd)
for(i in 1:n)
{
  number_of_drones <- as.numeric(nd[i])
  dist1 <- as.numeric(df$InitTotDist[df$Ndrones==number_of_drones][1])
  cat ("data:",number_of_drones, dist1,"\n")
  dfTotDist <- rbind(dfTotDist,c("InitSol ",number_of_drones,dist1))
}

dfTotDist$Ndrones <- as.numeric(dfTotDist$Ndrones)
dfTotDist$TotDist <- as.numeric(dfTotDist$TotDist)
 
ggplot(dfTotDist, aes(y=dfTotDist$TotDist, x=dfTotDist$Ndrones)) +     
  # geom_line(aes(color = df$ECC_PB, linetype = df$ECC_PB)) + 
  geom_point() +
  geom_line(aes(color = dfCov$Algo)) + 
  labs(title = "Total Distance of Drones from BS - With init soln, overflow, overlap ignored", x = "N Drones", y = "Total Distance(m)", color='Algo') +
  scale_x_continuous(breaks=seq(0,12,2))  
 


pngFname <- paste0(ftitle,"-totdist.png")
ggsave(pngFname, width = 40, height = 20, units = "cm")

cat("Plot saved in",pngFname,"\n\n") 
####################################################################################################
####################################################################################################


####################################################################################################
####################################################################################################
# Init_Oflow_Olap_Penalty.csv
####################################################################################################
filename="Init_Oflow_Olap_Penalty.csv"
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

#Plots for running time
####################################################################################################
df %>%  group_by(Algo, Ndrones) %>% summarize(average=mean(Time)) -> dfTime
colnames(dfTime) <- c("Algo","Ndrones","Time")

ggplot(dfTime, aes(y=dfTime$Time, x=dfTime$Ndrones)) + 
  #geom_line(aes(color = df$Algo, linetype = df$Algo)) + 
  geom_point() +
  geom_line(aes(color = dfTime$Algo)) + 
  labs(title = "Running Time - With init soln, overflow, overlap as penalty", x = "N Drones", y = "Time(sec)", color='Algo')+
  scale_x_continuous(breaks=seq(0,12,2))


pngFname <- paste0(ftitle,"-time.png")
ggsave(pngFname, width = 40, height = 20, units = "cm")

cat("Plot saved in",pngFname,"\n\n")
####################################################################################################


#Plots for coverage
####################################################################################################
df %>% group_by(Algo, Ndrones) %>% summarize(average=mean(FinCov)) -> dfCov
colnames(dfCov) <- c("Algo","Ndrones","Cov")

#Adding the init soln to dataframe

dfCov <- as.data.frame(lapply(dfCov, addInitSol))
nd <- unique(dfCov$Ndrones)
n = length(nd)
for(i in 1:n)
{
  number_of_drones <- as.numeric(nd[i])
  cov1 <- as.numeric(df$InitCov[df$Ndrones==number_of_drones][1])
  cat ("data:",number_of_drones, cov1,"\n")
  dfCov <- rbind(dfCov,c("InitSol ",number_of_drones,cov1))
}

dfCov$Ndrones <- as.numeric(dfCov$Ndrones)
dfCov$Cov <- as.numeric(dfCov$Cov)

ggplot(dfCov, aes(y=dfCov$Cov, x=dfCov$Ndrones)) + 
  # geom_line(aes(color = df$ECC_PB, linetype = df$ECC_PB)) + 
  geom_point() +
  geom_line(aes(color = dfCov$Algo)) + 
  labs(title = "Coverage Percent - With init soln, overflow, overlap as penalty", x = "N Drones", y = "Coverage Percent", color='Algo') +
  scale_y_continuous(breaks=seq(0,110,10))+
scale_x_continuous(breaks=seq(0,12,2))


pngFname <- paste0(ftitle,"-coverage.png")
ggsave(pngFname, width = 40, height = 20, units = "cm")

cat("Plot saved in",pngFname,"\n\n")

#Plots for TotDist
####################################################################################################
df %>% group_by(Algo, Ndrones) %>% summarize(average=mean(FinTotDist)) -> dfTotDist
colnames(dfTotDist) <- c("Algo","Ndrones","TotDist")


#Adding the init soln to dataframe

dfTotDist <- as.data.frame(lapply(dfTotDist, addInitSol))
nd <- unique(dfTotDist$Ndrones)
n = length(nd)
for(i in 1:n)
{
  number_of_drones <- as.numeric(nd[i])
  dist1 <- as.numeric(df$InitTotDist[df$Ndrones==number_of_drones][1])
  cat ("data:",number_of_drones, dist1,"\n")
  dfTotDist <- rbind(dfTotDist,c("InitSol ",number_of_drones,dist1))
}

dfTotDist$Ndrones <- as.numeric(dfTotDist$Ndrones)
dfTotDist$TotDist <- as.numeric(dfTotDist$TotDist)


ggplot(dfTotDist, aes(y=dfTotDist$TotDist, x=dfTotDist$Ndrones)) +     
  # geom_line(aes(color = df$ECC_PB, linetype = df$ECC_PB)) + 
  geom_point() +
  geom_line(aes(color = dfCov$Algo)) + 
  labs(title = "Total Distance of Drones from BS - With init soln, overflow, overlap as penalty", x = "N Drones", y = "Total Distance(m)", color='Algo') +
  scale_x_continuous(breaks=seq(0,12,2))  



pngFname <- paste0(ftitle,"-totdist.png")
ggsave(pngFname, width = 40, height = 20, units = "cm")

cat("Plot saved in",pngFname,"\n\n") 


####################################################################################################
####################################################################################################




####################################################################################################
####################################################################################################
# noInit_Oflow_Olap_Ignored.csv
####################################################################################################
filename="noInit_Oflow_Olap_Ignored.csv"
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

#Plots for running time
####################################################################################################
df %>%  group_by(Algo, Ndrones) %>% summarize(average=mean(Time)) -> dfTime
colnames(dfTime) <- c("Algo","Ndrones","Time")




ggplot(dfTime, aes(y=dfTime$Time, x=dfTime$Ndrones)) + 
  #geom_line(aes(color = df$Algo, linetype = df$Algo)) + 
  geom_point() +
  geom_line(aes(color = dfTime$Algo)) + 
  labs(title = "Running Time - Without init soln, overflow, overlap ignored", x = "N Drones", y = "Time(sec)", color='Algo')+
  scale_x_continuous(breaks=seq(0,12,2))


pngFname <- paste0(ftitle,"-time.png")
ggsave(pngFname, width = 40, height = 20, units = "cm")

cat("Plot saved in",pngFname,"\n\n")
####################################################################################################


#Plots for coverage
####################################################################################################
df %>% group_by(Algo, Ndrones) %>% summarize(average=mean(FinCov)) -> dfCov
colnames(dfCov) <- c("Algo","Ndrones","Cov")



#Adding the init soln to dataframe

dfCov <- as.data.frame(lapply(dfCov, addInitSol))
nd <- unique(dfCov$Ndrones)
n = length(nd)
for(i in 1:n)
{
  number_of_drones <- as.numeric(nd[i])
  cov1 <- as.numeric(df$InitCov[df$Ndrones==number_of_drones][1])
  cat ("data:",number_of_drones, cov1,"\n")
  dfCov <- rbind(dfCov,c("InitSol ",number_of_drones,cov1))
}

dfCov$Ndrones <- as.numeric(dfCov$Ndrones)
dfCov$Cov <- as.numeric(dfCov$Cov)

 

ggplot(dfCov, aes(y=dfCov$Cov, x=dfCov$Ndrones)) + 
  # geom_line(aes(color = df$ECC_PB, linetype = df$ECC_PB)) + 
  geom_point() +
  geom_line(aes(color = dfCov$Algo)) + 
  labs(title = "Coverage Percent - Without init soln, overflow, overlap ignored", x = "N Drones", y = "Coverage Percent", color='Algo') +
  scale_y_continuous(breaks=seq(0,110,10))+
scale_x_continuous(breaks=seq(0,12,2))


pngFname <- paste0(ftitle,"-coverage.png")
ggsave(pngFname, width = 40, height = 20, units = "cm")

cat("Plot saved in",pngFname,"\n\n")

#Plots for TotDist
####################################################################################################
df %>% group_by(Algo, Ndrones) %>% summarize(average=mean(FinTotDist)) -> dfTotDist
colnames(dfTotDist) <- c("Algo","Ndrones","TotDist")


#Adding the init soln to dataframe

dfTotDist <- as.data.frame(lapply(dfTotDist, addInitSol))
nd <- unique(dfTotDist$Ndrones)
n = length(nd)
for(i in 1:n)
{
  number_of_drones <- as.numeric(nd[i])
  dist1 <- as.numeric(df$InitTotDist[df$Ndrones==number_of_drones][1])
  cat ("data:",number_of_drones, dist1,"\n")
  dfTotDist <- rbind(dfTotDist,c("InitSol ",number_of_drones,dist1))
}

dfTotDist$Ndrones <- as.numeric(dfTotDist$Ndrones)
dfTotDist$TotDist <- as.numeric(dfTotDist$TotDist)




ggplot(dfTotDist, aes(y=dfTotDist$TotDist, x=dfTotDist$Ndrones)) +     
  # geom_line(aes(color = df$ECC_PB, linetype = df$ECC_PB)) + 
  geom_point() +
  geom_line(aes(color = dfCov$Algo)) + 
  labs(title = "Total Distance of Drones from BS - Without init soln, overflow, overlap ignored", x = "N Drones", y = "Total Distance(m)", color='Algo') +
  scale_x_continuous(breaks=seq(0,12,2))  



pngFname <- paste0(ftitle,"-totdist.png")
ggsave(pngFname, width = 40, height = 20, units = "cm")

cat("Plot saved in",pngFname,"\n\n") 



####################################################################################################
####################################################################################################




####################################################################################################
####################################################################################################
# noInit_Oflow_Olap_Penalty.csv
####################################################################################################
filename="noInit_Oflow_Olap_Penalty.csv"
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

#Plots for running time
####################################################################################################
df %>%  group_by(Algo, Ndrones) %>% summarize(average=mean(Time)) -> dfTime
colnames(dfTime) <- c("Algo","Ndrones","Time")

ggplot(dfTime, aes(y=dfTime$Time, x=dfTime$Ndrones)) + 
  #geom_line(aes(color = df$Algo, linetype = df$Algo)) + 
  geom_point() +
  geom_line(aes(color = dfTime$Algo)) + 
  labs(title = "Running Time - Without init soln, overflow, overlap as penalty", x = "N Drones", y = "Time(sec)", color='Algo')+
  scale_x_continuous(breaks=seq(0,12,2))


pngFname <- paste0(ftitle,"-time.png")
ggsave(pngFname, width = 40, height = 20, units = "cm")

cat("Plot saved in",pngFname,"\n\n")
####################################################################################################


#Plots for coverage
####################################################################################################
df %>% group_by(Algo, Ndrones) %>% summarize(average=mean(FinCov)) -> dfCov
colnames(dfCov) <- c("Algo","Ndrones","Cov")



#Adding the init soln to dataframe

dfCov <- as.data.frame(lapply(dfCov, addInitSol))
nd <- unique(dfCov$Ndrones)
n = length(nd)
for(i in 1:n)
{
  number_of_drones <- as.numeric(nd[i])
  cov1 <- as.numeric(df$InitCov[df$Ndrones==number_of_drones][1])
  cat ("data:",number_of_drones, cov1,"\n")
  dfCov <- rbind(dfCov,c("InitSol ",number_of_drones,cov1))
}

dfCov$Ndrones <- as.numeric(dfCov$Ndrones)
dfCov$Cov <- as.numeric(dfCov$Cov)




ggplot(dfCov, aes(y=dfCov$Cov, x=dfCov$Ndrones)) + 
  # geom_line(aes(color = df$ECC_PB, linetype = df$ECC_PB)) + 
  geom_point() +
  geom_line(aes(color = dfCov$Algo)) + 
  labs(title = "Coverage Percent - Without init soln, overflow, overlap as penalty", x = "N Drones", y = "Coverage Percent", color='Algo') +
  scale_y_continuous(breaks=seq(0,110,10))+
scale_x_continuous(breaks=seq(0,12,2))


pngFname <- paste0(ftitle,"-coverage.png")
ggsave(pngFname, width = 40, height = 20, units = "cm")

cat("Plot saved in",pngFname,"\n\n")

#Plots for TotDist
####################################################################################################
df %>% group_by(Algo, Ndrones) %>% summarize(average=mean(FinTotDist)) -> dfTotDist
colnames(dfTotDist) <- c("Algo","Ndrones","TotDist")



#Adding the init soln to dataframe

dfTotDist <- as.data.frame(lapply(dfTotDist, addInitSol))
nd <- unique(dfTotDist$Ndrones)
n = length(nd)
for(i in 1:n)
{
  number_of_drones <- as.numeric(nd[i])
  dist1 <- as.numeric(df$InitTotDist[df$Ndrones==number_of_drones][1])
  cat ("data:",number_of_drones, dist1,"\n")
  dfTotDist <- rbind(dfTotDist,c("InitSol ",number_of_drones,dist1))
}

dfTotDist$Ndrones <- as.numeric(dfTotDist$Ndrones)
dfTotDist$TotDist <- as.numeric(dfTotDist$TotDist)





ggplot(dfTotDist, aes(y=dfTotDist$TotDist, x=dfTotDist$Ndrones)) +     
  # geom_line(aes(color = df$ECC_PB, linetype = df$ECC_PB)) + 
  geom_point() +
  geom_line(aes(color = dfCov$Algo)) + 
  labs(title = "Total Distance of Drones from BS - Without init soln, overflow, overlap as penalty", x = "N Drones", y = "Total Distance(m)", color='Algo') +
  scale_x_continuous(breaks=seq(0,12,2))  



pngFname <- paste0(ftitle,"-totdist.png")
ggsave(pngFname, width = 40, height = 20, units = "cm")

cat("Plot saved in",pngFname,"\n\n") 

####################################################################################################
####################################################################################################





