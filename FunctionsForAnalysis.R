
#####Functions#####
##Bhattacharyya using MASS package
bhatta <- function(df1, df2, bns = 512, xmin = -15, xmax =15, ymin = -15, ymax =15){
  dfprob <-kde2d(df1$X,df1$Y,n=bns, lims=c(xmin, xmax, ymin, ymax))
  dfprob$z <- dfprob$z /sum(dfprob$z)
  dfprob2 <-kde2d(df2$X,df2$Y,n=bns, lims=c(xmin, xmax, ymin, ymax))
  dfprob2$z <-dfprob2$z /sum(dfprob2$z)
  bhat <- sum(sqrt(dfprob2$z * dfprob$z))
  return(bhat)
}

TIZ <- function(movementdf, rewarddf){
  #caculates the zone
  h<-mean(rewarddf$X)
  k<-mean(rewarddf$Y)
  r <- 6
  #Calculates the percent time in zone
  zonemv<- subset(movementdf, (X-h)^2+(Y-k)^2 <= r^2)
  timespent <- length(zonemv$X)/length(movementdf$X)
  return(timespent)
}



##RADNOM Bits of code#####

test <- subset(MasterPilot2, Participant == 201 & Session == 1 & BlockID == 3 & TrialTime > 5)

df1 <- dplyr::select(test, X,Y)
df2 <- dplyr::select(test, X,Y)

#manipulte the overlap between sets = 
df2$X <- df2$X+10 
df2$Y <- df2$Y+10 

d1 <- df1
d2 <- df2

d1$d <- 1
d2$d <- 2
d12<-rbind(d1,d2)

ggplot(d12, aes(x = X, y = Y, color = as.factor(d)))+
  geom_point(data = d12, aes(x = X, y = Y, alpha=0.0002, color = as.factor(d)))+
  stat_density2d(data = d12, aes(x = X, y = Y,color = as.factor(d), alpha=..level..), geom="polygon")+
  theme_bw() + theme(panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_rect(colour = "black", size=4)) + scale_y_continuous(limit = c(-15, 15)) + scale_x_continuous(limit = c(-15, 15))

mass_bhatta(df1,df2)
bhatta(df1,df2)
TIZ(df1,df2)


####Time In Zone - Calculates how much time the participant was moving in the correct Zone####
TIZQ <- function(movementdf, rewarddf){
  #caculates the zone
  ymax <- max(rewarddf$Y)
  ymin <- min(rewarddf$Y)
  xmax <- max(rewarddf$X)
  xmin <- min(rewarddf$X)
  #Quadrant I 
  if(((xmax+xmin)/2 > 0) & ((ymax+ymin)/2 > 0)){
    zonemv <- subset(movementdf, X >= 0 & Y >= 0)
  }
  #Quadrant II
  else if(((xmax+xmin)/2 < 0) & ((ymax+ymin)/2 > 0)){
    zonemv <- subset(movementdf, X <= 0 & Y >= 0)
  }
  #Quadrant III
  else if(((xmax+xmin)/2 < 0) & ((ymax+ymin)/2 < 0)){
    zonemv <- subset(movementdf, X <= 0 & Y <= 0)
  }
  #Quadrant IV
  else if(((xmax+xmin)/2 > 0) & ((ymax+ymin)/2 < 0)){
    zonemv <- subset(movementdf, X >= 0 & Y <= 0)
  }
  #Calculates the percent time in zone
  timespent <- length(zonemv$X)/length(movementdf$X)
  return(timespent)
}






AreaRatioC <- function(rewarddf){
  #creates the reward zone polygon
  #Quadrant I 
  zone<-spCircle(radius = 6, centerPoint = c(x = mean(rewarddf$X), y = mean(rewarddf$X)))
  #creates the circle arena polygon
  circle <-  spCircle(15,centerPoint = c(x = 0, y = 0))
  #creates square arena polygon
  square <- as(extent(-15, 15, -15, 15), 'SpatialPolygons')
  #creates a polygon of overlap between the reward zone and the circle arena
  olcircle <-raster::intersect(zone$spCircle, circle$spCircle)
  #Calculates the percent of the circle arena covered by the reward zone and prints it
  olratiocircle <- gArea(olcircle)/(pi*15^2)
  print(paste0("Circle Ratio: ", olratiocircle))
  #Plots the rewardzone and circle arena overlap to ensure correct
  plot(circle$spCircle, axes=T); plot(zone$spCircle, add = T); plot(olcircle, add=T, col='red')
  #Creates a polygon of the overlap between the reward zone and the square arena
  olsquare <- raster::intersect(zone$spCircle, square)
  #Calculates the percent of the square arena covered by the reward zone and prints it
  olratiosquare <- gArea(olsquare)/(30*30)
  print(paste0("Square Ratio: ",  olratiosquare))
  #Plots the rewardzone and circle arena overlap to ensure correct
  plot(square, axes=T); plot(zone$spCircle,add = T, axes=T); plot(olsquare, add=T, col='red')
  return(c(olratiosquare,olratiocircle))
}

rewarddf <- Reward
AreaRatioC(Reward)

spCircle()

TIZC <- function(movementdf, rewarddf){
  #caculates the zone
  h<-mean(rewarddf$X)
  k<-mean(rewarddf$Y)
  r <- 6
  #Calculates the percent time in zone
  zonemv<- subset(movementdf, (X-h)^2+(Y-k)^2 <= r^2)
  timespent <- length(zonemv$X)/length(movementdf$X)
  return(timespent)
}


####SANITY CHECKING ####
Reward <- subset(MasterPilot3, Participant == 301 & Environment == 2 & Sides == 4 & Session %in% c(1,2,3,4) & BlockID == 3 & TargetFound == 1)
AreaRatioC(Reward)

Movement <- subset(MasterPilot3, Participant == 301 & Environment == 2 & Sides == 40 & BlockID == 3 & Session == 'Test' & TrialTime > 5 & Movement == 1)

TIZC(Movement, Reward)

install.packages("ggforce")
library(ggforce)
?geom_circle
ggplot(Reward, aes(x = X, y = Y))+
  geom_point(data = Reward, aes(x = X, y = Y))+
  #geom_point(data = Movement, aes(x = X, y = Y, color = as.factor(Sides), alpha = 1))+
  geom_circle(aes(x0=mean(Reward$X),y0=mean(Reward$Y), r=6), inherit.aes = FALSE)+
  geom_circle(aes(x0=0,y0=0, r=15), inherit.aes = FALSE)+
  #stat_density2d(data = RewardLocations, aes(x = X, y = Y, color = as.factor(Sides)), bins = 6) +
  theme_bw() + theme(panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_rect(colour = "black", size=4)) + scale_y_continuous(limit = c(-15, 15)) + scale_x_continuous(limit = c(-15, 15))
#scale_fill_manual(values=c("#FFEBEE","#E8F5E9"))+

circle <-  spCircle(2, centerPoint = c(x=mean(Reward$X), y=mean(Reward$Y)))
plot(circle$spCircle, add = T)

AreaRatio(RewardLocations)

##TIZ Sanity Checks#
train <- subset(MasterPilot3, Participant == 301 & Session == 1 & BlockID == 3 & TargetFound ==1)
unique(train$Environment)

p<-306
Rewards <- subset(MasterPilot3 , Session == 1 & TargetFound == 1 & Participant == p)
Movement <- subset(MasterPilot3, Session == 'F' & Participant == p & Environment == unique(Rewards$Environment) & Sides == unique(Rewards$Sides) & Movement == 1 & Time.Since.Trial.Start..seconds. > 5 & BlockID ==2)
  ggplot()+
        geom_point(data = Rewards, aes(x = X, y = Y, color = 'green', alpha = 0.2))+
        stat_density2d(data = Rewards, aes(x = X, y = Y, color = 'green'), bins = 6) +
        geom_point(data = Movement, aes(x = X, y = Y, color = 'red', alpha = 0.2))+
        stat_density2d(data = Movement, aes(x = X, y = Y, color = 'red'), bins = 6) +
        theme_bw() + theme(panel.grid.major = element_blank(), 
                           panel.grid.minor = element_blank(),
                           panel.background = element_rect(colour = "black", size=4)) + scale_y_continuous(limit = c(-15, 15)) + scale_x_continuous(limit = c(-15, 15))+
        scale_fill_manual(values=c("#FFEBEE","#E8F5E9"))+
        ggtitle(paste0("Reward Distributions - Participant: ", p,  " Delay: ", unique(Reward$Delay))) + theme(plot.title = element_text(lineheight=.8, face="bold"))



movementdf <- select(Movement, X,Y)
zonedf <- select(Rewards, X,Y)

TIZ(movementdf, zonedf)

#manipultion
df2$X <- df2$X + 5
df2$Y <- df2$Y + 5

d1 <- df1
d2 <- df2

d1$d <- 1
d2$d <- 2
d12<-rbind(d1,d2)

ggplot(d12, aes(x = X, y = Y, color = as.factor(d)))+
  geom_point(data = d12, aes(x = X, y = Y, alpha=0.0002, color = as.factor(d)))+
  stat_density2d(data = d12, aes(x = X, y = Y,color = as.factor(d), alpha=..level..), geom="polygon")

bhatta(df1,df2)

###OLD CODE####
###Bhattacharrya Function - Calculates overlap between two data sets. ###
bhatta <- function(df1, df2, min = -15, max =15){
  probs <- data.frame()
  #Creates density functions for first data frame
  df1densx <- density(df1$X,from=min, to=max)
  df1densy <- density(df1$Y,from=min, to=max)
  #Creates density functions for second data frame
  df2densx <- density(df2$X, from=min, to=max)
  df2densy <- density(df2$Y, from=min, to=max)
  #creat probability dataframes
  for (ln in (1:length(df1densx$y))){
    probdf1 <- df1densx$y[ln]*diff(df1densx$x)[1]*df1densy$y*diff(df1densy$x)[1]
    probdf2 <- df2densx$y[ln]*diff(df2densx$x)[1]*df2densy$y*diff(df2densy$x)[1]
    df <- data.frame("prob1" = probdf1, "prob2" =probdf2)
    probs <- rbind(probs, df)
  }
  probs$bhat <- sqrt(probs$prob1*probs$prob2)
  bt<-sum(probs$bhat)
  return(bt)
}


JSDCal <- function(df1, df2, bns = 512, xmin = -15, xmax =15, ymin = -15, ymax =15){
  dfprob1 <-kde2d(df1$X,df1$Y,n=bns, lims=c(xmin, xmax, ymin, ymax))
  dfprob1$z <- dfprob1$z /sum(dfprob1$z)
  dfprob2 <-kde2d(df2$X,df2$Y,n=bns, lims=c(xmin, xmax, ymin, ymax))
  dfprob2$z <-dfprob2$z /sum(dfprob2$z)
  p<-as.vector(dfprob1$z)
  q<-as.vector(dfprob2$z)
  x<-rbind(p,q)
  jsd<-JSD(x)
  return(jsd)
}


##### this is the good stuff!!
install.packages("MASS")
library(MASS)
dfprob <-kde2d(df1$X,df1$Y,n=512, lims=c(-15, 15,-15,15))
dfprob$z <- dfprob$z /sum(dfprob$z)
dfprob2 <-kde2d(df2$X,df2$Y,n=512, lims=c(-15, 15,-15,15))
dfprob2$z <-dfprob2$z /sum(dfprob2$z)
probs <- sum(sqrt(dfprob2$z * dfprob$z))
image(sqrt(dfprob2$z * dfprob$z))

df1prob<-kde2d(df1$X, df2$Y, n = 5, lims=c(-15, 15,-15,15))
df1prob$z
sum(df1prob$z)

###Calculate the overlap between zone and total environment
install.packages('sampSurf')
library(swfscMisc)
library(sp)
library(raster)
library(rgdal)
library(rgeos)
library(maptools)
library(sampSurf)
### This function allows us to play around with different ways of defining the reward area and calculate the
#ratio of the reward area defined to the total area of the square arean and circle arena. 
AreaRatioQ <- function(rewarddf){
  ymax <- max(rewarddf$Y)
  ymin <- min(rewarddf$Y)
  xmax <- max(rewarddf$X)
  xmin <- min(rewarddf$X)
  #creates the reward zone polygon
  #Quadrant I 
  if(mean(xmax,xmin) > 0 &mean(ymax,ymin) > 0){
    zone <- as(extent(0, 15, 0, 15), 'SpatialPolygons')
  }
  #Quadrant II
  if(mean(xmax,xmin) < 0 &mean(ymax,ymin) > 0){
    zone <- as(extent(-15, 0, 0, 15), 'SpatialPolygons')
  }
  #Quadrant III
  if(mean(xmax,xmin) < 0 &mean(ymax,ymin) < 0){
    zone <- as(extent(-15, 0, -15, 0), 'SpatialPolygons')
  }
  #Quadrant IV
  if(mean(xmax,xmin) > 0 &mean(ymax,ymin) < 0){
    zone <- as(extent(0, 15, -15, 0), 'SpatialPolygons')
  }
  #creates the circle arena polygon
  circle <-  spCircle(15)
  #creates square arena polygon
  square <- as(extent(-15, 15, -15, 15), 'SpatialPolygons')
  #creates a polygon of overlap between the reward zone and the circle arena
  olcircle <-intersect(zone, circle$spCircle)
  #Calculates the percent of the circle arena covered by the reward zone and prints it
  olratiocircle <- gArea(olcircle)/(pi*15^2)
  print(paste0("Circle Ratio: ", olratiocircle))
  #Plots the rewardzone and circle arena overlap to ensure correct
  #plot(zone, axes=T); plot(circle$spCircle, add=T); plot(olcircle, add=T, col='red')
  #Creates a polygon of the overlap between the reward zone and the square arena
  olsquare <- intersect(zone, square)
  #Calculates the percent of the square arena covered by the reward zone and prints it
  olratiosquare <- gArea(olsquare)/(30*30)
  print(paste0("Square Ratio: ",  olratiosquare))
  #Plots the rewardzone and circle arena overlap to ensure correct
  #plot(zone, axes=T); plot(square, add=T); plot(olsquare, add=T, col='red')
  return(c(olratiosquare,olratiocircle))
}


TIZold <- function(movementdf, rewarddf){
  #caculates the zone
  ymax <- max(rewarddf$Y)
  ymin <- min(rewarddf$Y)
  xmax <- max(rewarddf$X)
  xmin <- min(rewarddf$X)
  zonemv <- subset(movementdf, X <= xmax & X >= xmin & Y <= ymax & Y >= ymin)
  #Calculates the percent time in zone
  timespent <- length(zonemv$X)/length(movementdf$X)
  return(timespent)
}

install.packages('FNN')

kl <- function(df1, df2, bns = 512, xmin = -15, xmax =15, ymin = -15, ymax =15){
  dfprob <-kde2d(df1$X,df1$Y,n=bns, lims=c(xmin, xmax, ymin, ymax))
  dfprob$z <- dfprob$z /sum(dfprob$z)
  dfprob2 <-kde2d(df2$X,df2$Y,n=bns, lims=c(xmin, xmax, ymin, ymax))
  dfprob2$z <-dfprob2$z /sum(dfprob2$z)
  KL.divergence(dfprob$z, dfprob2$z, k = 10, algorithm=c("kd_tree", "cover_tree", "brute"))
}

bhatta_new <- function(df1, df2, min = -15, max =15){
  probs <- data.frame()
  ysteps1 <- density(df1$Y,from=min, to=max)
  ysteps2 <- density(df2$Y,from=min, to=max)
  stepSize <- diff(ysteps1$x)[1]
  for (ln in (1:length(ysteps1$y))){
    ymin = ysteps1$x[ln]
    ymax = ymin + stepSize
    thisData1 <- filter(df1, Y>=ymin, Y<ymax)
    if (length(thisData1$X) > 0) {
      df1densx <- density(thisData1$X,from=min, to=max) # this might depend on circle?
      probdf1 <- df1densx$y*diff(df1densx$x)[1] * ysteps1$y[ln] * stepSize 
    }
    else {
      probdf1 <- 1*vector(length=512)# make nicer
    }
    
    thisData2 <- filter(df2, Y>=ymin, Y<ymax)
    if (length(thisData1$X) > 0) {
      df2densx <- density(thisData2$X,from=min, to=max) # this might depend on circle?
      probdf2 <- df2densx$y*diff(df2densx$x)[1] * ysteps2$y[ln] * stepSize 
    }
    else {
      probdf2 <- 1*vector(length=512)# make nicer
    }
    
    df <- data.frame("prob1" = probdf1, "prob2" = probdf2)
    probs <- rbind(probs, df)
  }
  probs$bhat <- sqrt(probs$prob1*probs$prob2)
  bt<-sum(probs$bhat)
  return(bt)
}
ln<-1
diff(df1densx$x)

plot(probs$prob1)
length(probs$prob1)/512
x = matrix(probs$prob2, nrow = 512, byrow = T)
dim(x)
image(x, zlim = c(min(x),.001), col = rainbow(21))

install.packages("corrplot")
library(corrplot)
corrplot(x, cl.lim )
length(df1densx$x)