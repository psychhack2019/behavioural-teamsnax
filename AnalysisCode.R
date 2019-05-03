library(reshape2); library(plyr); library(lme4); library(lmerTest); library(ggplot2); library(ez); library(schoRsch); library(cowplot); library(lsr);library(MASS)
library(dplyr);library(plotly);library(psych)
library(hexbin)
library(stats)
library(entropy)
library(gridExtra)
library(useful)
library(lestat)
library(ggforce)
library(tictoc)
####################################################
############### Prepare Data #######################
####################################################
### Read in Data, Aggregate, clean make Master Dt (MADT) ####
setwd("/Users/duncanlab/Desktop/Data/Masters2Session/Data")
filenames <- list.files(pattern="*.csv")
filenames
MasterML <- tribble
MasterML <- do.call("rbind", lapply(filenames, read.csv, header = TRUE,stringsAsFactors = FALSE,row.names=NULL)) 


MasterML$Subject[MasterML$Subject=='11'] <- 1
MasterML$Subject[MasterML$Subject=='22'] <- 2
MasterML$Subject[MasterML$Subject==' S2'] <- 2
MasterML$Subject[MasterML$Subject==' P3'] <- 3
MasterML$Subject[MasterML$Subject==' P4'] <- 4
MasterML$Subject[MasterML$Subject==' P5'] <- 5
MasterML$Subject[MasterML$Subject==' P5-1'] <- 5
MasterML$Subject[MasterML$Subject==' P6'] <- 6
MasterML$Subject[MasterML$Subject=='P6'] <- 6
MasterML$Subject[MasterML$Subject==' P7'] <- 7
MasterML$Subject[MasterML$Subject=='P7'] <- 7
MasterML$Subject[MasterML$Subject==' 7S'] <- 7
MasterML$Subject[MasterML$Subject==' P8'] <- 8
MasterML$Subject[MasterML$Subject==' P9'] <- 9
MasterML$Subject[MasterML$Subject==' P10'] <- 10
MasterML$Subject[MasterML$Subject==' S12'] <- 12
MasterML$Subject[MasterML$Subject==' P12'] <- 12
MasterML$Subject[MasterML$Subject==' P13'] <- 13
MasterML$Subject[MasterML$Subject==' P14'] <- 14
MasterML$Subject[MasterML$Subject==' P15'] <- 15
MasterML$Subject[MasterML$Subject==' P16'] <- 16
MasterML$Subject[MasterML$Subject==' P16 '] <- 16
MasterML$Subject[MasterML$Subject==' P17'] <- 17
MasterML$Subject[MasterML$Subject==' P18'] <- 18
MasterML$Subject[MasterML$Subject=='P19'] <- 19
MasterML$Subject[MasterML$Subject==' P19'] <- 19
MasterML$Subject[MasterML$Subject==' P20'] <- 20
MasterML$Subject[MasterML$Subject==' P21'] <- 21
MasterML$Subject[MasterML$Subject==' C22'] <- 22
MasterML$Subject[MasterML$Subject==' P22'] <- 22
MasterML$Subject[MasterML$Subject==' P23'] <- 23
MasterML$Subject[MasterML$Subject==' P 24'] <- 24
MasterML$Subject[MasterML$Subject==' P24'] <- 24
MasterML$Subject[MasterML$Subject==' P27'] <- 27
MasterML$Subject[MasterML$Subject==' P28'] <- 28
MasterML$Subject[MasterML$Subject==' P29'] <- 29
MasterML$Subject[MasterML$Subject==' P30'] <- 30
MasterML$Subject[MasterML$Subject==' P31'] <- 31
MasterML$Subject[MasterML$Subject==' P32'] <- 32
MasterML$Subject[MasterML$Subject==' P33'] <- 33
MasterML$Subject[MasterML$Subject==' P34'] <- 34
MasterML$Subject[MasterML$Subject==' P35'] <- 35
MasterML$Subject[MasterML$Subject==' P36'] <- 36
MasterML$Subject[MasterML$Subject==' P37'] <- 37
MasterML$Subject[MasterML$Subject==' P40'] <- 40
MasterML$Subject[MasterML$Subject==' P43'] <- 43
MasterML$Subject[MasterML$Subject==' P44'] <- 44
MasterML$Subject[MasterML$Subject==' P45'] <- 45
MasterML$Subject[MasterML$Subject==' P46'] <- 46
MasterML$Subject[MasterML$Subject==' P47'] <- 47
MasterML$Subject[MasterML$Subject==' P47 '] <- 47
MasterML$Subject[MasterML$Subject==' P48'] <- 48
MasterML$Subject[MasterML$Subject==' P49'] <- 49
MasterML$Subject[MasterML$Subject==' P51'] <- 51
MasterML$Subject[MasterML$Subject==' P52'] <- 52
MasterML$Subject[MasterML$Subject=='P52'] <- 52
MasterML$Subject[MasterML$Subject==' P53'] <- 53
MasterML$Subject[MasterML$Subject==' P54'] <- 54
MasterML$Subject[MasterML$Subject==' P55'] <- 55
MasterML$Subject[MasterML$Subject==' P56'] <- 56
MasterML$Subject[MasterML$Subject=='P56'] <- 56
MasterML$Subject[MasterML$Subject==' P61'] <- 61
MasterML$Subject[MasterML$Subject==' P62'] <- 62
MasterML$Subject[MasterML$Subject==' P64'] <- 64
MasterML$Subject[MasterML$Subject==' P65'] <- 65
MasterML$Subject[MasterML$Subject==' P66'] <- 66
MasterML$Subject[MasterML$Subject==' P67'] <- 67
MasterML$Subject[MasterML$Subject==' P68'] <- 68
MasterML$Subject[MasterML$Subject==' P68 '] <- 68
MasterML$Subject[MasterML$Subject==' P69'] <- 69
MasterML$Subject[MasterML$Subject==' P70'] <- 70
MasterML$Subject[MasterML$Subject==' P71'] <- 71
MasterML$Subject[MasterML$Subject==' P73'] <- 73
MasterML$Subject[MasterML$Subject==' P73 '] <- 73
unique(MasterML$Subject)



MasterML$Delay[MasterML$Delay==' D 3'] <- 'Short'
MasterML$Delay[MasterML$Delay=='3'] <- 'Short'
MasterML$Delay[MasterML$Delay==' D3 '] <- 'Short'
MasterML$Delay[MasterML$Delay==' S3 '] <- 'Short'
MasterML$Delay[MasterML$Delay==' D3'] <- 'Short'
MasterML$Delay[MasterML$Delay==' D27'] <- 'Long'
MasterML$Delay[MasterML$Delay==' D27 '] <- 'Long'
MasterML$Delay[MasterML$Delay==' d27'] <- 'Long'
unique(MasterML$Delay)


MasterML$PickupType[MasterML$TrialID=='22' & MasterML$Environment.Type == '2'] <- 'F'
MasterML$PickupType[MasterML$TrialID=='23' & MasterML$Environment.Type == '2'] <- 'F'
MasterML$PickupType[MasterML$TrialID=='24' & MasterML$Environment.Type == '2'] <- 'W'
MasterML$PickupType[MasterML$TrialID=='25' & MasterML$Environment.Type == '2'] <- 'W'
MasterML$PickupType[MasterML$TrialID=='26' & MasterML$Environment.Type == '2'] <- 'M'
MasterML$PickupType[MasterML$TrialID=='27' & MasterML$Environment.Type == '2'] <- 'M'

MasterML$PickupType[MasterML$TrialID=='46' & MasterML$Environment.Type == '3'] <- 'F'
MasterML$PickupType[MasterML$TrialID=='47' & MasterML$Environment.Type == '3'] <- 'F'
MasterML$PickupType[MasterML$TrialID=='48' & MasterML$Environment.Type == '3'] <- 'W'
MasterML$PickupType[MasterML$TrialID=='49' & MasterML$Environment.Type == '3'] <- 'W'
MasterML$PickupType[MasterML$TrialID=='50' & MasterML$Environment.Type == '3'] <- 'M'
MasterML$PickupType[MasterML$TrialID=='51' & MasterML$Environment.Type == '3'] <- 'M'

#Rename Sessions
unique(MasterML$Session)
MasterML<-subset(MasterML, Session %in% c(1,2))
MasterML$Session[MasterML$Session==1] <- 'Train'
MasterML$Session[MasterML$Session==2] <- 'Test'
unique(MasterML$Session)

unique(MasterML$PickupType)

#Add moving and rotation values Moving/Rotating
(MasterML$Rotation = ifelse(MasterML$Angle == lag(MasterML$Angle), 0, 1))
(MasterML$Movement = ifelse(MasterML$X == lag(MasterML$X), 0, 1))

#drop practice or 2D trial types and any "bleep" trials
MasterML <- subset(MasterML, (Environment == 2 | Environment == 3) & TrialTime >0.01)


#Create A column Name for Trial Number based on Block
for(s in unique(MasterML$Session)){
  CurSes <- subset(MasterML, Session == s)
  for(b in unique(CurSes$BlockID)){
    CurBlock <- subset(CurSes, BlockID == b)
    t <- 1
    for(tr in unique(CurBlock$Trial.Number)){
      MasterML$Trial[MasterML$Session==s & MasterML$BlockID==b & MasterML$Trial.Number==tr] <- t
      t=t+1
    }
  }
}

MasterML$RewardType[MasterML$RewardType == 'NoPickup'] <- 'NoReward'



#Rename some columns
MasterML <- rename(MasterML,
                   Environment = Environment.Type,
                   Participant = Subject,
                   TrialTime = Time..seconds.,
                   Block = BlockID,
                   RewardX = TargetX, 
                   RewardY = TargetY, 
                   RewardType = PickupType)

MasterML <- rename(MasterML, TwoDTrial = X2D)

colnames(MasterML)
colnames(MasterBackup)
MasterBackup <- MasterML

Hack <- MasterML[, c(16,25,17,6,14,28,18,19,2,3,4,11,10,8,9,26,27)]
Hack$Block <- Hack$Block+1

setwd("/Users/duncanlab/Desktop/Data/Masters2Session/Analysis")
write.csv(MasterML, file = "MasterML.csv")

setwd("/Users/duncanlab/Desktop/Data")
write.csv(Hack, file = "Hack.csv")
####CALCULAT BHATTA + TIZ####
#Set Conditions
it = c('F','M','W')

MasterML <- subset(MasterML, !Participant == 28)

MLBhattasTIZ <- data.frame()

for (p in unique(MasterML$Participant)){
  for (i in it){
    ##Create reward subset from Training
    print(paste0("Participant: ", p, "Item: ", i))
    Reward <- subset(MasterML, Participant == p & PickupType == i & Session == 'Train' & TargetFound == 1)
    #Create Movement from Test
    Movement <- subset(MasterML, Participant == p & Session == 'Test' & Movement == 1 & TrialTime > 5 & PickupType == i)
    #Calculate Performance - targets found in last block of session 1
    #tempmvl <-subset(pc, BlockID == lt & Rotation == 0 & Time..seconds.>5 & Time..seconds.<15)
    ##Calculates the number of rewards they found in the last block
    LastBlock <- subset(Reward, BlockID == max(Reward$BlockID))
    trials <- unique(LastBlock$Trial)
    found<-length(LastBlock$TargetFound)
    perfound <- found/length(trials)
    #Calulate Bhattacharyaa
    bhat <- round(bhatta(Movement, Reward), digits = 4)
    #Calculate TIZ
    timein <- round(TIZ(Movement, Reward), digits = 4)
    dt <- data.frame("Participant" = p,"Environmnet"= unique(Reward$Environment), "Delay" = unique(Reward$Delay), "Item" = i, "Bhattacharyya" = bhat, "TIZ" = timein,  "%Found" = perfound)
    MLBhattasTIZ <- rbind(MLBhattasTIZ, dt)
  }
}


MLMeans <- data.frame()

for (p in unique(MLBhattasTIZ$Participant)){
  pbhat<-subset(MLBhattasTIZ, Participant == p)
  env <- unique(pbhat$Environmnet)
  delay <- unique(pbhat$Delay)
  BpbhatF <- pbhat$Bhattacharyya[pbhat$Item == 'F']
  BpbhatW <- pbhat$Bhattacharyya[pbhat$Item == 'W']
  BpbhatM <- pbhat$Bhattacharyya[pbhat$Item == 'M']
  TpbhatF <- pbhat$TIZ[pbhat$Item == 'F']
  TpbhatW <- pbhat$TIZ[pbhat$Item == 'W']
  TpbhatM <- pbhat$TIZ[pbhat$Item == 'M']
  Bpbhatmean <- (BpbhatF+BpbhatW+BpbhatM)/3
  Tpbhatmean <- (TpbhatF+TpbhatW+TpbhatM)/3
  temp <- data.frame("Participant" = p, "Environment" = env, "Delay" = delay, "BFood" = BpbhatF, "BWater" = BpbhatW, "BMoney" = BpbhatM, "BMean" = Bpbhatmean,"TFood" = TpbhatF, "TWater" = TpbhatW, "TMoney" = TpbhatM, "TMean" = Tpbhatmean)
  MLMeans <- rbind(MLMeans, temp)
}

MLMeans <- rename(MLMeans,
                         Bhattacharyya = BMean,
                          TIZ = TMean)
MLMerge <- MLMeans
MLMerge$BFood <- NULL
MLMerge$BMoney <- NULL
MLMerge$BWater <- NULL
MLMerge$TFood <- NULL
MLMerge$TMoney <- NULL
MLMerge$TWater <- NULL
MLMerge$Pilot <- 1

ggplot(NLLP, aes(x=as.factor(Pilot), y=TIZ)) +
  facet_wrap(~Delay)+
  geom_violin() 

short<-subset(NLLP, Delay == 'Short')
long<-subset(NLLP, Delay == 'Long')
t.test(NLLP$TIZ[NLLP$Pilot ==2],NLLP$TIZ[NLLP$Pilot ==3])
t.test(short$Bhattacharyya[short$Pilot ==3],short$Bhattacharyya[short$Pilot ==2])
t.test(long$Bhattacharyya[long$Pilot ==2],long$Bhattacharyya[long$Pilot ==3])

meanshort<-subset(MLMeans, Delay == 'Short')
meanlong<-subset(MLMeans, Delay == 'Long')
t.test(meanshort$BMean,meanlong$BMean)
cohen.d(meanshort$BMean,meanlong$BMean)
t.test(meanshort$TMean,meanlong$TMean)
cohen.d(meanshort$TMean,meanlong$TMean)

mean2<-subset(MLMeans, Environment == 2)
mean3<-subset(MLMeans, Environment == 3)
t.test(mean2$BMean,mean3$BMean)
cohen.d(mean2$BMean,mean3$BMean)
t.test(mean2$TMean,mean3$TMean)
cohen.d(mean2$TMean,mean3$TMean)

ggplot(MLMeans, aes(x=Environment, y=Mean)) +
  geom_violin() +
  #scale_color_manual(values=c('#FFB11B',	'#00AA90', '#FFB11B',	'#00AA90')) +
  #scale_fill_manual(values=c('#FFB11B',	'#00AA90')) + 
  facet_wrap(~Delay) +
  geom_line(data = workingdf, aes(x = Sides, y = SquareTN, group = interaction(Participant, Delay), color = factor(Participant))) + 
  geom_point(data = workingdf, aes(x = Sides, y = SquareTN, group = interaction(Participant, Delay), color = factor(Participant))) + 
  #geom_dotplot(aes(x=Delay, y=CmSTIZ, group = interaction(LearnedSides_Ef, Delay),color=Delay, alpha=LearnedSides_Ef), position=position_dodge(0.1), dotsize=0.6, alpha =1, stackdir='center', binaxis='y')+
  #geom_errorbar(position=position_dodge(.9), width=.25, aes(ymin=df1[,4]-se, ymax=df1[,4]+se))+
  theme_bw()+  
  #geom_signif(annotation = formatC('*', digits = 1), 
  #y_position = 0.48, xmin = 1, xmax =2, 
  #tip_length = c(0.05, 0.05))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(), 
        axis.line =  element_line(colour = 'black'))+
  geom_hline(yintercept = 0)+
  scale_fill_discrete(guide = F)+
  ylim(0,7)+
  xlab("Movement Environment")+
  #ylab(Type)+
  #scale_fill_manual(values=c("#F39C12","#8E44AD"))+
  #ggtitle(paste0("Pilot(s): ",paste(unique(dfclean$Pilot),collapse=","), " - Block(s): ",paste(unique(dfclean$Block),collapse=",")," - ", Type,  " Direct" ))+
  theme(plot.title = element_text(size=26,hjust = 0.5), plot.subtitle = element_text(size=18,hjust=0.5),axis.text=element_text(size=22),
        axis.title=element_text(size=24))


Reward <- subset(MasterML, Participant == 28 & Session =='Train' & TargetFound ==1 & PickupType == 'F')
#Then we can subset the Movement Distribution we want to use for comparison
Movement <- subset(MasterML, Participant == 28 & Session =='Test' & TrialTime > 5 & BlockID == 2 & Movement == 1 & PickupType == 'F')
bhat <- round(bhatta(Movement, Reward), digits = 4)

for (p in unique(MasterML$Participant)){
  see<-subset(MasterML, Participant == p)
  print(paste0(p," ", unique(see$Session)))
}
see <- subset(MasterML, Participant == 28)
unique(see$Session)

#Wecan take a look at these two distributions
ggplot()+
  #geom_point(data = Reward, aes(x = X, y = Y, color = PickupType, alpha = 0.00002))+
  #stat_density2d(data = Reward, aes(x = X, y = Y, color = PickupType), bins = 6) +
  geom_point(data = Movement, aes(x = X, y = Y, color = PickupType, alpha = 0.00002))+
  stat_density2d(data = Movement, aes(x = X, y = Y, color = PickupType), bins = 6) +
  theme_bw() + theme(panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_rect(colour = "black", size=4)) + scale_y_continuous(limit = c(-15, 15)) + scale_x_continuous(limit = c(-15, 15))+
  scale_fill_manual(values=c("#FFEBEE","#E8F5E9"))+
  ggtitle(paste0("Reward Distributions - Participant: ", unique(Reward$Participant),  " Delay: ", unique(Reward$Delay))) + theme(plot.title = element_text(lineheight=.8, face="bold"))
