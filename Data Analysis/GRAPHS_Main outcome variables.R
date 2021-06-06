#--------------------------------------------------------------#
#--- Script producing final outcome variables' graphs ---------#
#--------------------------------------------------------------#
# ---- Matteo Bruno Ricozzi, 06.06.2021 -----------------------#

# Data Preparation
# ----

library(readr)
data <- read_csv("data_Final.csv", col_types = cols(climateScenario = col_character(), 
                                              freqBallots = col_character(), sigmaKernel = col_character(), 
                                              votingMechanism = col_character()))


data$voting <- as.factor(as.numeric(data$votingMechanism != "NO VOTE"))

# Select only interesting variables
colnames(data)

intData <- data[,c("runN", "sigmaKernel", "climateScenario", "voting", "votingMechanism", "year", 
                   "nFarms", "farmExits", "totProfits", "avgCapital",
                   "colBrandValue", "giWinePrice", 
                   "qualityStandard","votingOutcome",
                   "landIndex2", "totalFallow",
                   "giPrestige", "qualityLev", "colBrandLev", "giFarmsLev"
                   )]

# I adjusted landIndex to avoid division by zero, add 1 to both numerator and denominator 
# When there is no abandoned or acquired land the index is 1, as when the number of abandoned and acquired ha are the same.

colnames(intData)[15] <- "landIndex"

str(intData)



# FULL SAMPLE MEANS ALL VARIABLES for 12 scenarios

# divide data frame and calculate averages

test <- split(intData, list(intData$climateScenario, intData$sigmaKernel, intData$votingMechanism), drop = TRUE)


for (i in 1:length(test)) {
  assign(paste0("Scenario_",i), as.data.frame(test[[i]]))
}

# check levels of each scenario data frame

for (i in 1:length(test)) {
  print(paste0("Scenario_", i))
  str(test[[i]][,2:5])
}

#listDF <- list(Scenario_1,Scenario_2,Scenario_3,Scenario_4,Scenario_5,Scenario_6, Scenario_7,Scenario_8,Scenario_9,Scenario_10,Scenario_11,Scenario_12)

# create average over simulation for each time step for each scenario data frame


# Farm Exits

for (i in paste0(1:12)){
  dat <- get (paste0("Scenario_", i))
  dat$Scen <- rep(paste0("Scen_", i), 5600)     # also assign a new var with scenario name (later you merge them)
  dat$mFarmExits <- with(dat, ave(x = farmExits, year, FUN = mean))

  assign(paste0('Scenario_',i),dat)    #replace old data frame with new
}

# N FARMS

for (i in paste0(1:12)){
  dat <- get (paste0("Scenario_", i))
  dat$mNfarms <- with(dat, ave(x = nFarms, year, FUN = mean))
  dat$Scen <- rep(paste0("Scen_", i), 5600)     # also assign a new var with scenario name (later you merge them)
  assign(paste0('Scenario_',i),dat)    #replace old data frame with new
}


# Total Profits

for (i in paste0(1:12)){
  dat <- get (paste0("Scenario_", i))
  dat$mTotProfits <- with(dat, ave(x = totProfits, year, FUN = mean))
  assign(paste0('Scenario_',i),dat)    #replace old data frame with new
}

# AVG CAPITAL

for (i in paste0(1:12)){
  dat <- get (paste0("Scenario_", i))
  dat$mAvgCapital <- with(dat, ave(x = avgCapital, year, FUN = mean))
  assign(paste0('Scenario_',i),dat)    #replace old data frame with new
}

# ColBrandValue

for (i in paste0(1:12)){
  dat <- get (paste0("Scenario_", i))
  dat$mColBrandValue <- with(dat, ave(x = colBrandValue, year, FUN = mean))
  assign(paste0('Scenario_',i),dat)    #replace old data frame with new
}

# GI price

for (i in paste0(1:12)){
  dat <- get (paste0("Scenario_", i))
  dat$mGiWinePrice <- with(dat, ave(x = giWinePrice, year, FUN = mean))
  assign(paste0('Scenario_',i),dat)    #replace old data frame with new
}

# LandIndex

for (i in paste0(1:12)){
  dat <- get (paste0("Scenario_", i))
  dat$mLandIndex <- with(dat, ave(x = landIndex, year, FUN = mean)) # FUN = function(x) mean(x, na.rm=T)
  assign(paste0('Scenario_',i),dat)    #replace old data frame with new
}

# TOTAL FALLOW

for (i in paste0(1:12)){
  dat <- get (paste0("Scenario_", i))
  dat$mTotalFallow <- with(dat, ave(x = totalFallow, year, FUN = mean))
  assign(paste0('Scenario_',i),dat)    #replace old data frame with new
}

# VOTING OUTCOME

for (i in paste0(1:12)){
  dat <- get (paste0("Scenario_", i))
  dat$mVotingOutcome <- with(dat, ave(x = votingOutcome, year, FUN = mean))
  assign(paste0('Scenario_',i),dat)    #replace old data frame with new
}

# QUALITY STANDARD

for (i in paste0(1:12)){
  dat <- get (paste0("Scenario_", i))
  dat$mQualityStandard <- with(dat, ave(x = qualityStandard, year, FUN = mean))
  assign(paste0('Scenario_',i),dat)    #replace old data frame with new
}


# PRESTIGE
for (i in paste0(1:12)){
  dat <- get (paste0("Scenario_", i))
  dat$mGiPrestige <- with(dat, ave(x = giPrestige, year, FUN = mean))
  assign(paste0('Scenario_',i),dat)    #replace old data frame with new
}

# QUALITY LEVEL
for (i in paste0(1:12)){
  dat <- get (paste0("Scenario_", i))
  dat$mQualityLev <- with(dat, ave(x = qualityLev, year, FUN = mean))
  assign(paste0('Scenario_',i),dat)    #replace old data frame with new
}

# COLBRAND LEVEL
for (i in paste0(1:12)){
  dat <- get (paste0("Scenario_", i))
  dat$mColBrandLev <- with(dat, ave(x = colBrandLev, year, FUN = mean))
  assign(paste0('Scenario_',i),dat)    #replace old data frame with new
}

# GI FARMS LEVEL
for (i in paste0(1:12)){
  dat <- get (paste0("Scenario_", i))
  dat$mGiFarmsLev <- with(dat, ave(x = giFarmsLev, year, FUN = mean))
  assign(paste0('Scenario_',i),dat)    #replace old data frame with new
}

# Now merge all data frames into a biggy
bigData <- data.frame()

for (i in paste0(1:12)){
  dat <- get (paste0("Scenario_", i))
  bigData <- rbind(bigData, dat)
}

# I also need a dataframe only with the means 
# take first 56 rows in each scenario

smallData <- data.frame()

for (i in paste0(1:12)) {
  dat <- get (paste0("Scenario_", i))
  smallData <- rbind(smallData, dat[1:56,c(21,2:6,22:35)])
}
## ----

# GRAPH OVERAL
# ---- 

library(ggplot2)

scenNames <- c("2HetAM","3HetAM","2HomAM","3HomAM","2HetNV","3HetNV","2HomNV", "3HomNV", "2HetRM","3HetRM", "2HomRM", "3HomRM")
colors_12 <- c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c',
               '#fdbf6f','#ff7f00','#cab2d6','#6a3d9a','#bf812d','#8c510a')

# OVERAL Farm Exits

ggExit1 <- ggplot(smallData, aes(year, color = Scen)) +
  geom_line(aes(y = (mFarmExits/680)*100), size = 0.8) +
  labs(x= "Year", y= "%",
       title="Farm Exits")+
  theme_light()+
  theme(legend.justification=c(0,1), legend.position=c(0,1),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.title = element_text(face = "plain", size = 9),
        legend.text= element_text(size = 8),
        legend.key.size = unit(0.8, "cm"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_colour_manual(name = "Scenario" ,
                      breaks = c(paste0("Scen_", 1:12)),
                      labels = scenNames,
                      values = colors_12) +
  guides(colour = guide_legend(nrow = 2)) +
  xlim(5, 55)

# OVERAL profits

ggProf1 <- ggplot(smallData, aes(year, color = Scen)) +
  geom_line(aes(y = mTotProfits/(mNfarms*1000)), size = 0.8) +
  labs(x= "Year", y= "1000 €",
       title="Average Profit")+
  theme_light()+
  theme(legend.justification=c(0,0), legend.position=c(0,0),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.title = element_text(face = "plain", size = 9),
        legend.text= element_text(size = 8),
        legend.key.size = unit(0.8, "cm"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_colour_manual(name = "Scenario" ,
                      breaks = c(paste0("Scen_", 1:12)),
                      labels = scenNames,
                      values = colors_12) +
  guides(colour = guide_legend(ncol = 2))  +
  xlim(5, 55)


# OVERAL AVERAGE CAPITAL

ggCap1 <- ggplot(smallData, aes(year, color = Scen)) +
  geom_line(aes(y = mAvgCapital/1000), size = 0.8) +
  labs(x= "Year", y= "€",
       title="Average Capital (1000€)")+
  theme_light()+
  theme(legend.justification=c(0,1), legend.position=c(0,1),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.title = element_text(face = "plain", size = 9),
        legend.text= element_text(size = 8),
        legend.key.size = unit(0.8, "cm"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_colour_manual(name = "Scenario" ,
                      breaks = c(paste0("Scen_", 1:12)),
                      labels = scenNames,
                      values = colors_12) +
  guides(colour = guide_legend(nrow = 2))  +
  xlim(5, 55)

# OVERAL Collective brand value

ggBrand1 <- ggplot(smallData, aes(year, color = Scen)) +
  geom_line(aes(y = mColBrandValue/1000), size = 0.8) +
  labs(x= "Year", y= "1000 €",
       title="Collective Brand Value")+
  theme_light()+
  theme(legend.justification=c(0,1), legend.position=c(0,1),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.title = element_text(face = "plain", size = 9),
        legend.text= element_text(size = 8),
        legend.key.size = unit(0.8, "cm"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_colour_manual(name = "Scenario" ,
                      breaks = c(paste0("Scen_", 1:12)),
                      labels = scenNames,
                      values = colors_12) +
  guides(colour = guide_legend(nrow = 2, title.position = "left")) +
  xlim(5, 55)

# OVERAL GI PRICE


ggPrice1 <- ggplot(smallData, aes(year, color = Scen)) +
  geom_line(aes(y = mGiWinePrice), size = 0.8) +
  labs(x= "Year", y= "€",
       title="GI Wine Price")+
  theme_light()+
  theme(legend.justification=c(0,0), legend.position=c(0,0),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.title = element_text(face = "plain", size = 9),
        legend.text= element_text(size = 8),
        legend.key.size = unit(0.8, "cm"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_colour_manual(name = "Scenario" ,
                      breaks = c(paste0("Scen_", 1:12)),
                      labels = scenNames,
                      values = colors_12) +
  guides(colour = guide_legend(nrow = 2)) +
  xlim(5, 55)

# OVERAL Land USE

ggLand1 <- ggplot(smallData, aes(year, color = Scen)) +
  geom_line(aes(y = mLandIndex), size = 0.8) +
  labs(x= "Year", y= "Ab./Acq.",
       title="Land Use Index")+
  theme_light()+
  theme(legend.justification=c(0,1), legend.position=c(0,1),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.title = element_text(face = "plain", size = 9),
        legend.text= element_text(size = 8),
        legend.key.size = unit(0.8, "cm"),
        legend.direction = "vertical",
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_colour_manual(name = "Scenario" ,
                      breaks = c(paste0("Scen_", 1:12)),
                      labels = scenNames,
                      values = colors_12) +
  guides(colour = guide_legend(ncol = 1)) +
  xlim(5, 55)

# CHECK what is going on with Scenario 11

LAND11 <- ggplot(Scenario_7, aes(year)) +
  geom_point(aes(y = landIndex), size = 1, alpha = 0.6) +
  geom_line(aes(y = mLandIndex), size = 1, col = "red") +
  labs(x= "Year", y= "Ab./Acq.",
       title="Land use")+
  theme_light()+
  xlim(5, 55)

# SOLVED!


# OVERAL TOTAL FALLOW

ggFallow1 <- ggplot(smallData, aes(year, color = Scen)) +
  geom_line(aes(y = mTotalFallow), size = 0.8) +
  labs(x= "Year", y= "ha",
       title="Total Fallow")+
  theme_light()+
  theme(legend.justification=c(0,1), legend.position=c(0,1),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.title = element_text(face = "plain", size = 9),
        legend.text= element_text(size = 8),
        legend.key.size = unit(0.8, "cm"),
        legend.direction = "vertical",
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_colour_manual(name = "Scenario" ,
                      breaks = c(paste0("Scen_", 1:12)),
                      labels = scenNames,
                      values = colors_12) +
  guides(colour = guide_legend(ncol = 1)) +
  xlim(5, 55)


# VOTING OUTCOME

ggVote1 <- ggplot(smallData, aes(year, color = Scen)) +
  geom_point(aes(y = mVotingOutcome), size = 2) +
  labs(x= "Year", y= "-1 = decrease",
       title="Mean Voting outcome")+
  theme_light()+
  theme(legend.justification=c(0,0), legend.position=c(0,0),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.title = element_text(face = "plain", size = 9),
        legend.text= element_text(size = 8),
        legend.key.size = unit(0.8, "cm"),
        legend.direction = "vertical",
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_colour_manual(name = "Scenario" ,
                      breaks = c(paste0("Scen_", 1:12)),
                      labels = scenNames,
                      values = colors_12) +
  guides(colour = guide_legend(ncol = 1)) +
  xlim(5, 55)

# QUALITY STANDARD

ggStand1 <- ggplot(smallData, aes(year, color = Scen)) +
  geom_line(aes(y = mQualityStandard), size = 0.8) +
  labs(x= "Year", y= "%",
       title="Quality Standard")+
  theme_light()+
  theme(legend.justification=c(0,0), legend.position=c(0,0),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.title = element_text(face = "plain", size = 9),
        legend.text= element_text(size = 8),
        legend.key.size = unit(0.8, "cm"),
        legend.direction = "vertical",
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_colour_manual(name = "Scenario" ,
                      breaks = c(paste0("Scen_", 1:12)),
                      labels = scenNames,
                      values = colors_12) +
  guides(colour = guide_legend(ncol = 1)) +
  xlim(5, 55)

# PRESTIGE = GI PRICE (Useless)

#  GI QUALITY LEVEL

ggQLEV1 <- ggplot(smallData, aes(year, color = Scen)) +
  geom_line(aes(y = mQualityLev), size = 0.8) +
  labs(x= "Year", y= "Level",
       title="Quality GI farms (LEVEL)")+
  theme_light()+
  theme(legend.justification=c(0,0), legend.position=c(0,0),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.title = element_text(face = "plain", size = 9),
        legend.text= element_text(size = 8),
        legend.key.size = unit(0.8, "cm"),
        legend.direction = "vertical",
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_colour_manual(name = "Scenario" ,
                      breaks = c(paste0("Scen_", 1:12)),
                      labels = scenNames,
                      values = colors_12) +
  guides(colour = guide_legend(ncol = 1)) +
  xlim(5, 55)
 
# BRAND VALUE LEVEL

ggBLEV1 <- ggplot(smallData, aes(year, color = Scen)) +
  geom_line(aes(y = mColBrandLev), size = 0.8) +
  labs(x= "Year", y= "Level",
       title="Collective Brand Value (LEVEL)")+
  theme_light()+
  theme(legend.justification=c(0,0), legend.position=c(0,0),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.title = element_text(face = "plain", size = 9),
        legend.text= element_text(size = 8),
        legend.key.size = unit(0.8, "cm"),
        legend.direction = "vertical",
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_colour_manual(name = "Scenario" ,
                      breaks = c(paste0("Scen_", 1:12)),
                      labels = scenNames,
                      values = colors_12) +
  guides(colour = guide_legend(ncol = 1)) +
  xlim(5, 55)


# GI FARM LEVEL

ggFLEV1 <- ggplot(smallData, aes(year, color = Scen)) +
  geom_line(aes(y = mGiFarmsLev), size = 0.8) +
  labs(x= "Year", y= "Level",
       title="% GI of total number of farms (LEVEL)")+
  theme_light()+
  theme(legend.justification=c(0,0), legend.position=c(0,0),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.title = element_text(face = "plain", size = 9),
        legend.text= element_text(size = 8),
        legend.key.size = unit(0.8, "cm"),
        legend.direction = "vertical",
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_colour_manual(name = "Scenario" ,
                      breaks = c(paste0("Scen_", 1:12)),
                      labels = scenNames,
                      values = colors_12) +
  guides(colour = guide_legend(ncol = 1)) +
  xlim(5, 55)


# COMBINE OVERAL PLOTS

#-----------------------------------------------------------------#
#---All plots togheter with ggpubr package------------------------#
#-----------------------------------------------------------------#
library(ggpubr)
#-----------------------------------------------------------------#
# Custom function to Extract leggend from plot : 
#-----------------------------------------------------------------#
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

#---Arrange ggplots------------------------------------------------#
summaryResilience <- ggarrange(ggExit1, ggProf1, ggBrand1, ggLand1,
                         common.legend = T,
                        # legend.grob = get_legend(ggLand1),
                         legend = "none",
                         ncol = 2, nrow = 2,
                         heights= c(15, 15))

summaryResilience <- annotate_figure(summaryResilience, top = text_grob("Average Within Scenarios", size = 20))


summaryPrice1 <- ggarrange(ggPrice1, ggQLEV1, ggBLEV1, ggFLEV1,
                          common.legend = T,
                          #legend.grob = get_legend(ggBrand1),
                          legend = "none",
                          ncol = 2, nrow = 2,
                          heights= c(15, 15))

summaryPrice1 <- annotate_figure(summaryPrice1, top = text_grob("Average Within Scenarios", size = 20))


tryHuge <- ggarrange(ggExit1, ggProf1, 
                     ggBrand1, ggLand1,
                     ggPrice1, ggQLEV1, 
                     ggBLEV1, ggFLEV1,
                     
                     common.legend = T,
                     legend.grob = get_legend(ggProf1),
                     legend = "bottom",
                     ncol = 2, nrow = 4,
                     heights= c(15, 15))
tryHuge <- annotate_figure(tryHuge, top = text_grob("Average Within Scenarios", size = 20))

#---Export file---------------------------------------------------#
ggsave( 
  filename = "summaryPRICE1_NEW.png",
  plot = summaryPrice1,
  device = "png",
  path = NULL,
  scale = 1,
  width = 11.69,
  height = 8.27,
  units = "in",
  dpi = 300
)
## ----

# DATA OVERAL
# ----
# Take only data in ticks 15, 25, 35, 45, 55

targetRows<- grep("15|25|35|45|55", smallData$year)

dataTable <- smallData[targetRows,]
## ----

# SPLIT FULL DATA ACCORDING TO INSTITUTIONAL CHANGE SCENARIO
# ----
NVdata <- bigData[bigData$votingMechanism == "NO VOTE",]
YVdata <- bigData[bigData$votingMechanism != "NO VOTE",]
AMdata <- bigData[bigData$votingMechanism == "ABS Majority",]
RMdata <- bigData[bigData$votingMechanism == "REL Majority",]

# Free a little bit of space
rm(list=setdiff(ls(), c("NVdata","YVdata", "AMdata", "RMdata")))

# You can go directly to plot this time points and lines
## ----

# FARM EXITS (all data points DIFFERENCES IN INSTITUTIONAL CHANGE)
# ----

# NV
ggExitsNV <- ggplot(NVdata, aes(year, color = climateScenario)) +
  geom_point(aes(y = ((farmExits/680)*100), shape = sigmaKernel), size = 1, alpha = 0.6) +
  geom_line(aes(y = ((mFarmExits/680)*100), linetype = sigmaKernel), size = 1) +
  labs(x= "Year", y= "%",
       title="NO VOTE")+
  theme_light()+
  theme(legend.justification=c(0,1), legend.position=c(0,1),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.title = element_text(face = "plain", size = 11),
        legend.text= element_text(size = 10),
        legend.key.size = unit(1, "cm"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_colour_manual(name = "Climate" ,
                      breaks = c("+ 2°C","+ 3°C"),
                      labels = c("+2°C", "+3°C"),
                      values = c("#2ca25f", "#e34a33")) +
  scale_linetype_manual(name = "Heterogeneity" ,
                        breaks = c("1.5","3"),
                        labels = c("Sigma = 1.5", "Sigma = 3.0"),
                        values=c("solid", "dashed")) +
  scale_shape_manual(name = "Heterogeneity" ,
                     breaks = c("1.5","3"),
                     labels = c("Sigma = 1.5", "Sigma = 3.0"),
                     values=c(1,4)) +
  guides(colour = guide_legend(ncol = 1)) +
  xlim(5, 55) +
  ylim(0,60)

# YV
ggExitsYV <- ggplot(YVdata, aes(year, color = climateScenario)) +
  geom_point(aes(y = ((farmExits/680)*100), shape = sigmaKernel), size = 1, alpha = 0.6) +
  geom_line(aes(y = ((mFarmExits/680)*100), linetype = sigmaKernel), size = 1) +
  labs(x= "Year", y= "%",
       title="WITH Institutional Change")+
  theme_light()+
  theme(legend.justification=c(0,1), legend.position=c(0,1),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.title = element_text(face = "plain", size = 11),
        legend.text= element_text(size = 10),
        legend.key.size = unit(1, "cm"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_colour_manual(name = "Climate" ,
                      breaks = c("+ 2°C","+ 3°C"),
                      labels = c("+2°C", "+3°C"),
                      values = c("#2ca25f", "#e34a33")) +
  scale_linetype_manual(name = "Heterogeneity" ,
                        breaks = c("1.5","3"),
                        labels = c("Sigma = 1.5", "Sigma = 3.0"),
                        values=c("solid", "dashed")) +
  scale_shape_manual(name = "Heterogeneity" ,
                     breaks = c("1.5","3"),
                     labels = c("Sigma = 1.5", "Sigma = 3.0"),
                     values=c(1,4)) +
  guides(colour = guide_legend(ncol = 1)) +
  xlim(5, 55) +
  ylim(0,60)

# AM
ggExitsAM <- ggplot(AMdata, aes(year, color = climateScenario)) +
  geom_point(aes(y = ((farmExits/680)*100), shape = sigmaKernel), size = 1, alpha = 0.6) +
  geom_line(aes(y = ((mFarmExits/680)*100), linetype = sigmaKernel), size = 1) +
  labs(x= "Year", y= "%",
       title="ABSOLUTE MAJORITY")+
  theme_light()+
  theme(legend.justification=c(0,1), legend.position=c(0,1),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.title = element_text(face = "plain", size = 11),
        legend.text= element_text(size = 10),
        legend.key.size = unit(1, "cm"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_colour_manual(name = "Climate" ,
                      breaks = c("+ 2°C","+ 3°C"),
                      labels = c("+2°C", "+3°C"),
                      values = c("#2ca25f", "#e34a33")) +
  scale_linetype_manual(name = "Heterogeneity" ,
                        breaks = c("1.5","3"),
                        labels = c("Sigma = 1.5", "Sigma = 3.0"),
                        values=c("solid", "dashed")) +
  scale_shape_manual(name = "Heterogeneity" ,
                     breaks = c("1.5","3"),
                     labels = c("Sigma = 1.5", "Sigma = 3.0"),
                     values=c(1,4)) +
  guides(colour = guide_legend(ncol = 1)) +
  xlim(5, 55) +
  ylim(0,60)

# RM
ggExitsRM <- ggplot(RMdata, aes(year, color = climateScenario)) +
  geom_point(aes(y = ((farmExits/680)*100), shape = sigmaKernel), size = 1, alpha = 0.6) +
  geom_line(aes(y = ((mFarmExits/680)*100), linetype = sigmaKernel), size = 1) +
  labs(x= "Year", y= "%",
       title="RELATIVE MAJORITY")+
  theme_light()+
  theme(legend.justification=c(0,1), legend.position=c(0,1),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.title = element_text(face = "plain", size = 11),
        legend.text= element_text(size = 10),
        legend.key.size = unit(1, "cm"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_colour_manual(name = "Climate" ,
                      breaks = c("+ 2°C","+ 3°C"),
                      labels = c("+2°C", "+3°C"),
                      values = c("#2ca25f", "#e34a33")) +
  scale_linetype_manual(name = "Heterogeneity" ,
                        breaks = c("1.5","3"),
                        labels = c("Sigma = 1.5", "Sigma = 3.0"),
                        values=c("solid", "dashed")) +
  scale_shape_manual(name = "Heterogeneity" ,
                     breaks = c("1.5","3"),
                     labels = c("Sigma = 1.5", "Sigma = 3.0"),
                     values=c(1,4)) +
  guides(colour = guide_legend(ncol = 1)) +
  xlim(5, 55) +
  ylim(0,60)


# COMBINED FARM EXITS

library(ggpubr)
#---Arrange ggplots------------------------------------------------#
summaryFEXIT <- ggarrange(ggExitsNV, ggExitsAM, ggExitsRM,
                          common.legend = T,
                          legend.grob = get_legend(ggExitsYV),
                          legend = "right",
                          ncol = 1, nrow = 3)

summaryFEXIT <- annotate_figure(summaryFEXIT, top = text_grob("FARM EXITS FOR DIFFERENT INSTITUTIONAL SETTINGS", size = 18))

# print it directly as a picture

#---Export file---------------------------------------------------#
ggsave( 
  filename = "summaryFEXIT_NEW.png",
  plot = summaryFEXIT,
  device = "png",
  path = NULL,
  scale = 1,
  width = 8.27,
  height = 11.69,
  units = "in",
  dpi = 100
)
## ---- 

# AVERAGE PROFIT (all data points DIFFERENCES IN INSTITUTIONAL CHANGE)
# ----

# NV
ggPROFNV <- ggplot(NVdata, aes(year, color = climateScenario)) +
  geom_point(aes(y = totProfits/(nFarms*1000), shape = sigmaKernel), size = 1, alpha = 0.6) +
  geom_line(aes(y = mTotProfits/(mNfarms*1000), linetype = sigmaKernel), size = 1) +
  labs(x= "Year", y= "1000€",
       title="NO VOTE")+
  theme_light()+
  theme(legend.justification=c(0,0), legend.position=c(0,0),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.title = element_text(face = "plain", size = 11),
        legend.text= element_text(size = 10),
        legend.key.size = unit(1, "cm"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_colour_manual(name = "Climate" ,
                      breaks = c("+ 2°C","+ 3°C"),
                      labels = c("+2°C", "+3°C"),
                      values = c("#2ca25f", "#e34a33")) +
  scale_linetype_manual(name = "Heterogeneity" ,
                        breaks = c("1.5","3"),
                        labels = c("Sigma = 1.5", "Sigma = 3.0"),
                        values=c("solid", "dashed")) +
  scale_shape_manual(name = "Heterogeneity" ,
                     breaks = c("1.5","3"),
                     labels = c("Sigma = 1.5", "Sigma = 3.0"),
                     values=c(1,4)) +
  guides(colour = guide_legend(ncol = 1)) +
  xlim(5, 55) +
  ylim(-1, 17)


# AM
ggPROFAM <- ggplot(AMdata, aes(year, color = climateScenario)) +
  geom_point(aes(y = totProfits/(nFarms*1000), shape = sigmaKernel), size = 1, alpha = 0.6) +
  geom_line(aes(y = mTotProfits/(mNfarms*1000), linetype = sigmaKernel), size = 1) +
  labs(x= "Year", y= "1000€",
       title="ABSOLUTE MAJORITY")+
  theme_light()+
  theme(legend.justification=c(0,0), legend.position=c(0,0),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.title = element_text(face = "plain", size = 11),
        legend.text= element_text(size = 10),
        legend.key.size = unit(1, "cm"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_colour_manual(name = "Climate" ,
                      breaks = c("+ 2°C","+ 3°C"),
                      labels = c("+2°C", "+3°C"),
                      values = c("#2ca25f", "#e34a33")) +
  scale_linetype_manual(name = "Heterogeneity" ,
                        breaks = c("1.5","3"),
                        labels = c("Sigma = 1.5", "Sigma = 3.0"),
                        values=c("solid", "dashed")) +
  scale_shape_manual(name = "Heterogeneity" ,
                     breaks = c("1.5","3"),
                     labels = c("Sigma = 1.5", "Sigma = 3.0"),
                     values=c(1,4)) +
  guides(colour = guide_legend(ncol = 1)) +
  xlim(5, 55) +
  ylim(-1, 17)


# RM
ggPROFRM <- ggplot(RMdata, aes(year, color = climateScenario)) +
  geom_point(aes(y = totProfits/(nFarms*1000), shape = sigmaKernel), size = 1, alpha = 0.6) +
  geom_line(aes(y = mTotProfits/(mNfarms*1000), linetype = sigmaKernel), size = 1) +
  labs(x= "Year", y= "1000€",
       title="RELATIVE MAJORITY")+
  theme_light()+
  theme(legend.justification=c(0,0), legend.position=c(0,0),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.title = element_text(face = "plain", size = 11),
        legend.text= element_text(size = 10),
        legend.key.size = unit(1, "cm"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_colour_manual(name = "Climate" ,
                      breaks = c("+ 2°C","+ 3°C"),
                      labels = c("+2°C", "+3°C"),
                      values = c("#2ca25f", "#e34a33")) +
  scale_linetype_manual(name = "Heterogeneity" ,
                        breaks = c("1.5","3"),
                        labels = c("Sigma = 1.5", "Sigma = 3.0"),
                        values=c("solid", "dashed")) +
  scale_shape_manual(name = "Heterogeneity" ,
                     breaks = c("1.5","3"),
                     labels = c("Sigma = 1.5", "Sigma = 3.0"),
                     values=c(1,4)) +
  guides(colour = guide_legend(ncol = 1)) +
  xlim(5, 55) +
  ylim(-1, 17)


# COMBINED AVERAGE PROFITS

#---Arrange ggplots------------------------------------------------#
summaryPROF <- ggarrange(ggPROFNV, ggPROFAM, ggPROFRM,
                        common.legend = T,
                        legend.grob = get_legend(ggExitsYV),
                        legend = "right",
                        ncol = 1, nrow = 3)

summaryPROF <- annotate_figure(summaryPROF, top = text_grob("AVERAGE PROFIT FOR DIFFERENT INSTITUTIONAL SETTINGS", size = 18))

# print it directly as a picture

#---Export file---------------------------------------------------#
ggsave( 
  filename = "summaryPROF_NEW.png",
  plot = summaryPROF,
  device = "png",
  path = NULL,
  scale = 1,
  width = 8.27,
  height = 11.69,
  units = "in",
  dpi = 100
)
## ----

# COLLECTIVE BRAND VALUE (all data points DIFFERENCES IN INSTITUTIONAL CHANGE)
# ----

# NV
ggCbvNV <- ggplot(NVdata, aes(year, color = climateScenario)) +
  geom_point(aes(y = colBrandValue/1000, shape = sigmaKernel), size = 1, alpha = 0.6) +
  geom_line(aes(y = mColBrandValue/1000, linetype = sigmaKernel), size = 1) +
  labs(x= "Year", y= "1000€",
       title="NO VOTE")+
  theme_light()+
  theme(legend.justification=c(0,0), legend.position=c(0,0),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.title = element_text(face = "plain", size = 11),
        legend.text= element_text(size = 10),
        legend.key.size = unit(1, "cm"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_colour_manual(name = "Climate" ,
                      breaks = c("+ 2°C","+ 3°C"),
                      labels = c("+2°C", "+3°C"),
                      values = c("#2ca25f", "#e34a33")) +
  scale_linetype_manual(name = "Heterogeneity" ,
                        breaks = c("1.5","3"),
                        labels = c("Sigma = 1.5", "Sigma = 3.0"),
                        values=c("solid", "dashed")) +
  scale_shape_manual(name = "Heterogeneity" ,
                     breaks = c("1.5","3"),
                     labels = c("Sigma = 1.5", "Sigma = 3.0"),
                     values=c(1,4)) +
  guides(colour = guide_legend(ncol = 1)) +
  xlim(5, 55) +
  ylim(500, 3000)


# AM
ggCbvAM <- ggplot(AMdata, aes(year, color = climateScenario)) +
  geom_point(aes(y = colBrandValue/1000, shape = sigmaKernel), size = 1, alpha = 0.6) +
  geom_line(aes(y = mColBrandValue/1000, linetype = sigmaKernel), size = 1) +
  labs(x= "Year", y= "1000€",
       title="ABSOLUTE MAJORITY")+
  theme_light()+
  theme(legend.justification=c(0,0), legend.position=c(0,0),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.title = element_text(face = "plain", size = 11),
        legend.text= element_text(size = 10),
        legend.key.size = unit(1, "cm"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_colour_manual(name = "Climate" ,
                      breaks = c("+ 2°C","+ 3°C"),
                      labels = c("+2°C", "+3°C"),
                      values = c("#2ca25f", "#e34a33")) +
  scale_linetype_manual(name = "Heterogeneity" ,
                        breaks = c("1.5","3"),
                        labels = c("Sigma = 1.5", "Sigma = 3.0"),
                        values=c("solid", "dashed")) +
  scale_shape_manual(name = "Heterogeneity" ,
                     breaks = c("1.5","3"),
                     labels = c("Sigma = 1.5", "Sigma = 3.0"),
                     values=c(1,4)) +
  guides(colour = guide_legend(ncol = 1)) +
  xlim(5, 55) +
  ylim(500, 3000)


# RM
ggCbvRM <- ggplot(RMdata, aes(year, color = climateScenario)) +
  geom_point(aes(y = colBrandValue/1000, shape = sigmaKernel), size = 1, alpha = 0.6) +
  geom_line(aes(y = mColBrandValue/1000, linetype = sigmaKernel), size = 1) +
  labs(x= "Year", y= "1000€",
       title="RELATIVE MAJORITY")+
  theme_light()+
  theme(legend.justification=c(0,0), legend.position=c(0,0),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.title = element_text(face = "plain", size = 11),
        legend.text= element_text(size = 10),
        legend.key.size = unit(1, "cm"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_colour_manual(name = "Climate" ,
                      breaks = c("+ 2°C","+ 3°C"),
                      labels = c("+2°C", "+3°C"),
                      values = c("#2ca25f", "#e34a33")) +
  scale_linetype_manual(name = "Heterogeneity" ,
                        breaks = c("1.5","3"),
                        labels = c("Sigma = 1.5", "Sigma = 3.0"),
                        values=c("solid", "dashed")) +
  scale_shape_manual(name = "Heterogeneity" ,
                     breaks = c("1.5","3"),
                     labels = c("Sigma = 1.5", "Sigma = 3.0"),
                     values=c(1,4)) +
  guides(colour = guide_legend(ncol = 1)) +
  xlim(5, 55) +
  ylim(500, 3000)


# COMBINED COLLECTIVE BRAND

#---Arrange ggplots------------------------------------------------#
summaryCBV <- ggarrange(ggCbvNV, ggCbvAM, ggCbvRM,
                               common.legend = T,
                               legend.grob = get_legend(ggExitsYV),
                               legend = "right",
                               ncol = 1, nrow = 3)

summaryCBV <- annotate_figure(summaryCBV, top = text_grob("COLLECTIVE BRAND VALUE FOR DIFFERENT INSTITUTIONAL SETTINGS", size = 17))

# print it directly as a picture

#---Export file---------------------------------------------------#
ggsave( 
  filename = "summaryCBV_NEW.png",
  plot = summaryCBV,
  device = "png",
  path = NULL,
  scale = 1,
  width = 8.27,
  height = 11.69,
  units = "in",
  dpi = 100
)
## ----

# LAND INDEX (all data points DIFFERENCES IN INSTITUTIONAL CHANGE)
# ----

# NV
ggLandNV <- ggplot(NVdata, aes(year, color = climateScenario)) +
  geom_point(aes(y = landIndex, shape = sigmaKernel), size = 1, alpha = 0.6) +
  geom_line(aes(y = mLandIndex, linetype = sigmaKernel), size = 1) +
  labs(x= "Year", y= "Ab./Acq.",
       title="NO VOTE")+
  theme_light()+
  theme(legend.justification=c(0,0), legend.position=c(0,0),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.title = element_text(face = "plain", size = 11),
        legend.text= element_text(size = 10),
        legend.key.size = unit(1, "cm"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_colour_manual(name = "Climate" ,
                      breaks = c("+ 2°C","+ 3°C"),
                      labels = c("+2°C", "+3°C"),
                      values = c("#2ca25f", "#e34a33")) +
  scale_linetype_manual(name = "Heterogeneity" ,
                        breaks = c("1.5","3"),
                        labels = c("Sigma = 1.5", "Sigma = 3.0"),
                        values=c("solid", "dashed")) +
  scale_shape_manual(name = "Heterogeneity" ,
                     breaks = c("1.5","3"),
                     labels = c("Sigma = 1.5", "Sigma = 3.0"),
                     values=c(1,4)) +
  guides(colour = guide_legend(ncol = 1)) +
  xlim(5, 55) +
  ylim(1, 18)


# AM
ggLandAM <- ggplot(AMdata, aes(year, color = climateScenario)) +
  geom_point(aes(y = landIndex, shape = sigmaKernel), size = 1, alpha = 0.6) +
  geom_line(aes(y = mLandIndex, linetype = sigmaKernel), size = 1) +
  labs(x= "Year", y= "Ab./Acq.",
       title="ABSOLUTE MAJORITY")+
  theme_light()+
  theme(legend.justification=c(0,0), legend.position=c(0,0),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.title = element_text(face = "plain", size = 11),
        legend.text= element_text(size = 10),
        legend.key.size = unit(1, "cm"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_colour_manual(name = "Climate" ,
                      breaks = c("+ 2°C","+ 3°C"),
                      labels = c("+2°C", "+3°C"),
                      values = c("#2ca25f", "#e34a33")) +
  scale_linetype_manual(name = "Heterogeneity" ,
                        breaks = c("1.5","3"),
                        labels = c("Sigma = 1.5", "Sigma = 3.0"),
                        values=c("solid", "dashed")) +
  scale_shape_manual(name = "Heterogeneity" ,
                     breaks = c("1.5","3"),
                     labels = c("Sigma = 1.5", "Sigma = 3.0"),
                     values=c(1,4)) +
  guides(colour = guide_legend(ncol = 1)) +
  xlim(5, 55) +
  ylim(1, 18)


# RM
ggLandRM <- ggplot(RMdata, aes(year, color = climateScenario)) +
  geom_point(aes(y = landIndex, shape = sigmaKernel), size = 1, alpha = 0.6) +
  geom_line(aes(y = mLandIndex, linetype = sigmaKernel), size = 1) +
  labs(x= "Year", y= "Ab./Acq.",
       title="RELATIVE MAJORITY")+
  theme_light()+
  theme(legend.justification=c(0,0), legend.position=c(0,0),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.title = element_text(face = "plain", size = 11),
        legend.text= element_text(size = 10),
        legend.key.size = unit(1, "cm"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_colour_manual(name = "Climate" ,
                      breaks = c("+ 2°C","+ 3°C"),
                      labels = c("+2°C", "+3°C"),
                      values = c("#2ca25f", "#e34a33")) +
  scale_linetype_manual(name = "Heterogeneity" ,
                        breaks = c("1.5","3"),
                        labels = c("Sigma = 1.5", "Sigma = 3.0"),
                        values=c("solid", "dashed")) +
  scale_shape_manual(name = "Heterogeneity" ,
                     breaks = c("1.5","3"),
                     labels = c("Sigma = 1.5", "Sigma = 3.0"),
                     values=c(1,4)) +
  guides(colour = guide_legend(ncol = 1)) +
  xlim(5, 55) +
  ylim(1, 18)


# COMBINED LAND INDEX

#---Arrange ggplots------------------------------------------------#
summaryLAND <- ggarrange(ggLandNV, ggLandAM, ggLandRM,
                        common.legend = T,
                        legend.grob = get_legend(ggExitsYV),
                        legend = "right",
                        ncol = 1, nrow = 3)

summaryLAND <- annotate_figure(summaryLAND, top = text_grob("LAND USE INDEX FOR DIFFERENT INSTITUTIONAL SETTINGS", size = 18))

# print it directly as a picture

#---Export file---------------------------------------------------#
ggsave( 
  filename = "summaryLAND_NEW.png",
  plot = summaryLAND,
  device = "png",
  path = NULL,
  scale = 1,
  width = 8.27,
  height = 11.69,
  units = "in",
  dpi = 100
)
## ----

# GI PRICE (all data points DIFFERENCES IN INSTITUTIONAL CHANGE)
# ----

# NV
ggPriceNV <- ggplot(NVdata, aes(year, color = climateScenario)) +
  geom_point(aes(y = giWinePrice, shape = sigmaKernel), size = 1, alpha = 0.6) +
  geom_line(aes(y = mGiWinePrice, linetype = sigmaKernel), size = 1) +
  labs(x= "Year", y= "€",
       title="WITHOUT Institutional Change")+
  theme_light()+
  theme(legend.justification=c(0,0), legend.position=c(0,0),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.title = element_text(face = "plain", size = 9),
        legend.text= element_text(size = 8),
        legend.key.size = unit(0.8, "cm"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_colour_manual(name = "Climate" ,
                      breaks = c("+ 2°C","+ 3°C"),
                      labels = c("+2°C", "+3°C"),
                      values = c("#2ca25f", "#e34a33")) +
  scale_linetype_manual(name = "Heterogeneity" ,
                        breaks = c("1.5","3"),
                        labels = c("Sigma = 1.5", "Sigma = 3.0"),
                        values=c("solid", "dashed")) +
  scale_shape_manual(name = "Heterogeneity" ,
                     breaks = c("1.5","3"),
                     labels = c("Sigma = 1.5", "Sigma = 3.0"),
                     values=c(1,4)) +
  guides(colour = guide_legend(ncol = 1)) +
  xlim(5, 55) +
  ylim(0, 3)

# YV
ggPriceYV <- ggplot(YVdata, aes(year, color = climateScenario)) +
  geom_point(aes(y = giWinePrice, shape = sigmaKernel), size = 1, alpha = 0.6) +
  geom_line(aes(y = mGiWinePrice, linetype = sigmaKernel), size = 1) +
  labs(x= "Year", y= "€",
       title="WITH Institutional Change")+
  theme_light()+
  theme(legend.justification=c(0,0), legend.position=c(0,0),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.title = element_text(face = "plain", size = 9),
        legend.text= element_text(size = 8),
        legend.key.size = unit(0.8, "cm"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_colour_manual(name = "Climate" ,
                      breaks = c("+ 2°C","+ 3°C"),
                      labels = c("+2°C", "+3°C"),
                      values = c("#2ca25f", "#e34a33")) +
  scale_linetype_manual(name = "Heterogeneity" ,
                        breaks = c("1.5","3"),
                        labels = c("Sigma = 1.5", "Sigma = 3.0"),
                        values=c("solid", "dashed")) +
  scale_shape_manual(name = "Heterogeneity" ,
                     breaks = c("1.5","3"),
                     labels = c("Sigma = 1.5", "Sigma = 3.0"),
                     values=c(1,4)) +
  guides(colour = guide_legend(ncol = 1)) +
  xlim(5, 55) +
  ylim(0, 3)


# AM
ggPriceAM <- ggplot(AMdata, aes(year, color = climateScenario)) +
  geom_point(aes(y = giWinePrice, shape = sigmaKernel), size = 1, alpha = 0.6) +
  geom_line(aes(y = mGiWinePrice, linetype = sigmaKernel), size = 1) +
  labs(x= "Year", y= "€",
       title="ABSOLUTE MAJORITY")+
  theme_light()+
  theme(legend.justification=c(0,0), legend.position=c(0,0),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.title = element_text(face = "plain", size = 9),
        legend.text= element_text(size = 8),
        legend.key.size = unit(0.8, "cm"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_colour_manual(name = "Climate" ,
                      breaks = c("+ 2°C","+ 3°C"),
                      labels = c("+2°C", "+3°C"),
                      values = c("#2ca25f", "#e34a33")) +
  scale_linetype_manual(name = "Heterogeneity" ,
                        breaks = c("1.5","3"),
                        labels = c("Sigma = 1.5", "Sigma = 3.0"),
                        values=c("solid", "dashed")) +
  scale_shape_manual(name = "Heterogeneity" ,
                     breaks = c("1.5","3"),
                     labels = c("Sigma = 1.5", "Sigma = 3.0"),
                     values=c(1,4)) +
  guides(colour = guide_legend(ncol = 1)) +
  xlim(5, 55) +
  ylim(0, 3)


# RM
ggPriceRM <- ggplot(RMdata, aes(year, color = climateScenario)) +
  geom_point(aes(y = giWinePrice, shape = sigmaKernel), size = 1, alpha = 0.6) +
  geom_line(aes(y = mGiWinePrice, linetype = sigmaKernel), size = 1) +
  labs(x= "Year", y= "€",
       title="RELATIVE MAJORITY")+
  theme_light()+
  theme(legend.justification=c(0,0), legend.position=c(0,0),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.title = element_text(face = "plain", size = 9),
        legend.text= element_text(size = 8),
        legend.key.size = unit(0.8, "cm"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_colour_manual(name = "Climate" ,
                      breaks = c("+ 2°C","+ 3°C"),
                      labels = c("+2°C", "+3°C"),
                      values = c("#2ca25f", "#e34a33")) +
  scale_linetype_manual(name = "Heterogeneity" ,
                        breaks = c("1.5","3"),
                        labels = c("Sigma = 1.5", "Sigma = 3.0"),
                        values=c("solid", "dashed")) +
  scale_shape_manual(name = "Heterogeneity" ,
                     breaks = c("1.5","3"),
                     labels = c("Sigma = 1.5", "Sigma = 3.0"),
                     values=c(1,4)) +
  guides(colour = guide_legend(ncol = 1)) +
  xlim(5, 55) +
  ylim(0, 3)


# COMBINED GI PRICE

#---Arrange ggplots------------------------------------------------#
summaryPRICE <- ggarrange(ggPriceNV, ggPriceYV, ggPriceAM, ggPriceRM,
                         common.legend = T,
                         legend.grob = get_legend(ggExitsYV),
                         legend = "right",
                         ncol = 2, nrow = 2,
                         heights= c(15, 15))

summaryPRICE <- annotate_figure(summaryPRICE, top = text_grob("GI PRICE FOR DIFFERENT VOTING MECHANIMS", size = 18))

# print it directly as a picture

#---Export file---------------------------------------------------#
ggsave( 
  filename = "summaryPRICE_NEW.png",
  plot = summaryPRICE,
  device = "png",
  path = NULL,
  scale = 1,
  width = 11.69,
  height = 8.27,
  units = "in",
  dpi = 100
)
## ----