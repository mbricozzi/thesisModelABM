#--------------------------------------------------------------#
#--- Script producing producing boxplots and mean-sd table-----#
#--------------------------------------------------------------#
# ---- Matteo Bruno Ricozzi, 06.06.2021 -----------------------#


# Data Preparation
# ----

library(readr)
data <- read_csv("data_Final.csv", col_types = cols(climateScenario = col_character(), 
                                                    freqBallots = col_character(), sigmaKernel = col_character(), 
                                                    votingMechanism = col_character()))


data$voting <- as.factor(as.numeric(data$votingMechanism != "NO VOTE"))

# create mean farm exit, mean profit and col brand value in 1000 euros

data$farmExits <- (data$farmExits*100)/680
data$meanProfits <- data$totProfits / (data$nFarms * 1000)
data$colBrandValue <- data$colBrandValue / 1000

# Select only interesting variables
colnames(data)

intData <- data[,c("sigmaKernel", "climateScenario", "votingMechanism", "year", 
                   "farmExits", "meanProfits",
                   "colBrandValue", "giWinePrice", 
                   "qualityStandard","votingOutcome",
                   "landIndex2", "totalFallow",
                   "giPrestige", "qualityLev", "colBrandLev", "giFarmsLev"
)]

# I adjusted landIndex to avoid division by zero, add 1 to both numerator and denominator 
# When there is no abandoned or acquired land the index is 1, as when the number of abandoned and acquired ha are the same.

colnames(intData)[11] <- "landIndex"


# FULL SAMPLE MEANS ALL VARIABLES for 12 scenarios

# divide data frame and calculate averages

test <- split(intData, list(intData$climateScenario, intData$sigmaKernel, intData$votingMechanism), drop = TRUE)


for (i in 1:length(test)) {
  assign(paste0("Scenario_",i), as.data.frame(test[[i]]))
}

# create average over simulation for each time step for each scenario data frame



# Farm Exits MEAN

for (i in paste0(1:12)){
  dat <- get (paste0("Scenario_", i))
  dat$Scen <- rep(paste0("Scen_", i), 5600)     # also assign a new var with scenario name (later you merge them)
  dat$mFarmExits <- with(dat, ave(x = farmExits, year, FUN = mean))
  
  assign(paste0('Scenario_',i),dat)    #replace old data frame with new
}

# Farm Exits SD

for (i in paste0(1:12)){
  dat <- get (paste0("Scenario_", i))
  dat$Scen <- rep(paste0("Scen_", i), 5600)     # also assign a new var with scenario name (later you merge them)
  dat$sdFarmExits <- with(dat, ave(x = farmExits, year, FUN = sd))
  
  assign(paste0('Scenario_',i),dat)    #replace old data frame with new
}


# MEAN Profits mean

for (i in paste0(1:12)){
  dat <- get (paste0("Scenario_", i))
  dat$mMeanProfits <- with(dat, ave(x = meanProfits, year, FUN = mean))
  assign(paste0('Scenario_',i),dat)    #replace old data frame with new
}

# MEAN Profits SD

for (i in paste0(1:12)){
  dat <- get (paste0("Scenario_", i))
  dat$sdMeanProfits <- with(dat, ave(x = meanProfits, year, FUN = sd))
  assign(paste0('Scenario_',i),dat)    #replace old data frame with new
}

# ColBrandValue mean

for (i in paste0(1:12)){
  dat <- get (paste0("Scenario_", i))
  dat$mColBrandValue <- with(dat, ave(x = colBrandValue, year, FUN = mean))
  assign(paste0('Scenario_',i),dat)    #replace old data frame with new
}

# ColBrandValue sd

for (i in paste0(1:12)){
  dat <- get (paste0("Scenario_", i))
  dat$sdColBrandValue <- with(dat, ave(x = colBrandValue, year, FUN = sd))
  assign(paste0('Scenario_',i),dat)    #replace old data frame with new
}


# GI price mean

for (i in paste0(1:12)){
  dat <- get (paste0("Scenario_", i))
  dat$mGiWinePrice <- with(dat, ave(x = giWinePrice, year, FUN = mean))
  assign(paste0('Scenario_',i),dat)    #replace old data frame with new
}

# GI price sd

for (i in paste0(1:12)){
  dat <- get (paste0("Scenario_", i))
  dat$sdGiWinePrice <- with(dat, ave(x = giWinePrice, year, FUN = sd))
  assign(paste0('Scenario_',i),dat)    #replace old data frame with new
}

# LandIndex mean

for (i in paste0(1:12)){
  dat <- get (paste0("Scenario_", i))
  dat$mLandIndex <- with(dat, ave(x = landIndex, year, FUN = mean)) # FUN = function(x) mean(x, na.rm=T)
  assign(paste0('Scenario_',i),dat)    #replace old data frame with new
}

# LandIndex mean

for (i in paste0(1:12)){
  dat <- get (paste0("Scenario_", i))
  dat$sdLandIndex <- with(dat, ave(x = landIndex, year, FUN = sd)) # FUN = function(x) mean(x, na.rm=T)
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
  smallData <- rbind(smallData, dat[1:56,c(17,1:4,18:27)])
}

## ----

# Select only important years - CREATE TABLE MEAN-SD for year 55
# ----
rownames(smallData) <- 1:nrow(smallData)

targetRows<- grep("15|25|35|45|55", smallData$year)

dataTable <- smallData[targetRows,]

rownames(dataTable) <- 1:nrow(dataTable)

dataTableLastYear <- smallData[grep("55", smallData$year),]

# ---- 

# CREATE CUSTOM BOXPLOT GRAPHS with GG PLOT
# -----

scenNames <- c("2HetAM","3HetAM","2HomAM","3HomAM","2HetNV","3HetNV","2HomNV", "3HomNV", "2HetRM","3HetRM", "2HomRM", "3HomRM")
colors_12 <- c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c',
               '#fdbf6f','#ff7f00','#cab2d6','#6a3d9a','#bf812d','#8c510a')
  
require(ggplot2)

# GREP from bigdata to make box plots


targetRows<- grep("15|25|35|45|55", bigData$year)

dataTable <- bigData[targetRows,]

# OVERAL Farm Exits

G1 <- ggplot(dataTable, aes(as.factor(year), fill = Scen)) +
  stat_summary(fun=median, geom="line", aes(y = farmExits, group = Scen , color = Scen), position = position_dodge(width = 0.9), size = 0.8) +
  geom_boxplot(aes(y = farmExits), position = position_dodge(width = 0.9) ) +
  stat_summary(fun=mean, geom="point", aes(y = farmExits, group = Scen), colour = "black" , position = position_dodge(width = 0.9), size = 1.5, shape = 8) +
  labs(x= "Year", y= "%",
       title="Farm Exits") +
  theme_light()+
  theme(legend.justification=c(0,1), legend.position=c(0,1),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.title = element_text(face = "plain", size = 9),
        legend.text= element_text(size = 8),
        legend.key.size = unit(0.8, "cm"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_fill_manual(name = "Scenario" ,
                      breaks = c(paste0("Scen_", 1:12)),
                      labels = scenNames,
                      values = colors_12) +
  scale_color_manual(name = "Scenario" ,
                     breaks = c(paste0("Scen_", 1:12)),
                     labels = scenNames,
                     values = colors_12) +
  guides(fill = guide_legend(nrow = 2))

# OVERAL profits


G2 <- ggplot(dataTable, aes(x = as.factor(year),fill = Scen)) +
  stat_summary(fun=median, geom="line", aes(y = meanProfits, group = Scen , color = Scen), position = position_dodge(width = 0.9), size = 0.8) +
  geom_boxplot(aes(y = meanProfits), position = position_dodge(width = 0.9) ) +
  stat_summary(fun=mean, geom="point", aes(y = meanProfits, group = Scen), colour = "black" , position = position_dodge(width = 0.9), size = 1.5, shape = 8) +
  labs(x= "Year", y= "1000€",
       title="Average Profit")+
  theme_light()+
  theme(legend.justification=c(0,0), legend.position=c(0,0),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.title = element_text(face = "plain", size = 9),
        legend.text= element_text(size = 8),
        legend.key.size = unit(0.8, "cm"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_fill_manual(name = "Scenario" ,
                      breaks = c(paste0("Scen_", 1:12)),
                      labels = scenNames,
                      values = colors_12) +
  scale_color_manual(name = "Scenario" ,
                    breaks = c(paste0("Scen_", 1:12)),
                    labels = scenNames,
                    values = colors_12) +
  guides(fill = guide_legend(ncol = 2))

# OVERAL Collective brand value

G3 <- ggplot(dataTable, aes(as.factor(year), fill = Scen)) +
  stat_summary(fun=median, geom="line", aes(y = colBrandValue, group = Scen , color = Scen), position = position_dodge(width = 0.9), size = 0.8) +
  geom_boxplot(aes(y = colBrandValue), position = position_dodge(width = 0.9) ) +
  stat_summary(fun=mean, geom="point", aes(y = colBrandValue, group = Scen), colour = "black" , position = position_dodge(width = 0.9), size = 1.5, shape = 8) +
  labs(x= "Year", y= "1000€",
       title="Collective Brand Value")+
  theme_light()+
  theme(legend.justification=c(0,1), legend.position=c(0,1),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.title = element_text(face = "plain", size = 9),
        legend.text= element_text(size = 8),
        legend.key.size = unit(0.8, "cm"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_fill_manual(name = "Scenario" ,
                      breaks = c(paste0("Scen_", 1:12)),
                      labels = scenNames,
                      values = colors_12) +
  scale_color_manual(name = "Scenario" ,
                     breaks = c(paste0("Scen_", 1:12)),
                     labels = scenNames,
                     values = colors_12) +
  guides(fill = guide_legend(nrow = 2, title.position = "left"))


G4 <- ggplot(dataTable, aes(x = as.character(year), fill = Scen)) +
  stat_summary(fun=median, geom="line", aes(y = landIndex, group = Scen , color = Scen), position = position_dodge(width = 0.9), size = 0.8) +
  geom_boxplot(aes(y = landIndex), position = position_dodge(width = 0.9) ) +
  stat_summary(fun=mean, geom="point", aes(y = landIndex, group = Scen), colour = "black" , position = position_dodge(width = 0.9), size = 1.5, shape = 8) +
  labs(x= "Year", y= "Ab./Acq.",
       title="Land Use Index")+
  theme_light()+
  theme(legend.justification=c(0,1), legend.position="none",
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.title = element_text(face = "plain", size = 9),
        legend.text= element_text(size = 8),
        legend.key.size = unit(0.8, "cm"),
        legend.direction = "vertical",
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_fill_manual(name = "Scenario" ,
                      breaks = c(paste0("Scen_", 1:12)),
                      labels = scenNames,
                      values = colors_12) +
  scale_color_manual(name = "Scenario" ,
                     breaks = c(paste0("Scen_", 1:12)),
                     labels = scenNames,
                     values = colors_12) +
  guides(fill = guide_legend(nrow = 1)) +
  ylim(1, 8.5)

library(ggpubr)

summaryDistr <- ggarrange(G1, G2, G3, G4,
                          common.legend = T,
                          # legend.grob = get_legend(ggLand1),
                          legend = "none",
                          ncol = 2, nrow = 2,
                          heights= c(15, 15))

summaryDistr <- annotate_figure(summaryDistr, top = text_grob("Main variables' distribution at year 55", size = 20))
