#-----------------------------------------------------------------#
#--- Script producing producing histograms for year 55--#
#-----------------------------------------------------------------#
# ---- Matteo Bruno Ricozzi, 06.06.2021 --------------------------#

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

# HISTOGRAMS OF MAIN OUTPUT VARIABLES AT TIME 55
# ----
targetRows<- grep("55", bigData$year)

LASTYEAR <- bigData[targetRows,]

plot(LASTYEAR$farmExits)


colors <- c("Density plot" = "orange", "Mean" = "royalblue", "Median" = "red")

library(ggplot2)

Hist1 <- ggplot(LASTYEAR, aes(x = farmExits)) +
  geom_histogram(aes(y = ..density..), fill = NA , col = "black") +
  geom_density(lwd = 1, aes (col = "Density plot"), show.legend =TRUE) +
  geom_vline(aes(xintercept = mean(farmExits), col = "Mean"), lwd = 1, show.legend =TRUE) +
  geom_vline(aes(xintercept = median(farmExits), col = "Median"), lwd = 1, show.legend = TRUE) +
  labs(x= "%", y= "density",
       title="Farm Exits")+
  theme_light()+
  theme(legend.justification=c(1,1), legend.position=c(1,1),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.title = element_text(face = "plain", size = 9),
        legend.text= element_text(size = 8),
        legend.key.size = unit(0.8, "cm"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_colour_manual(name = "Legend" ,
                      values = c(colors))

Hist2 <- ggplot(LASTYEAR, aes(x = meanProfits)) +
  geom_histogram(aes(y = ..density..), fill = NA , col = "black") +
  geom_density(lwd = 1, aes (col = "Density plot"), show.legend =TRUE) +
  geom_vline(aes(xintercept = mean(meanProfits), col = "Mean"), lwd = 1, show.legend =TRUE) +
  geom_vline(aes(xintercept = median(meanProfits), col = "Median"), lwd = 1, show.legend = TRUE) +
  labs(x= "1000€", y= "density",
       title="Average Profit")+
  theme_light()+
  theme(legend.justification=c(1,1), legend.position=c(1,1),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.title = element_text(face = "plain", size = 9),
        legend.text= element_text(size = 8),
        legend.key.size = unit(0.8, "cm"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_colour_manual(name = "Legend" ,
                      values = c(colors))

Hist3 <- ggplot(LASTYEAR, aes(x = colBrandValue)) +
  geom_histogram(aes(y = ..density..), fill = NA , col = "black") +
  geom_density(lwd = 1, aes (col = "Density plot"), show.legend =TRUE) +
  geom_vline(aes(xintercept = mean(colBrandValue), col = "Mean"), lwd = 1, show.legend =TRUE) +
  geom_vline(aes(xintercept = median(colBrandValue), col = "Median"), lwd = 1, show.legend = TRUE) +
  labs(x= "1000€", y= "density",
       title="Collective Brand Value")+
  theme_light()+
  theme(legend.justification=c(1,1), legend.position=c(1,1),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.title = element_text(face = "plain", size = 9),
        legend.text= element_text(size = 8),
        legend.key.size = unit(0.8, "cm"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_colour_manual(name = "Legend" ,
                      values = c(colors))

Hist4 <- ggplot(LASTYEAR, aes(x = landIndex)) +
  geom_histogram(aes(y = ..density..), fill = NA , col = "black") +
  geom_density(lwd = 1, aes (col = "Density plot"), show.legend =TRUE) +
  geom_vline(aes(xintercept = mean(landIndex), col = "Mean"), lwd = 1, show.legend =TRUE) +
  geom_vline(aes(xintercept = median(landIndex), col = "Median"), lwd = 1, show.legend = TRUE) +
  labs(x= "AB./ACQ.", y= "density",
       title="Land Use Index")+
  theme_light()+
  theme(legend.justification=c(1,1), legend.position=c(1,1),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.title = element_text(face = "plain", size = 9),
        legend.text= element_text(size = 8),
        legend.key.size = unit(0.8, "cm"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_colour_manual(name = "Legend" ,
                      values = c(colors))

library(ggpubr)

summaryDistr <- ggarrange(Hist1, Hist2, Hist3, Hist4,
                               common.legend = T,
                               # legend.grob = get_legend(ggLand1),
                               legend = "right",
                               ncol = 2, nrow = 2,
                               heights= c(15, 15))

summaryDistr <- annotate_figure(summaryDistr, top = text_grob("Main variables' distribution at year 55", size = 20))

