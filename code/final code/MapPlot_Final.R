# I tried to make every possible and interesting data visualization in this file.

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rm(list = ls())

# import Ecomate.Rda
storm=readRDS('Cleandata_ecomate.Rda')
statecodeweb=url(paste("http://eunyoungko.com/resources/rprojectdata/economic/","statecode.csv", sep=""))
statecode <- read.csv(statecodeweb,sep=",", header=TRUE)
colnames(statecode)<-c("STATENAME","STATE")
storm_v=merge(storm,statecode, by="STATE", all.x=TRUE)

# Morbimotality total
storm_morbi.state <- aggregate(storm_v$MORBI_t, by=list(storm_v$STATENAME), FUN=sum)
colnames(storm_morbi.state) <- c("STATENAME", "Morbimortality")
mortality <- gvisGeoChart(storm_morbi.state, "STATENAME", "Morbimortality",
                             options=list(region="US", 
                                          displayMode="regions", 
                                          resolution="provinces",
                                          colors="orange",
                                          width=600, height=400))
plot(mortality)

# morbimotality per event per pop
storm_morbirate=storm_v
storm_morbirate$morbirate=storm_morbirate$MORBI_t/(storm_morbirate$pop*1000)*1000000
storm_morbirate.state <-aggregate(storm_morbirate$morbirate, by = list(storm_morbirate$STATENAME), FUN = mean)
colnames(storm_morbirate.state) <- c("STATENAME", "AvgMorbimortal")

avgMortality <- gvisGeoChart(storm_morbirate.state, "STATENAME", "AvgMorbimortal",
                             options=list(region="US", 
                                          displayMode="regions", 
                                          resolution="provinces",
                                          colors="red",
                                          width=600, height=400))
plot(avgMortality)

# morbimotality per year per pop
storm_morbirate=storm_v
storm_morbirate$morbirate=storm_morbirate$MORBI_t/(storm_morbirate$pop*1000)*1000000
storm_morbiperyear<-aggregate(storm_morbirate$morbirate, by = list(storm_morbirate$STATENAME, storm_morbirate$YEAR), FUN = sum)
colnames(storm_morbiperyear) <- c("STATENAME","YEAR","morbirate")

storm_morbiperyear.state<-aggregate(storm_morbiperyear$morbirate, by = list(storm_morbiperyear$STATENAME), FUN = mean)
colnames(storm_morbiperyear.state) <- c("STATENAME", "AvgMorbimortal")

peryearmorbi <- gvisGeoChart(storm_morbiperyear.state, "STATENAME", "AvgMorbimortal",
                             options=list(region="US", 
                                          displayMode="regions", 
                                          resolution="provinces",
                                          colors="orange",
                                          width=600, height=400))
plot(peryearmorbi)


# Real GDP per capita 
storm_v_2011=subset(storm_v, YEAR==2011)
storm_v_gdp11=storm_v_2011
storm_v_gdp11$rgdppc=round(storm_v_2011$rgdppc*1000)
rgdppc11 <- gvisGeoChart(storm_v_gdp11, "STATENAME", "rgdppc",
                         options=list(region="US", 
                                      displayMode="regions", 
                                      resolution="provinces",
                                      width=600, height=400))
plot(rgdppc11)


# DMG per year
storm_dmg=storm_v
storm_dmg<-aggregate(storm_v$DMG_t, by = list(storm_v$STATENAME, storm_v$YEAR), FUN = sum)
colnames(storm_dmg) <- c("STATENAME","YEAR","dmg")
storm_dmgperyear.state<-aggregate(storm_dmg$dmg, by = list(storm_dmg$STATENAME), FUN = mean)
colnames(storm_dmgperyear.state) <- c("STATENAME", "dmg")
storm_dmgperyear.state$dmg=round(log(storm_dmgperyear.state$dmg/1000)) #log 
peryeardmg <- gvisGeoChart(storm_dmgperyear.state, "STATENAME", "dmg",
                             options=list(region="US", 
                                          displayMode="regions", 
                                          resolution="provinces",
                                          colors="brown",
                                          width=600, height=400))
plot(peryeardmg)

# DMG per event
storm_dmg=storm_v
storm_dmgperevent.state<-aggregate(storm_v$DMG_t, by = list(storm_v$STATENAME), FUN = mean)
colnames(storm_dmgperevent.state) <- c("STATENAME","dmg")
storm_dmgperevent.state$dmg=round(storm_dmgperevent.state$dmg/1000000)
pereventdmg <- gvisGeoChart(storm_dmgperevent.state, "STATENAME", "dmg",
                           options=list(region="US", 
                                        displayMode="regions", 
                                        resolution="provinces",
                                        colors="brown",
                                        width=600, height=400))
plot(pereventdmg)



# Total fatalities
storm_fatal=storm_v
storm_fatal.state <-aggregate(storm_fatal$FATALITIES, by = list(storm_morbirate$STATENAME), FUN = sum)
colnames(storm_fatal.state) <- c("STATENAME", "Total Fatalities")

totalfat <- gvisGeoChart(storm_fatal.state, "STATENAME", "Total Fatalities",
                             options=list(region="US", 
                                          displayMode="regions", 
                                          resolution="provinces",
                                          colors="red",
                                          width=600, height=400))
plot(totalfat)
colnames(storm_fatal.state) <- c("STATENAME", "Fatalities")
fat=merge(storm_fatal.state,statecode, by="STATENAME", all.x=TRUE)
fat=fat[,-c(1)]
fat.sorted <- fat[order(-fat$Fatalities, na.last=TRUE), ]
nr=15
Fats<-fat.sorted[1:nr,]
Fats$STATE <- factor(Fats$STATE, levels = Fats$STATE[order(-Fats$Fatalities)])

ggplot(Fats, aes(x=STATE, y=Fatalities, fill=STATE))+
  geom_bar(stat="identity", position="dodge")+
  xlab("State")+
  ylab("Fatalities")+
  ggtitle("Fatalities vs State")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5))



# Total injuries
storm_inj=storm_v
storm_inj.state <-aggregate(storm_inj$INJURIES, by = list(storm_inj$STATENAME), FUN = sum)
colnames(storm_inj.state) <- c("STATENAME", "Total Injuries")

totalinj <- gvisGeoChart(storm_inj.state, "STATENAME", "Total Injuries",
                         options=list(region="US", 
                                      displayMode="regions", 
                                      resolution="provinces",
                                      colors="yellow",
                                      width=600, height=400))
plot(totalinj)
colnames(storm_inj.state) <- c("STATENAME", "Injuries")
inj=merge(storm_inj.state,statecode, by="STATENAME", all.x=TRUE)
inj=inj[,-c(1)]
inj.sorted <- inj[order(-inj$Injuries, na.last=TRUE), ]
nr=15
Injs<-inj.sorted[1:nr,]
Injs$STATE <- factor(Injs$STATE, levels = Injs$STATE[order(-Injs$Injuries)])

ggplot(Injs, aes(x=STATE, y=Injuries, fill=STATE))+
  geom_bar(stat="identity", position="dodge")+
  xlab("State")+
  ylab("Injuries")+
  ggtitle("Injuries vs State")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5))


# Total Morb
storm_morb=storm_v
storm_morb.state <-aggregate(storm_morb$MORBI_t, by = list(storm_morb$STATENAME), FUN = sum)
colnames(storm_morb.state) <- c("STATENAME", "Total Morbimortalities")

totalmorb <- gvisGeoChart(storm_morb.state, "STATENAME", "Total Morbimortalities",
                         options=list(region="US", 
                                      displayMode="regions", 
                                      resolution="provinces",
                                      colors="orange",
                                      width=600, height=400))
plot(totalmorb)
colnames(storm_morb.state) <- c("STATENAME", "Morbimortalities")
morb=merge(storm_morb.state,statecode, by="STATENAME", all.x=TRUE)
morb=morb[,-c(1)]
morb.sorted <- morb[order(-morb$Morbimortalities, na.last=TRUE), ]
nr=15
Morbs<-morb.sorted[1:nr,]
Morbs$STATE <- factor(Morbs$STATE, levels = Morbs$STATE[order(-Morbs$Morbimortalities)])

ggplot(Morbs, aes(x=STATE, y=Morbimortalities, fill=STATE))+
  geom_bar(stat="identity", position="dodge")+
  xlab("State")+
  ylab("Morbimortality")+
  ggtitle("Morbimortality vs State")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5))



# Total Property damage 
storm_prop=storm_v
storm_prop.state<-aggregate(storm_prop$PROPDMG_t, by = list(storm_prop$STATENAME), FUN = sum)
colnames(storm_prop.state) <- c("STATENAME", "PROPDMG_t")
storm_prop.state$PROPDMG_t=round(storm_prop.state$PROPDMG_t/1000)
prop <- gvisGeoChart(storm_prop.state, "STATENAME", "PROPDMG_t",
                           options=list(region="US", 
                                        displayMode="regions", 
                                        resolution="provinces",
                                        colors="red",
                                        width=600, height=400))
plot(prop)

# Total Crop damage
storm_crop=storm_v
storm_crop.state<-aggregate(storm_crop$CROPDMG_t, by = list(storm_crop$STATENAME), FUN = sum)
colnames(storm_crop.state) <- c("STATENAME", "CROPDMG_t")
storm_crop.state$CROPDMG_t=round(storm_crop.state$CROPDMG_t/1000) 
crop <- gvisGeoChart(storm_crop.state, "STATENAME", "CROPDMG_t",
                     options=list(region="US", 
                                  displayMode="regions", 
                                  resolution="provinces",
                                  colors="yellow",
                                  width=600, height=400))
plot(crop)


# Total damage
storm_dmg=storm_v
storm_dmg.state<-aggregate(storm_dmg$DMG_t, by = list(storm_dmg$STATENAME), FUN = sum)
colnames(storm_dmg.state) <- c("STATENAME", "DMG_t")
storm_dmg.state$DMG_t=round(storm_dmg.state$DMG_t/1000) 
dmg <- gvisGeoChart(storm_dmg.state, "STATENAME", "DMG_t",
                     options=list(region="US", 
                                  displayMode="regions", 
                                  resolution="provinces",
                                  colors="orange",
                                  width=600, height=400))
plot(dmg)


# Total Property damage 
storm_prop=storm_v

storm_prop.state<-aggregate(storm_prop$PROPDMG_t, by = list(storm_prop$STATENAME), FUN = sum)
colnames(storm_prop.state) <- c("STATENAME", "PROPDMG_t")
storm_prop.state$PROPDMG_t=round(storm_prop.state$PROPDMG_t/1000)
prop <- gvisGeoChart(storm_prop.state, "STATENAME", "PROPDMG_t",
                     options=list(region="US", 
                                  displayMode="regions", 
                                  resolution="provinces",
                                  colors="red",
                                  width=600, height=400))
plot(prop)

# Total Crop damage
storm_crop=storm_v
storm_crop.state<-aggregate(storm_crop$CROPDMG_t, by = list(storm_crop$STATENAME), FUN = sum)
colnames(storm_crop.state) <- c("STATENAME", "CROPDMG_t")
storm_crop.state$CROPDMG_t=round(storm_crop.state$CROPDMG_t/1000) 
crop <- gvisGeoChart(storm_crop.state, "STATENAME", "CROPDMG_t",
                     options=list(region="US", 
                                  displayMode="regions", 
                                  resolution="provinces",
                                  colors="yellow",
                                  width=600, height=400))
plot(crop)


# Total damage
storm_dmg=storm_v
storm_dmg.state<-aggregate(storm_dmg$DMG_t, by = list(storm_dmg$STATENAME), FUN = sum)
colnames(storm_dmg.state) <- c("STATENAME", "DMG_t")
storm_dmg.state$DMG_t=round(storm_dmg.state$DMG_t/1000) 
dmg <- gvisGeoChart(storm_dmg.state, "STATENAME", "DMG_t",
                    options=list(region="US", 
                                 displayMode="regions", 
                                 resolution="provinces",
                                 colors="orange",
                                 width=600, height=400))
plot(dmg)
