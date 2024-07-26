#library("lme4")
library("lmerTest") # gives p-values for those that want them
library("ggeffects")
library("interactions")
library('dplyr')
library('ggplot2')
library('DHARMa')
library('MuMIn')

setwd("~/Documents/Scripts/PostureAnalysis")
#csd_data <- read.csv("./full_df2.csv")
csd_data <- read.csv("./full_df3.csv")

csd_data$PostureSum <- 0
aggregate(csd_data$Posture,by=list(Category=csd_data$BirdID),FUN=sum)

filtered_csd <- csd_data %>%
  group_by(BirdID) %>%
  filter(sum(Posture) > 5)
aggregate(filtered_csd$Posture,by=list(Category=filtered_csd$BirdID),FUN=sum)

csd_data <- filtered_csd
#old_csd_data <- read.csv("~/Documents/Scripts/PostureAnalysis2/full_df.csv")
#csd_data_ <- csd_data[order(csd_data$AvgPotency),]
#old_csd_data_ <- old_csd_data[order(old_csd_data$AvgPotency),]

### LATENCY ####

## Check for effects of Latency: 

lat_data <- subset(csd_data,select=c("Posture","Latency","AvgPotency","BlockResponseRate","WrongResponseRate","Bird","BirdID","Song","Block","Aviary"))
lat_data <- lat_data[complete.cases(lat_data),]
lat_data <- lat_data[lat_data$Posture == 1,]
lat_data <- lat_data[lat_data$Latency > 0,]

lat_data['LogLatency'] <- log(lat_data['Latency'])
lat_data['SqrtLatency'] <- sqrt(lat_data['Latency'])
plot(lat_data$Latency,lat_data$AvgPotency)
mean(lat_data$Latency)
sd(lat_data$Latency)
resLat.aov <- aov(Latency ~ Bird, data = lat_data)
summary(resLat.aov)

latencyModel.lmer <- lmer(formula = Latency ~ AvgPotency + BlockResponseRate + 
                              (1|Bird) + (1|Song) + (1|Block),data=lat_data)
latencyModel.log <- lmer(formula = LogLatency ~ AvgPotency + BlockResponseRate + 
                            (1|Bird) + (1|Song) + (1|Block),data=lat_data)
latencyModel.sqrt <- lmer(formula = SqrtLatency ~ AvgPotency + BlockResponseRate + 
                           (1|Bird) + (1|Song) + (1|Block),data=lat_data)
latencyModel.lm <- lm(formula = Latency ~ AvgPotency + BlockResponseRate,data=lat_data)
latencyModel.lmer2 <- lmer(formula = Latency ~ AvgPotency + BlockResponseRate + 
                             (1|Block) + (1|Song),data=lat_data)
summary(latencyModel.lmer)
summary(latencyModel.lm)
AIC(latencyModel.lmer,latencyModel.lm)

simulationOutput <- simulateResiduals(fittedModel = latencyModel.lm, plot = F)
plot(simulationOutput)

simulationOutput <- simulateResiduals(fittedModel = latencyModel.lmer, plot = F)
plot(simulationOutput)
testDispersion(simulationOutput)

plot(fitted(latencyModel.lmer),resid(latencyModel.lmer))
qqnorm(resid(latencyModel.lmer))
qqline(resid(latencyModel.lmer))

plot(fitted(latencyModel.lm),resid(latencyModel.lm))
qqnorm(resid(latencyModel.lm))
qqline(resid(latencyModel.lm))

## Generate plots for Latency plots

lat_overlay_pot <- ggpredict(latencyModel,terms="AvgPotency")
p.lat.pot <- plot(lat_overlay_pot,add.data = TRUE) + 
  theme_classic() + 
  theme(legend.position = "none",
        rect=element_rect(fill="transparent"),
        panel.background=element_rect(fill="transparent"),
        axis.line = element_line(size = 0.5, colour = "black"),
        axis.ticks = element_line(colour = "black"),
        axis.text = element_text(size = 12, colour = "black"),
        axis.title = element_text(size = 14, face = "bold", colour = "black"),
        plot.title = element_text(hjust = 0.5, size = 0)) 
p.lat.pot
ggsave(file='./figures/f3b.png',plot=p.lat.pot,dpi=300,bg = "transparent")


lat_overlay_rsp <- ggpredict(latencyModel,terms="BlockResponseRate")
p.lat.resp <- plot(lat_overlay_rsp,add.data = TRUE) + 
  theme_classic() + 
  theme(legend.position = "none",
        rect=element_rect(fill="transparent"),
        panel.background=element_rect(fill="transparent"),
        axis.line = element_line(size = 0.5, colour = "black"),
        axis.ticks = element_line(colour = "black"),
        axis.text = element_text(size = 12, colour = "black"),
        axis.title = element_text(size = 14, face = "bold", colour = "black"),
        plot.title = element_text(hjust = 0.5, size = 0)) 
p.lat.resp
ggsave(file='./figures/f3c.png',plot=p.lat.resp,dpi=300,bg = "transparent")


ggpredict(latencyModel,terms="AvgPotency")

### DURATION ####
dur_data <- subset(csd_data,select=c("Posture","Duration","AvgPotency","BlockResponseRate","Bird","Block","Song","Aviary"))
dur_data <- dur_data[complete.cases(dur_data),]
dur_data <- dur_data[dur_data$Posture == 1,]
dur_data <- dur_data[dur_data$Duration> 0,]

mean(dur_data$Duration)
sd(dur_data$Duration)
resDur.aov <- aov(Duration ~ Bird, data = dur_data)
summary(resDur.aov)

durationModel <- lmer(formula = Duration ~ AvgPotency + BlockResponseRate + 
                        (1|Bird) + (1|Song) + (1|Block) + (1|Aviary),data=dur_data)

summary(durationModel)
simulationOutput <- simulateResiduals(fittedModel = durationModel, plot = F)
plot(simulationOutput)

#dur_overlay_pot <- ggpredict(durationModel,terms="AvgPotency")
#plot(dur_overlay_pot,add.data = TRUE)

#dur_overlay_rsp <- ggpredict(durationModel,terms="BlockResponseRate")
#plot(dur_overlay_rsp,add.data = TRUE)
plot(fitted(durationModel),resid(durationModel))

## Based on the residual plot above and the data content, a Gamma distribution 
#   seems much more appropriate
dur_data['LogDuration'] <- log(dur_data['Duration'])
dur_data['SqrtDuration'] <- sqrt(dur_data['Duration'])


duration.model.sqrt <- lm(formula = SqrtDuration ~ AvgPotency + BlockResponseRate,data=dur_data)
simulationOutput <- simulateResiduals(fittedModel = duration.model.sqrt, plot = F)
plot(simulationOutput)
summary(duration.model.sqrt)

duration.gamma.sqrt <- glmer(formula = SqrtDuration ~ AvgPotency + BlockResponseRate + 
                         (1|Bird) + (1|Song) + (1|Block) + (1|Aviary), 
                       family=Gamma(link="log"), data=dur_data)
simulationOutput <- simulateResiduals(fittedModel = duration.gamma.sqrt, plot = F)
plot(simulationOutput)

duration.model.glm <- glm(formula = Duration ~ AvgPotency + BlockResponseRate,
                         data=dur_data,family=Gamma(link="log"))
simulationOutput <- simulateResiduals(fittedModel = duration.model.log, plot = F)
plot(simulationOutput)
summary(duration.model.glm)

duration.model.log <- lm(formula = LogDuration ~ AvgPotency + BlockResponseRate,data=dur_data)
summary(duration.model.log)



simulationOutput <- simulateResiduals(fittedModel = duration.model.log, plot = F)
plot(simulationOutput)

duration.model.lm <- lm(formula = Duration ~ AvgPotency + BlockResponseRate,data=dur_data)
summary(duration.model.lm)

durationGamma <- glmer(formula = Duration ~ AvgPotency + BlockResponseRate + 
                           (1|Bird) + (1|Song) + (1|Block) + (1|Aviary), 
                         family=Gamma(link="log"), data=dur_data)
summary(durationGamma)
plot(fitted(durationGamma),resid(durationGamma)) ## Still get bands, but I believe that's ok for Gamma 

simulationOutput <- simulateResiduals(fittedModel = durationGamma, plot = F)
plot(simulationOutput)

AIC(durationGamma,duration.model.glm,duration.model.lm,duration.model.log,duration.model.sqrt)
### Generate duration plots
summary(duration.model.sqrt)

dur_overlay_pot <- ggpredict(durationGamma,terms="AvgPotency")

p.dur.pot <- plot(dur_overlay_pot,add.data = TRUE) + 
  theme_classic() + 
  theme(legend.position = "none",
        rect=element_rect(fill="transparent"),
        panel.background=element_rect(fill="transparent"),
        axis.line = element_line(size = 0.5, colour = "black"),
        axis.ticks = element_line(colour = "black"),
        axis.text = element_text(size = 12, colour = "black"),
        axis.title = element_text(size = 14, face = "bold", colour = "black"),
        plot.title = element_text(hjust = 0.5, size = 0)) 
p.dur.pot
ggsave(file='./figures/f4b.png',plot=p.dur.pot,dpi=300,bg = "transparent")

dur_overlay_rsp <- ggpredict(durationGamma,terms="BlockResponseRate")
p.dur.resp <- plot(dur_overlay_rsp,add.data = TRUE) + 
  theme_classic() + 
  theme(legend.position = "none",
        rect=element_rect(fill="transparent"),
        panel.background=element_rect(fill="transparent"),
        axis.line = element_line(size = 0.5, colour = "black"),
        axis.ticks = element_line(colour = "black"),
        axis.text = element_text(size = 12, colour = "black"),
        axis.title = element_text(size = 14, face = "bold", colour = "black"),
        plot.title = element_text(hjust = 0.5, size = 0)) 
p.dur.resp
ggsave(file='./figures/f4c.png',plot=p.dur.resp,dpi=300,bg = "transparent")

### PREDICTING POSTURE ####

## Run average potency model, testing specifically for whether there's a time effect
csd_binary <- csd_data
csd_binary[csd_binary$Posture == 2,"Posture"] <- 0
csd_binary <- csd_binary[csd_binary$Block < 10,]

csd_binary.clean <- na.omit(select(csd_binary,'Posture','Song','SongID','BirdID','Bird','Block','Aviary'))

csd_binary.blockresponse <- select(csd_binary,'Posture','AvgPotency','BlockResponseRate','BirdID','Bird','Block','Aviary')
csd_binary.blockresponse <- na.omit(csd_binary.blockresponse)

csd.model.lm <- lm(formula = Posture ~ AvgPotency, data=csd_binary.blockresponse)

csd.model.lm2 <- lm(formula = Posture ~ AvgPotency + BlockResponseRate, data=csd_binary.blockresponse)
csd.model.lm3 <- lm(formula = Posture ~ AvgPotency * BlockResponseRate, data=csd_binary.blockresponse)

csd.model.pot <- glmer(formula = Posture ~ AvgPotency + 
                         (1|Bird),
                       data = csd_binary.blockresponse, 
                       family=binomial)
csd.model.block <- glmer(formula = Posture ~ Block + 
                         (1|Bird),
                       data = csd_binary.blockresponse, 
                       family=binomial)
csd.model.simple <- glmer(formula = Posture ~ AvgPotency + Block + 
                          (1|Bird),
                    data = csd_binary.blockresponse, 
                    family=binomial)
csd.model.interaction <- glmer(formula = Posture ~ AvgPotency * Block + 
                                 (1|Bird), data = csd_binary.blockresponse, 
                               family=binomial)

csd.model.simple.resp <- glmer(formula = Posture ~ AvgPotency + BlockResponseRate +
                              (1|Bird), data = csd_binary.blockresponse, 
                            family=binomial)

csd.model.interaction.resp <- glmer(formula = Posture ~ AvgPotency * BlockResponseRate +
                                      (1|Bird), data = csd_binary.blockresponse, 
                                    family=binomial)
csd.model.aviary <- csd.model.simple.resp <- glmer(formula = Posture ~ AvgPotency + BlockResponseRate +
                                                     (1|Bird) + (1|Aviary), data = csd_binary.blockresponse, 
                                                   family=binomial)
csd.model.block.slope <- csd.model.simple.resp <- glmer(formula = Posture ~ AvgPotency + BlockResponseRate +
                                                     (BlockResponseRate|Bird), data = csd_binary.blockresponse, 
                                                   family=binomial)
csd.model.pot.slope <- glmer(formula = Posture ~ AvgPotency + BlockResponseRate +
                                                        (AvgPotency|Bird), data = csd_binary.blockresponse, 
                                                      family=binomial)
csd.model.pot.Aviary <- glmer(formula = Posture ~ AvgPotency + BlockResponseRate +
                               (AvgPotency|Bird) + (1|Aviary), data = csd_binary.blockresponse, 
                             family=binomial)
csd.model.pot.slope.interaction <- glmer(formula = Posture ~ AvgPotency * BlockResponseRate +
                               (AvgPotency|Bird), data = csd_binary.blockresponse, 
                             family=binomial)
csd.model.both.slope <- glmer(formula = Posture ~ AvgPotency + BlockResponseRate +
                                                        (AvgPotency|Bird) + (BlockResponseRate|Bird) + (1|Aviary), data = csd_binary.blockresponse, 
                                                      family=binomial)


DIC(csd.model.pot, csd.model.block, csd.model.simple,csd.model.interaction,csd.model.simple.resp,csd.model.interaction.resp,
    csd.model.aviary,csd.model.block.slope,csd.model.pot.slope,csd.model.both.slope,csd.model.pot.slope.interaction,csd.model.pot.Aviary)

AIC(csd.model.lm,csd.model.lm2,csd.model.lm3,csd.model.pot, csd.model.block, csd.model.simple,csd.model.interaction,csd.model.simple.resp,csd.model.interaction.resp,
    csd.model.aviary,csd.model.block.slope,csd.model.pot.slope,csd.model.both.slope,csd.model.pot.slope.interaction,csd.model.pot.Aviary)

aviaries <- unique(csd_binary.clean$Aviary)
l <- 1
effs.intercept <- list()
effs.song <- list()
effs.block <- list()

for(a in aviaries) {
  csd_binary.aviary <- csd_binary.clean[csd_binary.clean$Aviary == a,]
  songs <- unique(csd_binary.aviary$SongID)
  songs.pairs <- t(combn(songs,2))
  for (i in seq(1,length(songs.pairs) / 2)) {
    song.A <- songs.pairs[i,1]
    song.B <- songs.pairs[i,2]
    csd.aviary.subset <- csd_binary.aviary %>% filter(SongID == song.A | SongID == song.B)
    csd.model.pair <- glmer(formula = Posture ~ SongID + Block + (1|Bird),
                             data = csd.aviary.subset, 
                             family=binomial)
    print('doing something...')
      effs.intercept[l] <- fixef(csd.model.pair)[[1]]
      effs.song[l] <- abs(fixef(csd.model.pair)[[2]])
      effs.block[l] <- fixef(csd.model.pair)[[3]]
      l <- l + 1
  }
  break
}
effs.intercept <- unlist(effs.intercept,use.names=FALSE)
effs.song <- unlist(effs.song,use.names=FALSE)
effs.block <- unlist(effs.block,use.names=FALSE)

m.intercept <- mean(effs.intercept)
m.song <- mean(effs.song)
m.block <- mean(effs.block)

se.intercept <- sd(effs.intercept) / sqrt(length(effs.intercept))
se.song <- sd(effs.song) / sqrt(length(effs.intercept))
se.block <- sd(effs.block) / sqrt(length(effs.intercept))

print(m.intercept);print(se.intercept)
print(m.song);print(se.song)
print(m.block);print(se.block)


csd_binary.subset <- csd_binary.clean %>% filter(SongID == "BDY" | SongID == "2M")
csd.model.clean <- glmer(formula = Posture ~ SongID + Block + (1|Bird),
                       data = csd_binary.subset, 
                       family=binomial)
summary(csd.model.clean)

csd.model.clean.int <- glmer(formula = Posture ~ Song * Block + (1|Bird),
                         data = csd_binary.clean, 
                         family=binomial)


summary(csd.model.clean)
summary(csd.model.clean.int)
## Best model by DIC/AIC is 
summary(csd.model.pot.slope)

summary(csd.model.pot.slope.interaction)

plot(fitted(csd.model.pot.slope.interaction),resid(csd.model.pot.slope.interaction))
plot(fitted(csd.model.pot.slope),resid(csd.model.pot.slope)) 
plot(fitted(csd.model.simple.resp),resid(csd.model.simple.resp))
plot(fitted(csd.model.lm2),resid(csd.model.lm2))


scoreModel <-glmer(formula = Posture ~ AvgPotency * Block +
                   (1|Bird) + (1|Aviary) + (1|SongSet), data = csd_binary, 
                   family=binomial)




summary(scoreModel)
p.csd.data <- interact_plot(scoreModel,
                            modxvals=c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9),
                            pred=Block,modx=AvgPotency,colors=gray(1 - c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9))) + 
  theme_classic() + 
  theme(legend.position = "right",
        rect=element_rect(fill="transparent"),
        panel.background=element_rect(fill="transparent"),
        axis.line = element_line(size = 0.5, colour = "black"),
        axis.ticks = element_line(colour = "black"),
        axis.text = element_text(size = 12, colour = "black"),
        axis.title = element_text(size = 14, face = "bold", colour = "black"),
        plot.title = element_text(hjust = 0.5, size = 0)) 
p.csd.data
ggsave(file='./figures/5f-legend.png',plot=p.csd.data,dpi=300,bg = "transparent")

## Plot simulated experiments: 
sim.pot <- read.csv("./csdModel/tmp_pot.csv")
sim.pot[,"Posture"] <- sim.pot$Response

pot.model <-glmer(formula = Posture ~ AvgPotency * Block +
                     (1|Bird), data = sim.pot, 
                   family=binomial)
summary(pot.model)
sim.pot$Fits <- fitted(pot.model)

ggplot(data=sim.pot,aes(x=Block,y=Posture,group=Song)) + 
  geom_line()


p.csd.pot <- interact_plot(pot.model,
                            modxvals=c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9),
                            pred=Block,modx=AvgPotency,colors=gray(1 - c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9))) + 
  theme_classic() + 
  theme(legend.position = "none",
        rect=element_rect(fill="transparent"),
        panel.background=element_rect(fill="transparent"),
        axis.line = element_line(size = 0.5, colour = "black"),
        axis.ticks = element_line(colour = "black"),
        axis.text = element_text(size = 12, colour = "black"),
        axis.title = element_text(size = 14, face = "bold", colour = "black"),
        plot.title = element_text(hjust = 0.5, size = 0)) 
p.csd.pot
ggsave(file='./figures/5d.png',plot=p.csd.pot,dpi=300,bg = "transparent")


sim.thresh <- read.csv("./csdModel/tmp_thresh.csv")
sim.thresh[,"Posture"] <- sim.thresh$Response

thresh.model <-glmer(formula = Posture ~ AvgPotency * Block +
                    (1|Bird), data = sim.thresh, 
                  family=binomial)
summary(thresh.model)
sim.thresh$Fits <- fitted(thresh.model)

ggplot(data=sim.thresh,aes(x=Block,y=Fits,group=Song)) + 
  geom_line()


p.csd.thresh <- interact_plot(thresh.model,
                           modxvals=c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9),
                           pred=Block,modx=AvgPotency,colors=gray(1 - c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9))) + 
  theme_classic() + 
  theme(legend.position = "none",
        rect=element_rect(fill="transparent"),
        panel.background=element_rect(fill="transparent"),
        axis.line = element_line(size = 0.5, colour = "black"),
        axis.ticks = element_line(colour = "black"),
        axis.text = element_text(size = 12, colour = "black"),
        axis.title = element_text(size = 14, face = "bold", colour = "black"),
        plot.title = element_text(hjust = 0.5, size = 0)) 
p.csd.thresh
ggsave(file='./figures/5e.png',plot=p.csd.thresh,dpi=300,bg = "transparent")


## Run Block One Potency Model, b1 potency should not predict if preference is learned
B1scoreModel <-glmer(formula = Posture ~ B1Potency * Block + 
                       (1|Bird) + (1|Aviary) + (1|SongSet), data = csd_binary, family=binomial)
summary(B1scoreModel)
interact_plot(B1scoreModel,modxvals=c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9),pred=Block,modx=B1Potency)

## An overall good model, to see what explains Posture generally
goodModel <-glmer(formula = Posture ~ AvgPotency * BlockResponseRate +
                    (1|Bird) + (1|Aviary) + (1|SongSet), data = csd_binary, 
                  family=binomial)
summary(goodModel)
interact_plot(goodModel,modxvals=c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9),pred=BlockResponseRate,modx=AvgPotency)

### VELCOITY AND HEIGHT ####
traj_data <- subset(csd_data,select=c("Posture","MaxVelocity","PeakHeight","AvgPotency","BVPotency","Prefix","BlockResponseRate","Bird","Block","Song","Aviary"))
traj_data <- traj_data[complete.cases(traj_data),]
traj_data <- traj_data[traj_data$Posture == 1,]

plot(traj_data$MaxVelocity,traj_data$BVPotency)

velModel <- lmer(formula = MaxVelocity ~ AvgPotency + BlockResponseRate + (1|Bird),data=traj_data)
summary(velModel)

hist(traj_data$MaxVelocity)
plot(fitted(velModel),resid(velModel)) 
plot(traj_data$BVPotency,traj_data$MaxVelocity)

## Should block be a random effect here? 
peak_model <- lmer(formula = PeakHeight ~ BVPotency + BlockResponseRate + (1|Block) + (1|Bird),data=traj_data)
#peak_model <- lm(formula = PeakHeight ~ BVPotency,data=traj_data)

summary(peak_model)

### PARTIAL POSTURES ####
birdview_data = csd_data[csd_data$Aviary < 2,]
table(csd_data$Posture)
table(birdview_data$Posture)
n_playbacks <- length(birdview_data$Posture)
n_csd <- length(which(birdview_data$Posture == 1))
n_partials <- length(which(birdview_data$Posture == 2))

print(c("proportion of full CSD:",n_csd / n_playbacks))
print(c("proportion of partial CSDs:",n_partials / n_playbacks))

## Check whether songs that get postures shift with threshold
partial_data <- csd_data[csd_data$Posture == 2,]
partial_data <- subset(partial_data,select=c("Posture","AvgPotency","BlockResponseRate","Bird","Block","Song","Aviary"))

partial_data <- partial_data[complete.cases(partial_data),]

partialModel <- lmer(formula = AvgPotency ~ BlockResponseRate +
                       (1|Bird) + (1|Song) + (1|Block) + (1|Aviary),data=partial_data)
summary(partialModel)

plot(fitted(partialModel),resid(partialModel))
qqnorm(resid(partialModel))
qqline(resid(partialModel))

par_overlay_rsp <- ggpredict(partialModel,terms="BlockResponseRate")
plot(par_overlay_rsp,add.data = TRUE)

## Check whether latency is different for partials vs full postures
latPart_data <- subset(birdview_data,select=c("Posture","Latency","AvgPotency","BlockResponseRate","Bird","Block","Song"))
latPart_data <- latPart_data[latPart_data$Posture > 0,]
latPart_data <- latPart_data[complete.cases(latPart_data),]
latPart_data[latPart_data$Posture == 2,"Posture"] = 0
partialLatModel <- lmer(formula = Latency ~ Posture + AvgPotency + BlockResponseRate +
                          (1|Bird) + (1|Song) + (1|Block),data=latPart_data)
                                                

summary(partialLatModel)
plot(fitted(partialLatModel),resid(partialLatModel))
qqnorm(resid(partialLatModel))
qqline(resid(partialLatModel))

partLat_overlay_pot <- ggpredict(partialLatModel,terms="Posture")
plot(partLat_overlay_pot,add.data = TRUE)

### PRELIMINARY WHISTLE COMPARISON ####
song_set <- c("BDY","LB","BDY-","LB-")

aviary1_data <- csd_data[csd_data$Aviary == 0,]
whistle_data <- aviary1_data[aviary1_data$SongID %in% song_set,]
whistle_data$Whistle <- 0
whistle_data[whistle_data$SongID %in% c("BDY","LB"),"Whistle"] <- 1
whistle_data[whistle_data$SongID == "BDY-","Song"] <- 0
whistle_data[whistle_data$SongID == "LB-","Song"] <- 3

whistleLat_data <- subset(whistle_data,select=c("Posture","Latency","Duration","Song","Whistle","Bird","Block"))
whistleLat_data <- whistleLat_data[complete.cases(whistleLat_data),]

whistleLat_data_ <- whistleLat_data[whistleLat_data$Bird %in% c(1,2),]

## Just with ANOVA no difference in posture w/ w/o whistle
whisDur.aov <- aov(Posture ~ Whistle, data = whistle_data)
summary(whisDur.aov)

## With linear regression, predicts a weak, non significant effect on duration. 
whistleModel <- lmer(formula = Latency ~ Whistle +
                          (1|Bird) + (1|Song) + (1|Block),data=whistleLat_data_)


summary(whistleModel)

whistleDurModel <- lmer(formula = Duration ~ Whistle +
                       (1|Bird) + (1|Song) + (1|Block),data=whistleLat_data_)


summary(whistleDurModel)

mean(whistleLat_data)
plot(fitted(whistleDurModel),resid(whistleDurModel))
qqnorm(resid(whistleDurModel))
qqline(resid(whistleDurModel))



## Pairwise correlation is performed in a python script (Sfigure1.py)


### OLD LATENCY ####
## An initial pre-print had a significant result for Latency ~ RespRate
# The biggest reason was that I had accidentally included the playback when 
# calculating the block-average. There were also slight differences in the 
# input data, including missing a large group of postures. 
# The current approach is statistically more sound, more straightforward
# and much easier to check, and I am more confident it is correct, 
# But you can look at the old data here. 

# This only matters if you think p=0.04 and p=0.07 are substantively different results
old_data <- read.csv("~/Documents/Scripts/AnalyzePosture/posture_df5.csv")

old_lat <- subset(old_data,select=c("Latency","AvgPotency","ResponseRate","Bird"))
old_lat <- old_lat[complete.cases(old_lat),]
old_lat <- old_lat[old_lat$Latency> 0,]
library("nlme")
oldLatencyModel <- lme(Latency ~ AvgPotency + ResponseRate,random=~1|Bird,data=old_lat)
summary(oldLatencyModel)
