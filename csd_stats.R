#library("lme4")
library("lmerTest") # gives p-values for those that want them
library("ggeffects")
library("interactions")
library('dplyr')
library('ggplot2')
library('DHARMa')
library('MuMIn')

## To convert to markdown: knitr::spin('./csd_stats.R')
## To convert markdown to pdf: For reasons I don't fully understand, 
## first get rid of weird characters: ℹ
## Then rmarkdown::render("csd_stats.md", output_format = "pdf_document")

setwd("~/Documents/Scripts/PostureAnalysis")
#setwd("~/Downloads")
#csd_data <- read.csv("./full_df2.csv")
csd_data.raw <- read.csv("./full_df3.csv")

bird_list.all <- aggregate(csd_data.raw$Posture,by=list(Category=csd_data.raw$Bird),FUN=sum)
print(dim(bird_list.all))
filtered_csd <- csd_data.raw %>%
  group_by(Bird) %>%
  filter(sum(Posture) > 5)
bird_list.some <- aggregate(filtered_csd$Posture,by=list(Category=filtered_csd$Bird),FUN=sum)
print(dim(bird_list.some))
csd_data <- filtered_csd

## Check whether songs differ from random: 
csd_data.set0 <- csd_data[csd_data$SongSet == 0,]
csd_data.set0 <- csd_data.set0[csd_data.set0$Aviary != 0,]

song_counts <- aggregate(csd_data.set0$Posture,by=list(Category=csd_data.set0$SongID),FUN=sum)
res.chi <- chisq.test(song_counts$x)
print(res.chi)

## Get some summary stats on song preference: 
song_means <- csd_data.set0 %>%
  group_by(SongID,Aviary) %>%
  summarize(mean_pot = mean(Posture),std_pot = sd(Posture),pres_count = length(Posture))

#print(song_means)
#csd_data.set0[csd_data.set0$Aviary == 1,]

## Consistency of preference: 
song_means.birds <- csd_data.set0 %>%
  group_by(SongID,Bird,Aviary) %>%
  summarize(mean_pot = mean(Posture),std_pot = sd(Posture),pres_count = length(Posture))

aviaries <- unique(song_means$Aviary)

corrs.aviary <- c()
corrs.birds <- c()
for (a in aviaries) {
  birds <- unique(song_means.birds[song_means.birds$Aviary == a,]$Bird)
  song_means.sub <- song_means.birds[song_means.birds$Aviary == a,]
  birds <- unique(song_means.sub$Bird)
  for (i in birds) {
    for (j in birds) {
      if (i == j) next
      #print(c(i,j))
      res.cor <- cor.test(x=song_means.sub[song_means.sub$Bird == i,]$mean_pot,
                          y=song_means.sub[song_means.sub$Bird == j,]$mean_pot,method='pearson')
      #print(res.cor$estimate)
      corrs.birds <- c(corrs.birds,res.cor$estimate)
    }
  }
  for (b in aviaries) {

    if (a == b) next
    #print(c(a,b))
    res.cor <- cor.test(x=song_means[song_means$Aviary == a,]$mean_pot,y=song_means[song_means$Aviary == b,]$mean_pot,method='pearson')
    #print(res.cor$estimate)
    corrs.aviary <- c(corrs.aviary,res.cor$estimate)
    }
  }
print('Mean across-aviary pearsons R:')
c(mean(corrs.aviary),sd(corrs.aviary) / sqrt(length(corrs.aviary)))

print('Mean within-aviary pearsons R:')
c(mean(corrs.birds),sd(corrs.birds) / sqrt(length(corrs.birds)))


csd_data.raw.binary <- csd_data.raw
csd_data.raw.binary[csd_data.raw.binary$Posture == 2,"Posture"] <- 0
## Variation in response rate: 
resp_rate.birds <- csd_data.raw.binary %>%
  group_by(Bird,Block) %>%
  summarize(blockRate = mean(Posture),presCount = length(Posture))

resp_rate.overall <- csd_data.raw.binary %>%
  group_by(Bird) %>%
  summarize(birdRate = mean(Posture),presCount = length(Posture))


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
print('Number of CSDs:')
length(lat_data$Latency)
print('Mean,Std Latency')
mean(lat_data$Latency)
sd(lat_data$Latency)
resLat.aov <- aov(Latency ~ Bird, data = lat_data)
summary(resLat.aov)

latencyModel.lmer <- lmer(formula = Latency ~ AvgPotency + BlockResponseRate + 
                              (1|Bird) + (1|Song) + (1|Block),data=lat_data)


latencyModel.lm <- lm(formula = Latency ~ AvgPotency + BlockResponseRate,data=lat_data)
latencyModel.lmer2 <- lmer(formula = Latency ~ AvgPotency + BlockResponseRate + 
                             (1|Block) + (1|Song),data=lat_data)
summary(latencyModel.lmer)
confint(latencyModel.lmer)

AIC(latencyModel.lmer,latencyModel.lm,latencyModel.lmer2)

## Tried these out while trying to fix under-dispersion, but it didn't actually help
latencyModel.log <- lmer(formula = LogLatency ~ AvgPotency + BlockResponseRate + 
                           (1|Bird) + (1|Song) + (1|Block),data=lat_data)
latencyModel.sqrt <- lmer(formula = SqrtLatency ~ AvgPotency + BlockResponseRate + 
                            (1|Bird) + (1|Song) + (1|Block),data=lat_data)

### DHARMa shows that the mixed model is under-dispersed, suggesting low-power
simulationOutput <- simulateResiduals(fittedModel = latencyModel.lmer, plot = F)
plot(simulationOutput)
testDispersion(simulationOutput,alternative='less')
testDispersion(simulationOutput)

## While low power isn't a real concern here, the simpler model is evenly dispersed
# And has qualitatively similar results
simulationOutput <- simulateResiduals(fittedModel = latencyModel.lm, plot = F)
plot(simulationOutput)
testDispersion(simulationOutput,alternative='two.sided')
summary(latencyModel.lm)

## Additional residual plots, if you want that sort of thing. 
if (FALSE) {
plot(fitted(latencyModel.lmer),resid(latencyModel.lmer))
qqnorm(resid(latencyModel.lmer))
qqline(resid(latencyModel.lmer))

plot(fitted(latencyModel.lm),resid(latencyModel.lm))
qqnorm(resid(latencyModel.lm))
qqline(resid(latencyModel.lm))
}

## Generate plots for Latency plots

lat_overlay_pot <- ggpredict(latencyModel.lmer,terms="AvgPotency")
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


lat_overlay_rsp <- ggpredict(latencyModel.lmer,terms="BlockResponseRate")
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

print('Number of CSDs for duration')
length(dur_data$Duration)
print('Number of birds')
length(unique(dur_data$Bird))
print('Number of Aviaries')
length(unique(dur_data$Aviary))
mean(dur_data$Duration)
sd(dur_data$Duration)
resDur.aov <- aov(Duration ~ Bird, data = dur_data)
summary(resDur.aov)

## I got these in bash terminal: for i in ls *; do ffprobe -i $i -show_entries format=duration -v quiet -of csv="p=0"; done
song_durations <- c(1.054833,
                    1.064333,
                    1.286625,
                    0.999125,
                    1.281458,
                    1.149958,
                    0.924708,
                    1.101667,
                    1.121792,
                    1.142313)
print('mean,std of duration')
mean(song_durations)
sd(song_durations)

durationModel.lmer <- lmer(formula = Duration ~ AvgPotency + BlockResponseRate + 
                        (1|Bird) + (1|Song) + (1|Block) + (1|Aviary),data=dur_data)

summary(durationModel.lmer)
simulationOutput <- simulateResiduals(fittedModel = durationModel.lmer, plot = F)
plot(simulationOutput)

#dur_overlay_pot <- ggpredict(durationModel,terms="AvgPotency")
#plot(dur_overlay_pot,add.data = TRUE)

#dur_overlay_rsp <- ggpredict(durationModel,terms="BlockResponseRate")
#plot(dur_overlay_rsp,add.data = TRUE)
plot(fitted(durationModel.lmer),resid(durationModel.lmer))

## Based on the residual plot above and the data content, a Gamma distribution 
#   seems much more appropriate


durationGamma <- glmer(formula = Duration ~ AvgPotency + BlockResponseRate + 
                           (1|Bird) + (1|Song) + (1|Block) + (1|Aviary), 
                         family=Gamma(link="log"), data=dur_data)
summary(durationGamma)
plot(fitted(durationGamma),resid(durationGamma)) ## Still get bands, but I believe that's ok for Gamma 

simulationOutput <- simulateResiduals(fittedModel = durationGamma, plot = F)
testDispersion(simulationOutput,alternative='less')
plot(simulationOutput)

## I tried all sorts of things to deal with underdispersion, but 
##    the gamma distribution seems most appropriate overall
if (FALSE) {
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
  
  AIC(durationGamma,duration.model.glm,duration.model.lm,duration.model.log,duration.model.sqrt)
  ### Generate duration plots
  summary(duration.model.sqrt)
}

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


### VELCOITY AND HEIGHT ####
traj_data <- subset(csd_data,select=c("Posture","MaxVelocity","PeakHeight","AvgPotency","BVPotency","Prefix","BlockResponseRate","Bird","Block","Song","Aviary"))
traj_data <- traj_data[complete.cases(traj_data),]
traj_data <- traj_data[traj_data$Posture == 1,]

print('n CSD for trajectory:')
length(traj_data[traj_data$Posture == 1,]$Posture)
print('n birds for trajectory')
length(unique(traj_data$Bird))
plot(traj_data$MaxVelocity,traj_data$BVPotency)

traj_data$LogMaxVel <- log(traj_data$MaxVelocity)
hist(traj_data$LogMaxVel)

## Log transformation normalizes Velocities and improves residual, although Dharma still complains. 
velModel <- lmer(formula = LogMaxVel ~ AvgPotency + BlockResponseRate + (1|Bird),data=traj_data)
summary(velModel)

hist(traj_data$MaxVelocity)
plot(fitted(velModel),resid(velModel)) 
plot(traj_data$BVPotency,traj_data$MaxVelocity)
simulationOutput <- simulateResiduals(fittedModel = velModel, plot = F)
plot(simulationOutput)

## Should block be a random effect here? 
peak_model <- lmer(formula = PeakHeight ~ BVPotency + BlockResponseRate + (1|Block) + (1|Bird),data=traj_data)
#peak_model <- lm(formula = PeakHeight ~ BVPotency,data=traj_data)

summary(peak_model)


### PARTIAL POSTURES ####

## This is the proportion for the entire dataset, (which wasn't necessarily focused on partials)
birdview_data = csd_data[csd_data$Aviary < 2,]
csd_counts <- table(csd_data$Posture)
partial_prop <- csd_counts[3] / sum(csd_counts)
full_prop <- csd_counts[2] / sum(csd_counts)
print('Partial proportion, full proportion:')
print(c(partial_prop,full_prop))

## This is the proportion for just our data, which was very careful with partials
table(birdview_data$Posture)
n_playbacks <- length(birdview_data$Posture)
n_csd <- length(which(birdview_data$Posture == 1))
n_partials <- length(which(birdview_data$Posture == 2))

print(c("proportion of full CSD (my experiments):",n_csd / n_playbacks))
print(c("proportion of partial CSDs (my experiments):",n_partials / n_playbacks))


## Check whether latency is different for partials vs full postures
latPart_data <- subset(birdview_data,select=c("Posture","Latency","AvgPotency","BlockResponseRate","Bird","Block","Song"))
latPart_data <- latPart_data[latPart_data$Posture > 0,]
latPart_data <- latPart_data[complete.cases(latPart_data),]
latPart_data[latPart_data$Posture == 2,"Posture"] = 0
partialLatModel <- lmer(formula = Latency ~ Posture + AvgPotency + BlockResponseRate +
                          (1|Bird) + (1|Song) + (1|Block),data=latPart_data)


summary(partialLatModel)

print('mean,sd for full posture:')
mean(latPart_data[latPart_data$Posture == 1,]$Latency)
sd(latPart_data[latPart_data$Posture == 1,]$Latency)

print('mean,sd for partial posture:')
mean(latPart_data[latPart_data$Posture == 0,]$Latency)
sd(latPart_data[latPart_data$Posture == 0,]$Latency)

plot(fitted(partialLatModel),resid(partialLatModel))
qqnorm(resid(partialLatModel))
qqline(resid(partialLatModel))

partLat_overlay_pot <- ggpredict(partialLatModel,terms="Posture")
plot(partLat_overlay_pot,add.data = TRUE)

## Check if theres a potency effect for latency within partials
latPart_data.partials <- latPart_data[latPart_data$Posture == 0,]
partialModel.posture <- lmer(formula = Latency ~ AvgPotency + BlockResponseRate + (1|Bird),data=latPart_data.partials)
summary(partialModel.posture)

## Check potency of songs eliciting partial CSD: 

print('Potency of songs eliciting non,full, and partial CSD')
mean(birdview_data[birdview_data$Posture == 0,]$AvgPotency)
mean(birdview_data[birdview_data$Posture == 1,]$AvgPotency)
mean(birdview_data[birdview_data$Posture == 2,]$AvgPotency)

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


#DIC(csd.model.pot, csd.model.block, csd.model.simple,csd.model.interaction,csd.model.simple.resp,csd.model.interaction.resp,
#    csd.model.aviary,csd.model.block.slope,csd.model.pot.slope,csd.model.both.slope,csd.model.pot.slope.interaction,csd.model.pot.Aviary)

AIC(csd.model.lm,csd.model.lm2,csd.model.lm3,csd.model.pot, csd.model.block, csd.model.simple,csd.model.interaction,csd.model.simple.resp,csd.model.interaction.resp,
    csd.model.aviary,csd.model.block.slope,csd.model.pot.slope,csd.model.both.slope,csd.model.pot.slope.interaction,csd.model.pot.Aviary)

aviaries <- unique(csd_binary.clean$Aviary)
l <- 1
effs.intercept <- list()
effs.song <- list()
effs.block <- list()

### This was sort of a hairbrained idea to try to get the effect of song vs block
## without dealing with the circular logic of "potency", by having a model with just 
## two songs at a time (to get around the problem of having 10 categorical variables)
## I think it has promise, but is ultimately unnecessary. 

if (FALSE) {
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
      #print('doing something...')
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
  
  print('')
  print(m.intercept);print(se.intercept)
  print(m.song);print(se.song)
  print(m.block);print(se.block)
}

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

modx_vals= c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9)
#modx_vals=c(0.1,0.25,0.5,0.75,0.9)

p.csd.data <- interact_plot(scoreModel,
                            modxvals=modx_vals,
                            pred=Block,modx=AvgPotency,colors=gray(1 - modx_vals)) + 
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

## This assumes you have run the python simulation to get the tmp potency files

sim.pot <- read.csv("./csdModel/tmp_pot.csv")
sim.pot[,"Posture"] <- sim.pot$Response

pot.model <-glmer(formula = Posture ~ AvgPotency * Block +
                     (1|Bird), data = sim.pot, 
                   family=binomial)
summary(pot.model)


sim.pot$Fits <- fitted(pot.model)

p.csd.pot <- interact_plot(pot.model,
                            modxvals=modx_vals,
                            pred=Block,modx=AvgPotency,colors=gray(1 - modx_vals)) + 
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
                           modxvals=modx_vals,
                           pred=Block,modx=AvgPotency,colors=gray(1 - modx_vals)) + 
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
interact_plot(B1scoreModel,modxvals=modx_vals,pred=Block,modx=B1Potency)

## An overall good model, to see what explains Posture generally
goodModel <-glmer(formula = Posture ~ AvgPotency * BlockResponseRate +
                    (1|Bird) + (1|Aviary) + (1|SongSet), data = csd_binary, 
                  family=binomial)
summary(goodModel)
interact_plot(goodModel,modxvals=modx_vals,pred=BlockResponseRate,modx=AvgPotency)

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

mean(whistleLat_data$Latency,na.rm=TRUE)
plot(fitted(whistleDurModel),resid(whistleDurModel))
qqnorm(resid(whistleDurModel))
qqline(resid(whistleDurModel))


### Supplemental Figures! 

## Supplemental Figure One: 
csd_binary.blockMeans <- aggregate(csd_binary.clean$Posture, by = list(csd_binary.clean$Bird,csd_binary.clean$Block), FUN = mean)


df_summary <- csd_binary.clean %>%
  group_by(Aviary, Bird,Block) %>%
  summarize(BlockResponsivity = mean(Posture, na.rm = TRUE)) 
csd_binary.ranks <- left_join(csd_binary.clean,df_summary,by=c('Aviary','Bird','Block'))

df_summary <- csd_binary.ranks %>%
  group_by(Aviary) %>%
  summarize(GroupResponsivity = mean(BlockResponsivity,na.rm=TRUE)) %>%
  mutate(GroupsRank = rank(desc(GroupResponsivity),ties.method='first'))

csd_binary.ranks <- left_join(csd_binary.ranks,df_summary,by=c('Aviary'))

csd_binary.ranks
df_summary <- csd_binary.ranks %>%
  group_by(Aviary,Bird) %>%
  summarize(BirdResponsivity = mean(Posture,na.rm=TRUE)) %>%
  mutate(RankInGroup = rank(desc(BirdResponsivity),ties.method="first"))

csd_binary.ranks <- left_join(csd_binary.ranks,df_summary,by=c("Aviary","Bird"))
  
csd_binary.ranks

csd_binary.ranks$Order <- (csd_binary.ranks$GroupsRank -1) * 15 + csd_binary.ranks$RankInGroup

my_plot <- csd_binary.ranks %>%
  group_by(Bird,Block) %>%
  ggplot(aes(x=Order,y=BlockResponsivity,group=Order)) + 
  geom_boxplot() + 
  labs(x = "Birds (grouped by and sorted within experiment)", y="Responsivity") + 
  ggtitle("Variation in 2Responsivity") + 
  theme_classic() + 
  theme(legend.position = "none",
        rect=element_rect(fill="transparent"),
        panel.background=element_rect(fill="transparent"),
        axis.line = element_line(size = 0.5, colour = "black"),
        axis.ticks = element_line(colour = "black"),
        axis.text = element_text(size = 12, colour = "black"),
        axis.title = element_text(size = 14, face = "bold", colour = "black"),
        plot.title = element_text(hjust = 0.5, size = 0)) 
my_plot
csd_binary.birdResponse <- csd_binary.ranks %>%
  group_by(Bird,Block) %>%
  summarise(Responsivity = mean(BlockResponsivity))
csd_binary.birdResponse
Resp.aov <- aov(BlockResponsivity ~ Bird, data = csd_binary.ranks)
summary(Resp.aov)

csd_binary[csd_binary$SongSet == 0,] %>%
  group_by(Block) %>%
  summarise(Responsivity = mean(BlockResponseRate),sdRespon = sd(BlockResponseRate))

