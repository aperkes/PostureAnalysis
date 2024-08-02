

``` r
#library("lme4")
library("lmerTest") # gives p-values for those that want them
library("ggeffects")
library("interactions")
library('dplyr')
library('ggplot2')
library('DHARMa')
library('MuMIn')

setwd("~/Documents/Scripts/PostureAnalysis")
#setwd("~/Downloads")
#csd_data <- read.csv("./full_df2.csv")
csd_data.raw <- read.csv("./full_df3.csv")

bird_list.all <- aggregate(csd_data.raw$Posture,by=list(Category=csd_data.raw$Bird),FUN=sum)
print(dim(bird_list.all))
```

```
## [1] 68  2
```

``` r
filtered_csd <- csd_data.raw %>%
  group_by(Bird) %>%
  filter(sum(Posture) > 5)
bird_list.some <- aggregate(filtered_csd$Posture,by=list(Category=filtered_csd$Bird),FUN=sum)
print(dim(bird_list.some))
```

```
## [1] 53  2
```

``` r
csd_data <- filtered_csd

## Check whether songs differ from random: 
csd_data.set0 <- csd_data[csd_data$SongSet == 0,]
csd_data.set0 <- csd_data.set0[csd_data.set0$Aviary != 0,]

song_counts <- aggregate(csd_data.set0$Posture,by=list(Category=csd_data.set0$SongID),FUN=sum)
res.chi <- chisq.test(song_counts$x)
print(res.chi)
```

```
## 
## 	Chi-squared test for given probabilities
## 
## data:  song_counts$x
## X-squared = 58.223, df = 9, p-value = 2.945e-09
```

``` r
## Get some summary stats on song preference: 
song_means <- csd_data.set0 %>%
  group_by(SongID,Aviary) %>%
  summarize(mean_pot = mean(Posture),std_pot = sd(Posture),pres_count = length(Posture))
```

```
## `summarise()` has grouped output by 'SongID'. You can override using the `.groups`
## argument.
```

``` r
#print(song_means)
#csd_data.set0[csd_data.set0$Aviary == 1,]

## Consistency of preference: 
song_means.birds <- csd_data.set0 %>%
  group_by(SongID,Bird,Aviary) %>%
  summarize(mean_pot = mean(Posture),std_pot = sd(Posture),pres_count = length(Posture))
```

```
## `summarise()` has grouped output by 'SongID', 'Bird'. You can override using the
## `.groups` argument.
```

``` r
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
```

```
## [1] "Mean across-aviary pearsons R:"
```

``` r
c(mean(corrs.aviary),sd(corrs.aviary) / sqrt(length(corrs.aviary)))
```

```
## [1] 0.41455235 0.06363806
```

``` r
print('Mean within-aviary pearsons R:')
```

```
## [1] "Mean within-aviary pearsons R:"
```

``` r
c(mean(corrs.birds),sd(corrs.birds) / sqrt(length(corrs.birds)))
```

```
## [1] 0.19302517 0.01977689
```

``` r
csd_data.raw.binary <- csd_data.raw
csd_data.raw.binary[csd_data.raw.binary$Posture == 2,"Posture"] <- 0
## Variation in response rate: 
resp_rate.birds <- csd_data.raw.binary %>%
  group_by(Bird,Block) %>%
  summarize(blockRate = mean(Posture),presCount = length(Posture))
```

```
## `summarise()` has grouped output by 'Bird'. You can override using the `.groups`
## argument.
```

``` r
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
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-1.png)

``` r
print('Number of CSDs:')
```

```
## [1] "Number of CSDs:"
```

``` r
length(lat_data$Latency)
```

```
## [1] 268
```

``` r
print('Mean,Std Latency')
```

```
## [1] "Mean,Std Latency"
```

``` r
mean(lat_data$Latency)
```

```
## [1] 0.6281604
```

``` r
sd(lat_data$Latency)
```

```
## [1] 0.2486477
```

``` r
resLat.aov <- aov(Latency ~ Bird, data = lat_data)
summary(resLat.aov)
```

```
##              Df Sum Sq Mean Sq F value Pr(>F)    
## Bird          1  4.115   4.115   88.34 <2e-16 ***
## Residuals   266 12.392   0.047                   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

``` r
latencyModel.lmer <- lmer(formula = Latency ~ AvgPotency + BlockResponseRate + 
                              (1|Bird) + (1|Song) + (1|Block),data=lat_data)


latencyModel.lm <- lm(formula = Latency ~ AvgPotency + BlockResponseRate,data=lat_data)
latencyModel.lmer2 <- lmer(formula = Latency ~ AvgPotency + BlockResponseRate + 
                             (1|Block) + (1|Song),data=lat_data)
summary(latencyModel.lmer)
```

```
## Linear mixed model fit by REML. t-tests use Satterthwaite's
##   method [lmerModLmerTest]
## Formula: 
## Latency ~ AvgPotency + BlockResponseRate + (1 | Bird) + (1 |  
##     Song) + (1 | Block)
##    Data: lat_data
## 
## REML criterion at convergence: -181.5
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.3978 -0.6039 -0.1083  0.5214  3.2721 
## 
## Random effects:
##  Groups   Name        Variance  Std.Dev.
##  Block    (Intercept) 0.0008939 0.02990 
##  Song     (Intercept) 0.0011894 0.03449 
##  Bird     (Intercept) 0.1004904 0.31700 
##  Residual             0.0240207 0.15499 
## Number of obs: 268, groups:  Block, 20; Song, 12; Bird, 8
## 
## Fixed effects:
##                    Estimate Std. Error        df t value Pr(>|t|)
## (Intercept)         1.02160    0.12470   9.11335   8.193  1.7e-05
## AvgPotency         -0.22994    0.07057  20.07507  -3.258  0.00392
## BlockResponseRate  -0.11254    0.05031 185.88898  -2.237  0.02649
##                      
## (Intercept)       ***
## AvgPotency        ** 
## BlockResponseRate *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) AvgPtn
## AvgPotency  -0.328       
## BlckRspnsRt -0.249  0.067
```

``` r
confint(latencyModel.lmer)
```

```
## Computing profile confidence intervals ...
```

```
##                        2.5 %      97.5 %
## .sig01             0.0000000  0.06495950
## .sig02             0.0000000  0.06990481
## .sig03             0.1810165  0.54346467
## .sigma             0.1415142  0.17013961
## (Intercept)        0.7695045  1.27583935
## AvgPotency        -0.3729582 -0.09089307
## BlockResponseRate -0.2129882 -0.01422764
```

``` r
AIC(latencyModel.lmer,latencyModel.lm,latencyModel.lmer2)
```

```
##                    df        AIC
## latencyModel.lmer   7 -167.45329
## latencyModel.lm     4  -58.78402
## latencyModel.lmer2  6  -46.09291
```

``` r
## Tried these out while trying to fix under-dispersion, but it didn't actually help
latencyModel.log <- lmer(formula = LogLatency ~ AvgPotency + BlockResponseRate + 
                           (1|Bird) + (1|Song) + (1|Block),data=lat_data)
latencyModel.sqrt <- lmer(formula = SqrtLatency ~ AvgPotency + BlockResponseRate + 
                            (1|Bird) + (1|Song) + (1|Block),data=lat_data)

### DHARMa shows that the mixed model is under-dispersed, suggesting low-power
simulationOutput <- simulateResiduals(fittedModel = latencyModel.lmer, plot = F)
plot(simulationOutput)
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-2.png)

``` r
testDispersion(simulationOutput,alternative='less')
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-3.png)

```
## 
## 	DHARMa nonparametric dispersion test via sd of residuals
## 	fitted vs. simulated
## 
## data:  simulationOutput
## dispersion = 0.50603, p-value = 0.116
## alternative hypothesis: less
```

``` r
testDispersion(simulationOutput)
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-4.png)

```
## 
## 	DHARMa nonparametric dispersion test via sd of residuals
## 	fitted vs. simulated
## 
## data:  simulationOutput
## dispersion = 0.50603, p-value = 0.232
## alternative hypothesis: two.sided
```

``` r
## While low power isn't a real concern here, the simpler model is evenly dispersed
# And has qualitatively similar results
simulationOutput <- simulateResiduals(fittedModel = latencyModel.lm, plot = F)
plot(simulationOutput)
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-5.png)

``` r
testDispersion(simulationOutput,alternative='two.sided')
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-6.png)

```
## 
## 	DHARMa nonparametric dispersion test via sd of residuals
## 	fitted vs. simulated
## 
## data:  simulationOutput
## dispersion = 0.98916, p-value = 0.984
## alternative hypothesis: two.sided
```

``` r
summary(latencyModel.lm)
```

```
## 
## Call:
## lm(formula = Latency ~ AvgPotency + BlockResponseRate, data = lat_data)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.76444 -0.14136 -0.00999  0.13544  0.81766 
## 
## Coefficients:
##                   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)        0.97986    0.04123  23.763  < 2e-16 ***
## AvgPotency        -0.16010    0.05550  -2.885  0.00424 ** 
## BlockResponseRate -0.38150    0.04616  -8.265 6.76e-15 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.2148 on 265 degrees of freedom
## Multiple R-squared:  0.2591,	Adjusted R-squared:  0.2535 
## F-statistic: 46.34 on 2 and 265 DF,  p-value: < 2.2e-16
```

``` r
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
```

```
## Warning: Argument `add.data` is deprecated and will be removed in
##   the future. Please use `show_data` instead.
```

```
## Data points may overlap. Use the `jitter` argument to add
##   some amount of random variation to the location of data
##   points and avoid overplotting.
```

``` r
p.lat.pot
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-7.png)

``` r
ggsave(file='./figures/f3b.png',plot=p.lat.pot,dpi=300,bg = "transparent")
```

```
## Saving 7 x 7 in image
```

``` r
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
```

```
## Warning: Argument `add.data` is deprecated and will be removed in the future. Please
##   use `show_data` instead.
```

```
## Data points may overlap. Use the `jitter` argument to add some amount of
##   random variation to the location of data points and avoid overplotting.
```

``` r
p.lat.resp
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-8.png)

``` r
ggsave(file='./figures/f3c.png',plot=p.lat.resp,dpi=300,bg = "transparent")
```

```
## Saving 7 x 7 in image
```

``` r
ggpredict(latencyModel,terms="AvgPotency")
```

```
## Error in eval(expr, envir, enclos): object 'latencyModel' not found
```

``` r
### DURATION ####
dur_data <- subset(csd_data,select=c("Posture","Duration","AvgPotency","BlockResponseRate","Bird","Block","Song","Aviary"))
dur_data <- dur_data[complete.cases(dur_data),]
dur_data <- dur_data[dur_data$Posture == 1,]
dur_data <- dur_data[dur_data$Duration> 0,]

print('Number of CSDs for duration')
```

```
## [1] "Number of CSDs for duration"
```

``` r
length(dur_data$Duration)
```

```
## [1] 922
```

``` r
print('Number of birds')
```

```
## [1] "Number of birds"
```

``` r
length(unique(dur_data$Bird))
```

```
## [1] 45
```

``` r
print('Number of Aviaries')
```

```
## [1] "Number of Aviaries"
```

``` r
length(unique(dur_data$Aviary))
```

```
## [1] 6
```

``` r
mean(dur_data$Duration)
```

```
## [1] 4.220174
```

``` r
sd(dur_data$Duration)
```

```
## [1] 2.582053
```

``` r
resDur.aov <- aov(Duration ~ Bird, data = dur_data)
summary(resDur.aov)
```

```
##              Df Sum Sq Mean Sq F value Pr(>F)
## Bird          1     11  11.232   1.686  0.194
## Residuals   920   6129   6.662
```

``` r
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
```

```
## [1] "mean,std of duration"
```

``` r
mean(song_durations)
```

```
## [1] 1.112681
```

``` r
sd(song_durations)
```

```
## [1] 0.1131012
```

``` r
durationModel.lmer <- lmer(formula = Duration ~ AvgPotency + BlockResponseRate + 
                        (1|Bird) + (1|Song) + (1|Block) + (1|Aviary),data=dur_data)

summary(durationModel.lmer)
```

```
## Linear mixed model fit by REML. t-tests use Satterthwaite's method [lmerModLmerTest
## ]
## Formula: Duration ~ AvgPotency + BlockResponseRate + (1 | Bird) + (1 |  
##     Song) + (1 | Block) + (1 | Aviary)
##    Data: dur_data
## 
## REML criterion at convergence: 4201.8
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -2.3505 -0.7266 -0.1439  0.6100  3.9783 
## 
## Random effects:
##  Groups   Name        Variance Std.Dev.
##  Bird     (Intercept) 1.2330   1.1104  
##  Block    (Intercept) 0.1276   0.3572  
##  Song     (Intercept) 0.3098   0.5566  
##  Aviary   (Intercept) 0.5075   0.7124  
##  Residual             5.0054   2.2373  
## Number of obs: 922, groups:  Bird, 45; Block, 20; Song, 16; Aviary, 6
## 
## Fixed effects:
##                   Estimate Std. Error       df t value Pr(>|t|)    
## (Intercept)         2.0653     0.4846   7.4793   4.262  0.00321 ** 
## AvgPotency          4.3948     0.6382 177.7325   6.886 9.51e-11 ***
## BlockResponseRate   0.7849     0.3793 332.0493   2.069  0.03931 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) AvgPtn
## AvgPotency  -0.448       
## BlckRspnsRt -0.321  0.034
```

``` r
simulationOutput <- simulateResiduals(fittedModel = durationModel.lmer, plot = F)
plot(simulationOutput)
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-9.png)

``` r
#dur_overlay_pot <- ggpredict(durationModel,terms="AvgPotency")
#plot(dur_overlay_pot,add.data = TRUE)

#dur_overlay_rsp <- ggpredict(durationModel,terms="BlockResponseRate")
#plot(dur_overlay_rsp,add.data = TRUE)
plot(fitted(durationModel.lmer),resid(durationModel.lmer))
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-10.png)

``` r
## Based on the residual plot above and the data content, a Gamma distribution 
#   seems much more appropriate


durationGamma <- glmer(formula = Duration ~ AvgPotency + BlockResponseRate + 
                           (1|Bird) + (1|Song) + (1|Block) + (1|Aviary), 
                         family=Gamma(link="log"), data=dur_data)
summary(durationGamma)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) [
## glmerMod]
##  Family: Gamma  ( log )
## Formula: Duration ~ AvgPotency + BlockResponseRate + (1 | Bird) + (1 |  
##     Song) + (1 | Block) + (1 | Aviary)
##    Data: dur_data
## 
##      AIC      BIC   logLik deviance df.resid 
##   3983.3   4021.9  -1983.7   3967.3      914 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -1.5228 -0.7589 -0.1114  0.6397  3.6907 
## 
## Random effects:
##  Groups   Name        Variance Std.Dev.
##  Bird     (Intercept) 0.052852 0.22990 
##  Block    (Intercept) 0.004879 0.06985 
##  Song     (Intercept) 0.013424 0.11586 
##  Aviary   (Intercept) 0.011760 0.10844 
##  Residual             0.300027 0.54775 
## Number of obs: 922, groups:  Bird, 45; Block, 20; Song, 16; Aviary, 6
## 
## Fixed effects:
##                   Estimate Std. Error t value Pr(>|z|)    
## (Intercept)         0.7730     0.1411   5.478 4.31e-08 ***
## AvgPotency          1.1668     0.1751   6.662 2.71e-11 ***
## BlockResponseRate   0.2261     0.1004   2.252   0.0243 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) AvgPtn
## AvgPotency  -0.436       
## BlckRspnsRt -0.233  0.071
```

``` r
plot(fitted(durationGamma),resid(durationGamma)) ## Still get bands, but I believe that's ok for Gamma 
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-11.png)

``` r
simulationOutput <- simulateResiduals(fittedModel = durationGamma, plot = F)
testDispersion(simulationOutput,alternative='less')
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-12.png)

```
## 
## 	DHARMa nonparametric dispersion test via sd of residuals fitted vs.
## 	simulated
## 
## data:  simulationOutput
## dispersion = 0.12568, p-value < 2.2e-16
## alternative hypothesis: less
```

``` r
plot(simulationOutput)
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-13.png)

``` r
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
```

```
## Warning: Argument `add.data` is deprecated and will be removed in the future. Please
##   use `show_data` instead.
```

```
## Data points may overlap. Use the `jitter` argument to add some amount of
##   random variation to the location of data points and avoid overplotting.
```

``` r
p.dur.pot
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-14.png)

``` r
ggsave(file='./figures/f4b.png',plot=p.dur.pot,dpi=300,bg = "transparent")
```

```
## Saving 7 x 7 in image
```

``` r
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
```

```
## Warning: Argument `add.data` is deprecated and will be removed in the future. Please
##   use `show_data` instead.
```

```
## Data points may overlap. Use the `jitter` argument to add some amount of
##   random variation to the location of data points and avoid overplotting.
```

``` r
p.dur.resp
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-15.png)

``` r
ggsave(file='./figures/f4c.png',plot=p.dur.resp,dpi=300,bg = "transparent")
```

```
## Saving 7 x 7 in image
```

``` r
### VELCOITY AND HEIGHT ####
traj_data <- subset(csd_data,select=c("Posture","MaxVelocity","PeakHeight","AvgPotency","BVPotency","Prefix","BlockResponseRate","Bird","Block","Song","Aviary"))
traj_data <- traj_data[complete.cases(traj_data),]
traj_data <- traj_data[traj_data$Posture == 1,]

print('n CSD for trajectory:')
```

```
## [1] "n CSD for trajectory:"
```

``` r
length(traj_data[traj_data$Posture == 1,]$Posture)
```

```
## [1] 208
```

``` r
print('n birds for trajectory')
```

```
## [1] "n birds for trajectory"
```

``` r
length(unique(traj_data$Bird))
```

```
## [1] 6
```

``` r
plot(traj_data$MaxVelocity,traj_data$BVPotency)
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-16.png)

``` r
traj_data$LogMaxVel <- log(traj_data$MaxVelocity)
hist(traj_data$LogMaxVel)
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-17.png)

``` r
## Log transformation normalizes Velocities and improves residual, although Dharma still complains. 
velModel <- lmer(formula = LogMaxVel ~ AvgPotency + BlockResponseRate + (1|Bird),data=traj_data)
summary(velModel)
```

```
## Linear mixed model fit by REML. t-tests use Satterthwaite's method [lmerModLmerTest
## ]
## Formula: LogMaxVel ~ AvgPotency + BlockResponseRate + (1 | Bird)
##    Data: traj_data
## 
## REML criterion at convergence: 476.8
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -9.0024 -0.2833  0.1093  0.4654  2.6093 
## 
## Random effects:
##  Groups   Name        Variance Std.Dev.
##  Bird     (Intercept) 0.1878   0.4334  
##  Residual             0.5380   0.7335  
## Number of obs: 208, groups:  Bird, 6
## 
## Fixed effects:
##                   Estimate Std. Error       df t value Pr(>|t|)    
## (Intercept)        -5.9872     0.2896  21.8711 -20.672 7.65e-16 ***
## AvgPotency          0.1312     0.2621 187.6587   0.500    0.617    
## BlockResponseRate   0.3161     0.2405 184.6821   1.314    0.190    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) AvgPtn
## AvgPotency  -0.577       
## BlckRspnsRt -0.574  0.120
```

``` r
hist(traj_data$MaxVelocity)
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-18.png)

``` r
plot(fitted(velModel),resid(velModel)) 
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-19.png)

``` r
plot(traj_data$BVPotency,traj_data$MaxVelocity)
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-20.png)

``` r
simulationOutput <- simulateResiduals(fittedModel = velModel, plot = F)
plot(simulationOutput)
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-21.png)

``` r
## Should block be a random effect here? 
peak_model <- lmer(formula = PeakHeight ~ BVPotency + BlockResponseRate + (1|Block) + (1|Bird),data=traj_data)
#peak_model <- lm(formula = PeakHeight ~ BVPotency,data=traj_data)

summary(peak_model)
```

```
## Linear mixed model fit by REML. t-tests use Satterthwaite's method [lmerModLmerTest
## ]
## Formula: PeakHeight ~ BVPotency + BlockResponseRate + (1 | Block) + (1 |  
##     Bird)
##    Data: traj_data
## 
## REML criterion at convergence: -868.8
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -6.8788 -0.2549  0.1204  0.5394  3.5649 
## 
## Random effects:
##  Groups   Name        Variance  Std.Dev.
##  Block    (Intercept) 8.755e-05 0.009357
##  Bird     (Intercept) 7.371e-04 0.027150
##  Residual             6.947e-04 0.026357
## Number of obs: 208, groups:  Block, 20; Bird, 6
## 
## Fixed effects:
##                    Estimate Std. Error        df t value Pr(>|t|)    
## (Intercept)         0.30447    0.01653  21.03378  18.415 1.88e-14 ***
## BVPotency           0.01033    0.01361 192.75233   0.759    0.449    
## BlockResponseRate   0.01524    0.01003 164.16993   1.520    0.131    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) BVPtnc
## BVPotency   -0.618       
## BlckRspnsRt -0.437  0.122
```

``` r
### PARTIAL POSTURES ####

## This is the proportion for the entire dataset, (which wasn't necessarily focused on partials)
birdview_data = csd_data[csd_data$Aviary < 2,]
csd_counts <- table(csd_data$Posture)
partial_prop <- csd_counts[3] / sum(csd_counts)
full_prop <- csd_counts[2] / sum(csd_counts)
print('Partial proportion, full proportion:')
```

```
## [1] "Partial proportion, full proportion:"
```

``` r
print(c(partial_prop,full_prop))
```

```
##          2          1 
## 0.05700237 0.19650337
```

``` r
## This is the proportion for just our data, which was very careful with partials
table(birdview_data$Posture)
```

```
## 
##   0   1   2 
## 267 307  57
```

``` r
n_playbacks <- length(birdview_data$Posture)
n_csd <- length(which(birdview_data$Posture == 1))
n_partials <- length(which(birdview_data$Posture == 2))

print(c("proportion of full CSD (my experiments):",n_csd / n_playbacks))
```

```
## [1] "proportion of full CSD (my experiments):"
## [2] "0.486529318541997"
```

``` r
print(c("proportion of partial CSDs (my experiments):",n_partials / n_playbacks))
```

```
## [1] "proportion of partial CSDs (my experiments):"
## [2] "0.0903328050713154"
```

``` r
## Check whether latency is different for partials vs full postures
latPart_data <- subset(birdview_data,select=c("Posture","Latency","AvgPotency","BlockResponseRate","Bird","Block","Song"))
latPart_data <- latPart_data[latPart_data$Posture > 0,]
latPart_data <- latPart_data[complete.cases(latPart_data),]
latPart_data[latPart_data$Posture == 2,"Posture"] = 0
partialLatModel <- lmer(formula = Latency ~ Posture + AvgPotency + BlockResponseRate +
                          (1|Bird) + (1|Song) + (1|Block),data=latPart_data)


summary(partialLatModel)
```

```
## Linear mixed model fit by REML. t-tests use Satterthwaite's method [lmerModLmerTest
## ]
## Formula: Latency ~ Posture + AvgPotency + BlockResponseRate + (1 | Bird) +  
##     (1 | Song) + (1 | Block)
##    Data: latPart_data
## 
## REML criterion at convergence: -147
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.1669 -0.6089 -0.0822  0.5239  5.5798 
## 
## Random effects:
##  Groups   Name        Variance  Std.Dev.
##  Block    (Intercept) 0.0003755 0.01938 
##  Song     (Intercept) 0.0015093 0.03885 
##  Bird     (Intercept) 0.0804652 0.28366 
##  Residual             0.0300727 0.17341 
## Number of obs: 305, groups:  Block, 20; Song, 12; Bird, 8
## 
## Fixed effects:
##                    Estimate Std. Error        df t value Pr(>|t|)    
## (Intercept)         1.21243    0.11742  11.06891  10.326 5.08e-07 ***
## Posture            -0.25200    0.03271 290.56882  -7.704 2.09e-13 ***
## AvgPotency         -0.19459    0.07655  24.60338  -2.542   0.0177 *  
## BlockResponseRate  -0.06951    0.05093 160.35727  -1.365   0.1743    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) Postur AvgPtn
## Posture     -0.184              
## AvgPotency  -0.357 -0.093       
## BlckRspnsRt -0.257 -0.070  0.072
```

``` r
print('mean,sd for full posture:')
```

```
## [1] "mean,sd for full posture:"
```

``` r
mean(latPart_data[latPart_data$Posture == 1,]$Latency)
```

```
## [1] 0.6281604
```

``` r
sd(latPart_data[latPart_data$Posture == 1,]$Latency)
```

```
## [1] 0.2486477
```

``` r
print('mean,sd for partial posture:')
```

```
## [1] "mean,sd for partial posture:"
```

``` r
mean(latPart_data[latPart_data$Posture == 0,]$Latency)
```

```
## [1] 1.031541
```

``` r
sd(latPart_data[latPart_data$Posture == 0,]$Latency)
```

```
## [1] 0.293136
```

``` r
plot(fitted(partialLatModel),resid(partialLatModel))
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-22.png)

``` r
qqnorm(resid(partialLatModel))
qqline(resid(partialLatModel))
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-23.png)

``` r
partLat_overlay_pot <- ggpredict(partialLatModel,terms="Posture")
plot(partLat_overlay_pot,add.data = TRUE)
```

```
## Warning: Argument `add.data` is deprecated and will be removed in the future. Please
##   use `show_data` instead.
```

```
## Data points may overlap. Use the `jitter` argument to add some amount of
##   random variation to the location of data points and avoid overplotting.
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-24.png)

``` r
## Check if theres a potency effect for latency within partials
latPart_data.partials <- latPart_data[latPart_data$Posture == 0,]
partialModel.posture <- lmer(formula = Latency ~ AvgPotency + BlockResponseRate + (1|Bird),data=latPart_data.partials)
summary(partialModel.posture)
```

```
## Linear mixed model fit by REML. t-tests use Satterthwaite's method [lmerModLmerTest
## ]
## Formula: Latency ~ AvgPotency + BlockResponseRate + (1 | Bird)
##    Data: latPart_data.partials
## 
## REML criterion at convergence: 15.7
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -1.6222 -0.4431 -0.0816  0.3057  3.2621 
## 
## Random effects:
##  Groups   Name        Variance Std.Dev.
##  Bird     (Intercept) 0.05041  0.2245  
##  Residual             0.06858  0.2619  
## Number of obs: 37, groups:  Bird, 6
## 
## Fixed effects:
##                   Estimate Std. Error       df t value Pr(>|t|)   
## (Intercept)        0.92784    0.25719 18.67469   3.608  0.00192 **
## AvgPotency         0.09493    0.43585 33.24793   0.218  0.82891   
## BlockResponseRate  0.09577    0.20584 25.12813   0.465  0.64575   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) AvgPtn
## AvgPotency  -0.842       
## BlckRspnsRt -0.668  0.436
```

``` r
## Check potency of songs eliciting partial CSD: 

print('Potency of songs eliciting non,full, and partial CSD')
```

```
## [1] "Potency of songs eliciting non,full, and partial CSD"
```

``` r
mean(birdview_data[birdview_data$Posture == 0,]$AvgPotency)
```

```
## [1] 0.4062491
```

``` r
mean(birdview_data[birdview_data$Posture == 1,]$AvgPotency)
```

```
## [1] 0.5782163
```

``` r
mean(birdview_data[birdview_data$Posture == 2,]$AvgPotency)
```

```
## [1] 0.4593386
```

``` r
## Check whether songs that get postures shift with threshold
partial_data <- csd_data[csd_data$Posture == 2,]
partial_data <- subset(partial_data,select=c("Posture","AvgPotency","BlockResponseRate","Bird","Block","Song","Aviary"))

partial_data <- partial_data[complete.cases(partial_data),]


partialModel <- lmer(formula = AvgPotency ~ BlockResponseRate +
                       (1|Bird) + (1|Song) + (1|Block) + (1|Aviary),data=partial_data)
summary(partialModel)
```

```
## Linear mixed model fit by REML. t-tests use Satterthwaite's method [lmerModLmerTest
## ]
## Formula: AvgPotency ~ BlockResponseRate + (1 | Bird) + (1 | Song) + (1 |  
##     Block) + (1 | Aviary)
##    Data: partial_data
## 
## REML criterion at convergence: -490.9
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.1886 -0.5767 -0.0934  0.4898  3.0516 
## 
## Random effects:
##  Groups   Name        Variance  Std.Dev.
##  Bird     (Intercept) 2.601e-03 0.051002
##  Song     (Intercept) 3.964e-03 0.062962
##  Block    (Intercept) 3.641e-05 0.006034
##  Aviary   (Intercept) 5.327e-02 0.230810
##  Residual             8.294e-03 0.091073
## Number of obs: 312, groups:  Bird, 53; Song, 16; Block, 14; Aviary, 7
## 
## Fixed effects:
##                    Estimate Std. Error        df t value Pr(>|t|)    
## (Intercept)         0.34006    0.08991   6.32283   3.782  0.00830 ** 
## BlockResponseRate  -0.09437    0.02591 166.48004  -3.643  0.00036 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr)
## BlckRspnsRt -0.100
```

``` r
plot(fitted(partialModel),resid(partialModel))
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-25.png)

``` r
qqnorm(resid(partialModel))
qqline(resid(partialModel))
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-26.png)

``` r
par_overlay_rsp <- ggpredict(partialModel,terms="BlockResponseRate")
plot(par_overlay_rsp,add.data = TRUE)
```

```
## Warning: Argument `add.data` is deprecated and will be removed in the future. Please
##   use `show_data` instead.
```

```
## Data points may overlap. Use the `jitter` argument to add some amount of
##   random variation to the location of data points and avoid overplotting.
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-27.png)

``` r
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
```

```
## boundary (singular) fit: see help('isSingular')
```

``` r
#DIC(csd.model.pot, csd.model.block, csd.model.simple,csd.model.interaction,csd.model.simple.resp,csd.model.interaction.resp,
#    csd.model.aviary,csd.model.block.slope,csd.model.pot.slope,csd.model.both.slope,csd.model.pot.slope.interaction,csd.model.pot.Aviary)

AIC(csd.model.lm,csd.model.lm2,csd.model.lm3,csd.model.pot, csd.model.block, csd.model.simple,csd.model.interaction,csd.model.simple.resp,csd.model.interaction.resp,
    csd.model.aviary,csd.model.block.slope,csd.model.pot.slope,csd.model.both.slope,csd.model.pot.slope.interaction,csd.model.pot.Aviary)
```

```
##                                 df      AIC
## csd.model.lm                     3 4542.296
## csd.model.lm2                    4 2729.058
## csd.model.lm3                    5 2729.981
## csd.model.pot                    3 3926.600
## csd.model.block                  3 3718.874
## csd.model.simple                 4 3521.024
## csd.model.interaction            5 3521.048
## csd.model.simple.resp            6 3331.286
## csd.model.interaction.resp       5 3325.118
## csd.model.aviary                 5 3325.134
## csd.model.block.slope            6 3331.286
## csd.model.pot.slope              6 3320.522
## csd.model.both.slope            10 3319.933
## csd.model.pot.slope.interaction  7 3318.336
## csd.model.pot.Aviary             7 3319.777
```

``` r
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
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) [
## glmerMod]
##  Family: binomial  ( logit )
## Formula: Posture ~ SongID + Block + (1 | Bird)
##    Data: csd_binary.subset
## 
##      AIC      BIC   logLik deviance df.resid 
##    678.6    696.5   -335.3    670.6      643 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -2.5574 -0.5346 -0.2681  0.5743  3.7826 
## 
## Random effects:
##  Groups Name        Variance Std.Dev.
##  Bird   (Intercept) 2.568    1.602   
## Number of obs: 647, groups:  Bird, 43
## 
## Fixed effects:
##             Estimate Std. Error z value Pr(>|z|)    
## (Intercept) -0.56719    0.32884  -1.725   0.0846 .  
## SongIDBDY    1.26375    0.21920   5.765 8.15e-09 ***
## Block       -0.28519    0.04241  -6.725 1.76e-11 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##           (Intr) SIDBDY
## SongIDBDY -0.366       
## Block     -0.368 -0.168
```

``` r
csd.model.clean.int <- glmer(formula = Posture ~ Song * Block + (1|Bird),
                         data = csd_binary.clean, 
                         family=binomial)


summary(csd.model.clean)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) [
## glmerMod]
##  Family: binomial  ( logit )
## Formula: Posture ~ SongID + Block + (1 | Bird)
##    Data: csd_binary.subset
## 
##      AIC      BIC   logLik deviance df.resid 
##    678.6    696.5   -335.3    670.6      643 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -2.5574 -0.5346 -0.2681  0.5743  3.7826 
## 
## Random effects:
##  Groups Name        Variance Std.Dev.
##  Bird   (Intercept) 2.568    1.602   
## Number of obs: 647, groups:  Bird, 43
## 
## Fixed effects:
##             Estimate Std. Error z value Pr(>|z|)    
## (Intercept) -0.56719    0.32884  -1.725   0.0846 .  
## SongIDBDY    1.26375    0.21920   5.765 8.15e-09 ***
## Block       -0.28519    0.04241  -6.725 1.76e-11 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##           (Intr) SIDBDY
## SongIDBDY -0.366       
## Block     -0.368 -0.168
```

``` r
summary(csd.model.clean.int)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) [
## glmerMod]
##  Family: binomial  ( logit )
## Formula: Posture ~ Song * Block + (1 | Bird)
##    Data: csd_binary.clean
## 
##      AIC      BIC   logLik deviance df.resid 
##   3670.8   3703.5  -1830.4   3660.8     5170 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -7.0579 -0.4026 -0.2064 -0.0795 13.0316 
## 
## Random effects:
##  Groups Name        Variance Std.Dev.
##  Bird   (Intercept) 3.018    1.737   
## Number of obs: 5175, groups:  Bird, 53
## 
## Fixed effects:
##              Estimate Std. Error z value Pr(>|z|)    
## (Intercept) -0.197053   0.269429  -0.731    0.465    
## Song        -0.080252   0.019887  -4.035 5.45e-05 ***
## Block       -0.308608   0.028865 -10.691  < 2e-16 ***
## Song:Block  -0.004382   0.004834  -0.907    0.365    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##            (Intr) Song   Block 
## Song       -0.362              
## Block      -0.347  0.638       
## Song:Block  0.272 -0.767 -0.779
```

``` r
## Best model by DIC/AIC is 
summary(csd.model.pot.slope)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) [
## glmerMod]
##  Family: binomial  ( logit )
## Formula: Posture ~ AvgPotency + BlockResponseRate + (AvgPotency | Bird)
##    Data: csd_binary.blockresponse
## 
##      AIC      BIC   logLik deviance df.resid 
##   3320.5   3359.8  -1654.3   3308.5     5166 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -7.0911 -0.3333 -0.2082 -0.1323  8.1960 
## 
## Random effects:
##  Groups Name        Variance Std.Dev. Corr 
##  Bird   (Intercept) 0.3399   0.583         
##         AvgPotency  3.3099   1.819    -0.30
## Number of obs: 5172, groups:  Bird, 53
## 
## Fixed effects:
##                   Estimate Std. Error z value Pr(>|z|)    
## (Intercept)        -4.2251     0.1809  -23.36   <2e-16 ***
## AvgPotency          5.0994     0.4987   10.22   <2e-16 ***
## BlockResponseRate   4.6224     0.2004   23.07   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) AvgPtn
## AvgPotency  -0.740       
## BlckRspnsRt -0.349  0.087
```

``` r
summary(csd.model.pot.slope.interaction)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) [
## glmerMod]
##  Family: binomial  ( logit )
## Formula: Posture ~ AvgPotency * BlockResponseRate + (AvgPotency | Bird)
##    Data: csd_binary.blockresponse
## 
##      AIC      BIC   logLik deviance df.resid 
##   3318.3   3364.2  -1652.2   3304.3     5165 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -5.9647 -0.3321 -0.2004 -0.1260  8.5835 
## 
## Random effects:
##  Groups Name        Variance Std.Dev. Corr 
##  Bird   (Intercept) 0.2922   0.5406        
##         AvgPotency  3.1704   1.7806   -0.25
## Number of obs: 5172, groups:  Bird, 53
## 
## Fixed effects:
##                              Estimate Std. Error z value Pr(>|z|)    
## (Intercept)                   -4.3859     0.1970 -22.264   <2e-16 ***
## AvgPotency                     5.6674     0.5683   9.973   <2e-16 ***
## BlockResponseRate              5.2293     0.3563  14.676   <2e-16 ***
## AvgPotency:BlockResponseRate  -2.0553     0.9780  -2.102   0.0356 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) AvgPtn BlckRR
## AvgPotency  -0.790              
## BlckRspnsRt -0.524  0.453       
## AvgPtnc:BRR  0.409 -0.490 -0.830
```

``` r
plot(fitted(csd.model.pot.slope.interaction),resid(csd.model.pot.slope.interaction))
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-28.png)

``` r
plot(fitted(csd.model.pot.slope),resid(csd.model.pot.slope)) 
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-29.png)

``` r
plot(fitted(csd.model.simple.resp),resid(csd.model.simple.resp))
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-30.png)

``` r
plot(fitted(csd.model.lm2),resid(csd.model.lm2))
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-31.png)

``` r
scoreModel <-glmer(formula = Posture ~ AvgPotency * Block +
                   (1|Bird) + (1|Aviary) + (1|SongSet), data = csd_binary, 
                   family=binomial)
```

```
## boundary (singular) fit: see help('isSingular')
```

``` r
summary(scoreModel)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) [
## glmerMod]
##  Family: binomial  ( logit )
## Formula: Posture ~ AvgPotency * Block + (1 | Bird) + (1 | Aviary) + (1 |  
##     SongSet)
##    Data: csd_binary
## 
##      AIC      BIC   logLik deviance df.resid 
##   3528.1   3574.0  -1757.1   3514.1     5168 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -4.8546 -0.3829 -0.1950 -0.0764  8.7621 
## 
## Random effects:
##  Groups  Name        Variance  Std.Dev. 
##  Bird    (Intercept) 2.567e+00 1.602e+00
##  Aviary  (Intercept) 4.303e-10 2.074e-05
##  SongSet (Intercept) 1.738e-10 1.318e-05
## Number of obs: 5175, groups:  Bird, 53; Aviary, 7; SongSet, 3
## 
## Fixed effects:
##                  Estimate Std. Error z value Pr(>|z|)    
## (Intercept)      -2.21936    0.27854  -7.968 1.62e-15 ***
## AvgPotency        5.70024    0.54069  10.543  < 2e-16 ***
## Block            -0.30641    0.03401  -9.010  < 2e-16 ***
## AvgPotency:Block -0.13268    0.10189  -1.302    0.193    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) AvgPtn Block 
## AvgPotency  -0.542              
## Block       -0.385  0.539       
## AvgPtncy:Bl  0.376 -0.715 -0.837
## optimizer (Nelder_Mead) convergence code: 0 (OK)
## boundary (singular) fit: see help('isSingular')
```

``` r
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
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-32.png)

``` r
ggsave(file='./figures/5f-legend.png',plot=p.csd.data,dpi=300,bg = "transparent")
```

```
## Saving 7 x 7 in image
```

``` r
## Plot simulated experiments:

## This assumes you have run the python simulation to get the tmp potency files

sim.pot <- read.csv("./csdModel/tmp_pot.csv")
sim.pot[,"Posture"] <- sim.pot$Response

pot.model <-glmer(formula = Posture ~ AvgPotency * Block +
                     (1|Bird), data = sim.pot, 
                   family=binomial)
summary(pot.model)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) [
## glmerMod]
##  Family: binomial  ( logit )
## Formula: Posture ~ AvgPotency * Block + (1 | Bird)
##    Data: sim.pot
## 
##      AIC      BIC   logLik deviance df.resid 
##    888.6    913.2   -439.3    878.6      995 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.1274 -0.5533 -0.2401  0.3913  4.9088 
## 
## Random effects:
##  Groups Name        Variance Std.Dev.
##  Bird   (Intercept) 0.02954  0.1719  
## Number of obs: 1000, groups:  Bird, 10
## 
## Fixed effects:
##                   Estimate Std. Error z value Pr(>|z|)    
## (Intercept)       0.007609   0.243119   0.031    0.975    
## AvgPotency       -0.132028   0.663864  -0.199    0.842    
## Block            -0.687831   0.068185 -10.088  < 2e-16 ***
## AvgPotency:Block  1.353792   0.166080   8.151  3.6e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) AvgPtn Block 
## AvgPotency  -0.809              
## Block       -0.726  0.596       
## AvgPtncy:Bl  0.668 -0.767 -0.874
```

``` r
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
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-33.png)

``` r
ggsave(file='./figures/5d.png',plot=p.csd.pot,dpi=300,bg = "transparent")
```

```
## Saving 7 x 7 in image
```

``` r
sim.thresh <- read.csv("./csdModel/tmp_thresh.csv")
sim.thresh[,"Posture"] <- sim.thresh$Response

thresh.model <-glmer(formula = Posture ~ AvgPotency * Block +
                    (1|Bird), data = sim.thresh, 
                  family=binomial)
```

```
## boundary (singular) fit: see help('isSingular')
```

``` r
summary(thresh.model)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) [
## glmerMod]
##  Family: binomial  ( logit )
## Formula: Posture ~ AvgPotency * Block + (1 | Bird)
##    Data: sim.thresh
## 
##      AIC      BIC   logLik deviance df.resid 
##    452.5    477.0   -221.2    442.5      995 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -4.3994 -0.1549  0.0031  0.1654  5.0411 
## 
## Random effects:
##  Groups Name        Variance Std.Dev.
##  Bird   (Intercept) 0        0       
## Number of obs: 1000, groups:  Bird, 10
## 
## Fixed effects:
##                  Estimate Std. Error z value Pr(>|z|)    
## (Intercept)       -0.4909     0.5520  -0.889    0.374    
## AvgPotency        13.5151     1.5895   8.503   <2e-16 ***
## Block             -1.4609     0.1589  -9.194   <2e-16 ***
## AvgPotency:Block   0.1161     0.2433   0.477    0.633    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) AvgPtn Block 
## AvgPotency  -0.801              
## Block       -0.625  0.178       
## AvgPtncy:Bl  0.843 -0.726 -0.771
## optimizer (Nelder_Mead) convergence code: 0 (OK)
## boundary (singular) fit: see help('isSingular')
```

``` r
sim.thresh$Fits <- fitted(thresh.model)

ggplot(data=sim.thresh,aes(x=Block,y=Fits,group=Song)) + 
  geom_line()
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-34.png)

``` r
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
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-35.png)

``` r
ggsave(file='./figures/5e.png',plot=p.csd.thresh,dpi=300,bg = "transparent")
```

```
## Saving 7 x 7 in image
```

``` r
## Run Block One Potency Model, b1 potency should not predict if preference is learned
B1scoreModel <-glmer(formula = Posture ~ B1Potency * Block + 
                       (1|Bird) + (1|Aviary) + (1|SongSet), data = csd_binary, family=binomial)
summary(B1scoreModel)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) [
## glmerMod]
##  Family: binomial  ( logit )
## Formula: Posture ~ B1Potency * Block + (1 | Bird) + (1 | Aviary) + (1 |  
##     SongSet)
##    Data: csd_binary
## 
##      AIC      BIC   logLik deviance df.resid 
##   3641.3   3687.2  -1813.7   3627.3     5164 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -8.2797 -0.3997 -0.2061 -0.0848  9.0280 
## 
## Random effects:
##  Groups  Name        Variance  Std.Dev. 
##  Bird    (Intercept) 2.293e+00 1.5143533
##  Aviary  (Intercept) 7.067e-01 0.8406801
##  SongSet (Intercept) 1.502e-08 0.0001225
## Number of obs: 5171, groups:  Bird, 53; Aviary, 7; SongSet, 3
## 
## Fixed effects:
##                 Estimate Std. Error z value Pr(>|z|)    
## (Intercept)     -1.21807    0.43455  -2.803  0.00506 ** 
## B1Potency        1.70013    0.25695   6.617 3.67e-11 ***
## Block           -0.29910    0.03430  -8.721  < 2e-16 ***
## B1Potency:Block -0.06083    0.05442  -1.118  0.26366    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) B1Ptnc Block 
## B1Potency   -0.320              
## Block       -0.256  0.600       
## B1Ptncy:Blc  0.233 -0.745 -0.848
```

``` r
interact_plot(B1scoreModel,modxvals=modx_vals,pred=Block,modx=B1Potency)
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-36.png)

``` r
## An overall good model, to see what explains Posture generally
goodModel <-glmer(formula = Posture ~ AvgPotency * BlockResponseRate +
                    (1|Bird) + (1|Aviary) + (1|SongSet), data = csd_binary, 
                  family=binomial)
```

```
## boundary (singular) fit: see help('isSingular')
```

``` r
summary(goodModel)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) [
## glmerMod]
##  Family: binomial  ( logit )
## Formula: Posture ~ AvgPotency * BlockResponseRate + (1 | Bird) + (1 |  
##     Aviary) + (1 | SongSet)
##    Data: csd_binary
## 
##      AIC      BIC   logLik deviance df.resid 
##   3324.7   3370.6  -1655.3   3310.7     5165 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -4.1860 -0.3369 -0.2046 -0.1257  8.3027 
## 
## Random effects:
##  Groups  Name        Variance  Std.Dev. 
##  Bird    (Intercept) 2.859e-01 5.347e-01
##  Aviary  (Intercept) 3.165e-01 5.626e-01
##  SongSet (Intercept) 5.608e-10 2.368e-05
## Number of obs: 5172, groups:  Bird, 53; Aviary, 7; SongSet, 3
## 
## Fixed effects:
##                              Estimate Std. Error z value Pr(>|z|)    
## (Intercept)                   -4.6528     0.3064 -15.184   <2e-16 ***
## AvgPotency                     6.0344     0.5037  11.980   <2e-16 ***
## BlockResponseRate              5.3981     0.3805  14.187   <2e-16 ***
## AvgPotency:BlockResponseRate  -2.0664     0.9586  -2.156   0.0311 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) AvgPtn BlckRR
## AvgPotency  -0.587              
## BlckRspnsRt -0.404  0.554       
## AvgPtnc:BRR  0.237 -0.556 -0.779
## optimizer (Nelder_Mead) convergence code: 0 (OK)
## boundary (singular) fit: see help('isSingular')
```

``` r
interact_plot(goodModel,modxvals=modx_vals,pred=BlockResponseRate,modx=AvgPotency)
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-37.png)

``` r
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
```

```
##              Df Sum Sq Mean Sq F value Pr(>F)
## Whistle       1  0.096 0.09642   0.564  0.454
## Residuals   108 18.458 0.17091
```

``` r
## With linear regression, predicts a weak, non significant effect on duration. 
whistleModel <- lmer(formula = Latency ~ Whistle +
                          (1|Bird) + (1|Song) + (1|Block),data=whistleLat_data_)


summary(whistleModel)
```

```
## Linear mixed model fit by REML. t-tests use Satterthwaite's method [lmerModLmerTest
## ]
## Formula: Latency ~ Whistle + (1 | Bird) + (1 | Song) + (1 | Block)
##    Data: whistleLat_data_
## 
## REML criterion at convergence: -65.8
## 
## Scaled residuals: 
##      Min       1Q   Median       3Q      Max 
## -1.80167 -0.54023 -0.05015  0.47029  2.72632 
## 
## Random effects:
##  Groups   Name        Variance  Std.Dev.
##  Block    (Intercept) 0.0003588 0.01894 
##  Song     (Intercept) 0.0035421 0.05952 
##  Bird     (Intercept) 0.0180580 0.13438 
##  Residual             0.0200231 0.14150 
## Number of obs: 76, groups:  Block, 19; Song, 2; Bird, 2
## 
## Fixed effects:
##             Estimate Std. Error       df t value Pr(>|t|)  
## (Intercept)  0.51818    0.10767  1.37072   4.813   0.0805 .
## Whistle     -0.02347    0.03265 59.82028  -0.719   0.4750  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##         (Intr)
## Whistle -0.168
```

``` r
whistleDurModel <- lmer(formula = Duration ~ Whistle +
                       (1|Bird) + (1|Song) + (1|Block),data=whistleLat_data_)


summary(whistleDurModel)
```

```
## Linear mixed model fit by REML. t-tests use Satterthwaite's method [lmerModLmerTest
## ]
## Formula: Duration ~ Whistle + (1 | Bird) + (1 | Song) + (1 | Block)
##    Data: whistleLat_data_
## 
## REML criterion at convergence: 254.4
## 
## Scaled residuals: 
##      Min       1Q   Median       3Q      Max 
## -2.09311 -0.66663 -0.03756  0.68562  1.80033 
## 
## Random effects:
##  Groups   Name        Variance Std.Dev.
##  Block    (Intercept) 0.8159   0.9033  
##  Song     (Intercept) 1.2198   1.1045  
##  Bird     (Intercept) 1.3444   1.1595  
##  Residual             1.0865   1.0424  
## Number of obs: 76, groups:  Block, 19; Song, 2; Bird, 2
## 
## Fixed effects:
##             Estimate Std. Error      df t value Pr(>|t|)  
## (Intercept)   3.2299     1.1735  2.0871   2.752   0.1055  
## Whistle       0.4314     0.2476 54.3661   1.743   0.0871 .
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##         (Intr)
## Whistle -0.121
```

``` r
mean(whistleLat_data$Latency,na.rm=TRUE)
```

```
## [1] 0.4835556
```

``` r
plot(fitted(whistleDurModel),resid(whistleDurModel))
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-38.png)

``` r
qqnorm(resid(whistleDurModel))
qqline(resid(whistleDurModel))
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-39.png)

``` r
### Supplemental Figures! 

## Supplemental Figure One: 
csd_binary.blockMeans <- aggregate(csd_binary.clean$Posture, by = list(csd_binary.clean$Bird,csd_binary.clean$Block), FUN = mean)


df_summary <- csd_binary.clean %>%
  group_by(Aviary, Bird,Block) %>%
  summarize(BlockResponsivity = mean(Posture, na.rm = TRUE)) 
```

```
## `summarise()` has grouped output by 'Aviary', 'Bird'. You can override using the
## `.groups` argument.
```

``` r
csd_binary.ranks <- left_join(csd_binary.clean,df_summary,by=c('Aviary','Bird','Block'))

df_summary <- csd_binary.ranks %>%
  group_by(Aviary) %>%
  summarize(GroupResponsivity = mean(BlockResponsivity,na.rm=TRUE)) %>%
  mutate(GroupsRank = rank(desc(GroupResponsivity),ties.method='first'))

csd_binary.ranks <- left_join(csd_binary.ranks,df_summary,by=c('Aviary'))

csd_binary.ranks
```

```
## # A tibble: 5,175 × 10
## # Groups:   Bird [53]
##    Posture  Song SongID BirdID  Bird Block Aviary BlockResponsivity
##      <dbl> <int> <chr>  <chr>  <int> <int>  <int>             <dbl>
##  1       1     0 BDY    P1         0     0      0               0.5
##  2       1     1 BOD    P1         0     0      0               0.5
##  3       1     3 LB     P1         0     0      0               0.5
##  4       0     9 DMG    P1         0     0      0               0.5
##  5       0    10 BDY-   P1         0     0      0               0.5
##  6       0    12 LB-    P1         0     0      0               0.5
##  7       1     1 BOD    P1         0     1      0               0.5
##  8       1     9 DMG    P1         0     1      0               0.5
##  9       0    10 BDY-   P1         0     1      0               0.5
## 10       0    12 LB-    P1         0     1      0               0.5
## #  5,165 more rows
## #  2 more variables: GroupResponsivity <dbl>, GroupsRank <int>
```

``` r
df_summary <- csd_binary.ranks %>%
  group_by(Aviary,Bird) %>%
  summarize(BirdResponsivity = mean(Posture,na.rm=TRUE)) %>%
  mutate(RankInGroup = rank(desc(BirdResponsivity),ties.method="first"))
```

```
## `summarise()` has grouped output by 'Aviary'. You can override using the `.groups`
## argument.
```

``` r
csd_binary.ranks <- left_join(csd_binary.ranks,df_summary,by=c("Aviary","Bird"))
  
csd_binary.ranks
```

```
## # A tibble: 5,175 × 12
## # Groups:   Bird [53]
##    Posture  Song SongID BirdID  Bird Block Aviary BlockResponsivity
##      <dbl> <int> <chr>  <chr>  <int> <int>  <int>             <dbl>
##  1       1     0 BDY    P1         0     0      0               0.5
##  2       1     1 BOD    P1         0     0      0               0.5
##  3       1     3 LB     P1         0     0      0               0.5
##  4       0     9 DMG    P1         0     0      0               0.5
##  5       0    10 BDY-   P1         0     0      0               0.5
##  6       0    12 LB-    P1         0     0      0               0.5
##  7       1     1 BOD    P1         0     1      0               0.5
##  8       1     9 DMG    P1         0     1      0               0.5
##  9       0    10 BDY-   P1         0     1      0               0.5
## 10       0    12 LB-    P1         0     1      0               0.5
## #  5,165 more rows
## #  4 more variables: GroupResponsivity <dbl>, GroupsRank <int>,
## #   BirdResponsivity <dbl>, RankInGroup <int>
```

``` r
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
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-40.png)

``` r
csd_binary.birdResponse <- csd_binary.ranks %>%
  group_by(Bird,Block) %>%
  summarise(Responsivity = mean(BlockResponsivity))
```

```
## `summarise()` has grouped output by 'Bird'. You can override using the `.groups`
## argument.
```

``` r
csd_binary.birdResponse
```

```
## # A tibble: 472 × 3
## # Groups:   Bird [53]
##     Bird Block Responsivity
##    <int> <int>        <dbl>
##  1     0     0          0.5
##  2     0     1          0.5
##  3     0     2          1  
##  4     0     3          1  
##  5     0     4          1  
##  6     0     5          1  
##  7     0     6          0.5
##  8     1     0          0.5
##  9     1     1          1  
## 10     1     2          1  
## #  462 more rows
```

``` r
Resp.aov <- aov(BlockResponsivity ~ Bird, data = csd_binary.ranks)
summary(Resp.aov)
```

```
##               Df Sum Sq Mean Sq F value Pr(>F)    
## Bird           1    7.0   7.025   101.7 <2e-16 ***
## Residuals   5173  357.5   0.069                   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

``` r
csd_binary[csd_binary$SongSet == 0,] %>%
  group_by(Block) %>%
  summarise(Responsivity = mean(BlockResponseRate),sdRespon = sd(BlockResponseRate))
```

```
## # A tibble: 10 × 3
##    Block Responsivity sdRespon
##    <int>        <dbl>    <dbl>
##  1     0        0.481    0.318
##  2     1        0.405    0.330
##  3     2        0.315    0.355
##  4     3        0.255    0.278
##  5     4       NA       NA    
##  6     5       NA       NA    
##  7     6        0.139    0.244
##  8     7       NA       NA    
##  9     8        0.197    0.260
## 10     9        0.136    0.248
```

