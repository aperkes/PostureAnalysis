

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

csd_data.raw$PostureSum <- 0
aggregate(csd_data.raw$Posture,by=list(Category=csd_data.raw$BirdID),FUN=sum)
```

```
##     Category   x
## 1     CB-LB2  40
## 2  CB-Orange  64
## 3      CB-R2   0
## 4     CB-Red   6
## 5   CB-White  26
## 6      CB-Y2  80
## 7  CB-Yellow  47
## 8        F2B  16
## 9       F2MB  91
## 10      F2NL  26
## 11      F2PO  40
## 12      FBON   4
## 13      FBWB   4
## 14      FDMW  44
## 15      FGBD  34
## 16      FGHB  22
## 17      FGNP  11
## 18      FGOG   7
## 19      FGRN 102
## 20      FGWL  51
## 21      FGWO   3
## 22       FLG  53
## 23       FLN   5
## 24      FLOL  17
## 25      FLPD   1
## 26      FLRO   2
## 27      FLWD  35
## 28      FM2N  30
## 29      FN2R   2
## 30       FNM   3
## 31      FNMO   3
## 32       FNO  22
## 33      FNWD  38
## 34      FNYO  14
## 35      FO2P  38
## 36      FO2R   9
## 37      FOGO  29
## 38     FPG/M  41
## 39      FR2D 130
## 40      FR2M  58
## 41       FRG   2
## 42       FRM  46
## 43      FROY  30
## 44      FRWB   2
## 45      FWGP   9
## 46      FWMW  32
## 47      FWRW   7
## 48      FYGN   5
## 49       FYR  35
## 50      GR11  32
## 51      GR13  24
## 52      LB20  40
## 53        P1  14
## 54     PINK2  40
## 55      Wh14  14
## 56      WH15   0
## 57      WH19  28
## 58      WH20   2
## 59      WH24  29
## 60        Y1 104
```

``` r
filtered_csd <- csd_data.raw %>%
  group_by(BirdID) %>%
  filter(sum(Posture) > 5)
aggregate(filtered_csd$Posture,by=list(Category=filtered_csd$BirdID),FUN=sum)
```

```
##     Category   x
## 1     CB-LB2  40
## 2  CB-Orange  64
## 3     CB-Red   6
## 4   CB-White  26
## 5      CB-Y2  80
## 6  CB-Yellow  47
## 7        F2B  16
## 8       F2MB  91
## 9       F2NL  26
## 10      F2PO  40
## 11      FDMW  44
## 12      FGBD  34
## 13      FGHB  22
## 14      FGNP  11
## 15      FGOG   7
## 16      FGRN 102
## 17      FGWL  51
## 18       FLG  53
## 19      FLOL  17
## 20      FLWD  35
## 21      FM2N  30
## 22       FNO  22
## 23      FNWD  38
## 24      FNYO  14
## 25      FO2P  38
## 26      FO2R   9
## 27      FOGO  29
## 28     FPG/M  41
## 29      FR2D 130
## 30      FR2M  58
## 31       FRM  46
## 32      FROY  30
## 33      FWGP   9
## 34      FWMW  32
## 35      FWRW   7
## 36       FYR  35
## 37      GR11  32
## 38      GR13  24
## 39      LB20  40
## 40        P1  14
## 41     PINK2  40
## 42      Wh14  14
## 43      WH19  28
## 44      WH24  29
## 45        Y1 104
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
## `summarise()` has grouped output by 'SongID'. You can override using the `.groups` argument.
```

``` r
print(song_means)
```

```
## # A tibble: 40 × 5
## # Groups:   SongID [10]
##    SongID Aviary mean_pot std_pot pres_count
##    <chr>   <int>    <dbl>   <dbl>      <int>
##  1 2M          1    0.625   0.703         48
##  2 2M          2    0.167   0.376         66
##  3 2M          5    0.378   0.646         90
##  4 2M          6    0.327   0.552         98
##  5 BDY         1    0.830   0.524         47
##  6 BDY         2    0.591   0.554         66
##  7 BDY         5    0.489   0.604         90
##  8 BDY         6    0.520   0.692         98
##  9 BOD         1    0.477   0.698         44
## 10 BOD         2    0.242   0.466         66
## # ℹ 30 more rows
```

``` r
csd_data.set0[csd_data.set0$Aviary == 1,]
```

```
## # A tibble: 464 × 22
## # Groups:   BirdID [6]
##    Posture BirdID   Bird Block SongID  Song Aviary SongSet Duration Latency AvgPotency B1Potency AvgRank B1Rank
##      <dbl> <chr>   <int> <int> <chr>  <int>  <int>   <int>    <dbl>   <dbl>      <dbl>     <dbl>   <dbl>  <dbl>
##  1       1 CB-Whi…     3     0 BDY        0      1       0        5      NA      0.829       1         1    4  
##  2       0 CB-Whi…     3     0 BOD        1      1       0       NA      NA      0.265       1         9    4  
##  3       1 CB-Whi…     3     0 ND         2      1       0        4      NA      0.526       1         3    4  
##  4       0 CB-Whi…     3     0 LB         3      1       0       NA      NA      0.316       1         7    4  
##  5       0 CB-Whi…     3     0 2M         4      1       0       NA      NA      0.447       1         4    4  
##  6       0 CB-Whi…     3     0 DBR        5      1       0       NA      NA      0.147       0        10   12  
##  7       1 CB-Whi…     3     0 GRG        6      1       0        5      NA      0.429       1         5    4  
##  8       0 CB-Whi…     3     0 WG         7      1       0       NA      NA      0.540       1         2    4  
##  9       1 CB-Whi…     3     0 LNR        8      1       0        2      NA      0.389       0.5       6    8.5
## 10       0 CB-Whi…     3     0 DMG        9      1       0       NA      NA      0.270       0.5       8    8.5
## # ℹ 454 more rows
## # ℹ 8 more variables: BlockResponseRate <dbl>, LowResponse <chr>, WrongResponseRate <dbl>, Prefix <chr>,
## #   MaxVelocity <dbl>, PeakHeight <dbl>, BVPotency <dbl>, PostureSum <dbl>
```

``` r
## Consistency of preference: 
song_means.birds <- csd_data.set0 %>%
  group_by(SongID,BirdID,Aviary) %>%
  summarize(mean_pot = mean(Posture),std_pot = sd(Posture),pres_count = length(Posture))
```

```
## `summarise()` has grouped output by 'SongID', 'BirdID'. You can override using the `.groups` argument.
```

``` r
aviaries <- unique(song_means$Aviary)

corrs.aviary <- c()
corrs.birds <- c()
for (a in aviaries) {
  birds <- unique(song_means.birds[song_means.birds$Aviary == a,]$BirdID)
  song_means.sub <- song_means.birds[song_means.birds$Aviary == a,]
  birds <- unique(song_means.sub$BirdID)
  for (i in birds) {
    for (j in birds) {
      if (i == j) next
      print(c(i,j))
      res.cor <- cor.test(x=song_means.sub[song_means.sub$BirdID == i,]$mean_pot,
                          y=song_means.sub[song_means.sub$BirdID == j,]$mean_pot,method='pearson')
      print(res.cor$estimate)
      corrs.birds <- c(corrs.birds,res.cor$estimate)
    }
  }
  for (b in aviaries) {

    if (a == b) next
    print(c(a,b))
    res.cor <- cor.test(x=song_means[song_means$Aviary == a,]$mean_pot,y=song_means[song_means$Aviary == b,]$mean_pot,method='pearson')
    print(res.cor$estimate)
    corrs.aviary <- c(corrs.aviary,res.cor$estimate)
    }
  }
```

```
## [1] "CB-LB2"    "CB-Orange"
##        cor 
## 0.08551833 
## [1] "CB-LB2" "CB-Red"
##       cor 
## 0.6183155 
## [1] "CB-LB2"   "CB-White"
##       cor 
## 0.7272535 
## [1] "CB-LB2" "CB-Y2" 
##       cor 
## 0.1658263 
## [1] "CB-LB2"    "CB-Yellow"
##       cor 
## 0.5372215 
## [1] "CB-Orange" "CB-LB2"   
##        cor 
## 0.08551833 
## [1] "CB-Orange" "CB-Red"   
##         cor 
## -0.03738416 
## [1] "CB-Orange" "CB-White" 
##          cor 
## -0.009275626 
## [1] "CB-Orange" "CB-Y2"    
##       cor 
## -0.112255 
## [1] "CB-Orange" "CB-Yellow"
##       cor 
## 0.4594245 
## [1] "CB-Red" "CB-LB2"
##       cor 
## 0.6183155 
## [1] "CB-Red"    "CB-Orange"
##         cor 
## -0.03738416 
## [1] "CB-Red"   "CB-White"
##       cor 
## 0.2042016 
## [1] "CB-Red" "CB-Y2" 
##       cor 
## 0.2806249 
## [1] "CB-Red"    "CB-Yellow"
##       cor 
## 0.1236055 
## [1] "CB-White" "CB-LB2"  
##       cor 
## 0.7272535 
## [1] "CB-White"  "CB-Orange"
##          cor 
## -0.009275626 
## [1] "CB-White" "CB-Red"  
##       cor 
## 0.2042016 
## [1] "CB-White" "CB-Y2"   
##        cor 
## 0.01322967 
## [1] "CB-White"  "CB-Yellow"
##     cor 
## 0.25709 
## [1] "CB-Y2"  "CB-LB2"
##       cor 
## 0.1658263 
## [1] "CB-Y2"     "CB-Orange"
##       cor 
## -0.112255 
## [1] "CB-Y2"  "CB-Red"
##       cor 
## 0.2806249 
## [1] "CB-Y2"    "CB-White"
##        cor 
## 0.01322967 
## [1] "CB-Y2"     "CB-Yellow"
##        cor 
## -0.3453539 
## [1] "CB-Yellow" "CB-LB2"   
##       cor 
## 0.5372215 
## [1] "CB-Yellow" "CB-Orange"
##       cor 
## 0.4594245 
## [1] "CB-Yellow" "CB-Red"   
##       cor 
## 0.1236055 
## [1] "CB-Yellow" "CB-White" 
##     cor 
## 0.25709 
## [1] "CB-Yellow" "CB-Y2"    
##        cor 
## -0.3453539 
## [1] 1 2
##       cor 
## 0.8008971 
## [1] 1 5
##       cor 
## 0.2431226 
## [1] 1 6
##       cor 
## 0.4691869 
## [1] "GR11" "GR13"
##       cor 
## 0.7456437 
## [1] "GR11" "LB20"
##       cor 
## 0.8532783 
## [1] "GR11" "WH19"
##       cor 
## 0.3219165 
## [1] "GR11" "WH24"
##      cor 
## 0.593745 
## [1] "GR11" "Wh14"
##       cor 
## 0.5876964 
## [1] "GR13" "GR11"
##       cor 
## 0.7456437 
## [1] "GR13" "LB20"
##      cor 
## 0.598093 
## [1] "GR13" "WH19"
##       cor 
## 0.3289369 
## [1] "GR13" "WH24"
##       cor 
## 0.7879207 
## [1] "GR13" "Wh14"
##       cor 
## 0.6087617 
## [1] "LB20" "GR11"
##       cor 
## 0.8532783 
## [1] "LB20" "GR13"
##      cor 
## 0.598093 
## [1] "LB20" "WH19"
##       cor 
## 0.4269146 
## [1] "LB20" "WH24"
##       cor 
## 0.5513323 
## [1] "LB20" "Wh14"
##      cor 
## 0.736007 
## [1] "WH19" "GR11"
##       cor 
## 0.3219165 
## [1] "WH19" "GR13"
##       cor 
## 0.3289369 
## [1] "WH19" "LB20"
##       cor 
## 0.4269146 
## [1] "WH19" "WH24"
##       cor 
## 0.3871135 
## [1] "WH19" "Wh14"
##       cor 
## 0.6153846 
## [1] "WH24" "GR11"
##      cor 
## 0.593745 
## [1] "WH24" "GR13"
##       cor 
## 0.7879207 
## [1] "WH24" "LB20"
##       cor 
## 0.5513323 
## [1] "WH24" "WH19"
##       cor 
## 0.3871135 
## [1] "WH24" "Wh14"
##       cor 
## 0.6551399 
## [1] "Wh14" "GR11"
##       cor 
## 0.5876964 
## [1] "Wh14" "GR13"
##       cor 
## 0.6087617 
## [1] "Wh14" "LB20"
##      cor 
## 0.736007 
## [1] "Wh14" "WH19"
##       cor 
## 0.6153846 
## [1] "Wh14" "WH24"
##       cor 
## 0.6551399 
## [1] 2 1
##       cor 
## 0.8008971 
## [1] 2 5
##       cor 
## 0.1739432 
## [1] 2 6
##       cor 
## 0.5170011 
## [1] "FGBD" "FGRN"
##       cor 
## 0.2895103 
## [1] "FGBD" "FLG" 
##        cor 
## -0.4026464 
## [1] "FGBD" "FNYO"
##         cor 
## -0.09814658 
## [1] "FGBD" "FO2P"
##        cor 
## 0.09121731 
## [1] "FGBD" "FOGO"
##         cor 
## -0.03637952 
## [1] "FGBD" "FR2D"
##        cor 
## -0.4204574 
## [1] "FGBD" "FRM" 
##       cor 
## 0.4705955 
## [1] "FGBD" "FYR" 
##       cor 
## 0.1672787 
## [1] "FGRN" "FGBD"
##       cor 
## 0.2895103 
## [1] "FGRN" "FLG" 
##       cor 
## 0.4478909 
## [1] "FGRN" "FNYO"
##       cor 
## 0.1191828 
## [1] "FGRN" "FO2P"
##       cor 
## 0.4430735 
## [1] "FGRN" "FOGO"
##       cor 
## 0.3534154 
## [1] "FGRN" "FR2D"
##           cor 
## -1.828559e-16 
## [1] "FGRN" "FRM" 
##       cor 
## 0.5175492 
## [1] "FGRN" "FYR" 
##       cor 
## 0.2635231 
## [1] "FLG"  "FGBD"
##        cor 
## -0.4026464 
## [1] "FLG"  "FGRN"
##       cor 
## 0.4478909 
## [1] "FLG"  "FNYO"
##      cor 
## 0.159431 
## [1] "FLG"  "FO2P"
##       cor 
## 0.2298224 
## [1] "FLG"  "FOGO"
##       cor 
## 0.0960302 
## [1] "FLG"  "FR2D"
##         cor 
## -0.08812878 
## [1] "FLG" "FRM"
##       cor 
## 0.1730815 
## [1] "FLG" "FYR"
##       cor 
## 0.1007186 
## [1] "FNYO" "FGBD"
##         cor 
## -0.09814658 
## [1] "FNYO" "FGRN"
##       cor 
## 0.1191828 
## [1] "FNYO" "FLG" 
##      cor 
## 0.159431 
## [1] "FNYO" "FO2P"
##       cor 
## 0.7000666 
## [1] "FNYO" "FOGO"
##        cor 
## 0.03369683 
## [1] "FNYO" "FR2D"
##        cor 
## -0.3015113 
## [1] "FNYO" "FRM" 
##       cor 
## 0.4276686 
## [1] "FNYO" "FYR" 
##      cor 
## 0.452267 
## [1] "FO2P" "FGBD"
##        cor 
## 0.09121731 
## [1] "FO2P" "FGRN"
##       cor 
## 0.4430735 
## [1] "FO2P" "FLG" 
##       cor 
## 0.2298224 
## [1] "FO2P" "FNYO"
##       cor 
## 0.7000666 
## [1] "FO2P" "FOGO"
##       cor 
## 0.3265999 
## [1] "FO2P" "FR2D"
##         cor 
## -0.05337605 
## [1] "FO2P" "FRM" 
##       cor 
## 0.3668997 
## [1] "FO2P" "FYR" 
##       cor 
## 0.3024643 
## [1] "FOGO" "FGBD"
##         cor 
## -0.03637952 
## [1] "FOGO" "FGRN"
##       cor 
## 0.3534154 
## [1] "FOGO" "FLG" 
##       cor 
## 0.0960302 
## [1] "FOGO" "FNYO"
##        cor 
## 0.03369683 
## [1] "FOGO" "FO2P"
##       cor 
## 0.3265999 
## [1] "FOGO" "FR2D"
##       cor 
## 0.2142062 
## [1] "FOGO" "FRM" 
##       cor 
## 0.2194918 
## [1] "FOGO" "FYR" 
##       cor 
## -0.298026 
## [1] "FR2D" "FGBD"
##        cor 
## -0.4204574 
## [1] "FR2D" "FGRN"
##           cor 
## -1.828559e-16 
## [1] "FR2D" "FLG" 
##         cor 
## -0.08812878 
## [1] "FR2D" "FNYO"
##        cor 
## -0.3015113 
## [1] "FR2D" "FO2P"
##         cor 
## -0.05337605 
## [1] "FR2D" "FOGO"
##       cor 
## 0.2142062 
## [1] "FR2D" "FRM" 
##        cor 
## -0.6910233 
## [1] "FR2D" "FYR" 
##        cor 
## -0.6666667 
## [1] "FRM"  "FGBD"
##       cor 
## 0.4705955 
## [1] "FRM"  "FGRN"
##       cor 
## 0.5175492 
## [1] "FRM" "FLG"
##       cor 
## 0.1730815 
## [1] "FRM"  "FNYO"
##       cor 
## 0.4276686 
## [1] "FRM"  "FO2P"
##       cor 
## 0.3668997 
## [1] "FRM"  "FOGO"
##       cor 
## 0.2194918 
## [1] "FRM"  "FR2D"
##        cor 
## -0.6910233 
## [1] "FRM" "FYR"
##       cor 
## 0.7031465 
## [1] "FYR"  "FGBD"
##       cor 
## 0.1672787 
## [1] "FYR"  "FGRN"
##       cor 
## 0.2635231 
## [1] "FYR" "FLG"
##       cor 
## 0.1007186 
## [1] "FYR"  "FNYO"
##      cor 
## 0.452267 
## [1] "FYR"  "FO2P"
##       cor 
## 0.3024643 
## [1] "FYR"  "FOGO"
##       cor 
## -0.298026 
## [1] "FYR"  "FR2D"
##        cor 
## -0.6666667 
## [1] "FYR" "FRM"
##       cor 
## 0.7031465 
## [1] 5 1
##       cor 
## 0.2431226 
## [1] 5 2
##       cor 
## 0.1739432 
## [1] 5 6
##       cor 
## 0.2831633 
## [1] "F2MB" "F2PO"
##       cor 
## 0.4165726 
## [1] "F2MB" "FDMW"
##        cor 
## -0.2163856 
## [1] "F2MB" "FGHB"
##       cor 
## 0.4164556 
## [1] "F2MB" "FGNP"
##       cor 
## 0.3130952 
## [1] "F2MB" "FGOG"
##        cor 
## -0.3415145 
## [1] "F2MB" "FGRN"
##         cor 
## -0.07507377 
## [1] "F2MB" "FLOL"
##         cor 
## 0.006637087 
## [1] "F2MB" "FO2R"
##       cor 
## 0.2017045 
## [1] "F2MB" "FOGO"
##       cor 
## 0.2936662 
## [1] "F2MB"  "FPG/M"
##       cor 
## 0.7020318 
## [1] "F2MB" "FR2D"
##        cor 
## -0.2155414 
## [1] "F2MB" "FWMW"
##       cor 
## 0.3784475 
## [1] "F2MB" "FYR" 
##       cor 
## 0.2737346 
## [1] "F2PO" "F2MB"
##       cor 
## 0.4165726 
## [1] "F2PO" "FDMW"
##        cor 
## 0.05603318 
## [1] "F2PO" "FGHB"
##       cor 
## 0.8235162 
## [1] "F2PO" "FGNP"
##        cor 
## -0.1547818 
## [1] "F2PO" "FGOG"
##        cor 
## -0.5238095 
## [1] "F2PO" "FGRN"
##       cor 
## 0.4082483 
## [1] "F2PO" "FLOL"
##       cor 
## 0.2045226 
## [1] "F2PO" "FO2R"
##       cor 
## -0.108698 
## [1] "F2PO" "FOGO"
##       cor 
## 0.2241327 
## [1] "F2PO"  "FPG/M"
##       cor 
## 0.4241802 
## [1] "F2PO" "FR2D"
##        cor 
## -0.1833907 
## [1] "F2PO" "FWMW"
##         cor 
## -0.09799919 
## [1] "F2PO" "FYR" 
##       cor 
## 0.1653954 
## [1] "FDMW" "F2MB"
##        cor 
## -0.2163856 
## [1] "FDMW" "F2PO"
##        cor 
## 0.05603318 
## [1] "FDMW" "FGHB"
##        cor 
## 0.09421114 
## [1] "FDMW" "FGNP"
##        cor 
## -0.1652673 
## [1] "FDMW" "FGOG"
##         cor 
## -0.08716273 
## [1] "FDMW" "FGRN"
##       cor 
## 0.2802243 
## [1] "FDMW" "FLOL"
##        cor 
## 0.09909575 
## [1] "FDMW" "FO2R"
##        cor 
## 0.09495946 
## [1] "FDMW" "FOGO"
##        cor 
## 0.05769231 
## [1] "FDMW"  "FPG/M"
##        cor 
## -0.1314916 
## [1] "FDMW" "FR2D"
##       cor 
## 0.1204075 
## [1] "FDMW" "FWMW"
##         cor 
## -0.06726728 
## [1] "FDMW" "FYR" 
##         cor 
## -0.07568563 
## [1] "FGHB" "F2MB"
##       cor 
## 0.4164556 
## [1] "FGHB" "F2PO"
##       cor 
## 0.8235162 
## [1] "FGHB" "FDMW"
##        cor 
## 0.09421114 
## [1] "FGHB" "FGNP"
##        cor 
## -0.2120488 
## [1] "FGHB" "FGOG"
##        cor 
## -0.1245441 
## [1] "FGHB" "FGRN"
##       cor 
## 0.5774531 
## [1] "FGHB" "FLOL"
##         cor 
## -0.02697045 
## [1] "FGHB" "FO2R"
##       cor 
## 0.3987467 
## [1] "FGHB" "FOGO"
##       cor 
## 0.5338631 
## [1] "FGHB"  "FPG/M"
##       cor 
## 0.3680999 
## [1] "FGHB" "FR2D"
##       cor 
## 0.2949369 
## [1] "FGHB" "FWMW"
##       cor 
## 0.1785014 
## [1] "FGHB" "FYR" 
##       cor 
## 0.6091424 
## [1] "FGNP" "F2MB"
##       cor 
## 0.3130952 
## [1] "FGNP" "F2PO"
##        cor 
## -0.1547818 
## [1] "FGNP" "FDMW"
##        cor 
## -0.1652673 
## [1] "FGNP" "FGHB"
##        cor 
## -0.2120488 
## [1] "FGNP" "FGOG"
##       cor 
## 0.3076527 
## [1] "FGNP" "FGRN"
##         cor 
## -0.09829464 
## [1] "FGNP" "FLOL"
##        cor 
## -0.7806505 
## [1] "FGNP" "FO2R"
##         cor 
## 0.008327264 
## [1] "FGNP" "FOGO"
##        cor 
## 0.04721922 
## [1] "FGNP"  "FPG/M"
##       cor 
## 0.2190866 
## [1] "FGNP" "FR2D"
##         cor 
## -0.04703498 
## [1] "FGNP" "FWMW"
##       cor 
## -0.020646 
## [1] "FGNP" "FYR" 
##        cor 
## 0.05973378 
## [1] "FGOG" "F2MB"
##        cor 
## -0.3415145 
## [1] "FGOG" "F2PO"
##        cor 
## -0.5238095 
## [1] "FGOG" "FDMW"
##         cor 
## -0.08716273 
## [1] "FGOG" "FGHB"
##        cor 
## -0.1245441 
## [1] "FGOG" "FGNP"
##       cor 
## 0.3076527 
## [1] "FGOG" "FGRN"
##      cor 
## 0.196564 
## [1] "FGOG" "FLOL"
##        cor 
## -0.7392222 
## [1] "FGOG" "FO2R"
##       cor 
## 0.5918002 
## [1] "FGOG" "FOGO"
##      cor 
## 0.414023 
## [1] "FGOG"  "FPG/M"
##        cor 
## -0.0744976 
## [1] "FGOG" "FR2D"
##       cor 
## 0.5643472 
## [1] "FGOG" "FWMW"
##       cor 
## 0.0571662 
## [1] "FGOG" "FYR" 
##       cor 
## 0.5084376 
## [1] "FGRN" "F2MB"
##         cor 
## -0.07507377 
## [1] "FGRN" "F2PO"
##       cor 
## 0.4082483 
## [1] "FGRN" "FDMW"
##       cor 
## 0.2802243 
## [1] "FGRN" "FGHB"
##       cor 
## 0.5774531 
## [1] "FGRN" "FGNP"
##         cor 
## -0.09829464 
## [1] "FGRN" "FGOG"
##      cor 
## 0.196564 
## [1] "FGRN" "FLOL"
##       cor 
## -0.194824 
## [1] "FGRN" "FO2R"
##       cor 
## 0.4329999 
## [1] "FGRN" "FOGO"
##       cor 
## 0.5204165 
## [1] "FGRN"  "FPG/M"
##       cor 
## 0.1173093 
## [1] "FGRN" "FR2D"
##       cor 
## 0.6304181 
## [1] "FGRN" "FWMW"
##       cor 
## -0.081683 
## [1] "FGRN" "FYR" 
##       cor 
## 0.5176715 
## [1] "FLOL" "F2MB"
##         cor 
## 0.006637087 
## [1] "FLOL" "F2PO"
##       cor 
## 0.2045226 
## [1] "FLOL" "FDMW"
##        cor 
## 0.09909575 
## [1] "FLOL" "FGHB"
##         cor 
## -0.02697045 
## [1] "FLOL" "FGNP"
##        cor 
## -0.7806505 
## [1] "FLOL" "FGOG"
##        cor 
## -0.7392222 
## [1] "FLOL" "FGRN"
##       cor 
## -0.194824 
## [1] "FLOL" "FO2R"
##        cor 
## -0.4252459 
## [1] "FLOL" "FOGO"
##        cor 
## -0.3468351 
## [1] "FLOL"  "FPG/M"
##         cor 
## -0.05646447 
## [1] "FLOL" "FR2D"
##        cor 
## -0.4183386 
## [1] "FLOL" "FWMW"
##        cor 
## -0.1010995 
## [1] "FLOL" "FYR" 
##        cor 
## -0.4503646 
## [1] "FO2R" "F2MB"
##       cor 
## 0.2017045 
## [1] "FO2R" "F2PO"
##       cor 
## -0.108698 
## [1] "FO2R" "FDMW"
##        cor 
## 0.09495946 
## [1] "FO2R" "FGHB"
##       cor 
## 0.3987467 
## [1] "FO2R" "FGNP"
##         cor 
## 0.008327264 
## [1] "FO2R" "FGOG"
##       cor 
## 0.5918002 
## [1] "FO2R" "FGRN"
##       cor 
## 0.4329999 
## [1] "FO2R" "FLOL"
##        cor 
## -0.4252459 
## [1] "FO2R" "FOGO"
##       cor 
## 0.7868069 
## [1] "FO2R"  "FPG/M"
##       cor 
## 0.3378966 
## [1] "FO2R" "FR2D"
##       cor 
## 0.6447492 
## [1] "FO2R" "FWMW"
##       cor 
## 0.6050031 
## [1] "FO2R" "FYR" 
##       cor 
## 0.8046581 
## [1] "FOGO" "F2MB"
##       cor 
## 0.2936662 
## [1] "FOGO" "F2PO"
##       cor 
## 0.2241327 
## [1] "FOGO" "FDMW"
##        cor 
## 0.05769231 
## [1] "FOGO" "FGHB"
##       cor 
## 0.5338631 
## [1] "FOGO" "FGNP"
##        cor 
## 0.04721922 
## [1] "FOGO" "FGOG"
##      cor 
## 0.414023 
## [1] "FOGO" "FGRN"
##       cor 
## 0.5204165 
## [1] "FOGO" "FLOL"
##        cor 
## -0.3468351 
## [1] "FOGO" "FO2R"
##       cor 
## 0.7868069 
## [1] "FOGO"  "FPG/M"
##       cor 
## 0.6011046 
## [1] "FOGO" "FR2D"
##       cor 
## 0.3174379 
## [1] "FOGO" "FWMW"
##       cor 
## 0.1513514 
## [1] "FOGO" "FYR" 
##       cor 
## 0.8866031 
## [1] "FPG/M" "F2MB" 
##       cor 
## 0.7020318 
## [1] "FPG/M" "F2PO" 
##       cor 
## 0.4241802 
## [1] "FPG/M" "FDMW" 
##        cor 
## -0.1314916 
## [1] "FPG/M" "FGHB" 
##       cor 
## 0.3680999 
## [1] "FPG/M" "FGNP" 
##       cor 
## 0.2190866 
## [1] "FPG/M" "FGOG" 
##        cor 
## -0.0744976 
## [1] "FPG/M" "FGRN" 
##       cor 
## 0.1173093 
## [1] "FPG/M" "FLOL" 
##         cor 
## -0.05646447 
## [1] "FPG/M" "FO2R" 
##       cor 
## 0.3378966 
## [1] "FPG/M" "FOGO" 
##       cor 
## 0.6011046 
## [1] "FPG/M" "FR2D" 
##        cor 
## -0.3581864 
## [1] "FPG/M" "FWMW" 
##       cor 
## 0.2299722 
## [1] "FPG/M" "FYR"  
##       cor 
## 0.3115594 
## [1] "FR2D" "F2MB"
##        cor 
## -0.2155414 
## [1] "FR2D" "F2PO"
##        cor 
## -0.1833907 
## [1] "FR2D" "FDMW"
##       cor 
## 0.1204075 
## [1] "FR2D" "FGHB"
##       cor 
## 0.2949369 
## [1] "FR2D" "FGNP"
##         cor 
## -0.04703498 
## [1] "FR2D" "FGOG"
##       cor 
## 0.5643472 
## [1] "FR2D" "FGRN"
##       cor 
## 0.6304181 
## [1] "FR2D" "FLOL"
##        cor 
## -0.4183386 
## [1] "FR2D" "FO2R"
##       cor 
## 0.6447492 
## [1] "FR2D" "FOGO"
##       cor 
## 0.3174379 
## [1] "FR2D"  "FPG/M"
##        cor 
## -0.3581864 
## [1] "FR2D" "FWMW"
##       cor 
## 0.2680193 
## [1] "FR2D" "FYR" 
##       cor 
## 0.5938911 
## [1] "FWMW" "F2MB"
##       cor 
## 0.3784475 
## [1] "FWMW" "F2PO"
##         cor 
## -0.09799919 
## [1] "FWMW" "FDMW"
##         cor 
## -0.06726728 
## [1] "FWMW" "FGHB"
##       cor 
## 0.1785014 
## [1] "FWMW" "FGNP"
##       cor 
## -0.020646 
## [1] "FWMW" "FGOG"
##       cor 
## 0.0571662 
## [1] "FWMW" "FGRN"
##       cor 
## -0.081683 
## [1] "FWMW" "FLOL"
##        cor 
## -0.1010995 
## [1] "FWMW" "FO2R"
##       cor 
## 0.6050031 
## [1] "FWMW" "FOGO"
##       cor 
## 0.1513514 
## [1] "FWMW"  "FPG/M"
##       cor 
## 0.2299722 
## [1] "FWMW" "FR2D"
##       cor 
## 0.2680193 
## [1] "FWMW" "FYR" 
##       cor 
## 0.1796454 
## [1] "FYR"  "F2MB"
##       cor 
## 0.2737346 
## [1] "FYR"  "F2PO"
##       cor 
## 0.1653954 
## [1] "FYR"  "FDMW"
##         cor 
## -0.07568563 
## [1] "FYR"  "FGHB"
##       cor 
## 0.6091424 
## [1] "FYR"  "FGNP"
##        cor 
## 0.05973378 
## [1] "FYR"  "FGOG"
##       cor 
## 0.5084376 
## [1] "FYR"  "FGRN"
##       cor 
## 0.5176715 
## [1] "FYR"  "FLOL"
##        cor 
## -0.4503646 
## [1] "FYR"  "FO2R"
##       cor 
## 0.8046581 
## [1] "FYR"  "FOGO"
##       cor 
## 0.8866031 
## [1] "FYR"   "FPG/M"
##       cor 
## 0.3115594 
## [1] "FYR"  "FR2D"
##       cor 
## 0.5938911 
## [1] "FYR"  "FWMW"
##       cor 
## 0.1796454 
## [1] 6 1
##       cor 
## 0.4691869 
## [1] 6 2
##       cor 
## 0.5170011 
## [1] 6 5
##       cor 
## 0.2831633
```

``` r
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
## `summarise()` has grouped output by 'Bird'. You can override using the `.groups` argument.
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
length(lat_data$Latency)
```

```
## [1] 268
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
## Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
## Formula: Latency ~ AvgPotency + BlockResponseRate + (1 | Bird) + (1 |      Song) + (1 | Block)
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
## (Intercept)         1.02160    0.12470   9.11335   8.193  1.7e-05 ***
## AvgPotency         -0.22994    0.07057  20.07507  -3.258  0.00392 ** 
## BlockResponseRate  -0.11254    0.05031 185.88898  -2.237  0.02649 *  
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
## 	DHARMa nonparametric dispersion test via sd of residuals fitted vs. simulated
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
## 	DHARMa nonparametric dispersion test via sd of residuals fitted vs. simulated
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
## 	DHARMa nonparametric dispersion test via sd of residuals fitted vs. simulated
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
## Warning: Argument `add.data` is deprecated and will be removed in the future. Please use `show_data` instead.
```

```
## Data points may overlap. Use the `jitter` argument to add some amount of random variation to the
##   location of data points and avoid overplotting.
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
## Warning: Argument `add.data` is deprecated and will be removed in the future. Please use `show_data` instead.
```

```
## Data points may overlap. Use the `jitter` argument to add some amount of random variation to the
##   location of data points and avoid overplotting.
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

length(dur_data$Duration)
```

```
## [1] 922
```

``` r
length(unique(dur_data$Bird))
```

```
## [1] 45
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
## Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
## Formula: Duration ~ AvgPotency + BlockResponseRate + (1 | Bird) + (1 |      Song) + (1 | Block) + (1 | Aviary)
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
## Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
##  Family: Gamma  ( log )
## Formula: Duration ~ AvgPotency + BlockResponseRate + (1 | Bird) + (1 |      Song) + (1 | Block) + (1 | Aviary)
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
## 	DHARMa nonparametric dispersion test via sd of residuals fitted vs. simulated
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
## Warning: Argument `add.data` is deprecated and will be removed in the future. Please use `show_data` instead.
```

```
## Data points may overlap. Use the `jitter` argument to add some amount of random variation to the
##   location of data points and avoid overplotting.
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
## Warning: Argument `add.data` is deprecated and will be removed in the future. Please use `show_data` instead.
```

```
## Data points may overlap. Use the `jitter` argument to add some amount of random variation to the
##   location of data points and avoid overplotting.
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

length(traj_data[traj_data$Posture == 1,]$Posture)
```

```
## [1] 208
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
## Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
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
## Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
## Formula: PeakHeight ~ BVPotency + BlockResponseRate + (1 | Block) + (1 |      Bird)
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

print(c("proportion of full CSD:",n_csd / n_playbacks))
```

```
## [1] "proportion of full CSD:" "0.486529318541997"
```

``` r
print(c("proportion of partial CSDs:",n_partials / n_playbacks))
```

```
## [1] "proportion of partial CSDs:" "0.0903328050713154"
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
## Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
## Formula: Latency ~ Posture + AvgPotency + BlockResponseRate + (1 | Bird) +      (1 | Song) + (1 | Block)
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
## Warning: Argument `add.data` is deprecated and will be removed in the future. Please use `show_data` instead.
```

```
## Data points may overlap. Use the `jitter` argument to add some amount of random variation to the
##   location of data points and avoid overplotting.
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-24.png)

``` r
## Check if theres a potency effect for latency within partials
latPart_data.partials <- latPart_data[latPart_data$Posture == 0,]
partialModel.posture <- lmer(formula = Latency ~ AvgPotency + BlockResponseRate + (1|Bird),data=latPart_data.partials)
summary(partialModel.posture)
```

```
## Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
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
## Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
## Formula: AvgPotency ~ BlockResponseRate + (1 | Bird) + (1 | Song) + (1 |      Block) + (1 | Aviary)
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
## Warning: Argument `add.data` is deprecated and will be removed in the future. Please use `show_data` instead.
```

```
## Data points may overlap. Use the `jitter` argument to add some amount of random variation to the
##   location of data points and avoid overplotting.
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
```

```
## [1] "doing something..."
## [1] "doing something..."
## [1] "doing something..."
## [1] "doing something..."
## [1] "doing something..."
## [1] "doing something..."
## [1] "doing something..."
## [1] "doing something..."
## [1] "doing something..."
## [1] "doing something..."
## [1] "doing something..."
```

```
## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, : Model failed to converge with
## max|grad| = 0.0100034 (tol = 0.002, component 1)
```

```
## [1] "doing something..."
## [1] "doing something..."
## [1] "doing something..."
## [1] "doing something..."
```

``` r
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
```

```
## [1] 3.816911
```

```
## [1] 0.6054653
```

``` r
print(m.song);print(se.song)
```

```
## [1] 1.577381
```

```
## [1] 0.3019884
```

``` r
print(m.block);print(se.block)
```

```
## [1] -0.2579032
```

```
## [1] 0.0462037
```

``` r
csd_binary.subset <- csd_binary.clean %>% filter(SongID == "BDY" | SongID == "2M")
csd.model.clean <- glmer(formula = Posture ~ SongID + Block + (1|Bird),
                       data = csd_binary.subset, 
                       family=binomial)
summary(csd.model.clean)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
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
## Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
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
## Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
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
## Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
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
## Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
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
## Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
##  Family: binomial  ( logit )
## Formula: Posture ~ AvgPotency * Block + (1 | Bird) + (1 | Aviary) + (1 |      SongSet)
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
sim.pot <- read.csv("./csdModel/tmp_pot.csv")
sim.pot[,"Posture"] <- sim.pot$Response

pot.model <-glmer(formula = Posture ~ AvgPotency * Block +
                     (1|Bird), data = sim.pot, 
                   family=binomial)
summary(pot.model)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
##  Family: binomial  ( logit )
## Formula: Posture ~ AvgPotency * Block + (1 | Bird)
##    Data: sim.pot
## 
##      AIC      BIC   logLik deviance df.resid 
##   1252.0   1276.6   -621.0   1242.0      995 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -2.0316 -0.8286 -0.4892  0.9753  3.2536 
## 
## Random effects:
##  Groups Name        Variance Std.Dev.
##  Bird   (Intercept) 0.02891  0.17    
## Number of obs: 1000, groups:  Bird, 10
## 
## Fixed effects:
##                  Estimate Std. Error z value Pr(>|z|)    
## (Intercept)       0.16603    0.38705   0.429    0.668    
## AvgPotency       -0.17185    0.88797  -0.194    0.847    
## Block            -0.50186    0.08115  -6.185 6.23e-10 ***
## AvgPotency:Block  0.94332    0.18411   5.124 3.00e-07 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) AvgPtn Block 
## AvgPotency  -0.940              
## Block       -0.803  0.765       
## AvgPtncy:Bl  0.774 -0.816 -0.954
```

``` r
sim.pot$Fits <- fitted(pot.model)

ggplot(data=sim.pot,aes(x=Block,y=Posture,group=Song)) + 
  geom_line()
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-33.png)

``` r
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
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-34.png)

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
summary(thresh.model)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
##  Family: binomial  ( logit )
## Formula: Posture ~ AvgPotency * Block + (1 | Bird)
##    Data: sim.thresh
## 
##      AIC      BIC   logLik deviance df.resid 
##   1185.8   1210.3   -587.9   1175.8      995 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -2.4633 -0.7972  0.3579  0.7738  2.5236 
## 
## Random effects:
##  Groups Name        Variance Std.Dev.
##  Bird   (Intercept) 0.008664 0.09308 
## Number of obs: 1000, groups:  Bird, 10
## 
## Fixed effects:
##                  Estimate Std. Error z value Pr(>|z|)    
## (Intercept)      -1.28245    0.37427  -3.427 0.000611 ***
## AvgPotency        4.47949    0.72890   6.146 7.97e-10 ***
## Block            -0.20755    0.07548  -2.750 0.005968 ** 
## AvgPotency:Block -0.01001    0.13835  -0.072 0.942320    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) AvgPtn Block 
## AvgPotency  -0.929              
## Block       -0.821  0.753       
## AvgPtncy:Bl  0.801 -0.842 -0.940
```

``` r
sim.thresh$Fits <- fitted(thresh.model)

ggplot(data=sim.thresh,aes(x=Block,y=Fits,group=Song)) + 
  geom_line()
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-35.png)

``` r
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
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-36.png)

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
## Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
##  Family: binomial  ( logit )
## Formula: Posture ~ B1Potency * Block + (1 | Bird) + (1 | Aviary) + (1 |      SongSet)
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
interact_plot(B1scoreModel,modxvals=c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9),pred=Block,modx=B1Potency)
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-37.png)

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
## Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
##  Family: binomial  ( logit )
## Formula: Posture ~ AvgPotency * BlockResponseRate + (1 | Bird) + (1 |      Aviary) + (1 | SongSet)
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
interact_plot(goodModel,modxvals=c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9),pred=BlockResponseRate,modx=AvgPotency)
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-38.png)

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
## Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
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
## Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
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
mean(whistleLat_data)
```

```
## Warning in mean.default(whistleLat_data): argument is not numeric or logical: returning NA
```

```
## [1] NA
```

``` r
plot(fitted(whistleDurModel),resid(whistleDurModel))
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-39.png)

``` r
qqnorm(resid(whistleDurModel))
qqline(resid(whistleDurModel))
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-40.png)

``` r
### Supplemental Figures! 

## Supplemental Figure One: 
csd_binary.blockMeans <- aggregate(csd_binary.clean$Posture, by = list(csd_binary.clean$Bird,csd_binary.clean$Block), FUN = mean)


df_summary <- csd_binary.clean %>%
  group_by(Aviary, Bird,Block) %>%
  summarize(BlockResponsivity = mean(Posture, na.rm = TRUE)) 
```

```
## `summarise()` has grouped output by 'Aviary', 'Bird'. You can override using the `.groups` argument.
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
## # Groups:   BirdID [45]
##    Posture  Song SongID BirdID  Bird Block Aviary BlockResponsivity GroupResponsivity GroupsRank
##      <dbl> <int> <chr>  <chr>  <int> <int>  <int>             <dbl>             <dbl>      <int>
##  1       1     0 BDY    P1         0     0      0               0.5             0.795          1
##  2       1     1 BOD    P1         0     0      0               0.5             0.795          1
##  3       1     3 LB     P1         0     0      0               0.5             0.795          1
##  4       0     9 DMG    P1         0     0      0               0.5             0.795          1
##  5       0    10 BDY-   P1         0     0      0               0.5             0.795          1
##  6       0    12 LB-    P1         0     0      0               0.5             0.795          1
##  7       1     1 BOD    P1         0     1      0               0.5             0.795          1
##  8       1     9 DMG    P1         0     1      0               0.5             0.795          1
##  9       0    10 BDY-   P1         0     1      0               0.5             0.795          1
## 10       0    12 LB-    P1         0     1      0               0.5             0.795          1
## # ℹ 5,165 more rows
```

``` r
df_summary <- csd_binary.ranks %>%
  group_by(Aviary,Bird) %>%
  summarize(BirdResponsivity = mean(Posture,na.rm=TRUE)) %>%
  mutate(RankInGroup = rank(desc(BirdResponsivity),ties.method="first"))
```

```
## `summarise()` has grouped output by 'Aviary'. You can override using the `.groups` argument.
```

``` r
csd_binary.ranks <- left_join(csd_binary.ranks,df_summary,by=c("Aviary","Bird"))
  
csd_binary.ranks
```

```
## # A tibble: 5,175 × 12
## # Groups:   BirdID [45]
##    Posture  Song SongID BirdID  Bird Block Aviary BlockResponsivity GroupResponsivity GroupsRank
##      <dbl> <int> <chr>  <chr>  <int> <int>  <int>             <dbl>             <dbl>      <int>
##  1       1     0 BDY    P1         0     0      0               0.5             0.795          1
##  2       1     1 BOD    P1         0     0      0               0.5             0.795          1
##  3       1     3 LB     P1         0     0      0               0.5             0.795          1
##  4       0     9 DMG    P1         0     0      0               0.5             0.795          1
##  5       0    10 BDY-   P1         0     0      0               0.5             0.795          1
##  6       0    12 LB-    P1         0     0      0               0.5             0.795          1
##  7       1     1 BOD    P1         0     1      0               0.5             0.795          1
##  8       1     9 DMG    P1         0     1      0               0.5             0.795          1
##  9       0    10 BDY-   P1         0     1      0               0.5             0.795          1
## 10       0    12 LB-    P1         0     1      0               0.5             0.795          1
## # ℹ 5,165 more rows
## # ℹ 2 more variables: BirdResponsivity <dbl>, RankInGroup <int>
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

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-41.png)

``` r
csd_binary.birdResponse <- csd_binary.ranks %>%
  group_by(Bird,Block) %>%
  summarise(Responsivity = mean(BlockResponsivity))
```

```
## `summarise()` has grouped output by 'Bird'. You can override using the `.groups` argument.
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
## # ℹ 462 more rows
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

