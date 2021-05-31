#! /usr/bin/env python

## Little script for running stats: 


import pandas as pd
from pymer4.models import Lmer

from scipy.stats import pearsonr
from scipy.stats import norm
import numpy as np
from copy import copy

def pearsonr_ci(x,y,alpha=0.05):
    ## Copied and lightly adapted from zhiyzuo.github.io/Pearson-Correlation-CI-inPython/
    r,p = pearsonr(x,y)
    r_z = np.arctanh(r)
    se = 1/np.sqrt(x.size-3)
    z = norm.ppf(1-alpha/2)
    lo_z, hi_z = r_z-z*se, r_z+z*se
    lo, hi = np.tanh((lo_z, hi_z))
    n = len(x)
    return r, p, lo, hi,n

if True:
    song_bird_df = pd.read_csv('./song_bird_df2.csv')
    model_sb = Lmer('Posture~ResponseRate + AvgPotency + (1|Bird) + (1|Block) + (1|Song) + (1|Aviary) + (1|SongSet)',data=song_bird_df)
    print(model_sb.fit())
    print('##### Pearson Correlation:')
    print('r,p,lo,hi,n')
    #print(pearsonr(song_bird_df['Posture'],song_bird_df['Block']))
    print('Overall posture:block')
    print(pearsonr_ci(song_bird_df['Posture'],song_bird_df['Block']))
    sub_df = song_bird_df[song_bird_df['Posture'] == 1]
    print('song potency needed:block')
    print(pearsonr_ci(sub_df['AvgPotency'],sub_df['Block']))
    #sub_df = song_bird_df[song_bird_df['Block'] > 1]
    late_df = song_bird_df[song_bird_df['Block'] > 1]
    late_sub_df = sub_df[sub_df['Block'] > 1]
    print('late Potency needed:Block')
    print(pearsonr_ci(late_sub_df['AvgPotency'],late_sub_df['Block']))
    print('Late posture:block')
    print(pearsonr_ci(late_df['Posture'],late_df['Block']))

if True:
    print('#### Effect between Top song and all other songs ####')
    song_df = pd.read_csv('./song_df2.csv')

    model_song = Lmer('S1_ResponseRate ~ All_ResponseRate + (1|Aviary) + (1|SongSet) + (1|Block)',data=song_df)
    print(model_song.fit())

if True:
    print('#### Effect between first block preference and later block preferences ####')
    block_df = pd.read_csv('./block_df2.csv')
    model_block = Lmer('Later_Rate ~ B1_Rate + (1|Aviary) + (1|SongSet) + (1|Song)',data=block_df)
    print(model_block.fit())

if True:
    f_block_df = pd.read_csv('./f_block_df2.csv')
    model_bf = Lmer('Later_Rate~B1_Posture + AvgPotency + (1|Bird) + (1|Song) + (1|Aviary) + (1|SongSet)',data=f_block_df)
    print(model_bf.fit())



if True:
    birdview_df = pd.read_csv('./posture_df5.csv')
    if False:
        birdview_df = birdview_df[birdview_df['Latency'] < 2]
    if True:
        counts = birdview_df['Bird'].value_counts()
        birdview_df = birdview_df.loc[birdview_df['Bird'].isin(counts[counts >= 5].index), :]
    birdview_df = birdview_df.dropna(subset=['Latency','ResponseRate','RelPotency','Bird','Song','Year'])
    #birdview_df = birdview_df.dropna(subset=['Latency','ResponseRate','AvgPotency','Bird','Song','Year'])

    model_blat = Lmer('Latency ~ Block + (1|Bird) + (1|Song) + (1|Year)',data=birdview_df)
    print('#### Latency and block ######')
    print(model_blat.fit())

    model = Lmer('Latency ~ AvgPotency + ResponseRate + (1|Bird) + (1|Song) + (1|Year)',data=birdview_df)
    print(model.fit())

    if False:
        ## Show with Avg potency. Here the confidence interval is similar, but the degrees of freedom are very very low, so the p is non significant
        ## I don't even really know how the confidence interval can be above zero but p non significant, but whatever
        model = Lmer('Latency ~ AvgPotency + ResponseRate + (1|Bird) + (1|Song) + (1|Year)',data=birdview_df)
        print(model.fit())
## This is just duration for Birdview birds. within those 7, we don't see an effect on duration
    if False:
        model_nobird = Lmer('Latency ~ AvgPotency + ResponseRate + (1|Song) + (1|Year)',data=birdview_df)
        print(model_nobird.fit())
    if False:
        model_dur = Lmer('Duration ~ AvgPotency + ResponseRate + (1|Bird) + (1|Song) + (1|Year)',data=birdview_df)
        print(model_dur.fit())

if False:
    birdview_df = pd.read_csv('./posture_df4.csv')
    if True:
        counts = birdview_df['Bird'].value_counts()
        birdview_df = birdview_df.loc[birdview_df['Bird'].isin(counts[counts > 5].index), :]
    birdview_df = birdview_df.dropna(subset=['AutoLatency','AutoDuration','ResponseRate','AvgPotency','Bird','Song','Year'])
    model_autolat = Lmer('AutoLatency ~ AvgPotency + ResponseRate + (1|Bird) + (1|Song) + (1|Year)',data=birdview_df)
    print(model_autolat.fit())

    model_autodur = Lmer('AutoDuration ~ AvgPotency + ResponseRate + (1|Bird) + (1|Song) + (1|Year)',data=birdview_df)
    print(model_autodur.fit())

if False:
    model_lat = Lmer('Latency ~ Block + AvgPotency + ResponseRate + (1|Bird) + (1|Song)',data=birdview_df)
    print(model_lat.fit())
### Print Durations from large group:

## Consistent with the latency above, I use relative potency here
# AvgPotency gives a similar effect with a slightly wider confidence interval. 

if True:
    durations_df = pd.read_csv('./durations_df4.csv')

    if False:
        counts = durations_df['Bird'].value_counts()
        durations_df = durations_df.loc[durations_df['Bird'].isin(counts[counts >=5].index), :]
    if False:
        durations_df = durations_df[durations_df['Duration'] >= 1]
    if True:
        durations_df = durations_df.dropna(subset=['Duration','AvgPotency','ResponseRate','Bird','Song','Aviary'])
    #print('Null values:',durations_df.isnull().sum())
    #model_dur2 = Lmer('LogDuration ~ AvgPotency + ResponseRate + (1|Bird) + (1|Song) + (1|Aviary)',data=durations_df)
    #print(model_dur2.fit())

    model_dur3 = Lmer('Duration ~ AvgPotency + ResponseRate + (1|Bird) + (1|Song) + (1|Aviary)',data=durations_df)

    print(model_dur3.fit())
    
    if False:## Excluding bird as group removes effect
        model_dur4 = Lmer('Duration ~ AvgPotency + ResponseRate + (1|Song) + (1|Aviary)',data=durations_df)
        print(model_dur4.fit())

if False:
    csd_df = pd.read_csv('./csd_df3.csv')
    if False:
        model_csd = Lmer('Partial_CSD ~ AvgPotency + (1|Bird) + (1|Block) + (1|Song) + (1|Aviary) + (1|SongSet)',data=csd_df) 
        print(model_csd.fit())

    csd_df = csd_df[csd_df['ResponseRate'] > 0]
    csd_df = csd_df[csd_df['ResponseRate'] < 1]
    model_pcsd = Lmer('Partial_CSD ~ ResponseRate + (1|Bird) + (1|Aviary)',data=csd_df)
    print(model_pcsd.fit())

if True:
    partials_df = pd.read_csv('./partials_df3.csv')
    model_partials = Lmer('AvgPotency~Threshold + (1|Bird) + (1|Block) + (1|Aviary) + (1|SongSet)',data=partials_df)
    print('***partials***')
    print(model_partials.fit())

if False:
    max_song_df = pd.read_csv('./max_song_df.csv')
    model_ms = Lmer('Posture~ResponseRate + AvgPotency + (1|Bird) + (1|Block) + (1|Song) + (1|Aviary) + (1|SongSet)',data=max_song_df)
    print(model_ms.fit())

if True:
    shape_df = pd.read_csv('./shape_df.csv')
## Drop birds with few values
    for i in pd.unique(shape_df['Bird']):
        if len(shape_df[shape_df['Bird'] == i]) <= 5:
            shape_df.drop(shape_df[shape_df['Bird'] == i].index,inplace=True)
    #model_height = Lmer('MeanHeight ~ AvgPotency + ResponseRate + (1|Bird) + (1|Block) + (1|Song) + (1|Year)',data=shape_df)
    #print(model_height.fit())

    ## Rel potency is stronger here, and since I'm arguing there's not really an effect, I use that
    # Response rate doesn't have an effect here regardless. 
    model_height2 = Lmer('MeanHeight ~ RelPotency + ResponseRate + (1|Bird) + (1|Block) + (1|Song) + (1|Year)',data=shape_df)
    print(model_height2.fit())

## Avg potency looks the same as relative potency
    model_vel = Lmer('MaxVelocity ~ AvgPotency + ResponseRate + (1|Bird) + (1|Block) + (1|Song) + (1|Year)',data=shape_df)
    print(model_vel.fit())
