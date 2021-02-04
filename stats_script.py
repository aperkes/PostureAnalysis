#! /usr/bin/env python

## Little script for running stats: 


import pandas as pd
from pymer4.models import Lmer

from scipy.stats import pearsonr
if False:
    birdview_df = pd.read_csv('./posture_df2.csv')
    if True:
        counts = birdview_df['Bird'].value_counts()
        birdview_df = birdview_df.loc[birdview_df['Bird'].isin(counts[counts > 5].index), :]
    birdview_df = birdview_df.dropna(subset=['Latency','Duration','ResponseRate','AvgPotency','Bird','Song','Year'])

    model = Lmer('Latency ~ AvgPotency + ResponseRate + (1|Bird) + (1|Song) + (1|Year)',data=birdview_df)
    print(model.fit())

    if True:
        model_nobird = Lmer('Latency ~ AvgPotency + ResponseRate + (1|Song) + (1|Year)',data=birdview_df)
        print(model_nobird.fit())
    model_dur = Lmer('Duration ~ AvgPotency + ResponseRate + (1|Bird) + (1|Song) + (1|Year)',data=birdview_df)
    print(model_dur.fit())

if False:
    birdview_df = pd.read_csv('./posture_df2.csv')
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

if False:
    durations_df = pd.read_csv('./durations_df2.csv')

    if True:
        counts = durations_df['Bird'].value_counts()
        durations_df = durations_df.loc[durations_df['Bird'].isin(counts[counts > 5].index), :]
    if True:
        durations_df = durations_df.dropna(subset=['Duration','AvgPotency','ResponseRate','Bird','Song','Aviary'])
    print('Null values:',durations_df.isnull().sum())
    #model_dur2 = Lmer('LogDuration ~ AvgPotency + ResponseRate + (1|Bird) + (1|Song) + (1|Aviary)',data=durations_df)
    #print(model_dur2.fit())

    model_dur3 = Lmer('Duration ~ AvgPotency + ResponseRate + (1|Bird) + (1|Song) + (1|Aviary)',data=durations_df)
    print(model_dur3.fit())

    model_dur4 = Lmer('Duration ~ AvgPotency + ResponseRate + (1|Song) + (1|Aviary)',data=durations_df)
    print(model_dur4.fit())
if False:
    song_df = pd.read_csv('./song_df.csv')

    model_song = Lmer('S1_ResponseRate ~ All_ResponseRate + (1|Aviary) + (1|SongSet) + (1|Block)',data=song_df)
    print(model_song.fit())

if False:
    block_df = pd.read_csv('./block_df.csv')
    model_block = Lmer('Later_Rate ~ B1_Rate + (1|Aviary) + (1|SongSet) + (1|Song)',data=block_df)
    print(model_block.fit())

if False:
    f_block_df = pd.read_csv('./f_block_df.csv')
    model_bf = Lmer('Later_Rate~B1_Posture + AvgPotency + (1|Bird) + (1|Song) + (1|Aviary) + (1|SongSet)',data=f_block_df)
    print(model_bf.fit())

if False:
    csd_df = pd.read_csv('./csd_df.csv')
    model_csd = Lmer('Partial_CSD ~ AvgPotency + (1|Bird) + (1|Block) + (1|Song) + (1|Aviary) + (1|SongSet)',data=csd_df) 
    print(model_csd.fit())

if False:
    partials_df = pd.read_csv('./partials_df2.csv')
    model_partials = Lmer('AvgPotency~Threshold + (1|Bird) + (1|Block) + (1|Aviary) + (1|SongSet)',data=partials_df)
    print('***partials***')
    print(model_partials.fit())

if True:
    song_bird_df = pd.read_csv('./song_bird_df.csv')
    model_sb = Lmer('Posture~ResponseRate + AvgPotency + (1|Bird) + (1|Block) + (1|Song) + (1|Aviary) + (1|SongSet)',data=song_bird_df)
    print(model_sb.fit())
    print('##### Pearson Correlation:')
    print(pearsonr(song_bird_df['Posture'],song_bird_df['Block']))

if False:
    max_song_df = pd.read_csv('./max_song_df.csv')
    model_ms = Lmer('Posture~ResponseRate + AvgPotency + (1|Bird) + (1|Block) + (1|Song) + (1|Aviary) + (1|SongSet)',data=max_song_df)
    print(model_ms.fit())

if False:
    shape_df = pd.read_csv('./shape_df.csv')
    model_height = Lmer('MeanHeight ~ AvgPotency + ResponseRate + (1|Bird) + (1|Block) + (1|Song) + (1|Year)',data=shape_df)
    print(model_height.fit())

    model_height2 = Lmer('MeanHeight ~ RelPotency + ResponseRate + (1|Bird) + (1|Block) + (1|Song) + (1|Year)',data=shape_df)
    print(model_height2.fit())

    model_vel = Lmer('MaxVelocity ~ RelPotency + ResponseRate + (1|Bird) + (1|Block) + (1|Song) + (1|Year)',data=shape_df)
    print(model_vel.fit())
