#! /usr/bin/env python

## Little script for running stats: 


import pandas as pd
from pymer4.models import Lmer

if True:
    birdview_df = pd.read_csv('./posture_df.csv')

    model = Lmer('Latency ~ AvgPotency + ResponseRate + (1|Bird) + (1|Song)',data=birdview_df)

    print(model.fit())

    model_dur = Lmer('Duration ~ AvgPotency + ResponseRate + (1|Bird) + (1|Song)',data=birdview_df)
    print(model_dur.fit())


### Print Durations from large group:

if True:
    durations_df = pd.read_csv('./durations_df.csv')

    model_dur2 = Lmer('LogDuration ~ AvgPotency + ResponseRate + (1|Bird) + (1|Song) + (1|Aviary)',data=durations_df)
    print(model_dur2.fit())

    model_dur3 = Lmer('Duration ~ AvgPotency + ResponseRate + (1|Bird) + (1|Song) + (1|Aviary)',data=durations_df)
    print(model_dur3.fit())

if True:
    song_df = pd.read_csv('./song_df.csv')

    model_song = Lmer('S1_ResponseRate ~ All_ResponseRate + (1|Aviary) + (1|SongSet) + (1|Block)',data=song_df)
    print(model_song.fit())

if True:
    block_df = pd.read_csv('./block_df.csv')
    model_block = Lmer('Later_Rate ~ B1_Rate + (1|Aviary) + (1|SongSet) + (1|Song)',data=block_df)
    print(model_block.fit())

if True:
    csd_df = pd.read_csv('./csd_df.csv')
    model_csd = Lmer('Partial_CSD ~ AvgPotency + (1|Bird) + (1|Block) + (1|Song) + (1|Aviary) + (1|SongSet)',data=csd_df) 
    print(model_csd.fit())

if True:
    partials_df = pd.read_csv('./partials_df.csv')
    model_partials = Lmer('AvgPotency~ResponseRate + (1|Bird) + (1|Block) + (1|Aviary) + (1|SongSet)',data=partials_df)
    print(model_partials.fit())

if True:
    song_bird_df = pd.read_csv('./song_bird_df.csv')
    model_sb = Lmer('Posture~ResponseRate + AvgPotency + (1|Bird) + (1|Block) + (1|Song) + (1|Aviary) + (1|SongSet)',data=song_bird_df)
    print(model_sb.fit())

if True:
    max_song_df = pd.read_csv('./max_song_df.csv')
    model_ms = Lmer('Posture~ResponseRate + AvgPotency + (1|Bird) + (1|Block) + (1|Song) + (1|Aviary) + (1|SongSet)',data=max_song_df)
    print(model_ms.fit())
