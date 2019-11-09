#! /usr/bin/env python

import os
from fastdtw import fastdtw
from fastdtw import dtw

from scipy.spatial.distance import euclidean,pdist,cdist
from scipy.ndimage.filters import gaussian_filter1d
import numpy as np
import pickle
import random
import pdb
import datetime
import pandas as pd
import path


### This needs to bring in the 20x point data
## Store everything necessary
## Perform internal analyses (i.e. build PCA structure) 

## Set file locations ###
bv2TimeDir = '/data/birds/postures/birdview2-2019/2019_timestamps_birdview-2'
bvTimeDir = '/data/birds/postures/birdview-2019/2019_timestamps_birdview'
bvOffsetsCSV = '/data/birds/postures/birdview-2019/2019-onsets-birdview.txt'
bv2OffsetsCSV = '/data/birds/postures/birdview2-2019/2019-onsets-birdview-2.txt'
databaseCSV = '/data/birds/postures/presentation_info.csv'
songfile = './song_list.txt'

## Read in Files ###
song_list = np.genfromtxt(songfile,dtype=str)
## All of the meta data for all of the videos
video_df = pd.read_csv(databaseCSV)

## Pandas dataframes with the offsets for each posture
col_names = ['FileName','Offset','P-text','Power','R-text','Roll']
col_types = [str,float,str,float,str,float]
bv_offsets = pd.read_csv(bvOffsetsCSV,delim_whitespace=True, names=col_names)
bv2_offsets = pd.read_csv(bv2OffsetsCSV,delim_whitespace=True, names=col_names)

## Dictionaries that provide an np array of rostime for each video frame
bvTimeDict = {}
bv2TimeDict = {}
for f in os.listdir(bv2TimeDir):
    file_name = f.split('/')[-1]
    seq_name = file_name.split('.')[0]
    bv2TimeDict[seq_name] = np.genfromtxt(bv2TimeDir + '/' + file_name,usecols=[1])

for f in os.listdir(bvTimeDir):
    file_name = f.split('/')[-1]
    seq_name = file_name.split('.')[0]
    bvTimeDict[seq_name] = np.genfromtxt(bvTimeDir + '/' + file_name,usecols=[1])

## Handy container for the sequence and its metrics
STD = 3

def angle(v1,v2):
    #angle = np.arccos(np.dot(v1,v2)/(np.linalg.norm(v1) * np.linalg.norm(v2)))
    angle = np.arctan(np.linalg.norm(v1-v2)/np.linalg.norm(v1))
## Quick hack to keep track of directionality 
    angle = np.round(np.degrees(angle),4)
    if v2[2] < v1[2]:
        angle = angle * -1
    return angle

class Trajectory:
    def __init__(self,file_path,index=-1,use_smooth=True,smooth_std=STD,sparse=True):
        self.index = index
        if 'birdview2' in file_path or 'birdview-2' in file_path:
            self.machine = 'birdview-2'
        else:
            self.machine = 'birdview'
        self.file_path = file_path
        self.file_name = file_path.split('/')[-2]
        self.seq_name = self.file_name.split('.')[0]
        self.rough_data = np.load(file_path)
        self.smooth_data = gaussian_filter1d(self.rough_data,smooth_std,0)
        if use_smooth:
            self.data = self.smooth_data
        else:
            self.data = self.rough_data
        self.time_string = self.seq_name.split('_')[0]
        print(self.seq_name,self.time_string)
        self.save_time = datetime.datetime.strptime(self.time_string,'%Y-%m-%d-%H-%M-%S')

        self.get_meta()
        self.all_angles,_ = self.define_angles()

        ## This works regardless on dimensions!
        self.vel = np.diff(self.data,axis=0)
        self.acc = np.diff(self.vel,axis=0)
        self.vel_t = self.ts[:-1]
        self.acc_t = self.ts[:-2]

        if True:
            self.define_window()

    def get_meta(self):

        try:
            #pdb.set_trace()
            meta_line = video_df.loc[video_df['SeqName'] == self.seq_name]
            self.bird = meta_line['Bird'].values[0]
            self.song = meta_line['Song'].values[0]
            if pd.isnull(self.song):
                #pdb.set_trace()
                for s in song_list:
                    if s in self.file_name:
                        if s + '-' in self.file_name:
                            self.song = s + '-'
                        else:
                            self.song = s
                        break
            self.posture = meta_line['Posture'].values[0]
            self.sample_rate = meta_line['FrameRate'].values[0]
            self.notes = meta_line['Notes'].values[0]
            if pd.isnull(self.bird):
                self.bird = 'unknown'
            if pd.isnull(self.notes):
                self.notes = 'none'
            self.meta = meta_line
            if np.isnan(self.sample_rate):
                self.sample_rate = 50
        except:
            pdb.set_trace()
            print('could not find meta data for',self.file_path)
            self.bird = 'Unknown'
            self.song = 'Unknown'
            self.posture = -1 
            self.sample_rate = 50
            self.notes = 'none'
        if self.machine == 'birdview':
            time_dict = bvTimeDict
            audio_offsets = bv_offsets
        elif self.machine == 'birdview-2':
            time_dict = bv2TimeDict
            audio_offsets = bv2_offsets
        try:
            self.timestamps = time_dict[self.seq_name]
            self.timestamp = self.timestamps[0]
            self.offset = audio_offsets.loc[audio_offsets['FileName'] == self.file_name + '.wav.bag']['Offset'].values[0]
            self.power = audio_offsets.loc[audio_offsets['FileName'] == self.file_name + '.wav.bag']['Power'].values[0]
            if self.power < 1:
                self.offset = self.timestamp
            if not isinstance(self.offset,float):
                print(self.offset)
                pdb.set_trace()
                self.offset = float(self.offset)
            self.ts = self.timestamps - self.offset
        except:
            #pdb.set_trace()
            print('Timestamps not found for',self.file_path)
            print('Using file time, offset at 0th frame')
            DT = datetime.datetime(1970,1,1,00,00,00)
            my_datetime = self.save_time - datetime.timedelta(minutes=3)
            self.timestamp = (my_datetime - DT).total_seconds()
            n_frames = len(self.data)
            self.timestamps = np.arange(self.timestamp,self.timestamp + n_frames * self.sample_rate,1/self.sample_rate)
            self.ts = self.timestamps - self.timestamps[0]
            self.offset = 0
        self.time = datetime.datetime.fromtimestamp(self.timestamp).strftime('%H:%M')
        self.hour = self.time.split(':')[0]
        self.date = datetime.datetime.fromtimestamp(self.timestamp).strftime('%Y-%m-%d')

## This defines the response window.
## Just a place holder currently...
    def define_window(self):
        if self.offset == 0:
            ## Can't really do this. 
            return 0 

        ys = self.data[:,3,2] ## Decide on the 1d transform. This could be PCA
        vel = np.diff(ys)
        BASE_WINDOW = .5 * -1 ## Amount of time before
        N_STD = 5 ## How many std defines the baseline window
        REACT_WINDOW = 2
        self.tzero = min(self.ts[self.ts >= 0])
        self.tzero_index = np.where(self.ts == self.tzero)

## Baseline:
        self.t_baseline = min(self.ts[self.ts >= -1 * BASE_WINDOW])
        self.t_baseline_index = np.where(self.ts == self.t_baseline)

        self.baseline = np.mean(self.smooth_data[np.logical_and(self.ts >= BASE_WINDOW,self.ts <= 0.0)])
        self.baseline_std = np.std(self.smooth_data[np.logical_and(self.ts >= BASE_WINDOW,self.ts <= 0.0)])
## Calculage first motion
        first_motion_index = np.argmax(np.logical_and(self.ts > 0, ys > (self.baseline + N_STD * self.baseline_std)))

        first_motion = self.ts[first_motion_index]

        self.t_latency = self.ts[first_motion_index]
        self.t_latency_index = first_motion_index
### Calculate VMAX
        reaction_range = (self.ts > 0) & (self.ts < REACT_WINDOW)
        vel_reaction_window = vel[reaction_range[:-1]]

        self.peak_vel = max(vel_reaction_window)
        self.peak_vel_index = np.where(vel == self.peak_vel)
        self.t_peak_vel = self.ts[self.peak_vel_index]
## CAlculate stabilization (proxy for peak)
        neg_vel = np.where(vel <= 0)[0]
        self.stable_vel_index = neg_vel[neg_vel > self.peak_vel_index[0]][0]
        self.t_stable_vel = self.ts[self.stable_vel_index]
## Refraction
        refraction_range = ys[self.ts > self.ts[self.stable_vel_index]]
        peak = ys[self.stable_vel_index]
        half_peak = peak - (peak - self.baseline) / 2
        refraction_value = refraction_range[np.argmax(refraction_range < half_peak)]
        self.refraction_index = np.where(ys == refraction_value)
        self.t_refraction = self.ts[self.refraction_index]

## Duration
        self.duration = np.asscalar(self.t_refraction - self.t_latency)

        self.response_range = (self.ts > 0) & (self.ts < self.t_refraction)
        self.posture_range = (self.ts > self.t_peak_vel) & (self.ts < self.t_refraction)
        self.response_data = self.data[self.response_range]
        self.response_ts = self.ts
        return 0

## This builds the data vectors (return 4 types)
# One is a bunch of angles (and velocities?)
# the other is a set of pairwise distances (and velocities?)
    def build_vectors(self):
        all_angles,_ = self.define_angles()
        all_distances,_ = self.define_distances()
        return all_distances,all_angles
         
    def define_distances(self,CDIST=False):
        T,P,D = np.shape(self.data)
        X_len = int(P * (P - 1) / 2)
        all_distances = np.empty([T,X_len])
        self.moving_distances = np.empty([T,P])
        pairwise_matrix = 0
        for t in range(T):
            all_distances[t] = pdist(self.data[t])
            self.moving_distances[t] = np.linalg.norm((self.data[t] - self.data[t-1]),axis=1)
        self.moving_distances[0] = np.zeros(P)
        if CDIST:
            pairwise_matrix = np.empty([T,P,P])
            for t in range(T):
                pairwise_matrix[t] = cdist(data[t],data[t])
        return all_distances,pairwise_matrix

    def define_angles(self):
        eye_center = np.mean([self.data[:,4],self.data[:,12]],0)
        shoulder_center = np.mean([self.data[:,5],self.data[:,13]],0)
        
        full_data = np.empty([len(self.data),22,3])
        full_data[:,:20] = self.data
        full_data[:,20] = eye_center
        full_data[:,21] = shoulder_center    
        """
        0-2 Beak Tip 0 BT
        3-5 Keel 1 KE
        6-8 Tailbone 2 TB
        9-  Tip of Tail 3 TT
        12- Left Eye 4 LE
        15- Left Shoulder 5 LS
        18- Left Elbow 6 LB
        21- Left Wrist 7 LW
        24- Left Wing Tip 8 LT
        27- Left Knee 9 LK
        30- Left Ankle 10 LA
        33- Left Heel 11 LH
        36- Right Eye 12 RE
        39- Right Shoulder 13 RS
        42- Right Elbow 14 RB
        45- RIght Wrist 15 RW
        48- Right Wing Tip 16 RT
        51- Right Knee 17 RK
        54- Right Ankle 18 RA
        57- Right Heel 19 RH
        Eye Center (mean of 4,12): 20 EC 
        Neck (shoulder Center, mean of 5,13): 21 NC
        """
## Define all the angles!!! =/
# Calculate all the angles!!! :D
        angle_dict = {
            'neck-eye-beak':0,
            'eye-eye_center-horizontal':1,
            'beak-eye_center-sagital':2,
            'left_elbow-wrist-tip':3,
            'right_elbow-wrist-tip':4,
            'left_wrist-neck-wrist':5,
            'beak-neck-tailbone':6,
            'neck-tailbone-tail':7,
            'left_knee-tailbone-knee':8,
            'tailbone-left_knee-ankle':9,
            'tailbone-right_knee-ankle':10,
            'left_knee-ankle-heel':11,
            'right_knee-ankle-heel':12,
        }
        all_angles = np.zeros([len(self.data),14])
# Neck-eye-beak (NC_EC_BT): 20-21,20-0 
        EC_NC = full_data[:,20] - full_data[:,21]
        EC_BT = full_data[:,20] - full_data[:,0]
        NC_EC_BT = [angle(v1,v2) for v1,v2 in zip(EC_NC,EC_BT)]
        all_angles[:,0] = NC_EC_BT

# Eye-Eye_center-horizontal plane: 20-12(with z20),20-12
        horizontal_eye = np.array(full_data[:,12])
        horizontal_eye[:,2] = full_data[:,20,2]
        EC_LE = full_data[:,20] - full_data[:,12]
        EC_HP = full_data[:,20] - horizontal_eye
        LE_EC_HP = [angle(v1,v2) for v1,v2 in zip(EC_LE,EC_HP)]
        all_angles[:,1] = LE_EC_HP

# Beak-eye_center-sagital plane
        NC_TB = full_data[:,21] - full_data[:,2]
        KE_TB = full_data[:,1] - full_data[:,2]
        sagital_plane = np.cross(NC_TB,KE_TB)
        BK_EC_SP = [angle(v1,v2) for v1,v2 in zip(EC_BT,sagital_plane)]
        all_angles[:,2] = BK_EC_SP

# Elbow-wrist-tip: 7-6,7-8 
        LW_LB = full_data[:,7] - full_data[:,6]
        LW_LT = full_data[:,7] - full_data[:,8]
        LB_LW_LT = [angle(v1,v2) for v1,v2 in zip(LW_LB,LW_LT)]
        all_angles[:,3] = LB_LW_LT

# Right EWT: 15-14, 15-16
        RW_RB = full_data[:,15] - full_data[:,14]
        RW_RT = full_data[:,15] - full_data[:,16]
        RB_RW_RT = [angle(v1,v2) for v1,v2 in zip(RW_RB,RW_RT)]
        all_angles[:,4] = RB_RW_RT

# Wrist-Neck-wrist: 21-7,21-15
        NC_LW = full_data[:,21] - full_data[:,7]
        NC_RW = full_data[:,21] - full_data[:,15]
        LW_NC_RW = [angle(v1,v2) for v1,v2 in zip(NC_LW,NC_RW)]
        all_angles[:,5] = LW_NC_RW

# Beak-Neck-Tailbone: 21-2,21-20
        NC_TB = full_data[:,21] - full_data[:,2]
        NC_BT = full_data[:,21] - full_data[:,0]
        BT_NC_TB = [angle(v1,v2) for v1,v2 in zip(NC_BT,EC_NC)]
        all_angles[:,6] = BT_NC_TB

# Neck-Tailbone-Tail: 2-21,2-3
        TB_NC = full_data[:,2] - full_data[:,21]
        TB_TT = full_data[:,2] - full_data[:,3]
        NC_TB_TT = [angle(v1,v2) for v1,v2 in zip(TB_NC,TB_TT)]
        all_angles[:,7] = NC_TB_TT

# Knee-Tailbone-Knee: 2-9,2-17
        TB_LK = full_data[:,2] - full_data[:,9]
        TB_RK = full_data[:,2] - full_data[:,17]
        LK_TB_RK = [angle(v1,v2) for v1,v2 in zip(TB_LK,TB_RK)]
        all_angles[:,8] = LK_TB_RK

# Tailbone-Knee-Ankle: 9-2,9-10
        LK_TB = full_data[:,9] - full_data[:,2]
        LK_LA = full_data[:,9] - full_data[:,10]
        TB_LK_LA = [angle(v1,v2) for v1,v2 in zip(LK_TB,LK_LA)]
        all_angles[:,9] = TB_LK_LA

# Right TKA: 17-2,17-18
        RK_TB = full_data[:,17] - full_data[:,2]
        RK_RA = full_data[:,17] - full_data[:,18]
        TB_RK_RA = [angle(v1,v2) for v1,v2 in zip(RK_TB,RK_RA)]
        all_angles[:,10] = TB_RK_RA

# Knee-Ankle-Heel: 10-9,10-11
        LA_LK = full_data[:,10] - full_data[:,9]
        LA_LH = full_data[:,10] - full_data[:,11]
        LK_LA_LH = [angle(v1,v2) for v1,v2 in zip(LA_LK,LA_LH)]
        all_angles[:,11] = LK_LA_LH

# Right KAH: 18-17,18-19
        RA_RK = full_data[:,18] - full_data[:,17]
        RA_RH = full_data[:,18] - full_data[:,19]
        RK_RA_RH = [angle(v1,v2) for v1,v2 in zip(RA_RK,RA_RH)]
        all_angles[:,12] = RK_RA_RH 
        #self.full_data = full_data
        return all_angles,full_data

def parse_postures(posture_dir):
    seqs = []
    for seq_file in os.listdir(posture_dir):
        if '.csv' in p:
            file_path = posture_dir + p
            seq_name = seq_file.split('.')[0]
            self.seq_data = np.genfromtxt(file_path)
            song_name = songDict[seq_name]
            Seq = Sequence(self.seq_data,seq_name,song_name)
            if 'm' in Seq.song_name:
                pass
            else:
                seqs.append(Seq.ys)
    return seqs
  
if __name__ == "__main__":
## Read through all the postures and build a list of all of them
    posture_dir = '/data/birds/postures/'
    birdview_dir = 'birdview-2019/'
    birdview2_dir = 'birdview2-2019/'

    birdview_list = sorted(os.listdir(posture_dir + birdview_dir))
    birdview2_list = sorted(os.listdir(posture_dir + birdview2_dir))
    seqs = []
    bird_list = [birdview_list,birdview2_list]
    bird_dirs = [birdview_dir,birdview2_dir]
    count = 0
    seqs_size = 0
## NOTE: This works, but only just. 
## Loading all the seqs into memory takes up the majority of my RAM
## I think it pulls like 5Gb, and saving the seqs.dat breaks my computer
## Once I fix the lost data files, or include 2018, this is probably unsustainable.
    for i in range(2):
        if i == 0:
            continue
        posture_list = bird_list[i]
        for s in range(len(posture_list)):
            if '.wav.mp4' in posture_list[s]:
                trimmed_path = posture_list[s].split('.')[0]
                file_path = posture_dir + bird_dirs[i] + trimmed_path + '/pred_keypoints_3d.npy'
                if not os.path.exists(file_path):
                    print('no 3d keypoints for',file_path)
                    print('skipping to next posture...')
                    continue

                Seq = Trajectory(file_path,index=count)
                if Seq.posture != 1 or "floor" in Seq.notes:
                    print('Floor or non-posture, skipping....')
                    continue
                #""" Comment these lines out to do old approach
                elif count == 0:
                    angle_vector = Seq.all_angles
                    distance_vector,_ = Seq.define_distances()
                else:
                    new_distances,_ = Seq.define_distances()
                    angle_vector = np.vstack([angle_vector,Seq.all_angles])
                    distance_vector = np.vstack([distance_vector,new_distances])
                #"""
                #seqs.append(Seq)
                with open('./SeqClasses/' + Seq.seq_name + '.obj','wb') as p:
                    pickle.dump(Seq,p,pickle.HIGHEST_PROTOCOL)
                count += 1 

            else:
                continue

    """
# Even for pretty small subsets, this makes it up to 4gb, which is really too big. 
# I think I'll need to save the seq's individually and load them as needed. 
    with open('posture_seqs.dat','wb') as p:
        pickle.dump(seqs,p,pickle.HIGHEST_PROTOCOL)
    """
    """
## Build angle and distance vectors for seqs
    angle_vector = seqs[0].all_angles
    distance_vector = seqs[0].all_distances

    for s in range(1,len(seqs)):
        pdb.set_trace()
        angle_vector = np.vstack([angle_vector,seqs[s].all_angles])
        distance_vector = np.vstack([distance_vector,seqs[s].all_distances])
    """ 
## Great, now do the PCA!
## Debut everything else first though...

## I should save these...
    #np.save('posture_vector.npy',distance_vector)
    #np.save('posture_angle_vector.npy',angle_vector)
    print("I did it!")

