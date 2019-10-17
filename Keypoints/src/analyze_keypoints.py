# Keypoints are structured as follows: 

# 3d.csv: 
# 20 * 3 entries, each line is a time point
# Each triplet is the 3d (x,y,z) location of one of the keypoints
# order is as follows: 


import sys, os
import numpy as np
import pandas as pd
import pdb

## Only need these lines if you're running remotely...
from scipy.ndimage.filters import gaussian_filter1d as g_filter

import matplotlib
matplotlib.use('Agg')

from matplotlib import pyplot as plt

BV_offset = 21.8 #ms Thanks bernd! 
BV2_offset =  19.2 #ms
BV_onsets = pd.read_csv('./2019-onsets-birdview.txt',delim_whitespace=True)
BV2_onsets = pd.read_csv('./2019-onsets-birdview-2.txt',delim_whitespace=True)

onset_dict = dict(zip(BV_onsets.iloc[:,0],BV_onsets.iloc[:,1]))
onset_dict2 = dict(zip(BV2_onsets.iloc[:,0],BV2_onsets.iloc[:,1]))
"""
0-2 Beak Tip 0
3-5 Keel 1
6-8 Tailbone 2
9-  Tip of Tail 3
12- Left Eye 4
15- Left Shoulder 5
18- Left Elbow 6
21- Left Wrist 7
24- Left Wing Tip 8
27- Left Knee 9
30- Left Ankle 10
33- Left Heel 11
36- Right Eye 12
39- Right Shoulder 13
42- Right Elbow 14
45- RIght Wrist 15
48- Right Wing Tip 16
51- Right Knee 17
54- Right Ankle 18
57- Right Heel 19
"""

## ...all.csv starts the same, but is followed by x,y,confidence intervals
## For each of the 20 keypoints for each of the cameras

## So 1-60 3D detections, 61-120 2D detections for cameras 1, 121:180 Camera 2, etc

# function to calculate angle between two vectors
def angle(v1,v2, acute=False):
    #angle = np.arccos(np.dot(v1,v2)/(np.linalg.norm(v1) * np.linalg.norm(v2)))
    angle = np.arctan(np.linalg.norm(v1-v2)/np.linalg.norm(v1))
## Quick hack to keep track of directionality 
    if v2[2] < v1[2] and acute:
        angle = angle * -1
    deg_angle = np.round(np.degrees(angle),4)
    return deg_angle

# calcuate vector between two points, and the projection of v2 onto the horizontal plane.
def point_angle(p1,p2):
    pz = [p2[0],p2[1],p1[2]]
    v1 = pz - p1
    v2 = p2 - p1
    return angle(v1,v2)

class Sequence2:
    def __init__(self,f_name,song='?',bird='?',keep_data=True):
        self.f_name = f_name
        self.song = song
        self.bird = bird
        self.data = np.nan
        self.keep_data = keep_data
        
        self.read_data()

    def read_data(self):
        data = np.genfromtxt(self.f_name,delimiter = ',')
        nice_data = np.reshape(data,[len(data),20,3])
        if self.keep_data:
            self.data = data
            self.nice_data = nice_data
        line_count = len(data)
        self.line_count = line_count
        """
        bodies = np.empty(line_count)
        tails = np.empty_like(bodies)
        heads = np.empty_like(bodies)
        wings = np.empty_like(bodies)

        for l in range(line_count):
## This would be a little faster with proper array methods
## But this is honestly probably more intuitive and legible
            l_array = data[l]
## Calculate Tail angle
            tail_b = l_array[6:9]
            tail_t = l_array[9:12]

            tail_theta = point_angle(tail_b,tail_t)
            tails[l] = tail_theta

## Calculate Head Angle
            eye_l = l_array[12:15]
            eye_r = l_array[36:39]
            eye_m = np.mean([eye_l,eye_r],0)
            beak = l_array[0:3]
            head_theta = point_angle(beak,eye_m)
            heads[l] = head_theta

## Calculate wing distance
            l_wing = l_array[24:27]
            r_wing = l_array[48:51]
            wing_distance = np.round(np.linalg.norm(l_wing - r_wing),4)
            wings[l] = wing_distance

## Calculate body angle
            shoulder_l = l_array[15:18]
            shoulder_r = l_array[39:42]
            shoulder_m = np.mean([shoulder_l,shoulder_r],0)
            body_theta = point_angle(shoulder_m,tail_b)
            bodies[l] = body_theta

        self.bodies = bodies
        self.tails = tails
        self.heads = heads
        self.wings = wings
        """
## 9 Body metrics (based off of reliable metrics)
## Should probably do this with matrix notation to avoid all the loops (or do it fast to save my own time)

## Smoothed central point for point of reference. a bit hacky...
        eyes_l = nice_data[:,5]
        eyes_r = nice_data[:,12]
        eyes_m = np.mean([eyes_l,eyes_r],0)
        beaks = nice_data[:,0]
        self.center_point = g_filter(np.mean(nice_data[:,[1,2,5,6,7,8,9,13,14,15,16,17]],1),5)
        #print(np.shape(self.center_point))
        self.top_point = np.mean(nice_data[:,[5,6,7,13,14,15]],1)
        self.bottom_point = np.mean(nice_data[:,[2,8,9,16,17]],1)
        #import pdb
        #pdb.set_trace()
        self.body_angle = np.array([point_angle(self.top_point[i],self.bottom_point[i]) for i in range(len(data))])
        #self.head_rho =  np.array([angle(x,y) for x,y in zip(np.array(eyes_l - eyes_r),np.array(eyes_m - self.top_point))])
        #self.head_theta = np.array([angle(x,y) for x,y in zip(np.array(beaks - eyes_m),np.array(eyes_m - self.top_point))])
        self.head_rho = np.array([point_angle(eyes_r[i],eyes_l[i]) for i in range(len(data))])
        self.head_theta = np.array([point_angle(beaks[i],eyes_m[i]) for i in range(len(data))])
        self.wing_distance = np.array([np.linalg.norm(np.abs(nice_data[i,4] - nice_data[i,16])) for i in range(len(data))])
        self.tail_rho = [angle(x,y) for x,y in zip(np.array(nice_data[:,3] - nice_data[:,2]),np.array(nice_data[:,19] - nice_data[:,11]))]
        self.tail_theta = [angle(x,y) for x,y in zip(np.array(nice_data[:,3] - nice_data[:,2]),nice_data[:,2] - self.center_point)]
        self.tail_height = nice_data[:,3,2]
        self.right_mean = np.mean(nice_data[:,19],0)
        self.left_mean = np.mean(nice_data[:,11],0)
        self.right_disp = np.array([np.round(np.linalg.norm(nice_data[i,19] - self.right_mean)) for i in range(len(data))])
        self.left_disp = np.array([np.round(np.linalg.norm(nice_data[:,11] - self.left_mean)) for i in range(len(data))])

        self.all_features = np.array([self.body_angle,self.head_rho,self.head_theta,self.wing_distance,self.tail_rho,self.tail_theta,self.right_disp,self.left_disp,self.tail_height])

        self.norm_features = np.transpose(np.transpose(self.all_features) / np.max(self.all_features,1))
        #import pdb
        #pdb.set_trace()
        self.feature_list = ['body angle','head rotation','head angle','wing distance','tail rotation','tail angle','right leg','left leg','tail height']
        return 0

    def plot_me(self,show=True,save=False,smooth=False):
        fig, ax = plt.subplots()
        if True:
            plot_data = self.norm_features
        else:
            plot_data = self.all_features
        import pdb
        pdb.set_trace()
        for f in range(len(plot_data)):
            sigma = 2
            if smooth:
                smoothed = g_filter(plot_data[f],sigma)
                ax.plot(smoothed,label=self.feature_list[f])
            else:
                ax.plot(plot_data[f],label=self.feature_list[f])
        #ax.plot(self.tail_height,color='black')
        ax.legend()
        ax.set_xlabel('Time (ms)')
        ax.set_ylabel('Angle (degrees)')
        #ax.set_ylim([.20,.40])
        fig.set_size_inches(8,4)
        fig.tight_layout()
        if show:
            fig.show()
            prompt = raw_input('return?')
        if save:
            fig_file = self.f_name.split('.')[0] + '.jpg'
            fig_file = 'test2.png'
            print('Saving to:',fig_file)
            fig.savefig(fig_file,dpi=300)
        return fig,ax


## XXX : Next steps, convert into a class so I can run for all the postures
# 1. This needs latency...
# 2. Do some sort of PCA on postures? 
# 3. Smooth across postures? 
# 4. Focus on paper: is this a useful method for analyzing posture. why? 
if __name__ == "__main__":
    #f_name = sys.argv[1]
    birdview2 = False
    if birdview2:
        offset = BV2_offset
        onset_dict = onset_dict2
    else:
        offset = BV_offset 
    outdir = './tail_heights'
    data_dir = '/data/birds/postures/keypoints/2017_detections'
    for seq_dir in os.listdir(data_dir):
        print('working on',seq_dir)
        if '201' in seq_dir:
            f_name = data_dir + '/' + seq_dir + '/detections_3d.csv'
            seq = Sequence2(f_name)
            my_color = 'green'
            fig, ax = plt.subplots()
            if seq_dir in os.listdir('./video_timestamps'):
                timestamps = np.genfromtxt('./video_timestamps/' + seq_dir + '.wav.txt',delimiter=' ')
                ts = timestamps - timestamps[0]
            else:
                ts = np.arange(len(seq.tail_height))
                timestamps = ts
                print('timestamps not found for',seq_dir)
                #pdb.set_trace()
                my_color = 'blue'
            if seq_dir + '.wav.bag' in onset_dict.keys():
                if onset_dict[seq_dir + '.wav.bag'] == 'has':
                    #pdb.set_trace()
                    print('no audio for',seq_dir)
                    my_color = 'red'
                else:
                    onset = float(onset_dict[seq_dir + '.wav.bag']) - (float(offset) * .001)
                    stim_index = np.argmin(timestamps[:,1] >= onset)
                    ax.axvline(ts[stim_index],color='r')
            else:
                print('onset not found for ',seq_dir)
                my_color = 'blue'
            smooth_tail = g_filter(seq.tail_height,5)
            ax.plot(ts,smooth_tail,color=my_color)
            ax.set_ylim([0.0,.4])
            fig.savefig(outdir + '/' + seq_dir + '.png')
            plt.cla()
