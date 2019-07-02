# Keypoints are structured as follows: 

# 3d.csv: 
# 20 * 3 entries, each line is a time point
# Each triplet is the 3d (x,y,z) location of one of the keypoints
# order is as follows: 


import sys
import numpy as np
import pdb

## Only need these lines if you're running remotely...
from scipy.ndimage.filters import gaussian_filter1d as g_filter

import matplotlib
matplotlib.use('Agg')

from matplotlib import pyplot as plt

"""
0-2 Beak Tip
3-5 Keel
6-8 Tailbone
9-  Tip of Tail
12- Left Eye
15- Left Shoulder
18- Left Elbow
21- Left Wrist
24- Left Wing Tip
27- Left Knee
30- Left Ankle
33- Left Heel
36- Right Eye
39- Right Shoulder
42- Right Elbow
45- RIght Wrist
48- Right Wing Tip
51- Right Knee
54- Right Ankle
57- Right Heel
"""

## ...all.csv starts the same, but is followed by x,y,confidence intervals
## For each of the 20 keypoints for each of the cameras

## So 1-60 3D detections, 61-120 2D detections for cameras 1, 121:180 Camera 2, etc

# function to calculate angle between two vectors
def angle(v1,v2):
    #angle = np.arccos(np.dot(v1,v2)/(np.linalg.norm(v1) * np.linalg.norm(v2)))
    angle = np.arctan(np.linalg.norm(v1-v2)/np.linalg.norm(v1))
## Quick hack to keep track of directionality 
    if v2[2] < v1[2]:
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
    def __init__(self,f_name,song='?',bird='?',keep_data=False):
        self.f_name = f_name
        self.song = song
        self.bird = bird
        self.data = np.nan
        self.keep_data = keep_data
        
        self.read_data()

    def read_data(self):

        data = np.genfromtxt(self.f_name,delimiter = ',')
        if self.keep_data:
            self.data = data
        line_count = len(data)
        self.line_count = line_count

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
        #self.all_features = np.array([heads,tails,bodies,wings])
        self.all_features = np.array([heads,tails,bodies])#NOTE:Temporary
        self.feature_list = ['heads','tails','bodies']
        return heads, tails, bodies, wings

    def plot_me(self,show=True,save=False,smooth=False):
        fig, ax = plt.subplots()
        for f in range(len(self.all_features)):
            sigma = 10
            if smooth:
                smoothed = g_filter(self.all_features[f],sigma)
                ax.plot(smoothed,label=self.feature_list[f])
            else:
                ax.plot(self.all_features[f],label=self.feature_list[f])
        ax.legend()
        fig.set_size_inches(8,4)
        fig.tight_layout()
        if show:
            fig.show()
        if save:
            fig_file = self.f_name.split('.')[0] + '.jpg'
            fig.savefig(fig_file,dpi=300)
        return fig,ax


## XXX : Next steps, convert into a class so I can run for all the postures
# 1. This needs latency...
# 2. Do some sort of PCA on postures? 
# 3. Smooth across postures? 
# 4. Focus on paper: is this a useful method for analyzing posture. why? 
if __name__ == "__main__":
    f_name = sys.argv[1]
    my_seq = Sequence2(f_name)
    my_seq.plot_me(show=False,save=True,smooth=True)

#XXX OLD CODE (still haven't set up the git for this...) 
"""
f_name = sys.argv[1]
with open(f_name,'r') as my_file:
    #line_count = len(my_file.readlines())
    line_count = sum(1 for line in my_file) # slightly more memory efficient...?

bodies = np.empty(line_count)
tails = np.empty_like(bodies)
heads = np.empty_like(bodies)
wings = np.empty_like(bodies)

with open(f_name,'r') as my_file:
    for l in range(line_count):
        line = my_file.readline()
        line = line.split(',')
        l_array = np.array(line,dtype=float)

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

        print(tail_theta,head_theta,body_theta,wing_distance)
    print(tails)

fig,ax = plt.subplots()
ax.plot(tails,label='tails')
ax.plot(heads,label='heads')
ax.plot(wings * 100,label='wings')
ax.plot(bodies,label='bodies')
ax.legend()

fig.set_size_inches(10,5)
fig.tight_layout()
fig.savefig('test.jpg',dpi=300)
"""


