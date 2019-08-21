#! /usr/bin/env pyton

## Code to take 3d points and map into posture space
## Written by Ammon Perkes. 
## Contact perkes.ammon@gmail.com for questions


import sys, os
import numpy as np
from matplotlib import pyplot as plt
from mpl_toolkits import mplot3d

from scipy import stats
from scipy.ndimage.filters import gaussian_filter as g_filter
from sklearn.decomposition import PCA

## For Reference: 
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
def angle(v1,v2):
    angle = np.arctan(np.linalg.norm(v1-v2)/np.linalg.norm(v1))
# Quick hack to track directionality
    if v2[2] < v1[2]:
        angle = angle * -1
    deg_angle = np.round(np.degrees(angle),4)


class PoseTrajectory:
    def __init__(self,f_name,song='?',bird='?',keep_data=True,save_data=False):
        self.f_name = f_name
        self.song = song
        self.bird = bird
        self.data = np.nan
        self.keep_data = keep_data
        self.save_data = save_data 
        full_data = self.read_data()
        self.all_angles = self.get_angle_data(full_data)

## This might be unnecessary, but it seems nice to have it all internal
    def angle(self,v1,v2):
## This is faster and a bit more robust
## it still breaks if one of the vectors is 0
## Also, it always gives the acute angle.
        v1_u = v1 / np.linalg.norm(v1)
        v2_u = v2 / np.linalg.norm(v2)
        angle = np.arccos(np.clip(np.dot(v1_u, v2_u), -1.0, 1.0)) 

        return angle

    def read_data(self):

        flat_data = np.genfromtxt(self.f_name,delimiter = ',')
        data = np.reshape(flat_data,[len(flat_data),20,3])

        if self.keep_data:
            self.data = data
            self.flat_data = flat_data
        if self.save_data:
            new_name = self.fname.replace('.csv','_processed.npy')
            np.save(new_name,data)
        self.line_count = len(data)
        full_data = np.empty([len(data),22,3])
        eye_center=np.mean([data[:,4],data[:,12]],0)
        shoulder_center = np.mean([data[:,5],data[:,13]],0)
        full_data[:,:20] = data
        full_data[:,20] = eye_center
        full_data[:,21] = shoulder_center

        return full_data

## Mammoth hard coded calculation of the relevent angles. 
    def get_angle_data(self,full_data):
        self.angle_dict = {
            'neck-eye-beak':0,
            'eye-eye_center-horizontal':1,
            'left_elbow-wrist-tip':2,
            'right_elbow-wrist-tip':3,
            'left_wrist-neck-wrist':4,
            'eye-neck-tailbone':5,
            'neck-tailbone-tail':6,
            'left_knee-tailbone-knee':7,
            'tailbone-left_knee-ankle':8,
            'tailbone-right_knee-ankle':9,
            'left_knee-ankle-heel':10,
            'right_knee-ankle-heel':11
        }
        all_angles = np.zeros([len(full_data),12])
# Neck-eye-beak (NC_EC_BT): 20-21,20-0 
        EC_NC = full_data[:,20] - full_data[:,21]
        EC_BT = full_data[:,20] - full_data[:,0]
        NC_EC_BT = [self.angle(v1,v2) for v1,v2 in zip(EC_NC,EC_BT)]
        all_angles[:,0] = NC_EC_BT

# Eye-Eye_center-horizontal plane: 20-12(with z20),20-12
        horizontal_eye = np.array(full_data[:,12])
        horizontal_eye[:,2] = full_data[:,20,2]
        EC_LE = full_data[:,20] - full_data[:,12]
        EC_HP = full_data[:,20] - horizontal_eye
        LE_EC_HP = [self.angle(v1,v2) for v1,v2 in zip(EC_LE,EC_HP)]
        all_angles[:,1] = LE_EC_HP

# Elbow-wrist-tip: 7-6,7-8 
        LW_LB = full_data[:,7] - full_data[:,6]
        LW_LT = full_data[:,7] - full_data[:,8]
        LB_LW_LT = [self.angle(v1,v2) for v1,v2 in zip(LW_LB,LW_LT)]
        all_angles[:,2] = LB_LW_LT

# Right EWT: 15-14, 15-16
        RW_RB = full_data[:,15] - full_data[:,14]
        RW_RT = full_data[:,15] - full_data[:,16]
        RB_RW_RT = [self.angle(v1,v2) for v1,v2 in zip(RW_RB,RW_RT)]
        all_angles[:,3] = RB_RW_RT

# Wrist-Neck-wrist: 21-7,21-15
        NC_LW = full_data[:,21] - full_data[:,7]
        NC_RW = full_data[:,21] - full_data[:,15]
        LW_NC_RW = [self.angle(v1,v2) for v1,v2 in zip(NC_LW,NC_RW)]
        all_angles[:,4] = LW_NC_RW

# Eye-Neck-Tailbone: 21-2,21-20
        NC_TB = full_data[:,21] - full_data[:,2]
        NC_EC = full_data[:,21] - full_data[:,20]
        EC_NC_TB = [self.angle(v1,v2) for v1,v2 in zip(NC_TB,NC_EC)]
        all_angles[:,5] = EC_NC_TB

# Neck-Tailbone-Tail: 2-21,2-3
        TB_NC = full_data[:,2] - full_data[:,21]
        TB_TT = full_data[:,2] - full_data[:,3]
        NC_TB_TT = [self.angle(v1,v2) for v1,v2 in zip(TB_NC,TB_TT)]
        all_angles[:,6] = NC_TB_TT

# Knee-Tailbone-Knee: 2-9,2-17
        TB_LK = full_data[:,2] - full_data[:,9]
        TB_RK = full_data[:,2] - full_data[:,17]
        LK_TB_RK = [self.angle(v1,v2) for v1,v2 in zip(TB_LK,TB_RK)]
        all_angles[:,7] = LK_TB_RK

# Tailbone-Knee-Ankle: 9-2,9-10
        LK_TB = full_data[:,9] - full_data[:,2]
        LK_LA = full_data[:,9] - full_data[:,10]
        TB_LK_LA = [self.angle(v1,v2) for v1,v2 in zip(LK_TB,LK_LA)]
        all_angles[:,8] = TB_LK_LA

# Right TKA: 17-2,17-18
        RK_TB = full_data[:,17] - full_data[:,2]
        RK_RA = full_data[:,17] - full_data[:,18]
        TB_RK_RA = [self.angle(v1,v2) for v1,v2 in zip(RK_TB,RK_RA)]
        all_angles[:,9] = TB_RK_RA

# Knee-Ankle-Heel: 10-9,10-11
        LA_LK = full_data[:,10] - full_data[:,9]
        LA_LH = full_data[:,10] - full_data[:,11]
        LK_LA_LH = [self.angle(v1,v2) for v1,v2 in zip(LA_LK,LA_LH)]
        all_angles[:,10] = LK_LA_LH

# Right KAH: 18-17,18-19
        RA_RK = full_data[:,18] - full_data[:,17]
        RA_RH = full_data[:,18] - full_data[:,19]
        RK_RA_RH = [self.angle(v1,v2) for v1,v2 in zip(RA_RK,RA_RH)]
        all_angles[:,11] = RK_RA_RH
        return all_angles 

def get_all_poses(postures_dir):
## Initialize array
    all_poses = np.empty([0,12])
    all_detections = os.listdir(postures_dir)
## Read through directory, appending the flat data to the array
    for detection in all_detections:
        if 'detections_3d.csv' in os.listdir(postures_dir + detection):
            trajectory_path = postures_dir + detection + '/detections_3d.csv'
            trajectory = PoseTrajectory(trajectory_path) 
            all_poses = np.vstack([all_poses,trajectory.all_angles])
    return all_poses

def clean_pose(pose_array):
    pose_array = np.nan_to_num(pose_array)
    pose_array = np.round(pose_array,6)
    return pose_array

if __name__ == '__main__':
    #postures_dir = os.path.expanduser(sys.argv[1])
    #postures_dir = os.path.abspath(postures_dir) + '/'
    postures_dir = '/home/ammon/Documents/Scripts/AnalyzePosture/Keypoints/detections/'
    all_detections = sorted(os.listdir(postures_dir))
    print('getting all the poses...')
    """
    all_poses = get_all_poses(postures_dir)
## This handles the edge case where some angle failed
    all_poses = clean_pose(all_poses)
    #np.save('./all_poses.npy',all_poses)
    """
    all_poses = np.load('./all_poses.npy')
    print('running pca')
    pca = PCA()
    pca.fit(all_poses)
    print('projecting onto pca')
    choice = '1'
    while choice != 'q':
        try:
            idx = int(choice.strip())
        except:
            print("That's no int! Try again:")
            choice = input('Select an int less than ' + str(len(all_detections)) + ': ')
            continue
        one_pose_path = postures_dir + all_detections[idx] + '/detections_3d.csv'
        one_pose = PoseTrajectory(one_pose_path)
        pose_space = pca.transform(clean_pose(one_pose.all_angles))
        fig,ax = plt.subplots()
        ax.plot(pose_space[:,0],pose_space[:,1])
        ax.set_xlim([-4,4])
        ax.set_ylim([-4,4])
        ax.set_title(one_pose_path.split('/')[-2])
        fig.show()
        choice = input('Select an int less than ' + str(len(all_detections)) + ': ')
