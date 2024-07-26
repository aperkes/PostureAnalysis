#! /usr/bin/env python

## Code for visualizing 2d keypoints over videos
# I swear I have already written this, but I can't find it. So here we are. 

## For questions, contact Ammon (perkes.ammon@gmail.com)

## Load in packages
import numpy as np
import cv2
from matplotlib import pyplot as plt
import argparse 

def build_parse():
    parser = argparse.ArgumentParser()
    parser.add_argument('--video','-v',default='./testvid.mp4',help='Required path to video that the keypoints will be overlayed on')
    parser.add_argument('--keypoints','-k',default='./pred_keypoints_2d.npy',help='Path to predicted 2d keypoints .npy file. If no file is specified, defaults to ./pred_keypoints_2d.npy')
    parser.add_argument('--dir','-d',default='.',help='Directory to story the images, defaults to current')
    parser.add_argument('--resolution','-r',default=50,help='How many frames to skip (r=50 means every 50th frame)')
    return parser.parse_args()

args = build_parse()
    
# Load 2d keypoints from all 4 clips
keypoints_2d = np.load(args.keypoints)
#print(keypoints_2d.shape)

color_key = [
    'brown', # beak
    #'sandybrown', # left eye
    #'saddlebrown', #right eye
    'black', #nape
    #'green', #left wrist
    #'darkgreen', #right wrist
    #'red', #left wingtip
    #'darkred',#right wingtip
    'purple',#tail base
    'indigo', #tail tip
    'lightblue', #left ankle
    'darkblue', #right ankle
    'blue', #left foot
    'skyblue', #right foot
    ]

bones = [
    [0,1],#beak-neck
    [1,2], #neck-center tailbone
    [2,3], #tailbone tail-tip
    [2,4],#tailbone left-ankle
    [4,5],#left-ankle left-heel
    [2,6], #tailbone right-ankle
    [6,7],#right-ankle right-heel
]


bone_colors = ['sandybrown','saddlebrown', # beak and spine
 'white', # tail
 'lime','blue', # left leg
 'limegreen','darkblue', # right leg
 ]


# Load video
f = 0
step = int(args.resolution)
corrected_keypoints = None
print('loading',args.video)
if True:
    cap = cv2.VideoCapture(args.video)

    while cap.isOpened(): 
        ret,frame = cap.read()
        #if ret == True:
        if ret == True and f % step ==0:
            print(f)
            if corrected_keypoints is None:
                x_c = int(frame.shape[0]/2)
                y_c = int(frame.shape[1]/2)
                if 'BV2' in args.video: ## frame shape defaults to w > h
                    x_c,y_c = y_c,x_c
                corrected_keypoints = np.array(keypoints_2d)
                corrected_keypoints[1,:,0] = corrected_keypoints[1,:,0] + x_c
                corrected_keypoints[2,:,1] = corrected_keypoints[2,:,1] + y_c
                corrected_keypoints[3,:,0] = corrected_keypoints[3,:,0] + x_c
                corrected_keypoints[3,:,1] = corrected_keypoints[3,:,1] + y_c
            #cv2.imshow('test',frame)
            plt.imshow(frame[...,::-1])
            for n in range(keypoints_2d.shape[1]):
                for q in range(4):
                    plt.scatter(corrected_keypoints[q,:,0,f],corrected_keypoints[q,:,1,f],s=3,color=color_key)
                    for b in range(len(bones)):
                        bone = bones[b]
                        plt.plot(corrected_keypoints[q,bone,0,f],corrected_keypoints[q,bone,1,f],color=bone_colors[b],linewidth=1)
            plt.axis('off')
            plt.savefig(args.dir + '/image2d' + f'{f:04}' + '.png',bbox_inches='tight',dpi=300)
            #plt.show()
            plt.cla()
            #f += step
            #cap.set(cv2.CAP_PROP_POS_FRAMES, f) ## skips forward in the vid
        elif ret == False:
            break
        f += 1
    cap.release()
    cv2.destroyAllWindows()

         
