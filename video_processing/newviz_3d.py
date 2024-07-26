from matplotlib import pyplot as plt
import numpy as np
from mpl_toolkits.mplot3d import Axes3D
import argparse

fig = plt.figure()
ax = fig.add_subplot(111, projection='3d')

"""Node names:['Beak', 
'Right-eye', 
'Left-eye', 
'Nape', 
'Left-wrist', 
'Right-wrist', 
'Left-wingtip', 
'Right-wingtip', 
'Tail-base', 
'Tail-tip', 
'Left-ankle', 
'Right-ankle', 
'Left-foot', 
'Right-foot']
"""

def build_parse():
    parser = argparse.ArgumentParser()
    parser.add_argument('--keypoints','-k',default='./pred_keypoints_3d.npy',help='Path to predicted 3d keypoints .npy file. If no file is specified, defaults to ./pred_keypoints_3d.npy')
    parser.add_argument('--dir','-d',default='.',help='Directory to story the images, defaults to current')
    parser.add_argument('--resolution','-r',default=50,help='How many frames to skip (r=50 means every 50th frame)')
    return parser.parse_args()

args = build_parse()
 
smooth_data = np.load(args.keypoints) ## Not actually smooth at the moment
EC = np.mean(smooth_data[:,[1,2]],1)
SC = smooth_data[:,[3]]
print(smooth_data.shape)

bones = [
    [0,1],#beak-neck
    [1,2], #neck-center tailbone
    [2,3], #tailbone tail-tip
    [2,4],#tailbone left-ankle
    [4,5],#left-ankle left-heel
    [2,6], #tailbone right-ankle
    [6,7],#right-ankle right-heel
]

bone_colors = ['sandybrown','saddlebrown', #head
 'black','purple', # spine and tail
 'blue','blue', # left leg
 'darkblue','darkblue', # right leg
 ]

step = int(args.resolution)
for t in range(len(smooth_data)):
    if t % step == 0:
        print(t)
        ax.scatter(smooth_data[t,:,0],smooth_data[t,:,1],smooth_data[t,:,2])
        #full_data = np.empty([len(smooth_data),22,3])
        #full_data[:,:8] = smooth_data
        full_data = smooth_data
        #full_data[:,14] = EC

        for n in range(len(bones)):
            b = bones[n]
            ax.plot(full_data[t,b,0],full_data[t,b,1],full_data[t,b,2],color=bone_colors[n])
            ax.set_xlim([0,.4])
            ax.set_ylim([0,.4])
            ax.set_zlim([0,.4])
        fig.set_size_inches(7,7)
        fig.savefig(args.dir + "/image3d" f'{t:04}' + '.png',dpi=300)
        ax.clear()

## To generate pallet: 
# ffmpeg -i %04d.png -vf palettegen palette.png
## To make gif:
## ffmpeg -y -r 2 -i %04d.png -i palette.png -lavfi paletteuse output.gif
print('done!')
