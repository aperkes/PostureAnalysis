
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.animation as animation 

import sys

## First figure out all the names of files
timestamps_dir = '/data/CSD/timestamps/'
onsets_dir = '/data/CSD/onsets/'
keypoints = sys.argv[1]

kp_name = keypoints.split('/')[-1]
timestamps_file = kp_name.replace('3d.npy','ts.txt')
timestamps_path = timestamps_dir + timestamps_file

if 'BV2' in kp_name:
    if '2019' in kp_name:
        onsets_file = '2019-onsets-birdview-2.txt'
    else:
        onsets_file = '2018-onsets-birdview-2.txt'
else:
    if '2019' in kp_name:
        onsets_file = '2019-onsets-birdview.txt'
    else:
        onsets_file = '2018-onsets-birdview.txt'
onsets_path = onsets_dir + onsets_file
name_list = kp_name.replace('.','-')
onset_prefix = '-'.join(name_list.split('-')[1:6])

## Load keypoints
kpts_array = np.load(keypoints)

## Load timesetamp array (2 x n_frames)
ts_array = np.loadtxt(timestamps_path)
## Find the timestamp of the start of the song
with open(onsets_path,'r') as f:
    while True:
        line = f.readline()
        if onset_prefix in line:
            ts_start = float(line.split()[1])
            break
        if not line:
            break

i_start = np.argmax(ts_array[:,1] >= ts_start)

## Now for plotting
zs = kpts_array[:,3,2]
ts = np.arange(len(zs))
fig,ax = plt.subplots()
ax.set_ylabel('tail height (m)')
ax.set_xlabel('frame number')

scat = ax.scatter(ts[0],zs[0],s=5,color='red')
line = ax.plot(ts[0],zs[0])[0]

ax.axvline(i_start,color='gray',linestyle=':')

ax.set_ylim([0.15,0.4])
ax.set_xlim([0,len(ts)])
def update(frame):
    x = ts[frame]
    y = zs[frame] 

    data_scat = np.stack([x,y]).T
    scat.set_offsets(data_scat)

    line.set_xdata(ts[:frame])
    line.set_ydata(zs[:frame])
    return scat,line
print(kpts_array.shape)
ani = animation.FuncAnimation(fig=fig,func=update,frames=len(ts),interval=1)

fig.set_size_inches([2,3])
fig.tight_layout()
Writer = animation.writers['ffmpeg']
writer = Writer(fps=50,metadata=dict(artist='Ammon'),bitrate=1800)
ani.save('test3.mp4',writer=writer,dpi=300)

#ax.plot(kpts_array[:,3,2])
plt.show()
