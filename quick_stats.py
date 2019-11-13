from pose_class import Trajectory
import numpy as np
import pickle, os

song_list = np.genfromtxt('./song_list.txt',dtype=str)

latency_dict = dict(zip(list(song_list),[[] for x in range(len(song_list))]))
duration_dict = dict(zip(list(song_list),[[] for x in range(len(song_list))]))
count = 0
for s in os.listdir('./SeqClasses/'):
    seq = pickle.load(open('./SeqClasses/' + s,'rb'))

    if seq.offset != 0:
        print(seq.song,':',seq.tzero,seq.t_vmax)
        try:
            latency_dict[seq.song].append(seq.t_vmax)
            duration_dict[seq.song].append(seq.t_refraction)
            count += 1
        except:
            print('not a song...')

for k in latency_dict.keys():
    print(k,'latency:',str(np.median(latency_dict[k])))
for k in latency_dict.keys():
    print(k,'duration:',str(np.median(duration_dict[k])))
print('final count:',str(count))
