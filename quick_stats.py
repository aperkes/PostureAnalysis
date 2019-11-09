from pose_class import Trajectory
import numpy as np
import pickle, os

song_list = np.genfromtxt('./song_list.txt',dtype=str)

for s in os.listdir('./SeqClasses/'):
    seq = pickle.load(open('./SeqClasses/' + s,'rb'))

    if seq.offset != 0:
        print(seq.song,':',seq.tzero,seq.t_peak_vel)


