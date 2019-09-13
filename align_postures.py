#! /usr/bin/env python

import os
from fastdtw import fastdtw
from scipy.spatial.distance import euclidean
import numpy as np
import pickle
import random
import pdb
## Handy container for the sequence and its metrics
class Trajectory:
    def __init__(self,index,from_file = True, seq_file = 0, seq1 = 0, seq2 = 0, path = 0):
        self.index = index
        self.file_path = seq_file
        self.from_file = from_file
        if from_file:
## For now I don't need to store the seq
            seq_data = np.genfromtxt(seq_file)
            self.data = seq_data[:,5]
            self.ts = seq_data[:,0]
            self.cost = 0
            self.sum_cost = 0
            self.distance = 0
            self.hierarchy = index
            self.t_cost = 0
        else:
            self.data,self.cost, self.ts = dtw_align(seq1,seq2,path)
            self.sum_cost = self.cost + seq1.cost + seq2.cost
            self.t_cost = calc_Tcost(seq1,seq2)
            self.hierarchy = [seq1.hierarchy,seq2.hierarchy]

## fastdtw has no concept of time shift
## This will provide some cost for the time shift itself if needed (a bit unlikely, but worth having) 
def calc_Tcost(seq1,seq2):

    Tcost = np.abs(seq1.ts[0] - seq2.ts[0])
    return Tcost

def parse_postures(posture_dir):
    seqs = []
    for seq_file in os.listdir(posture_dir):
        if '.csv' in p:
            file_path = posture_dir + p
            seq_name = seq_file.split('.')[0]
            seq_data = np.genfromtxt(file_path)
            song_name = songDict[seq_name]
            Seq = Sequence(seq_data,seq_name,song_name)
            if 'm' in Seq.song_name:
                pass
            else:
                seqs.append(Seq.ys)
    return seqs

## This however is big
def dtw_align(p1,p2,path=0,strat="cost"):
    if not path:
        distance,path = fastdtw(p1,pw)
## Other strategies are always random, or use the length vs med_length
    p3_data = np.empty(len(path))
    for p in range(len(path)):
        pair = path[p]
# For now this is 1d, so no axis needed, eventually I will probably need an axis, or something
        p3_data[p] = np.mean([p1.data[pair[0]],p2.data[pair[1]]]) 
    p3_ts,cost = get_ts(p1,p2,path)
# Should the alignment actually exist inside the object? 
    return p3_data, p3_ts, cost

## Most complicated function here, figures out new ts
def get_ts(p1,p2,path,strat='cost'):
    costs, counts, indices = dtw_cost(path)
    if strat == 'cost':
        primary = get_primary(p1,p2,strat='cost')
        p_primary = (p1,p2)[primary]
        p_secondary = (p1,p2)[not primary] ## a bit cheeky, but it works
    else:
        pass
    p_counts = counts[primary]
    #s_counts = counts[not primary]
    p_indices = indices[primary]
    new_ts = np.empty(len(path))
    t = 0
    while t < len(path):
        p_index = path[t][primary]
        point_t = p_primary.ts[p_index]
        c_index = int(p_indices[p_indices == p_index])
        t_count = p_counts[c_index]
        if t_count == 1:
            new_ts[t] = point_t
        if t_count > 1:
            try:
                point_t0 = p_primary.ts[p_index - 1]
                point_t2 = p_primary.ts[p_index + 1]
            except:
            # Oops, out of bounds:
                if p_index == 0:
                    point_t2 = p_primary.ts[p_index + 1]
                    point_t0 = p_primary.ts[p_index] - abs(p_primary.ts[p_index] - point_t2)
                elif p_index == len(p_primary.ts) - 1:
                    point_t0 = p_primary.ts[p_index - 1]
                    point_t2 = p_primary.ts[p_index] + abs(p_primary.ts[p_index] - point_t0)
                else:
                    print("Something weird happened, not sure what...")
                    pdb.set_trace()
            t_start = point_t - abs(point_t - point_t0)/2
            t_end = point_t + abs(point_t2 - point_t)/2
            t_len = abs(t_end - t_start)
            for t_i in range(t_count):
                new_ts[t+t_i] = t_start + t_len / t_count
        t += t_count
    return new_ts, costs[primary]

## This could be seen as a boolean question:
# "Is p2 better than p1?"
def get_primary(p1,p2,strat='cost'):
## Or you just take the average path...
    if strat == 'cost':
        if p1.cost > p2.cost:
            return 0
            "Use p1"
        elif p2.cost > p1.cost:
            return 1
            "use p2"
        else:
            strat = 'random'
    elif strat == 'sum_cost':
        if p1.sum_cost > p2.sum_cost:
            return 0 
        elif p1.sum_cost < p2.sum_cost:
            return 1
        else:
            strat = 'random'
    if strat == 'random':
        return random.randint(0,1)
# Just pick randomly

## This too...calculates cost of a path
## There is probably a more clever way to do this...
def dtw_cost(path):
    path_array = np.array(path)
    indices_0, counts_0 = np.unique(path_array[:,0],return_counts=True)
    indices_1, counts_1 = np.unique(path_array[:,1],return_counts=True)
    cost_points_0 = np.log(counts_0)
    cost_points_1 = np.log(counts_1)
    cost_0,cost_1 = np.sum(cost_points_0),np.sum(cost_points_1)
    costs = (cost_0,cost_1)
    counts = (counts_0,counts_1)
    indices = (indices_0,indices_1)
    return costs, counts, indices
    
if __name__ == "__main__":
## Start with lots of postures (in N Dimensional space) 
## This needs to be done well
    posture_dir = './processed_postures/'
    posture_list = os.listdir(posture_dir)
    seqs = []
    path_dict = {}
    for s in range(len(posture_list)):
        index = s
        file_path = posture_dir + posture_list[s]
        Seq = Trajectory(index,from_file=True,seq_file = file_path)
        seqs.append(Seq)

## Initialize sparse array of distances
    #"""
    distance_array = np.load('distance_array.npy')
    with open('path_dict.pkl','rb') as f:
        path_dict = pickle.load(f)
    distance_key = list(range(len(seqs)))
    """
    distance_array = np.empty([len(seqs),len(seqs)])
    distance_array.fill(np.nan)

## Calculate the distances
    print('calculating distances....')
    for i in range(len(seqs)):
        for j in range(i+1,len(seqs)):
### This defines what your posture data actually is, should be edited perhaps (to include eigen values also?)
            p1_data = seqs[i].data
            p2_data = seqs[j].data
            print('working on',str(i),str(j))
            distance,path = fastdtw(p1_data,p2_data,dist=euclidean)
## Store the paths! You'll want some of them later
            key = str(i) + ',' + str(j)
            path_dict[key] = path
            distance_array[i,j] = distance

    np.save('distance_array.npy',distance_array)
    with open('path_dict.pkl','wb') as pickle_file:
        pickle.dump(path_dict, pickle_file,pickle.HIGHEST_PROTOCOL)
    #"""
    new_array = np.copy(distance_array)
    print('done with the first part! On to alignment')
    ## XXX Something is going on with my indexing...
    steps = len(seqs) - 2
    n_baseSeqs = len(seqs)
    all_distances = np.empty([len(distance_array) + steps,len(distance_array)])
    all_distances.fill(np.nan)
    all_distances[:len(distance_array),:len(distance_array)] = distance_array[:]
    for n in range(steps):
## Find min distance
        print('setting up indices')
        min_index = np.unravel_index(np.nanargmin(new_array), new_array.shape)

        all_index = np.arange(len(new_array))
## Align those 2 postures
        i,j = min_index
        print('working on',i,j)
        print('starting alignment....')
        new_index = n_baseSeqs + n -1
        key = str(i) + ',' + str(j)
        p_aligned = Trajectory(new_index,from_file=False,seq1=seqs[i],seq2=seqs[j],path=path_dict[key])
## Append to the sequence, and also append to the key. Thus the last entry will be the full phylogeny :D 
        seqs.append(p_aligned)
        distance_key.append([distance_key[i],distance_key[j]])
        print('finished!')

## Prefill tables (basically you're copying everything but the columns you used
## then adding the new distances as the last column) 
        #pdb.set_trace()
        print('prepping new array')
        old_array = np.copy(new_array)
        new_array = np.empty([len(old_array)-1,len(old_array)-1])
        new_array.fill(np.nan)
        sub_index = all_index[(all_index != i) & (all_index != j)]
        new_array[:len(old_array)-2,:len(old_array)-2] = np.array(old_array[sub_index,:])[:,sub_index]
        #pdb.set_trace()
        print('calculating new distances...')
## Calculate the new column
        for i in range(len(new_array)-1):
            print('working on',str(i),str(new_index))
            p1 = seqs[i]
            p2 = seqs[new_index]
            new_array[i,-1],path = fastdtw(p1.data,p2.data,dist=euclidean)
            key = str(i) + ',' + str(new_index)
            path_dict[key] = path
        all_distances[n_baseSeqs + n,:np.shape(new_array)[1]] = new_array[-1]
## Repeat that loop
## I should track the distances also...
with open('path_dict.pkl','wb') as p:
    pickle.dump(path_dict, p,pickle.HIGHEST_PROTOCOL)
with open('seqs.dat','wb') as p:
    pickle.dump(seqs,p,pickle.HIGHEST_PROTOCOL)
np.save('all_distances.npy',all_distances)
print("I did it!")

