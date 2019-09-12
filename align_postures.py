#! /usr/bin/env python

from fastdtw import fastdtw
from scipy.spatial.distance import euclidean
import numpy as np
from posture_seq import Sequence

## Start with lots of postures (in N Dimensional space) 
## This needs to be done well
posture_list = parse_postures('./posture_dir/')

## Handy container for the sequence and its metrics
class Trajectory:
    def __init__(self,index,from_file = True, seq_file = 0, seq1 = 0, seq2 = 0, path = 0):
        self.index = index
        self.file_path = seq_file
        self.from_file = from_file
        if from_file:
## For now I don't need to store the seq
            seq = Sequence(seq_path)
            self.data = seq.ys
            self.ts = seq.ts
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
        path,distance = fastdtw(p1,pw)
## Other strategies are always random, or use the length vs med_length
    for p in range(len(path)):
        pair = path[p]
        p3_data[p] = np.mean(p1[pair[0]],p2[pair[1]],0) ## NOTE: check that this is the right axis
    p3_ts,cost = get_ts(p1,p2,path)
# Should the alignment actually exist inside the object? 
    return p3_data, p3_ts, cost

## Most complicated function here, figures out new ts
def get_ts(p1,p2,path,strat='cost'):
    cost, counts, indices = dtw_cost(path)
    if strat = 'cost':
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
    while t <= len(path):
        p_index = path[t][primary]
        point_t = p_primary.ts[p_index]
        c_index = p_indices[p_indices == p_index]
        t_count = p_counts[c_index]
        if t_count == 1:
            new_ts[t] = point_t
        if t_count > 1:
            point_t0 = p_primary.ts[p_index - 1]
            point_t2 = p_primary.ts[p_index + 1]
            t_start = point_t - abs(point_t - point_t0)/2
            t_end = point_t + abs(point_t1 - point_t)/2
            t_len = abs(t_end - t_start)
            for t_i in range(t_count):
                new_ts[t+t_i] = t_start + t_len / t_count
        t += t_count
    return new_ts, cost

## This could be seen as a boolean question:
# "Is p2 better than p1?"
def get_primary(p1,p2,strat='cost'):
## Or you just take the average path...
    if strat = 'cost':
        if p1.cost > p2.cost:
            return 0
            "Use p1"
        elif p2.cost > p1.cost:
            return 1
            "use p2"
        else:
            strat = 'random'
    elif strat = 'sum_cost':
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
def dtw_cost(path):
    path_array = np.array(path)
    indices, counts = np.unique(path_array,return_counts=True)
    cost_points = np.log(counts)
    cost = np.sum(cost_points)
    return cost, counts, indices
    
## Initialize sparse array of distances
distance_array = np.empty([len(seqs),len(seqs)])
distance_array.fill(np.nan)
distance_key = list(range(len(seqs)))

## Calculate the distances
for i in range(len(seqs)):
    for j in range(i,len(seqs)):
### This defines what your posture data actually is, should be edited perhaps (to include eigen values also?)
        p1_data = seqs[i]
        p2_data = seqs[j]
        path, distance = fastdtw(p1_data,p2_data,dist=euclidean)
## Store the paths! You'll want some of them later
        path_dict[i][j] = path
        distance_array[i,j] = distance

new_array = np.copy(distance_array)

for n in range(len(seqs) - 1):
    n_remaining = len(seqs) - n
## Find min distance
    min_index = np.unravel_index(new_array.argmin(), new_array.shape)

    all_index = np.arange(len(seqs))
## Align those 2 postures
    i,j = min_index
    print('starting alignment....')
    p_aligned = dtw_align(seqs[i],seqs[j],path)
## Append to the sequence, and also append to the key. Thus the last entry will be the full phylogeny :D 
    seqs.append(p_aligned)
    distance_key.append([distance_key[i],distance_key[j]])
    print('finished!')

## Prefill tables (basically you're copying everything but the columns you used
## then adding the new distances as the last column) 
    old_array = np.copy(new_array)
    new_array = np.empty([n_remaining,n_remaining])
    new_array.fill(np.nan)
    sub_index = all_index[(all_index != i) & (all_index != j)]
    new_array[:-1,:-1] = np.array(old_array[sub_index,:])[:,sub_index]

## Calculate the new column
    for i in range(n_remaining -1):
        p1 = seqs[i]
        p2 = aligned_seqs[
        path, new_array[i,-1] = fastdtw(p1,p2,dist=euclidean)
## Repeat that loop
## P's should be class objects that keep a cost variable. 

p_aligned, cost = dtw_align(p1.data,p2.data,dist=euclidean)

