#! /usr/bin/env python

import os
from fastdtw import fastdtw
from fastdtw import dtw

from scipy.spatial.distance import euclidean
from scipy.ndimage.filters import gaussian_filter
import numpy as np
import pickle
import random
import pdb
import datetime
dimensions = True

## Need to scale parameters, since I'm calculating euclidean distance, duh
t_scale = 8
e_scale = 42 * 10**4
song_list = np.loadtxt('./song_list.txt', dtype=str,delimiter='/')
song_dict = dict(zip(song_list[:,1],song_list[:,2]))
## Handy container for the sequence and its metrics
class Trajectory:
    def __init__(self,index,from_file = True, seq_file = 0, seq1 = 0, seq2 = 0, path = 0):
        self.index = index
        self.file_path = seq_file
        self.from_file = from_file
        if from_file:
## For now I don't need to store the seq
            self.file_name = seq_file.split('/')[-1]
            seq_name = self.file_name.split('.')[0]
            self.seq_data = np.genfromtxt(seq_file)
            self.ys = self.seq_data[:,5]
            self.e2 = self.seq_data[:,3]
            if dimensions:
                self.data = self.seq_data[:,[5,3,0]]
                #self.data = self.seq_data[:,[5,3]]
## This will scale time to make it relative
                self.data[:,2] = self.data[:,2] * t_scale
                self.data[:,1] = self.data[:,1] * e_scale
            else:
                self.data = self.ys
            self.ts = self.seq_data[:,0]
            ## This works regardless on dimensions!
            self.vel = np.diff(self.data,axis=0)
            self.acc = np.diff(self.vel,axis=0)
            self.vel_t = self.ts[:-1]
            self.acc_t = self.ts[:-2]
            self.cost = 0
            self.sum_cost = 0
            self.cost_ts = 0
            self.response_cost = 0
            self.distance = 0
            self.hierarchy = index
            self.parents = [self.index,self.index]
            self.song = song_dict[seq_name]
            self.timestamp = self.seq_data[1,1]
            self.time = datetime.datetime.fromtimestamp(self.timestamp).strftime('%H:%M')
            self.hour = self.time.split(':')[0]
            self.date = datetime.datetime.fromtimestamp(self.timestamp).strftime('%Y-%m-%d')
            self.bird = 'CB1'
        else:
            self.data,self.ts, self.cost,self.cost_ts = dtw_align(seq1,seq2,path,strat='mean')
            self.vel = np.diff(self.data,axis=0)
            self.vel_t = self.ts[:-1]
            self.acc = np.diff(self.vel,axis=0)
            self.acc_t = self.ts[:-2]
            self.sum_cost = self.cost + seq1.cost + seq2.cost
            self.t_cost = calc_Tcost(seq1,seq2)
            self.hierarchy = [seq1.hierarchy,seq2.hierarchy]
            self.parents = [seq1.index,seq2.index]
            self.song = np.nan
            self.timestamp = np.nan
            self.time = np.nan
            self.date = np.nan
            self.bird = np.nan

        self.define_window()
        if True:
            self.vel = np.diff(self.response_data)
            self.acc = np.diff(self.vel)
            self.data = self.response_data
            self.ts = self.response_ts
            self.cost_ts = self.response_cost

    def define_window(self):
        #smooth_data = np.transpose(gaussian_filter1d(np.transpose(self.data),sigma=2))
        #baseline = np.mean(smooth_data[np.logical_and(self.ts>= -.2,self.ts <= 0.0)])
        try:
            t_baseline = min(self.ts[self.ts>= -0.1])
        except:
            print('something broke!')
            pdb.set_trace()
        t_end = max(self.ts[self.ts<=5])
        t_baseline_index = np.where(self.ts == t_baseline)[0][0]
        t_end_index = np.where(self.ts == t_end)[0][0]
        """
        reaction_range = (self.ts > 0) & (self.ts < 2)
        peak_angle = np.max(smooth_data[reaction_range])
        peak_index = np.where(smooth_data == peak_angle)[0][0]
        half_peak = peak_angle - (peak_angle - baseline) / 2
        refraction_range = smooth_data[self.ts > self.ts[peak_index]]

        try:
            if len([refraction_range < half_peak]) > 1:
                refraction_index_fake = np.argmax(refraction_range < half_peak)
            else:
                pdb.set_trace()
                refraction_index_fake = -1
        except:
            pdb.set_trace()
        refraction_value = refraction_range[refraction_index_fake]
        refraction_index = np.where(smooth_data == refraction_value)[0][0]
        """
        self.response_ts = self.ts[t_baseline_index:t_end_index]
        self.response_data = self.data[t_baseline_index:t_end_index]
        if not self.from_file:
            #pdb.set_trace()
            self.response_cost = self.cost_ts[t_baseline_index:t_end_index]

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
            self.seq_data = np.genfromtxt(file_path)
            song_name = songDict[seq_name]
            Seq = Sequence(self.seq_data,seq_name,song_name)
            if 'm' in Seq.song_name:
                pass
            else:
                seqs.append(Seq.ys)
    return seqs

## This however is big
def dtw_align(p1,p2,path=0,strat="cost"):
    if not path:
        if True:
            #pdb.set_trace()
            distance,path = fastdtw(p1.vel,p2.vel,dist=euclidean)
            #distance,path = dtw(p1.vel,p2.vel,dist=euclidean)
            #distance,path = fastdtw(p1.acc,p2.acc,dist=euclidean)
        else:
            #distance,path = dtw(p1.data,p2.data,dist=euclidean)
            distance,path = fastdtw(p1.data,p2.data,dist=euclidean)
## Other strategies are always random, or use the length vs med_length
    if len(np.shape(p1.data)) > 1:
        dims = np.shape(p1.data)[1]
        p3_data = np.empty([len(path),dims])
    else:
        p3_data = np.empty(len(path))
    for p in range(len(path)):
        pair = path[p]
# For now this is 1d, so no axis needed, eventually I will probably need an axis, or something
        if strat=='cost' or strat=='mean':
            p3_data[p] = np.mean([p1.data[pair[0]],p2.data[pair[1]]],0) 
        elif strat == 'canon':
            p3_data[p] = p2.data[pair[1]]
    p3_ts,cost,cost_path = get_ts(p1,p2,path,strat)
# Should the alignment actually exist inside the object? 
    if len(p3_data) != len(cost_path):
        pdb.set_trace()
    return p3_data, p3_ts, cost, cost_path

## Most complicated function here, figures out new ts
def get_ts(p1,p2,path,strat='cost'):
    costs, counts, indices,cost_path = dtw_cost(path)
    if strat == 'canon':
        primary = 0
        new_ts = np.empty(len(path))
        for p in range(len(path)):
            pair = path[p]
            new_ts[p] = p1.ts[pair[0]]
    elif strat == 'mean':
        new_ts = np.empty(len(path))
        for t in range(len(path)):
            ## I'm still using the primary secondary naming, but here they are arbitrary
            p_index = path[t][0]
            s_index = path[t][1]
            new_ts[t] = np.mean([p1.ts[p_index],p2.ts[s_index]])
    elif strat == 'cost':
        primary = get_primary(p1,p2,strat='cost')
        p_primary = (p1,p2)[primary]
        p_secondary = (p1,p2)[not primary] ## a bit cheeky, but it works
        get_middle = True
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
    return new_ts, np.sum(costs), cost_path

## This could be seen as a boolean question:
# "Is p2 better than p1?"
def get_primary(p1,p2,strat='sum_cost'):
## Or you just take the average path...
    if strat == 'cost':
        if p1.cost > p2.cost:
            return 0
        elif p1.cost < p2.cost:
            return 1
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
    cost_array = np.zeros_like(path_array,dtype=float)
    cost_array0 = np.zeros(len(path_array),dtype=float)
    cost_array1 = np.zeros_like(cost_array0,dtype=float)

    indices_0, counts_0 = np.unique(path_array[:,0],return_counts=True)
    indices_1, counts_1 = np.unique(path_array[:,1],return_counts=True)
    for p in range(len(path_array)):
        cost0 = counts_0[indices_0==path_array[p,0]]
        cost1 = counts_1[indices_1==path_array[p,1]]
        cost_array0[p] = np.log(cost0) / cost0
        cost_array1[p] = np.log(cost1) / cost1
        
    cost_array[:,0] = cost_array0
    cost_array[:,1] = cost_array1

    costs = np.sum(cost_array,0)
    #pdb.set_trace()
    cost_path = np.max(cost_array,1)
    counts = (counts_0,counts_1)
    indices = (indices_0,indices_1)
    return costs, counts, indices, cost_path
    
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
    #distance_key = list(range(len(seqs)))
    """
    distance_array = np.load('distance_array.npy')
    with open('path_dict.pkl','rb') as f:
        path_dict = pickle.load(f)
    """
    distance_array = np.empty([len(seqs),len(seqs)])
    distance_array.fill(np.nan)

## Calculate the distances
    print('calculating distances....')
    for i in range(len(seqs)):
        for j in range(i+1,len(seqs)):
### This defines what your posture data actually is, should be edited perhaps (to include eigen values also?)
            if False:
                p1_data = seqs[i].acc
                p2_data = seqs[j].acc
            else:
                p1_data = seqs[i].data
                p2_data = seqs[j].data
            print('working on',str(i),str(j))
            #distance,path = dtw(p1_data,p2_data,dist=euclidean)
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
# It keeps track of how the array index relates to the seq index
    steps = len(seqs) - 1
    n_baseSeqs = len(seqs)
    index_key = list(range(n_baseSeqs))
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
        i_true = index_key[i]
        j_true = index_key[j]
        print('working on',i,j)
        print('i.e.:',i_true,j_true)
        print('starting alignment....')
        new_index = n_baseSeqs + n
## Add the new index and remove the two old ones
        index_key.append(new_index)
## Delete higher one first, fortunately, j should always be higher
        del(index_key[j]);del(index_key[i])

        key = str(i_true) + ',' + str(j_true)
        p_aligned = Trajectory(new_index,from_file=False,seq1=seqs[i_true],seq2=seqs[j_true],path=path_dict[key])
## Append to the sequence, and also append to the key. Thus the last entry will be the full phylogeny :D 
        seqs.append(p_aligned)
        #distance_key.append([distance_key[i_true],distance_key[j_true]])
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
        for i in range(len(index_key)-1):
            i_true = index_key[i]
            print('working on',str(i_true),str(new_index))
            p1 = seqs[i_true]
            p2 = seqs[new_index]
            #new_array[i,-1],path = fastdtw(p1.vel,p2.vel,dist=euclidean)
            #new_array[i,-1],path = fastdtw(p1.acc,p2.acc,dist=euclidean)
            new_array[i,-1],path = fastdtw(p1.data,p2.data,dist=euclidean)
            key = str(i_true) + ',' + str(new_index)
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

