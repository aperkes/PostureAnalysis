import numpy as np
import pandas as pd
from matplotlib import pyplot as plt
from matplotlib import colormaps as cm
from scipy.stats.kde import gaussian_kde

paper_cmap = cm.get_cmap('viridis')
## Make data and plot figure 7a, Get durations,latencies by bird: 
song_list = ['BDY','BOD','ND','LB','2M','DBR','GRG','WG','LNR','DMG']
plot_bird_list = ['Y1','CB-Orange','PINK2','CB-Y2','CB-LB2','CB-Yellow','P1']
#simple_bird_list = ['B1-2018','B1-2019','B2-2018','B2-2019','B3-2019','B4-2019','B3-2018']
simple_bird_list = ['B1-2018','B2-2018','B3-2018','B1-2019','B2-2019','B3-2019','B4-2019','B5-2019']

plot_aviary_names = [
    '2018*',
    '2019*',
    '2019-racks',
    '2007',
    '2009',
    '2010',
    '2011'
]


base_l = 1.75
top_l = 3.75

base_d = .35
top_d = .6
## Same for latency

bird_df = pd.read_csv('./full_df3.csv')
all_durations = bird_df.Duration
good_durations = all_durations[~np.isnan(all_durations)]
good_durations = good_durations[good_durations > 0]

all_latencies = bird_df.Latency
good_latencies = all_latencies[~np.isnan(all_latencies)]
good_latencies = good_latencies[good_latencies > 0]

bird_durations = {}
bird_latencies = {}
a_list = []

med_latencies = []

for a in pd.unique(bird_df.Aviary):
    a_df = bird_df[bird_df.Aviary == a]

    for b in pd.unique(a_df.BirdID):
        
        b_df = a_df[a_df.BirdID == b]
        b_durations = b_df.Duration
        b_durations = b_durations[~np.isnan(b_durations)]
        b_durations = b_durations[b_durations > 0]
        if len(b_durations) > 0:
            if b in bird_durations.keys():
                b_ = '.'.join([b,str(a)])
            else:
                b_ = b
            bird_durations[b_] = b_durations.values
            a_list.append(a)

        b_latencies = b_df.Latency
        b_latencies = b_latencies[~np.isnan(b_latencies)]
        b_latencies = b_latencies[b_latencies > 0]
        if len(b_latencies) > 0:
            bird_latencies[b] = b_latencies.values
            med_latencies.append(np.mean(b_latencies.values))

#fig,(ax1,ax2) = plt.subplots(1,2)
fig,ax1 = plt.subplots()
fig2,ax2 = plt.subplots()

## base_h defined above
dur_heights = np.linspace(base_d,top_d,len(bird_durations.keys()))[::-1]
lat_heights = np.linspace(base_l,top_l,len(bird_latencies.keys()))[::-1]
lat_heights = lat_heights[np.argsort(np.argsort(med_latencies))]

dur_keys = list(bird_durations.keys())
lat_keys = list(bird_latencies.keys())
n_durs = len(dur_keys)
n_lats = len(lat_keys)
dur_widths = [(top_d - base_d) / n_durs] * n_durs
lat_widths = [(top_l - base_l) / n_lats] * n_lats


#bird_color_values = [0.6,0.0,0.7,.3,.2,.1,.65,0,0,0]
bird_color_values = np.linspace(0,1,n_lats)
bird_colors = [paper_cmap(v) for v in bird_color_values]

#aviary_color_values = [.1,.2,.2,.3,.4,.5,.6]
aviary_color_values = np.linspace(0,1,n_durs)
aviary_colors = [paper_cmap(v) for v in aviary_color_values]


for d in range(len(dur_keys)):
    k = dur_keys[d]
    #a = pd.unique(bird_df[bird_df.BirdID == k].Aviary)[0]
    a = a_list[d]
    #ys_dur = np.array([dur_heights[s]] * len(bird_durations[s]))
    #ys_dur = dict(zip([list(bird_durations.keys()),dur_heights]))
    ys_dur = np.array([[dur_heights[d]] * len(bird_durations[k])])                  
    jitter_dur = (np.random.rand(len(ys_dur)) - .5) * .25

    ys_dur = ys_dur + jitter_dur
    ax2.scatter(bird_durations[k],ys_dur,alpha=.5,marker='.',color=aviary_colors[d])

    mark_h = .5

for l in range(len(lat_keys)):
    k = lat_keys[l]
    ys_lat = np.array([[lat_heights[l]] * len(bird_latencies[k])])
                      
    jitter_lat = (np.random.rand(len(ys_lat)) - .5) * .25

    ys_lat = ys_lat + jitter_lat
    ax1.scatter(bird_latencies[k],ys_lat,alpha=.5,marker='.',color=bird_colors[l]) #,label=simple_bird_list[l])

    mark_h = .5
    #ax1.scatter(np.nanmean(bird_latencies[s]),mark_h,marker='v',color=bird_colors[i])
    #ax2.scatter(np.nanmean(bird_durations[s]),mark_h,marker='v',color=bird_colors[i])

ax2.boxplot(bird_durations.values(),positions=dur_heights,vert=False,widths=dur_widths,showfliers=False)
#print(lat_heights)
#print(bird_latencies)
ax1.boxplot(bird_latencies.values(),positions=lat_heights,vert=False,widths=lat_widths,showfliers=False)

dur_xs = np.linspace(-5,25,100)    
lat_xs = np.linspace(-2,3,100)
#ax1.scatter(np.nanmean(good_latencies),mark_h *3,marker='v',color='red',s=60)
#ax2.scatter(np.nanmean(good_durations),mark_h *3,marker='v',color='red',s=60)

ax1.hist(good_latencies,bins=20,color='gray',alpha=.2,density=True)
ax2.hist(good_durations,bins=10,color='gray',alpha=.2,density=True)

ax1.scatter(np.mean(good_latencies),.1,marker='v',color='black')
ax2.scatter(np.mean(good_durations),.1,marker='v',color='black')
lat_curve =  gaussian_kde(good_latencies)
ax1.plot(lat_xs,lat_curve(lat_xs),color='black')

dur_curve =  gaussian_kde(good_durations)
ax2.plot(dur_xs,dur_curve(dur_xs),color='black')

ax1.set_xlim([-.5,2.5])       
ax1.set_ylim([0,4.0])  

ax1.set_yticks(np.arange(0,2))
ax1.set_yticklabels([0,1])

ax2.set_yticks(np.linspace(0,0.30,3))
ax2.set_yticklabels([0,0.15,0.3])
#ax1.set_yticklabels(np.arange(0,10))
ax2.set_xlim([-1,15.5])
ax2.set_ylim(0,0.75)
"""
ax2.set_yticks(np.arange(0,2))
ax2.set_yticklabels([0,1])
ax2.set_ylabel('   ')
ax2.set_xlabel('Duration (s)')
ax2.axvline(0,linestyle=':',color='black')
"""
ax1.set_ylabel('Density')

ax1.set_xlabel('Latency (s)')

ax1.axvline(0,linestyle=':',color='black')

#fig.legend()

ax1.legend(loc='lower right')
#fig.set_size_inches(10,5)
fig.set_size_inches(8,5)
fig.tight_layout()
fig.show()
fig2.show()
plt.show()

print('Latencies:',np.mean(good_latencies),np.std(good_latencies),len(good_latencies))
print('Median Latency:',np.median(good_latencies))
print('Durations:',np.mean(good_durations),np.std(good_durations),len(good_durations))

if False:
    fig.savefig('./figures/7a.png',dpi=300)
    fig.savefig('./figures/7a.svg')
