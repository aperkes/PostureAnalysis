## More complex script to iterate through many paramters 
# For my paper on CSD production

import warnings
if True:
    print("Heads up, warnings are off")
    warnings.simplefilter(action='ignore', category=FutureWarning)
    warnings.filterwarnings("ignore")

import numpy as np
import pandas as pd
import subprocess
import copy 
from matplotlib import pyplot as plt
from tqdm import tqdm
from joblib import Parallel, delayed
from scipy.stats import pearsonr,linregress

DATA = False
NOISE = False
DEBUG = False


class Params():
    def __init__(self,n_birds=10,n_songs=10,n_rounds=10,iterations=100,thresh=0.5,thresh_range=0.5,thresh_noise=0.1,signal_noise=0.1,bias=0.9,curve=3,naive_potency=0.5,lr=10):
        self.n_birds = n_birds
        self.n_songs = n_songs
        self.n_rounds = n_rounds
        self.iterations = iterations
        self.thresh = thresh
        self.thresh_range = thresh_range
        self.thresh_noise = thresh_noise
        self.signal_noise = signal_noise
        self.bias = bias
        self.curve = curve
        self.naive_potency = naive_potency
        if lr > n_rounds - 1:
            self.lr = n_rounds - 1
        else:
            self.lr = lr
    def summary(self):
        for a in dir(self):
            if '__' not in a:
                print(a,':',self.__get_attribute__(a))
    def copy(self):
        return copy.copy(self)

## Jenky work around if you can't get pymer4 to work
def subprocessR(df_thresh,df_pot):
## If you can't get pymer4 to work, use this subprocess
    df_thresh.to_csv('./tmp_thresh.csv') # check current dir  
    df_pot.to_csv('./tmp_pot.csv')
    cmd = ['rscript','./check_interactions.r']
    sig_list = []
    p_list = []
    ef_list = []
    try:
        r_output = subprocess.run(cmd,check=True, capture_output=True, text=True).stdout
    except:
        return 0
    p_list = r_output.strip().split() ## p_pot,p_thresh

    for p_ in range(4):
        efsize = p_list[p_ + 4]
        neg = False
        if efsize[0] == '-' and p_ > 1:
            neg = True ## need check for spurious negative correlations
        p_raw = p_list[p_]
        if 'e' in p_raw:
            pvalue,exp = p_raw.split('e-')
            ppvalue,exp = float(pvalue),int(exp)
            if exp > 3:
                pvalue = 0
            else:
                pvalue = value / 10 ** exp
        else:
            pvalue = float(p_raw)
        p_list.append(pvalue)
        ef_list.append(float(efsize))
        if pvalue < 0.01 and not neg:
            sig = 1    
        else:
            sig = 0
        sig_list.append(sig)
    return sig_list


## Quick function to check whether interaction effects diverge
def check_divergence(df,model):
    df['Fit'] = model.fits
    df['SQuartile'] = pd.qcut(df['Song'],q=4,labels=False)
    df0 = df[df.Block == 0]
    df9 = df[df.Block == 9] 
    L0 = np.mean(df0[df0.SQuartile == 0].Fit)
    H0 = np.mean(df0[df0.SQuartile == 3].Fit)
    L9 = np.mean(df9[df9.SQuartile == 0].Fit)
    H9 = np.mean(df9[df9.SQuartile == 3].Fit)
    slopeL = L9 - L0
    slopeH = H9 - H0
    return np.sign(slopeL * slopeH)

## An easier statistical approach, for a more civilized age. 
## This next section needs to return: 
#      return [ [pot_interaction,thresh_interaction],[pot_divergence,thresh_divergence],e_list,p_list]
def stupidStats(df_thresh,df_pot):
    bad_songs,good_songs = [],[]
    for s in pd.unique(df_thresh.Song):
        if len(np.unique(df_thresh[df_thresh.Song == s].Response)) == 1:
            bad_songs.append(s)
        else:
            good_songs.append(s)
    if True:
        good_songs = np.arange(10)
    if len(good_songs) > 2:
        dfT_high = df_thresh[df_thresh.Song == good_songs[-1]]
        dfT_low = df_thresh[df_thresh.Song == good_songs[0]]

        highT_xs = dfT_high.Block
        highT_ys = dfT_high.Response
        lowT_xs = dfT_low.Block
        lowT_ys = dfT_low.Response

        highT_reg = linregress(highT_xs,highT_ys)
        lowT_reg = linregress(lowT_xs,lowT_ys)

        highT_slope = highT_reg[0]
        lowT_slope = lowT_reg[0]

        thresh_divergence = np.sign(lowT_slope * highT_slope)
    else:
        thresh_divergence = 0

    bad_songs,good_songs = [],[]
    for s in pd.unique(df_pot.Song):
        if len(np.unique(df_pot[df_pot.Song == s].Response)) == 1:
            bad_songs.append(s)
        else:
            good_songs.append(s)

    if True:
        good_songs = np.arange(10)
    if len(good_songs) > 2:
        dfP_high = df_pot[df_pot.Song == good_songs[-1]]
        dfP_low = df_pot[df_pot.Song == good_songs[0]]

        #dfP_high = df_pot[df_pot.Song >= 8]
        #dfP_low = df_pot[df_pot.Song <= 2]
        highP_xs = dfP_high.Block
        highP_ys = dfP_high.Response
        lowP_xs = dfP_low.Block
        lowP_ys = dfP_low.Response

        highP_reg = linregress(highP_xs,highP_ys)
        lowP_reg = linregress(lowP_xs,lowP_ys)


        highP_slope = highP_reg[0]
        lowP_slope = lowP_reg[0]
        pot_divergence = np.sign(lowP_slope * highP_slope)
    else:
        pot_divergence = 0

    if pot_divergence == 1 and DEBUG:
        print('fail...')
        import pdb;pdb.set_trace()
        pass
    elif pot_divergence == 0 and DEBUG:
        print('null?')
        import pdb;pdb.set_trace()
        pass
    if False:
        if np.sign(highT_slope) > np.sign(lowT_slope):
            thresh_divergence = True
        else:
            thresh_divergence = False
        if np.sign(highP_slope) > np.sign(lowP_slope):
            pot_divergence = True
        else:
            pot_divergence = False
    e_list = [None,None]
    p_list = [None,None]
    pot_interaction,thresh_interaction = [None,None]

    return [ [pot_interaction,thresh_interaction],[pot_divergence,thresh_divergence],e_list,p_list]

## This is a much more reasponable way to treat real data, but for the simulation it is very slow, 
# and because the agents are quite similar to themselves, you tend to get some weird outcomes (singular fits, no convergence, etc)
def getRstats(df_thresh,df_pot):
    if True:
        from pymer4.models import Lmer

## There are a couple conditions that can't converge. Some are easier to catch than others
        if len(df_pot.Response.unique()) > 1:
            try:
                modelPot = Lmer("Response ~ Song * Block + (1|Bird)",data=df_pot,family='binomial')
                modelPot.fit(summary=False,no_warnings=True)

                pot_divergence = check_divergence(df_pot,modelPot)
                pot_interaction = modelPot.coefs["P-val"]["Song:Block"] 
            except:
                print('oops')
                pot_divergence = np.nan
                pot_interaction = np.nan
                #import pdb;pdb.set_trace()
        else:
            print('Singular pot')
            pot_divergence = np.nan
            pot_interaction = np.nan

        if len(df_thresh.Response.unique()) > 1:
            try:
                modelThresh = Lmer("Response ~ Song * Block + (1|Bird)",data=df_thresh,family='binomial')
                modelThresh.fit(summary=False,no_warnings=True)

                thresh_divergence = check_divergence(df_thresh,modelThresh)
                thresh_interaction = modelThresh.coefs["P-val"]["Song:Block"] 
            except:
                print('oopsie')
                #import pdb;pdb.set_trace()
                thresh_divergence,thresh_interaction = np.nan,np.nan
        else:
            print('Singular Thresh')
            thresh_divergence,thresh_interaction = np.nan,np.nan
        #df_pot = df_pot[df_pot.Block != 0]
        #df_thresh = df_thresh[df_thresh.Block != 0]
        if len(df_pot.B1Potency.unique()) > 1 and len(df_pot.Response.unique()) > 1:
            try:
                modelPot1 = Lmer("Response ~ B1Potency + (1|Bird) + (1|Song)",data=df_pot,family='binomial')
                modelPot1.fit(summary=False,no_warnings=True)
                p_pot = modelPot1.coefs["P-val"].B1Potency
                e_pot = modelPot1.coefs.Estimate.B1Potency
            except:
                if not modelPot1.fitted:
                    print('Potency Model failed to converge!')
                    e_pot,p_pot = np.nan,np.nan
                else:
                    print('Did not predict intercept??')
                    e_pot,p_pot = np.nan,np.nan
                    import pdb;pdb.set_trace()
        else:
            print('Potency model singular')
            e_pot,p_pot = np.nan,np.nan
        if len(df_thresh.B1Potency.unique()) > 1 and len(df_thresh.Response.unique()) > 1:
            #import pdb;pdb.set_trace()

            try:
                modelThresh1 = Lmer("Response ~ B1Potency + (1|Bird) + (1|Song)",data=df_thresh,family='binomial')
                modelThresh1.fit(summary=False,no_warnings=True)
                p_thresh = modelThresh1.coefs["P-val"].B1Potency
                e_thresh = modelThresh1.coefs.Estimate.B1Potency
            except:
                if not modelThresh1.fitted:
                    #print('Threshold Model failed to converge')
                    e_thresh,p_thresh = np.nan,np.nan
                else:
                    print('test:')
                    import pdb;pdb.set_trace()
        else:
            e_thresh,p_thresh = np.nan,np.nan
            #print('Thresh model singular')
        e_list = [e_pot,e_thresh]
        p_list = [p_pot,p_thresh]
        
    else:
        e_list,p_list = subprocessR(df_pot,df_thresh)
    return [ [pot_interaction,thresh_interaction],[pot_divergence,thresh_divergence],e_list,p_list]

def run_exp(params,save_dfs = False):
    thresh_array = np.empty([params.n_birds,params.n_rounds])
    for b in range(params.n_birds):
        start_noise = (np.random.random() - 0.5) * params.thresh_noise
        end_noise = (np.random.random() - 0.5) * params.thresh_noise

        thresh_min = max([0,params.thresh - params.thresh_range + start_noise])
        thresh_max = min([1,params.thresh + params.thresh_range + end_noise])

        thresh_array[b] = np.linspace(thresh_min,thresh_max,params.n_rounds)
## Set up potency arrays
    bias_potency = np.linspace(0.1,.9,params.n_songs)
    random_potency = np.sort(np.random.random(params.n_songs)) ## if it's random, it's in the i loop
    true_potency = params.bias * bias_potency + (1-params.bias) * random_potency

    #print(true_potency)
    #relative_potency = np.array(true_potency)
    #true_potency = np.linspace(0.05,0.95,n_songs)**3
    relative_potency = true_potency**params.curve
    relative_potency = np.round(relative_potency,3)
    #relative_potency = np.linspace(0.05,0.95,n_songs)**3
    #relative_potency[:-3] -= 0.3

    potency_array = np.zeros([params.n_rounds,params.n_songs])
    #noise_array = np.linspace(params.signal_noise,0,params.n_rounds) ## Would be good to try this with different values...
    noise_array = np.zeros(params.n_rounds)
    noise_array[:params.lr] = np.linspace(0.5,0,params.lr) ## Would be good to try this with different values...
    #noise_array[:params.lr] = np.linspace(params.signal_noise,0,params.lr) ## Would be good to try this with different values...
    for s in range(params.n_songs):
        potency_array[:params.lr,s] = np.linspace(params.naive_potency,relative_potency[s],params.lr)
        potency_array[params.lr:,s] = relative_potency[s]
    response_array_thresh = np.full([params.n_birds,params.n_rounds,params.n_songs],np.nan)
    response_array_pot = np.array(response_array_thresh)
    response_array_noise = np.array(response_array_thresh)
#potency_array[n_songs-3:] 
    df_pot = []
    df_thresh = []
    for b in range(params.n_birds):
        bird_df_pot = []
        bird_df_thresh = []
        bird_df_noise = []

        pot_list_flat = np.full(params.n_rounds * params.n_songs,np.nan)
        thresh_list_flat = np.array(pot_list_flat)
        noise_list_flat = np.array(pot_list_flat)

        count = 0
        for r in range(params.n_rounds):
            threshold = thresh_array[b,r] 

## Here we're calculating the response rate per day
            for s in range(params.n_songs):
                day = count % 8
                realized_potency_thresh = true_potency[s] + np.random.choice([-1,1]) * np.random.random() * params.signal_noise
                realized_potency_pot = potency_array[r,s] + np.random.choice([-1,1]) * np.random.random() * params.signal_noise
                realized_potency_noise = true_potency[s] + np.random.choice([-1,1]) * np.random.random() * noise_array[r]
                if realized_potency_thresh > threshold: 
                    thresh_response = 1
                else:
                    thresh_response = 0
                response_array_thresh[b,r,s] = thresh_response
                thresh_list_flat[count] = thresh_response
                if realized_potency_pot > params.thresh:
                    pot_response = 1
                else:
                    pot_response = 0
                response_array_pot[b,r,s] = pot_response
                pot_list_flat[count] = pot_response
                if realized_potency_noise > params.thresh:
                    noise_response = 1
                else:
                    noise_response = 0

                response_array_noise[b,r,s] = noise_response
                noise_list_flat[count] = noise_response
                count += 1
                
                #df_thresh.append([b,r,s,day,thresh_response,mean_thresh_peri])
                #df_pot.append([b,r,s,day,pot_response,mean_pot_peri])
                bird_df_thresh.append([b,r,s,day,thresh_response,0,0,0])
                bird_df_pot.append([b,r,s,day,pot_response,0,0,0])
                bird_df_noise.append([b,r,s,day,noise_response,0,0,0])

## have to go through a second time to get peri-stimulus and first and last

## Optionally, instead of "weight shift" use "noise reduction", which I think should be equivalent
        if NOISE:
            #print('using noise')
            response_array_pot = response_array_noise
            pot_list_flat = noise_list_flat
            bird_df_pot = bird_df_noise
        for c in range(len(pot_list_flat)): ## get through each line of the dataframe
            s = bird_df_thresh[c][2]
            thresh_b1 = response_array_thresh[b,0,s]
            thresh_b10 = response_array_thresh[b,9,s]
            pot_b1 = response_array_pot[b,0,s]
            pot_b10 = response_array_pot[b,9,s]

            back2 = max([c-2,0])
            forward2 = c+3
            pot_peri_response = pot_list_flat[back2:forward2]
            mean_pot_peri = np.nanmean(pot_peri_response[np.arange(len(pot_peri_response)) != 2])

            thresh_peri_response = thresh_list_flat[c-2:c+2]
            mean_thresh_peri = np.nanmean(thresh_peri_response[np.arange(len(thresh_peri_response)) != 2])
            bird_df_thresh[c][-1] = mean_thresh_peri
            bird_df_pot[c][-1] = mean_pot_peri

            bird_df_thresh[c][-2] = thresh_b10
            bird_df_pot[c][-2] = pot_b10

            bird_df_thresh[c][-3] = thresh_b1
            bird_df_pot[c][-3] = pot_b1

        df_thresh.extend(bird_df_thresh)
        df_pot.extend(bird_df_pot)
## NOTE: This is where I can calculate correlation

    mean_response_array_pot = np.mean(response_array_pot,axis=0)
    mean_response_array_thresh = np.mean(response_array_thresh,axis=0)
    measured_potency_pot = np.argsort(np.mean(mean_response_array_pot,axis=0))[::-1] 
    measured_potency_thresh = np.argsort(np.mean(mean_response_array_thresh,axis=0))[::-1]
    flat_response_pot = mean_response_array_pot[:,measured_potency_pot].flatten()
    flat_response_thresh = mean_response_array_thresh[:,measured_potency_thresh].flatten()

    #pot_similarity,_ = pearsonr(flat_response_pot,flat_response_data)
    #thresh_similarity,_ = pearsonr(flat_response_thresh,flat_response_data)
    if DATA:
        pot_similarity_ = ((flat_response_pot - flat_response_data)**2).mean()
        thresh_similarity_ = ((flat_response_thresh - flat_response_data)**2).mean()
        correlation_array[i] = pot_similarity_,thresh_similarity_

    #import pdb;pdb.set_trace()

    #df_thresh = np.array(df_thresh)
    #df_pot = np.array(df_pot)
    columns = ['Bird','Block','Song','Day','Response','B1Response','B10Response','PeriStimMean']
    df_thresh = pd.DataFrame(df_thresh,columns=columns)
    df_pot = pd.DataFrame(df_pot,columns=columns)

## Add in one more column
    columns.append('B1Potency')
    df_thresh['B1Potency'] = 0.0
    df_pot['B1Potency'] = 0.0 
    for b in range(params.n_birds):
        other_birds = np.arange(params.n_birds)[np.arange(params.n_birds) != b]
        thresh_avgb1 = np.nanmean(response_array_thresh[other_birds,0],0)
        pot_avgb1 = np.nanmean(response_array_pot[other_birds,0],0)
        for s in range(params.n_songs):
            df_thresh.loc[(df_thresh.Bird == b) & (df_thresh.Song == s),"B1Potency"] = thresh_avgb1
            df_pot.loc[(df_pot.Bird == b) & (df_pot.Song == s),"B1Potency"] = pot_avgb1

    if save_dfs:
        df_thresh['AvgPotency'] = 0.0
        df_pot['AvgPotency'] = 0.0

        n_birds = params.n_birds
        n_songs = params.n_songs
        for b in range(n_birds):
            other_birds = np.arange(n_birds)[np.arange(n_birds) != b]
            thresh_scores = np.nanmean(response_array_thresh[other_birds],axis=(0,1))
            pot_scores = np.nanmean(response_array_pot[other_birds],axis=(0,1))
            for s in range(n_songs):
                df_thresh.loc[(df_thresh.Bird == b) & (df_thresh.Song == s),'AvgPotency'] = thresh_scores[s]
                df_pot.loc[(df_pot.Bird == b) & (df_pot.Song == s),'AvgPotency'] = pot_scores[s]


        df_thresh.to_csv('./tmp_thresh.csv')
        df_pot.to_csv('./tmp_pot.csv')
    #import pdb;pdb.set_trace()
    #all_threshs[i] = response_array_thresh
    #all_pots[i] = response_array_pot

## In theory, this works, but it can be hard to get your R and python environments to play nice

    ## This next section needs to return: 
    #      return [ [pot_interaction,thresh_interaction],[pot_divergence,thresh_divergence],e_list,p_list]
    #print('heading into it now:')
    if False:
        return getRstats(df_thresh,df_pot)

    elif True:
        return stupidStats(df_thresh,df_pot)
    elif True:
        from pymer4.models import Lmer

## There are a couple conditions that can't converge. Some are easier to catch than others
        if len(df_pot.Response.unique()) > 1:
            try:
                modelPot = Lmer("Response ~ Song * Block + (1|Bird)",data=df_pot,family='binomial')
                modelPot.fit(summary=False,no_warnings=True)

                pot_divergence = check_divergence(df_pot,modelPot)
                pot_interaction = modelPot.coefs["P-val"]["Song:Block"] 
            except:
                print('oops')
                pot_divergence = np.nan
                pot_interaction = np.nan
                #import pdb;pdb.set_trace()
        else:
            print('Singular pot')
            pot_divergence = np.nan
            pot_interaction = np.nan

        if len(df_thresh.Response.unique()) > 1:
            try:
                modelThresh = Lmer("Response ~ Song * Block + (1|Bird)",data=df_thresh,family='binomial')
                modelThresh.fit(summary=False,no_warnings=True)

                thresh_divergence = check_divergence(df_thresh,modelThresh)
                thresh_interaction = modelThresh.coefs["P-val"]["Song:Block"] 
            except:
                print('oopsie')
                #import pdb;pdb.set_trace()
                thresh_divergence,thresh_interaction = np.nan,np.nan
        else:
            print('Singular Thresh')
            thresh_divergence,thresh_interaction = np.nan,np.nan
        #df_pot = df_pot[df_pot.Block != 0]
        #df_thresh = df_thresh[df_thresh.Block != 0]
        if len(df_pot.B1Potency.unique()) > 1 and len(df_pot.Response.unique()) > 1:
            try:
                modelPot1 = Lmer("Response ~ B1Potency + (1|Bird) + (1|Song)",data=df_pot,family='binomial')
                modelPot1.fit(summary=False,no_warnings=True)
                p_pot = modelPot1.coefs["P-val"].B1Potency
                e_pot = modelPot1.coefs.Estimate.B1Potency
            except:
                if not modelPot1.fitted:
                    print('Potency Model failed to converge!')
                    e_pot,p_pot = np.nan,np.nan
                else:
                    print('Did not predict intercept??')
                    e_pot,p_pot = np.nan,np.nan
                    import pdb;pdb.set_trace()
        else:
            print('Potency model singular')
            e_pot,p_pot = np.nan,np.nan
        if len(df_thresh.B1Potency.unique()) > 1 and len(df_thresh.Response.unique()) > 1:
            #import pdb;pdb.set_trace()

            try:
                modelThresh1 = Lmer("Response ~ B1Potency + (1|Bird) + (1|Song)",data=df_thresh,family='binomial')
                modelThresh1.fit(summary=False,no_warnings=True)
                p_thresh = modelThresh1.coefs["P-val"].B1Potency
                e_thresh = modelThresh1.coefs.Estimate.B1Potency
            except:
                if not modelThresh1.fitted:
                    #print('Threshold Model failed to converge')
                    e_thresh,p_thresh = np.nan,np.nan
                else:
                    print('test:')
                    import pdb;pdb.set_trace()
        else:
            e_thresh,p_thresh = np.nan,np.nan
            #print('Thresh model singular')
        e_list = [e_pot,e_thresh]
        p_list = [p_pot,p_thresh]
        
    else:
        e_list,p_list = subprocessR(df_pot,df_thresh)
    #import pdb;pdb.set_trace()
    return [ [pot_interaction,thresh_interaction],[pot_divergence,thresh_divergence],e_list,p_list]


## Run a single simulation
def run_sim(params,debug=False):
    #all_threshs = np.zeros([params.iterations,params.n_birds,params.n_rounds,params.n_songs]).astype(bool)
    #all_pots = np.zeros_like(all_threshs)

    effect_array = np.empty([params.iterations,2])
    interaction_array = np.empty([params.iterations,2])
    divergence_array = np.empty([params.iterations,2])
    correlation_array = np.empty([params.iterations,2])
    p_array = np.empty([params.iterations,2])
    for i in range(params.iterations):
## Set up threshold array for each bird
        if True:
            outcome = run_exp(params)
            interaction_array[i] = outcome[0]
            divergence_array[i] = outcome[1]
            effect_array[i] = outcome[2]
            p_array[i] = outcome[3]

        else:
            #import pdb;pdb.set_trace()
            thresh_array = np.empty([params.n_birds,params.n_rounds])
            for b in range(params.n_birds):
                start_noise = (np.random.random() - 0.5) * params.thresh_noise
                end_noise = (np.random.random() - 0.5) * params.thresh_noise

                thresh_min = max([0,params.thresh - params.thresh_range + start_noise])
                thresh_max = min([1,params.thresh + params.thresh_range + end_noise])

                thresh_array[b] = np.linspace(thresh_min,thresh_max,params.n_rounds)
## Set up potency arrays
            bias_potency = np.linspace(0.1,.9,params.n_songs)
            random_potency = np.sort(np.random.random(params.n_songs)) ## if it's random, it's in the i loop
            true_potency = params.bias * bias_potency + (1-params.bias) * random_potency

            #print(true_potency)
            #relative_potency = np.array(true_potency)
            #true_potency = np.linspace(0.05,0.95,n_songs)**3
            relative_potency = true_potency**params.curve
            relative_potency = np.round(relative_potency,3)
            #relative_potency = np.linspace(0.05,0.95,n_songs)**3
            #relative_potency[:-3] -= 0.3

            potency_array = np.zeros([params.n_rounds,params.n_songs])
            #noise_array = np.linspace(params.signal_noise,0,params.n_rounds) ## Would be good to try this with different values...
            noise_array = np.linspace(0.5,0,params.n_rounds) ## Would be good to try this with different values...
            for s in range(params.n_songs):
                potency_array[:params.lr,s] = np.linspace(params.naive_potency,relative_potency[s],params.lr)
                potency_array[params.lr:,s] = relative_potency[s]
            response_array_thresh = np.full([params.n_birds,params.n_rounds,params.n_songs],np.nan)
            response_array_pot = np.array(response_array_thresh)
            response_array_noise = np.array(response_array_thresh)
#potency_array[n_songs-3:] 
            df_pot = []
            df_thresh = []
            for b in range(params.n_birds):
                bird_df_pot = []
                bird_df_thresh = []
                bird_df_noise = []

                pot_list_flat = np.full(params.n_rounds * params.n_songs,np.nan)
                thresh_list_flat = np.array(pot_list_flat)
                noise_list_flat = np.array(pot_list_flat)

                count = 0
                for r in range(params.n_rounds):
                    threshold = thresh_array[b,r] 

## Here we're calculating the response rate per day
                    for s in range(params.n_songs):
                        day = count % 8
                        if False:
                            realized_potency_thresh = true_potency[s] + np.random.choice([-1,1]) * np.random.random() * params.signal_noise
                            realized_potency_pot = potency_array[r,s] + np.random.choice([-1,1]) * np.random.random() * params.signal_noise
                            realized_potency_noise = true_potency[s] + np.random.choice([-1,1]) * np.random.random() * noise_array[r]
                        else:
                            realized_potency_thresh = true_potency[s] + np.random.normal(0,params.signal_noise)
                            realized_potency_pot = potency_array[r,s] + np.random.normal(0,params.signal_noise)
                            realized_potency_noise = true_potency[s] + np.random.normal(0,noise_array[r]) 
                        if realized_potency_thresh > threshold: 
                            thresh_response = 1
                        else:
                            thresh_response = 0
                        response_array_thresh[b,r,s] = thresh_response
                        thresh_list_flat[count] = thresh_response
                        if realized_potency_pot > params.thresh:
                            pot_response = 1
                        else:
                            pot_response = 0
                        response_array_pot[b,r,s] = pot_response
                        pot_list_flat[count] = pot_response
                        if realized_potency_noise > params.thresh:
                            noise_response = 1
                        else:
                            noise_response = 0

                        response_array_noise[b,r,s] = noise_response
                        noise_list_flat[count] = noise_response
                        count += 1
                        
                        #df_thresh.append([b,r,s,day,thresh_response,mean_thresh_peri])
                        #df_pot.append([b,r,s,day,pot_response,mean_pot_peri])
                        bird_df_thresh.append([b,r,s,day,thresh_response,0,0,0])
                        bird_df_pot.append([b,r,s,day,pot_response,0,0,0])
                        bird_df_noise.append([b,r,s,day,noise_response,0,0,0])

## have to go through a second time to get peri-stimulus and first and last

## Optionally, instead of "weight shift" use "noise reduction", which I think should be equivalent
                print('using pot!')
                if False:
                    response_array_pot = response_array_noise
                    pot_list_flat = noise_list_flat
                    bird_df_pot = bird_df_noise
                for c in range(len(pot_list_flat)): ## get through each line of the dataframe
                    s = bird_df_thresh[c][2]
                    thresh_b1 = response_array_thresh[b,0,s]
                    thresh_b10 = response_array_thresh[b,9,s]
                    pot_b1 = response_array_pot[b,0,s]
                    pot_b10 = response_array_pot[b,9,s]

                    back2 = max([c-2,0])
                    forward2 = c+3
                    pot_peri_response = pot_list_flat[back2:forward2]
                    mean_pot_peri = np.nanmean(pot_peri_response[np.arange(len(pot_peri_response)) != 2])

                    thresh_peri_response = thresh_list_flat[c-2:c+2]
                    mean_thresh_peri = np.nanmean(thresh_peri_response[np.arange(len(thresh_peri_response)) != 2])
                    bird_df_thresh[c][-1] = mean_thresh_peri
                    bird_df_pot[c][-1] = mean_pot_peri

                    bird_df_thresh[c][-2] = thresh_b10
                    bird_df_pot[c][-2] = pot_b10

                    bird_df_thresh[c][-3] = thresh_b1
                    bird_df_pot[c][-3] = pot_b1

                df_thresh.extend(bird_df_thresh)
                df_pot.extend(bird_df_pot)
## NOTE: This is where I can calculate correlation

            mean_response_array_pot = np.mean(response_array_pot,axis=0)
            mean_response_array_thresh = np.mean(response_array_thresh,axis=0)
            measured_potency_pot = np.argsort(np.mean(mean_response_array_pot,axis=0))[::-1] 
            measured_potency_thresh = np.argsort(np.mean(mean_response_array_thresh,axis=0))[::-1]
            flat_response_pot = mean_response_array_pot[:,measured_potency_pot].flatten()
            flat_response_thresh = mean_response_array_thresh[:,measured_potency_thresh].flatten()

            #pot_similarity,_ = pearsonr(flat_response_pot,flat_response_data)
            #thresh_similarity,_ = pearsonr(flat_response_thresh,flat_response_data)
            if DATA:
                pot_similarity_ = ((flat_response_pot - flat_response_data)**2).mean()
                thresh_similarity_ = ((flat_response_thresh - flat_response_data)**2).mean()
                correlation_array[i] = pot_similarity_,thresh_similarity_

            #import pdb;pdb.set_trace()

            #df_thresh = np.array(df_thresh)
            #df_pot = np.array(df_pot)
            columns = ['Bird','Block','Song','Day','Response','B1Response','B10Response','PeriStimMean']
            df_thresh = pd.DataFrame(df_thresh,columns=columns)
            df_pot = pd.DataFrame(df_pot,columns=columns)

## Add in one more column
            columns.append('B1Potency')
            df_thresh['B1Potency'] = 0.0
            df_pot['B1Potency'] = 0.0 
            for b in range(params.n_birds):
                other_birds = np.arange(params.n_birds)[np.arange(params.n_birds) != b]
                thresh_avgb1 = np.nanmean(response_array_thresh[other_birds,0],0)
                pot_avgb1 = np.nanmean(response_array_pot[other_birds,0],0)
                for s in range(params.n_songs):
                    df_thresh.loc[(df_thresh.Bird == b) & (df_thresh.Song == s),"B1Potency"] = thresh_avgb1
                    df_pot.loc[(df_pot.Bird == b) & (df_pot.Song == s),"B1Potency"] = pot_avgb1

            #import pdb;pdb.set_trace()
            #all_threshs[i] = response_array_thresh
            #all_pots[i] = response_array_pot

## In theory, this works, but it can be hard to get your R and python environments to play nice

            if True:
                from pymer4.models import Lmer

## There are a couple conditions that can't converge. Some are easier to catch than others
                if len(df_pot.Response.unique()) > 1:
                    try:
                        modelPot = Lmer("Response ~ Song * Block + (1|Bird)",data=df_pot,family='binomial')
                        modelPot.fit(summary=False,no_warnings=True)

                        pot_divergence = check_divergence(df_pot,modelPot)
                        pot_interaction = modelPot.coefs["P-val"]["Song:Block"] 
                    except:
                        print('oops')
                        pot_divergence = np.nan
                        pot_interaction = np.nan
                        #import pdb;pdb.set_trace()
                else:
                    print('Singular pot')
                    pot_divergence = np.nan
                    pot_interaction = np.nan

                if len(df_thresh.Response.unique()) > 1:
                    try:
                        modelThresh = Lmer("Response ~ Song * Block + (1|Bird)",data=df_thresh,family='binomial')
                        modelThresh.fit(summary=False,no_warnings=True)

                        thresh_divergence = check_divergence(df_thresh,modelThresh)
                        thresh_interaction = modelThresh.coefs["P-val"]["Song:Block"] 
                    except:
                        print('oopsie')
                        #import pdb;pdb.set_trace()
                        thresh_divergence,thresh_interaction = np.nan,np.nan
                else:
                    print('Singular Thresh')
                    thresh_divergence,thresh_interaction = np.nan,np.nan
                #df_pot = df_pot[df_pot.Block != 0]
                #df_thresh = df_thresh[df_thresh.Block != 0]
                if len(df_pot.B1Potency.unique()) > 1 and len(df_pot.Response.unique()) > 1:
                    try:
                        modelPot1 = Lmer("Response ~ B1Potency + (1|Bird) + (1|Song)",data=df_pot,family='binomial')
                        modelPot1.fit(summary=False,no_warnings=True)
                        p_pot = modelPot1.coefs["P-val"].B1Potency
                        e_pot = modelPot1.coefs.Estimate.B1Potency
                    except:
                        if not modelPot1.fitted:
                            print('Potency Model failed to converge!')
                            e_pot,p_pot = np.nan,np.nan
                        else:
                            print('Did not predict intercept??')
                            e_pot,p_pot = np.nan,np.nan
                            import pdb;pdb.set_trace()
                else:
                    print('Potency model singular')
                    e_pot,p_pot = np.nan,np.nan
                if len(df_thresh.B1Potency.unique()) > 1 and len(df_thresh.Response.unique()) > 1:
                    #import pdb;pdb.set_trace()

                    try:
                        modelThresh1 = Lmer("Response ~ B1Potency + (1|Bird) + (1|Song)",data=df_thresh,family='binomial')
                        modelThresh1.fit(summary=False,no_warnings=True)
                        p_thresh = modelThresh1.coefs["P-val"].B1Potency
                        e_thresh = modelThresh1.coefs.Estimate.B1Potency
                    except:
                        if not modelThresh1.fitted:
                            #print('Threshold Model failed to converge')
                            e_thresh,p_thresh = np.nan,np.nan
                        else:
                            print('test:')
                            import pdb;pdb.set_trace()
                else:
                    e_thresh,p_thresh = np.nan,np.nan
                    #print('Thresh model singular')
                e_list = [e_pot,e_thresh]
                p_list = [p_pot,p_thresh]
                
            else:
                e_list,p_list = subprocessR(df_pot,df_thresh)
            #import pdb;pdb.set_trace()
            interaction_array[i] = [pot_interaction,thresh_interaction]
            divergence_array[i] = [pot_divergence,thresh_divergence]
            effect_array[i] = e_list
            p_array[i] = p_list
        if debug:
            import pdb;pdb.set_trace()
    mean_effects = np.nanmean(effect_array,axis=0)
    sig_array = np.zeros_like(p_array)
    clean_effect_array = np.array(effect_array)
    clean_effect_array[np.isnan(effect_array)] = 0
    sig_array[:,0] = (p_array[:,0] < 0.05) & (clean_effect_array[:,0] > 0)
    sig_array[:,1] = (p_array[:,1] < 0.05) & (clean_effect_array[:,1] > 0)
    prop_divergent = np.mean(divergence_array,0)
    prop_p = np.mean(sig_array,axis=0)
    return divergence_array,correlation_array

## Assumes a bunch of global params exist, including def_params
n_results = 4
def run_many_sims(err):
    #import warnings
    if True:
        warnings.simplefilter(action='ignore', category=FutureWarning)
        warnings.filterwarnings("ignore")

    some_arrays = np.empty([b_res,l_res,t_res,def_params.iterations,n_results])
    s_params = def_params.copy()
    s_params.signal_noise = err
    for b_ in tqdm(range(b_res)):
        b = bias_list[b_]
        for l_ in range(l_res):
            l = lr_list[l_]
            for t_ in range(t_res):
                t = thresh_list[t_]
                s_params.lr = l
                s_params.thresh = t
                s_params.bias = b
                divergence_results,correlation_results = run_sim(s_params)
                some_arrays[b_,l_,t_,:,:2] = divergence_results
                some_arrays[b_,l_,t_,:,2:] = correlation_results
    return some_arrays
            
#print(np.mean(response_array_thresh,0))
if __name__ == "__main__":
    if False:
        print('running test sim')
        test_params = Params()
        test_params.iterations = 5
        test_params.lr = 9
        test_params.thresh = 0.5
        test_params.bias = 1
        test_params.signal_noise = 0.1
        div,corr = run_sim(test_params,False)
        import pdb;pdb.set_trace()
    if DATA:
        response_data = np.load('../response_data.npy')
        flat_response_data = response_data.flatten()
    def_params = Params()
    def_params.iterations = 3
    def_params.iterations = 100
    def_params.n_birds = 15
## Build potency array
    sep_count = 0

## Define parameter array
    e_res = 6
    b_res = 6 
    l_res = 3
    t_res = 3

    noise_list = np.linspace(0,0.5,6) # e
    bias_list = np.linspace(0.0,1,6)  # b
    lr_list = [1,5,10]                # l
    thresh_list = [0.25,0.5,0.75]     # t
## Need to set up all the parameters, and set it up to run in parallel

    if False:
        one_result = run_sim(def_params)
        print(one_results)
    e = 0.1
    #some_results = run_many_sims(e)
    if not DEBUG:
        all_arrays = Parallel(n_jobs=6)(delayed(run_many_sims)(err) for err in noise_list)
        all_arrays = np.array(all_arrays)
    else:
        all_arrays = np.empty([e_res,b_res,l_res,t_res,def_params.iterations,n_results])
        for e_ in tqdm(range(e_res)):
            err = noise_list[e_]
            all_arrays[e_] = run_many_sims(err)
    print(all_arrays.shape)
    #import pdb;pdb.set_trace()
## Iterate through parameters in parallel
    array_check = (all_arrays[:,:,:,:,:,0] == -1) & (all_arrays[:,:,:,:,:,1] == 1)
    NegWeight = all_arrays[:,:,:,:,:,0] == -1 ## Where divergence occured for weight shift
    PosWeight = all_arrays[:,:,:,:,:,0] == 1 ## Where parallel occured for weight shift
    NullWeight = (1 - NegWeight) & (1 - PosWeight) ## Where neither (e.g., high/low did not change)

    NegThresh = all_arrays[:,:,:,:,:,1] == -1 ## Where divergence occurred for threshold
    PosThresh = all_arrays[:,:,:,:,:,1] == 1 ## Where divergence occurred for threshold.
    NullThresh = (1- NegThresh) & (1 - PosThresh) ## Where neither occurred.

    ProbNegWeight = np.mean(NegWeight,axis=4).transpose([2,0,3,1]).reshape([18,18])
    ProbPosWeight = np.mean(PosWeight,axis=4).transpose([2,0,3,1]).reshape([18,18])
    ProbNullWeight = np.mean(NullWeight,axis=4).transpose([2,0,3,1]).reshape([18,18])

    ProbNegThresh = np.mean(NegThresh,axis=4).transpose([2,0,3,1]).reshape([18,18])
    ProbPosThresh = np.mean(PosThresh,axis=4).transpose([2,0,3,1]).reshape([18,18])
    ProbNullThresh = np.mean(NullThresh,axis=4).transpose([2,0,3,1]).reshape([18,18])
    P_W_neg = 0.5 * ProbNegWeight / (0.5 * ProbNegWeight + 0.5 * ProbNegThresh)
    P_T_pos = 0.5 * ProbPosThresh / (0.5 * ProbPosThresh + 0.5 * ProbPosWeight)
    P_T_null = 0.5 * ProbNullThresh / (0.5 * ProbNullThresh + 0.5 * ProbNullWeight)
    P_correct = (ProbNegWeight + ProbPosThresh) / 2
    P_wrong = (ProbPosWeight + ProbNegThresh) / 2 
    P_null = (ProbNullWeight + ProbNullThresh) / 2 
    
    fig,axes = plt.subplots(2,3)
    axes[0,0].set_title("P(W | Divergence)") 
    axes[0,0].imshow(P_W_neg,vmin=0,vmax=1)

    axes[0,1].set_title("P(T | Parallel)") 
    axes[0,1].imshow(P_T_pos,vmin=0,vmax=1)

    axes[0,2].set_title("P(T | Neither)") 
    axes[0,2].imshow(P_T_null,vmin=0,vmax=1)

    axes[1,0].set_title("P of correct conclusion")
    axes[1,0].imshow(P_correct,vmin=0,vmax=1)

    axes[1,1].set_title("P of false conclusion")
    axes[1,1].imshow(P_wrong,vmin=0,vmax=1)

    axes[1,2].set_title("P of inconclusive")
    axes[1,2].imshow(P_null,vmin=0,vmax=1)

    fig2,(ax1,ax2) = plt.subplots(1,2)
    mse_pot = all_arrays[:,:,:,:,:,2]
    mse_thresh = all_arrays[:,:,:,:,:,3]
    mean_mse_pot = mse_pot.mean(axis=4).transpose([2,0,3,1]).reshape([18,18])
    mean_mse_thresh = mse_thresh.mean(axis=4).transpose([2,0,3,1]).reshape([18,18])
    super_max = max([np.nanmax(mean_mse_pot),np.nanmax(mean_mse_thresh)])
    ax1.set_title("MSE Potency")
    ax2.set_title("MSE Threshold")
    ax1.imshow(mean_mse_pot,vmin=0,vmax=super_max)
    ax2.imshow(mean_mse_thresh,vmin=0,vmax=super_max)
    print(super_max)
    fig.suptitle(str(np.nanmean(P_wrong)) + ' ' + str(np.nanmean(P_T_pos)))
    fig.savefig("param_exploration.png",dpi=300)
    fig2.savefig("best_fit.png",dpi=300)
    plt.show()

    mean_check = np.mean(array_check,axis=4)
    mean_checkT = mean_check.transpose([2,0,3,1]).reshape([18,18])
    import pdb;pdb.set_trace()
## NOTE: start here START HERE:
# Visualize heatmap and line plots to gain intuition.
    if False:
        fig,ax=plt.subplots()
        ax.imshow(mean_checkT)
        plt.show()
        import pdb;pdb.set_trace()
            

#print(np.mean(response_array_pot,0))
    if False:
        fig,(ax,ax1) = plt.subplots(1,2)

        print('n skipped:',sep_count)
        print(np.nanmean(sig_array,0))
        mean_response_thresh = np.nanmean(all_threshs,axis=(0,1))
        mean_response_pots = np.nanmean(all_pots,axis=(0,1))

        ax.imshow(mean_response_pots,vmax=1,vmin=0)
        im = ax1.imshow(mean_response_thresh,vmax=1,vmin=0)
        ax.invert_xaxis()
        ax1.invert_xaxis()
#fig.colorbar(im,ax=ax1,shrink=0.7)

        fig.show()
        plt.show()

