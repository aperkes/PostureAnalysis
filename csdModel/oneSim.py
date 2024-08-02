from checkParams import Params,run_exp

params = Params()
params.bias=1.0
params.signal_noise=0.25
params.thresh_range=0.5
params.thresh=0.50
params.lr = 10
run_exp(params,save_dfs=True)
