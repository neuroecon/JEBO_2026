


import pandas as pd
from patsy import dmatrix
import numpy as np
import statsmodels.api as sm
import os,sys
import matplotlib
import matplotlib.gridspec as gridspec
import matplotlib.pyplot as plt
from scipy import stats
import pymc as mc
import operator
import logging
from pandas import Series
import datetime
import hddm
import random


date_var = datetime.date.today().strftime("%m-%d-%Y")
#exp_dir = '/Users/chennan/Dropbox/DDM + Market Share/Analysis/all/results'
exp_dir = 'C:\Users\Nan\Documents\Dropbox\DDM + Market Share\Analysis\All\Results'
analysis_dir = os.path.join(exp_dir,'session3_random')
if os.path.isdir(analysis_dir) == False:
    os.mkdir(analysis_dir)
date_analysis_dir = os.path.join(analysis_dir,date_var)
if os.path.isdir(date_analysis_dir) == False:
    os.mkdir(date_analysis_dir)

# Read data
#dta=hddm.load_csv("/Users/chennan/Dropbox/DDM + Market Share/Analysis/all/data/session3/yn.csv")
dta=hddm.load_csv("C:\Users\Nan\Documents\Dropbox\DDM + Market Share\Analysis\All\Data\session3/yn.csv")
id_list=dta.ID.unique()
random.seed(99999)
dta["rt"]=np.random.rand(dta.shape[0])*5
id_list=dta.ID.unique()
id_list=id_list[id_list>0]
id_list=id_list[id_list<22]
# Initiation
niter =12000
burn=2000
num_samples = 100#PPC
logit_lb=-9
logit_ub=9



# Estimation for each subject
for id in id_list:

    # data
    data=dta[dta.ID==id]

    #cut out RT outliers
    rt_thresh_lb = 0.2 
    rt_thresh_ub = np.min([10,np.mean(data['rt']) + 5*np.std(data['rt'])]) 
    data = data[data['rt'] > rt_thresh_lb]
    data = data[data['rt'] < rt_thresh_ub]


    # DDM
    v_reg = {'model': 'v ~ item+Discount_25+Discount_99', 'link_func': lambda x: x}
    ddm_model=hddm.HDDMRegressor(data, v_reg,informative=False)
    ddm_model.sample(niter,burn=burn)


    #Logit
    # set up
    response = data['response']
    beta0 = mc.Uniform('beta0',logit_lb,logit_ub,value=0)
    beta1 = mc.Uniform('beta1',logit_lb,logit_ub,value=0)
    beta2 = mc.Uniform('beta2',logit_lb,logit_ub,value=0)
    beta3 = mc.Uniform('beta3',logit_lb,logit_ub,value=0)
    beta4 = mc.Uniform('beta4',logit_lb,logit_ub,value=0)
    beta5 = mc.Uniform('beta5',logit_lb,logit_ub,value=0)
    beta6 = mc.Uniform('beta6',logit_lb,logit_ub,value=0)
    beta7 = mc.Uniform('beta7',logit_lb,logit_ub,value=0)

 
    item_dummy = pd.get_dummies(data['item'])
    V_0 = item_dummy['V_0']
    V_1 = item_dummy['V_1']    
    V_2 = item_dummy['V_2']
    V_3 = item_dummy['V_3']    
    V_4 = item_dummy['V_4']
    V_5 = item_dummy['V_5']    
    V_6 = data['Discount_25']   
    V_7 = data['Discount_99']   


    @mc.deterministic
    def logit_p(V_1 = V_1, V_2 = V_2, V_3 = V_3, V_4 = V_4, V_5 = V_5, V_6=V_6, V_7 = V_7,     
                beta0=beta0, beta1 = beta1, beta2 = beta2, beta3 = beta3, beta4 = beta4, beta5 = beta5, beta6 = beta6,beta7=beta7):
        return 1.0/(1.+np.exp(-(beta0+beta1*V_1+beta2*V_2+beta3*V_3+beta4*V_4+beta5*V_5+beta6*V_6+beta7*V_7)) )

    observed_p = mc.Bernoulli("bernoulli_obs",logit_p,value=response,observed=True)
    model_logit = mc.Model([observed_p,beta0,beta1,beta2,beta3,beta4,beta5,beta6,beta7])
    model_logit_mc = mc.MCMC(model_logit)
    map_logit = mc.MAP(model_logit)
    map_logit.fit()
    #estimation
    model_logit_mc.sample(niter,burn)


    #save results for ddm
    sub_analysis_dir = os.path.join(date_analysis_dir,str(id))
    if os.path.isdir(sub_analysis_dir) == False:
        os.mkdir(sub_analysis_dir)  
    filename = '%s_ddm_estimates.csv'%id
    ddm_model_out_path=os.path.join(sub_analysis_dir, filename) 

    ddm_model_stats=ddm_model.gen_stats()
    ddm_model_estimates=pd.DataFrame(ddm_model_stats)
    ddm_model_estimates.to_csv(ddm_model_out_path)

    #save ddm plots
    ddm_model_posterior='%s_ddm_posterior'%id
    ddm_posterior_path = os.path.join(sub_analysis_dir,ddm_model_posterior)
    ddm_model.plot_posteriors(save=True,path=ddm_posterior_path)
    plt.close('all')  

    ##PPC
    num_subs=id
    reg_model_num=id
    group_reg_ppc_analysis_dir = os.path.join(date_analysis_dir,'PPC')
    if os.path.isdir(group_reg_ppc_analysis_dir) == False:
       os.mkdir(group_reg_ppc_analysis_dir)
    group_model = ddm_model #to keep it simple below
    group_data=data
    ##generate ppc data##
    ##probably takes a while to generate for large number of samples##
    ppc_data = hddm.utils.post_pred_gen(group_model,samples= num_samples)
    ppc_data_name = date_var + '_ppc_data_model_group_reg' +str(reg_model_num) + '_n' + str(num_subs) + '_ns' + str(num_samples) +'.csv'
    ppc_data_path = os.path.join(group_reg_ppc_analysis_dir,ppc_data_name)
    ppc_data.to_csv(ppc_data_path)
    #ppc stats#
    ppc_compare = hddm.utils.post_pred_stats(group_data, ppc_data)
    #save to csv#
    ppc_compare_name = date_var + '_ppc_stats_all_hddm_group_model_reg' +str(reg_model_num) + '_n' + str(num_subs) + '_ns' + str(num_samples) +'.csv'
    ppc_compare_path = os.path.join(group_reg_ppc_analysis_dir,ppc_compare_name)
    ppc_compare.to_csv(ppc_compare_path)
   

    #save results for logit
    beta_mean_array = np.array([beta0.stats()['mean'],beta1.stats()['mean'],beta2.stats()['mean'],beta3.stats()['mean'],beta4.stats()['mean'],beta5.stats()['mean'],beta6.stats()['mean'],beta7.stats()['mean']])
    beta_std_array = np.array([beta0.stats()['standard deviation'],beta1.stats()['standard deviation'],beta2.stats()['standard deviation'],beta3.stats()['standard deviation'],beta4.stats()['standard deviation'],beta5.stats()['standard deviation'],beta6.stats()['standard deviation'],beta7.stats()['standard deviation']])
    beta_error_array = np.array([beta0.stats()['mc error'],beta1.stats()['mc error'],beta2.stats()['mc error'],beta3.stats()['mc error'],beta4.stats()['mc error'],beta5.stats()['mc error'],beta6.stats()['mc error'],beta7.stats()['mc error']])
    #save betas
    betas = pd.DataFrame(beta_mean_array)
    betas = betas.rename(columns={0:'mean'})
    betas['std'] = beta_std_array
    betas['mc_error'] = beta_error_array
    betas.index = ["Intercept","V_1","V_2", "V_3","V_4","V_5","Discount_25","Discount_99"]
    betas_stats_name = '%s_logit_estimates.csv'%id
    betas_stats_path = os.path.join(sub_analysis_dir,betas_stats_name)
    betas.to_csv(betas_stats_path)

    #save 
    rt_thresh_array = np.array([rt_thresh_ub,data.shape[0]])
    df_thresh = pd.DataFrame(rt_thresh_array)
    df_thresh = df_thresh.rename(columns={0:'value'})
    df_thresh['parameter'] = np.array(['rt_thresh','num_trials'])
    df_thresh_name =  '%s_RTthresh.csv'%id
    df_thresh.to_csv(os.path.join(sub_analysis_dir,df_thresh_name))

    print "Subject %s done \n\n\n\n"%id

##################################################################   











