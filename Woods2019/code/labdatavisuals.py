# -*- coding: utf-8 -*-
"""
Created on Wed Apr 29 08:51:51 2020
Visualising labdata
@author: petra
"""
#Looking at ratio of large/small, num morphs etc
import pandas as pd
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
import matplotlib as mpl
import numpy as np
import seaborn as sns
%matplotlib inline
#get the data

data = pd.read_csv('../data/woodprops.csv')

#get a subset of climate vars and labdata 
colnames = data.columns.values.tolist()

#this is subset of all useful data
subset = data[['grazed','type','continuity','elevation','max daily temp','min daily temp','ave rainfall',
               'rainfall in sampling month','rainfall month previous to sampling',
               'type2','lat','long','mean_lsOR','meanrateOR','meancgOR','morphsOR','propoccOR',
               'mean_lsA','meanrateA','meancgA','morphsA','propoccA']]

#this is second subset of continuous vars to look at pairwise correlations
subsetcont = subset[['continuity','elevation','max daily temp','min daily temp','ave rainfall',
               'rainfall in sampling month','rainfall month previous to sampling',
               'lat','long','mean_lsOR','meanrateOR','meancgOR','morphsOR','propoccOR',
               'mean_lsA','meanrateA','meancgA','morphsA','propoccA']]

subsetclimate = subset[['max daily temp','min daily temp','ave rainfall',
               'rainfall in sampling month','rainfall month previous to sampling',
               'long','lat']]

subsetlabdata = subset[['mean_lsOR','meanrateOR','meancgOR','morphsOR','propoccOR',
               'mean_lsA','meanrateA','meancgA','morphsA','propoccA']]

def heatmap(data):
# Correlation Matrix Heatmap
    f, ax = plt.subplots(figsize=(10, 6))
    corr = data.corr()
    mask = np.triu(np.ones_like(corr, dtype=np.bool))
    hm = sns.heatmap(round(corr,2),mask = mask, annot=True, ax=ax, cmap="coolwarm",fmt='.2f',
                 linewidths=.05)
    f.subplots_adjust(top=0.93)
    t= f.suptitle('Attributes Correlation Heatmap', fontsize=14)
   

heatmap(subsetclimate)
heatmap(subsetlabdata)
heatmap(subsetcont)

   
# Compute the correlation matrix
corr = subsetcont.corr()

# Generate a mask for the upper triangle
mask = np.triu(np.ones_like(corr, dtype=np.bool))

# Set up the matplotlib figure
f, ax = plt.subplots(figsize=(11, 9))

# Generate a custom diverging colormap
cmap = sns.diverging_palette(220, 10, as_cmap=True)

# Draw the heatmap with the mask and correct aspect ratio
sns.heatmap(corr, mask=mask, cmap=cmap, vmax=.3, center=0,
            square=True, linewidths=.5, cbar_kws={"shrink": .5})







