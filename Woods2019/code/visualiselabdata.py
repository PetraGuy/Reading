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
    hm = sns.heatmap(round(corr,2), annot=True, ax=ax, cmap="coolwarm",fmt='.2f',
                 linewidths=.05)
    f.subplots_adjust(top=0.93)
    t= f.suptitle('Attributes Correlation Heatmap', fontsize=14)

heatmap(subsetclimate)
heatmap(subsetlabdata)
heatmap(subsetcont)

   
#pairwise scatter plots
g = sns.PairGrid(subsetlabdata)
g.map(plt.scatter);

#box plots of OR versus A


sns.set(style="whitegrid", palette="pastel", color_codes=True)

# Load the example tips dataset

# Draw a nested violinplot and split the violins for easier comparison

sns.violinplot(x="day", y="total_bill", hue="smoker",
               split=True, inner="quart",
               palette={"Yes": "y", "No": "b"},
               data=tips)
sns.despine(left=True)