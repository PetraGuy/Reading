# -*- coding: utf-8 -*-
"""
Created on Wed Jul 22 11:47:19 2020

@author: petra
"""

#are all invasives the same

import pandas as pd


treesinplots = pd.read_csv('../data/TreesByPlot.csv')#trees by 
shrubsinplots = pd.read_csv('../data/shrub_cover_richness.csv')

shrubstotals = shrubsinplots.groupby(['Yr','species'])['cover'].sum().to_frame().reset_index()

treesinplots['sumdbh'] = treesinplots['DBH_class']*treesinplots['Count']
totaltrees = treesinplots.groupby(['Yr','BRC names'])['sumdbh'].sum().to_frame().reset_index()

shrubstotals.to_csv('../data/shrubstotals.csv')
totaltrees.to_csv('../data/totaltrees.csv')
