# -*- coding: utf-8 -*-
"""
Created on Fri Apr 24 13:15:12 2020
Add mycor status to trees in plots
@author: petra
"""
import pandas as pd

#get the data

trees = pd.read_csv('../data/treespecieslist.csv')
treesinplots = pd.read_csv('../data/TreesByPlot.csv')
#merge to add mycor to trees list
treeswithmycor = pd.merge(treesinplots,trees, left_on = 'BRC',
                          right_on = 'BRC', how = 'left')
treeswithmycor.to_csv('../data/Trees.csv', index = False)


