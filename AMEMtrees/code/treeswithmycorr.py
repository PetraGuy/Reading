# -*- coding: utf-8 -*-
"""
Created on Fri Apr 24 13:15:12 2020
Add mycor status to trees usoing fungal root
@author: petra
"""
import pandas as pd

#get the data
fungalroot = pd.read_csv('../data/FungalRoot3.csv')
trees = pd.read_csv('../data/treespecieslist.csv')

#merge to add mycor to trees list
treeswithmycor = pd.merge(trees,fungalroot, left_on = 'BRC names',
                          right_on = 'species', how = 'left').drop_duplicates()
treeswithmycor.to_csv('../data/treeswithmycor.csv', index = False)
