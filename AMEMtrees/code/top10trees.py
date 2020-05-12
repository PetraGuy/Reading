# -*- coding: utf-8 -*-
"""
Created on Fri May  8 17:54:11 2020
Find tree numbers
@author: petra
"""
import pandas as pd
# How many trees of each species are there in bunce?
trees = pd.read_csv('../data/TreesInBunceFromDBH.csv')

import collections

counter = collections.Counter(trees['BRC names'])
counterdf = pd.DataFrame(list(counter.items()), columns = ['tree','number'])

totaltreecount = sum(counterdf['number'])

def calcprop(x):
    prop = x/totaltreecount
    
counterdf['proportion']= (counterdf['number']/totaltreecount)*100

counterdf  = counterdf.sort_values(['proportion'], ascending=['False'])
