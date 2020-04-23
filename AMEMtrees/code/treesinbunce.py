# -*- coding: utf-8 -*-
"""
Created on Wed Apr 22 10:37:42 2020
Get Trees from Bunce
@author: petra
"""
#I already did ths by going thru veg codes by hand
#but realise that dbh live counts in bunce just contains trees
#so import this and remove duplicates - it will be a list of trees
#need to find shrubs
# want to create 2 tables, one by site/plot and 1 just straight list so I can
#look into all species and makes sure they are wanted.

import pandas as pd

#read in data
trees = pd.read_csv("../data/DBHAllTrees.csv")
BRC = pd.read_csv('../data/specieslibraryfull.csv')

#remove brc and use amalgams
del trees['BRC_number']

#rename cols so can do joins
trees.rename(columns = {'Amalgams': 'BRC'}, inplace = True)
BRC.rename(columns = {'BRC number': 'BRC'}, inplace = True)


#get species names by cross ref with BRC codes
treenames = pd.merge(trees,BRC,on = 'BRC', how = 'left')


treenames.to_csv('../data/TreesInBunceFromDBH.csv')
