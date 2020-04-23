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

import pandas as pd

trees = pd.read_csv("../data/DBHAllTrees.csv").drop_duplicates('Amalgams')
#rename columns so they are in correct format for joins
trees.rename(columns={'BRC_number':'BRC number'}, inplace = True )
# this is maller than my previous by hand, possibly because does not contain shrubs?

#get species names by cross ref with BRC codes
BRC = pd.read_csv('../data/BRCCodes.csv')
treenames = pd.merge(BRC,trees,on = 'BRC number')
treenames.to_csv('../data/TreesInBunceFromDBH.csv')
