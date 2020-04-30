# -*- coding: utf-8 -*-
"""
Created on Thu Apr 30 18:16:28 2020
Update woodprops with Ndep
@author: petra
"""
import pandas as pd
import os

#get data 
woodprops = pd.read_csv('../data/woodprops.csv')
ndepsep = pd.read_csv('../data/ndepbysite.csv')
ndeptotal = pd.read_csv('../data/ndeptotalbysite.csv')

#take out ndep columns to me merged with woodprops
somecolumns = ndeptotal[['Value','X.']]

#merge
woodprops = pd.merge(woodprops,somecolumns, left_on = 'site', right_on ='X.', how = 'left')

#get rid of additional site column and rename Valkue as ndeptotal
del  woodprops['X.']
woodprops.rename(columns = {'Value': 'NdepTotal'}, inplace = True)

woodprops.to_csv('../data/woodprops.csv')
