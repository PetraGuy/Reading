# -*- coding: utf-8 -*-
"""
Created on Wed May  6 12:07:52 2020
Assign mycor status to woody shrubs
@author: petra
"""



import pandas as pd

df = pd.read_csv('../data/fungalroot3.csv')

#make sure all delimeters are ;, some are commas
df['mycorrhiza type'] = df['mycorrhiza type'].str.replace(',',';')

#now just take everything before the ; but this doesn't work
df['mycorrhiza type'] = df['mycorrhiza type'].str.rstrip(';')


#this works, but dont understand it, just copied from SO, whats the str[0]
df['mycorrhiza type'] = df['mycorrhiza type'].str.split(';').str[0]


