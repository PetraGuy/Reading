# -*- coding: utf-8 -*-
"""
Created on Tue May 12 16:03:25 2020
Sort out ground flora in herbs and shrub dfs
@author: petra
"""
import pandas as pd

#get the data files
floralist = pd.read_csv('../data/grndspeciesfullinfoWS.csv')
groundflora = pd.read_csv('../data/GroundFlora.csv')
trees = pd.read_csv('../data/Trees.csv')


#take the herbs and woody shrubs out of the ground flora
herbs = floralist.loc[floralist['woodiness'] == 'h']
shrubs = floralist.loc[floralist['woodiness'].isin (['ws','wt','sw'])]
#these two lines give error message, but they do change the appearance of BRC column to int
#the sci notation seem sto cause problems with joins
herbs['BRC'] = herbs['BRC'].astype(int)
shrubs['BRC'] = shrubs['BRC'].astype(int)


#grnd flora is a bit of a mess, duplicates in BRC etc - easier to separate 
#years 1 and 2
flora1 = groundflora.loc[:,['Site', 'Plot', 'Nest', 'CoverYr1','AmalgamsYr1']].drop_duplicates().dropna()
flora2 = groundflora.loc[:,['Site', 'Plot', 'Nest', 'CoverYr2','AmalgamsYr2']].drop_duplicates().dropna()

#sci notation is an issue - BRC 92003 not recognised as same as 9.2003e4
#try to eliminate as above. 
flora1['AmalgamsYr1'] = flora1['AmalgamsYr1'].astype(int)
flora2['AmalgamsYr2'] = flora2['AmalgamsYr2'].astype(int)

#now merge herbs/shrubs with flora1/2 to get herbfloraYr1, shrubfloraYr1 etc
#herbflora1 and 2 are all the herbs with site/plot etc, that were in Yr1 and 2 of Bunce
herbflora1 = pd.merge(flora1,herbs, left_on = 'AmalgamsYr1', right_on = 'BRC',how = 'right')
herbflora2= pd.merge(flora2,herbs, left_on = 'AmalgamsYr2', right_on = 'BRC',how = 'right')

#shrubs1 and 2 are all the woody shrubs/tree seedlings that were in Yr1 and 2 of Bunce
shrubs1 = pd.merge(flora1,shrubs, left_on = 'AmalgamsYr1', right_on = 'BRC',how = 'right')
shrubs2= pd.merge(flora2,shrubs, left_on = 'AmalgamsYr2', right_on = 'BRC',how = 'right')


################# looking at some rows to check for errors############
def check(df,site,plot,nest):
    rows = df.loc[(df['Site']==site)&(df['Plot']==plot)&(df['Nest']==nest)]
    print(rows)
def check2(df):
    rows = df.loc[df['CoverYr2']<0]
    print(rows)   

check2(shrubs2)
check(herbflora2,1,2,3)
check(flora2,1,2,3)

check(shrubs2,2,3,4)
check(flora2,2,3,4)
###################################################################


