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
import math
#work on trees and get %am etc
#this area of trees = area trunk x num trunks is surrogate for amount of am innoc
def area(count,dbh):
    area = (((2.5+(dbh-1)*5)/2)*math.pi)*count
    return(area)

# change this if you want to include the effect of dual status trees
def percentam(row):
    if row['ms']=='am':
        val = row['%']
    else:
        val = 0
    return(val)
    
#year 1 and 2 are together fir trees, split out so trees corresponds with shrubs and herbs
trees1 = trees[trees['Yr']==1]
trees2 = trees[trees['Yr']==2]

#create a new columns with %a as calculated above
trees1['%']= trees1.apply(lambda x: area(x['Count'], x['DBH_class']), axis=1)
trees2['%']= trees2.apply(lambda x: area(x['Count'], x['DBH_class']), axis=1)

#create %am - if ms column = am, else enter 0 value
#note = dual trees therefore NOT included as source
trees1['%am'] = trees1.apply(percentam,axis = 1)
trees2['%am'] = trees2.apply(percentam,axis = 1)

#create array of %am per plot ready to add to plot level df when ready
trees1plotam = trees1.groupby(['SITE','PLOT'])['%am'].sum()
trees2plotam = trees2.groupby(['SITE','PLOT'])['%am'].sum()
#########################################################################

#need %am due to shrubs which is going to be cover
shrubs1['%']=shrubs1['CoverYr1']*2
shrubs1['amcover']=shrubs1.apply(percentam,axis = 1)
shrubs2['%']=shrubs2['CoverYr2']*2
shrubs2['amcover']=shrubs2.apply(percentam,axis = 1)

#get am cover by plot for the shrubs

shrubs1plotam = shrubs1.groupby(['Site','Plot'])['amcover'].sum()
shrubs2plotam = shrubs2.groupby(['Site','Plot'])['amcover'].sum()

#######################################################################

#shrub amcover is m2 of am cover
#trees%am is cm2 of treetrunk 
#I'll keep these separate for now

#get herb species richness
herbrichness1 = herbflora1.groupby(['Site','Plot']).size()
herbrichness2 = herbflora2.groupby(['Site','Plot']).size()

######################################################################

#get a count of the allolopaths by plot or site
shrubs1allelos=shrubs1.groupby(['Site','Plot'])['flag'].sum()
shrubs2allelos=shrubs2.groupby(['Site','Plot'])['flag'].sum()


#################################################################
