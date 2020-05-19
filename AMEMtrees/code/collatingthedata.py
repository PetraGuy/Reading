# -*- coding: utf-8 -*-
"""
Created on Tue May 12 16:03:25 2020
Sort out ground flora in herbs and shrub dfs
@author: petra
"""
import pandas as pd

#1) any changes to tree species and this must be redone
#this is where ms is added to tree list
trees = pd.read_csv('../data/treespecieslist.csv')#tree species details
treesinplots = pd.read_csv('../data/TreesByPlot.csv')#trees by plot
#merge to add mycor to trees list
treeswithmycor = pd.merge(treesinplots,trees, left_on = 'BRC',
                          right_on = 'BRC', how = 'left')
treeswithmycor.to_csv('../data/Trees.csv', index = False)#now trees by plot have details

#######################################################

#get the data files
floralist = pd.read_csv('../data/grndspeciesfullinfoWS.csv')# species details
groundflora = pd.read_csv('../data/GroundFlora.csv')#flora by plot
trees = pd.read_csv('../data/Trees.csv')#trees by plot with details
lba = pd.read_csv('../data/Live_basal_area.csv')#lba
#############################################


#2) separate herbs from woody shrubs in ground flora, split into years and merge with
#specie edtails with grnd flora to get species of herbs/shrub by year by plot

#take the herbs and woody shrubs out of the ground flora
herbs = floralist.loc[floralist['woodiness'] == 'h']
shrubs = floralist.loc[floralist['woodiness'].isin (['ws','wt','sw'])]
#these two lines give error message, but they do change the appearance of BRC column to int
#the sci notation seem sto cause problems with joins
herbs['BRC'] = herbs['BRC'].astype(int)
shrubs['BRC'] = shrubs['BRC'].astype(int)


#grnd flora is a bit of a mess, duplicates in BRC etc 
#separate herbs and shrubs into years 1 and 2
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
    
#year 1 and 2 are together for trees, split out so trees corresponds with shrubs and herbs
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
trees1plotam = trees1.groupby(['SITE','PLOT'])['%am'].sum().to_frame().reset_index()
trees2plotam = trees2.groupby(['SITE','PLOT'])['%am'].sum().to_frame().reset_index()
#########################################################################

#need %am due to shrubs which is going to be cover
shrubs1['%']=shrubs1['CoverYr1']*2
shrubs1['amcover']=shrubs1.apply(percentam,axis = 1)
shrubs2['%']=shrubs2['CoverYr2']*2
shrubs2['amcover']=shrubs2.apply(percentam,axis = 1)

#get am cover by plot for the shrubs
shrubs1plotam = shrubs1.groupby(['Site','Plot'])['amcover'].sum().to_frame().reset_index()
shrubs2plotam = shrubs2.groupby(['Site','Plot'])['amcover'].sum().to_frame().reset_index()

#######################################################################

#shrub amcover is m2 of am cover
#trees%am is cm2 of treetrunk 
#I'll keep these separate for now

#get herb species richness
herbrichness1 = herbflora1.groupby(['Site','Plot']).size().to_frame().reset_index()
herbrichness2 = herbflora2.groupby(['Site','Plot']).size().to_frame().reset_index()

######################################################################

#get a count of the allolopaths by plot or site for shrubs
#This needs to reflect cover of allelo plant, not just sum of plants -
#what if bracken covers half of plot for eg.
#add allelos column  =  coverxflag
shrubs1['allelos'] = shrubs1['CoverYr1']*shrubs1['flag']
shrubs2['allelos'] = shrubs2['CoverYr2']*shrubs2['flag']

shrubs1allelos=shrubs1.groupby(['Site','Plot','CoverYr1'])['allelos'].sum().to_frame().reset_index()
shrubs2allelos=shrubs2.groupby(['Site','Plot','CoverYr2'])['allelos'].sum().to_frame().reset_index()


#################################################################


#get count of allelos for trees per plot
#add count and dbh class so can get "quantity of allelo, as for shrubs above, already have a row% for trees
#so add col %xflag will give allel val 0 if flag, 0
trees1['allelos']=trees1['%']*trees1['flag']
trees2['allelos']=trees2['%']*trees2['flag']

trees1allelos=trees1.groupby(['SITE','PLOT'])['allelos'].sum().to_frame().reset_index()
trees2allelos=trees2.groupby(['SITE','PLOT'])['allelos'].sum().to_frame().reset_index()

####################################################################

#Combine tree and shrub allelo counts per plot
#merge the shrub and tree allelos first coz there are missing plots
#need to tidy colnames

def tidycols(df):
    df.rename(columns={'SITE':'Site',
                   'PLOT':'Plot'},
          inplace=True)
    return(df)

trees2allelos = tidycols(trees2allelos)

#have allelos_x = shrubs and allelos_y = trees
allelos1 = pd.merge(shrubs1allelos,trees1allelos, on=['Site','Plot'],how ='left')
allelos2 = pd.merge(shrubs2allelos,trees2allelos, on=['Site','Plot'],how ='left')

#now add cols to get total allelos per plot
#but one is cover one is num stems, so do this later aftrer normalising
#allelos1['totalallelos']=allelos1.fillna(0)['flag_x']+allelos1.fillna(0)['flag_y']
#allelos2['totalallelos']=allelos2.fillna(0)['flag_x']+allelos2.fillna(0)['flag_y']

########################################################################

#get herb cover to be used as abundance
herbcover1 = herbflora1.groupby(['Site','Plot'])['CoverYr1'].sum().to_frame().reset_index()
herbcover2 = herbflora2.groupby(['Site','Plot'])['CoverYr2'].sum().to_frame().reset_index()

##########################################################################

#sort out the lba as the shading parameter
#split into yr1 and yr2

lba1 = lba[['SITE','PLOT','y71']]
lba2 = lba[['SITE','PLOT','y03']]

#########################################################################

#now get other cols - NVC,SOM,soilpH
som = pd.read_csv('../data/soil_som.csv')
pH = pd.read_csv('../data/soil_ph.csv')
NVC = pd.read_csv('../data/NVC.csv')

#dont want nvc subcodes. i.e. want W10a as W10, so delete last character if a letter
from deletefromstrings import deletelast
nvc = list(NVC['NVC'])
NVC['shortcodes'] = deletelast(nvc)
NVC.to_csv('../data/NVCshort.csv')

#now split into years
nvc1 =  NVC[NVC['Yr']==1]
nvc2 = NVC[NVC['Yr']==2]
som1 = som[['SITE','PLOT','SOM1971']]
som2 = som[['SITE','PLOT','SOM resurvey']]
pH1 = pH[['SITE','PLOT','pH1971']]
pH2 = pH[['SITE','PLOT','pH resurvey']]

############################################################################
#combine allNVC which has NVC mycor status with nvc codes above

nvctypes = pd.read_csv('../data/allnvcs.csv')
nvc1full = pd.merge(nvc1,nvctypes,left_on ='shortcodes',right_on = 'nvc' )
nvc2full = pd.merge(nvc2,nvctypes,left_on ='shortcodes',right_on = 'nvc' )

############################################################################
#now i have all the variables to pull together into 2 plot level dfs, one for each year
#Base site/plot must be ground flora for that year, as that is where all flora stored
#Start with herbrichness
#rename cols and tidy as you go


year1data = herbrichness1.rename(columns={0: 'alpha'})

#add herbcover
year1data = pd.merge(year1data,herbcover1,on=['Site','Plot'],how = 'left')
year1data = year1data.rename(columns={'CoverYr1': 'cover'})

#add count of allelos
year1data = year1data = pd.merge(year1data,allelos1,on=['Site','Plot'],how = 'left')
del year1data['CoverYr1']
year1data = year1data.rename(columns={'allelos_x': 'shrub_allelo','allelos_y':'tree_allelo'})
#year1data = year1data.drop(['flag_x','flag_x','flag_y'],axis = 1)
#year1data = year1data.rename(columns={'totalallelos_y': 'allelos'})

#add lba
lba1 = lba1.rename(columns={'SITE': 'Site','PLOT':'Plot'})
year1data = year1data = pd.merge(year1data,lba1,on=['Site','Plot'],how = 'left')
year1data = year1data.rename(columns={'y71': 'lba'})

#addSOM
som1 = som1.rename(columns={'SITE': 'Site','PLOT':'Plot','SOM1971':'som'})
year1data = pd.merge(year1data,som1,on=['Site','Plot'],how = 'left')

#add pH
pH1 = pH1.rename(columns={'SITE': 'Site','PLOT':'Plot','pH1971':'pH'})
year1data = pd.merge(year1data,pH1,on=['Site','Plot'],how = 'left')

#add %am for trees
trees1plotam = trees1plotam.rename(columns={'SITE': 'Site','PLOT':'Plot','%am':'amtrees'})
year1data = pd.merge(year1data,trees1plotam,on=['Site','Plot'],how = 'left')

#add am cover from shrubs
year1data = pd.merge(year1data,shrubs1plotam,on=['Site','Plot'],how = 'left')

#add NVC data
nvc1full = nvc1full.drop(['NVC','Yr','shortcodes','Unnamed: 0'],axis = 1)
nvc1full = nvc1full.rename(columns={'SITE':'Site','PLOT':'Plot'})
year1data = pd.merge(year1data,nvc1full,on=['Site','Plot'],how = 'left')

###################################################################
#repeat for year 2

year2data = herbrichness2.rename(columns={0: 'alpha'})

#add herbcover
year2data = pd.merge(year2data,herbcover2,on=['Site','Plot'],how = 'left')
year2data = year2data.rename(columns={'CoverYr2': 'cover'})

#add count of allelos
year2data = pd.merge(year2data,allelos2,on=['Site','Plot'],how = 'left')
del year2data['CoverYr2']
year2data = year2data.rename(columns={'allelos_x': 'shrub_allelo','allelos_y':'tree_allelo'})
#year1data = year1data.drop(['flag_x','flag_x','flag_y'],axis = 1)
#year1data = year1data.rename(columns={'totalallelos_y': 'allelos'})

#add lba
lba2 = lba2.rename(columns={'SITE': 'Site','PLOT':'Plot'})
year2data = year2data = pd.merge(year2data,lba2,on=['Site','Plot'],how = 'left')
year2data = year2data.rename(columns={'y03': 'lba'})

#addSOM
som2 = som2.rename(columns={'SITE': 'Site','PLOT':'Plot','SOM resurvey':'som'})
year2data = pd.merge(year2data,som2,on=['Site','Plot'],how = 'left')

#add pH
pH2 = pH2.rename(columns={'SITE': 'Site','PLOT':'Plot','pH resurvey':'pH'})
year2data = pd.merge(year2data,pH2,on=['Site','Plot'],how = 'left')

#add %am for trees
trees2plotam = trees2plotam.rename(columns={'SITE': 'Site','PLOT':'Plot','%am':'amtrees'})
year2data = pd.merge(year2data,trees2plotam,on=['Site','Plot'],how = 'left')

#add am cover from shrubs
year2data = pd.merge(year2data,shrubs2plotam,on=['Site','Plot'],how = 'left')

#add NVC data
nvc2full = nvc2full.drop(['NVC','Yr','shortcodes','Unnamed: 0'],axis = 1)
nvc2full = nvc2full.rename(columns={'SITE':'Site','PLOT':'Plot'})
year2data = pd.merge(year2data,nvc2full,on=['Site','Plot'],how = 'left')

#####################################################################

#save the files

year1data.to_csv('../data/year1dataA.csv',index=False)
year2data.to_csv('../data/year2dataA.csv',index=False)
