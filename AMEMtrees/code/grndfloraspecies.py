# -*- coding: utf-8 -*-
"""
Created on Wed Apr 22 13:37:36 2020
Get list of herbaceous species in Bunce
@author: petra
"""

#There is need to see what herbs are in grnd flora, might to to remove some
#like brambles
import pandas as pd

# get the two data files
BRC = pd.read_csv('../data/specieslibraryfull.csv')
grndflora = pd.read_csv('../data/GroundFlora.csv')

#rename cols so can join later
BRC.rename(columns = {'BRC number': 'BRC'}, inplace = True)

#NB there are no duplicates in BRC
names = BRC.BRC.value_counts()

#note - although eng notation, if you extract a value it is correct, so
#920189e6 = 9201892, so there aren't duplicates where it looks like
#thre are lots of values of 920189e6



# need to make long by year, so have amalgam/year 
#not sure why I need this, but couldn't just use i = index without it
grndflora['Index'] = grndflora.index 
grndlong = pd.wide_to_long(grndflora, ["Cover", "Amalgams"], i='Index', j="year", sep = 'Yr')

#year is an index not a column, this fixes that.
grndlong.reset_index(inplace=True)
grndlong.drop("Index",axis=1,inplace=True)

#rename cols so can join
grndlong.rename(columns = {'Amalgams': 'BRC'}, inplace = True)

#there are some duplicates because when a there were more species in one year 
#than the other, then a species name was just repeated, so looks like
#acer x 6 entries for site 1 plot 1 nest 1 because there are 6 species
#in next year in site 1 plot 1 nest 1. So remove duplicate rows
#but in other cases it looks like just blanks were left. 
#So need to remove empty lines without amalgams AND delete duplicate amalgams

#if nan in BRC, delete that row
grndlong = grndlong.dropna()

#drop any rows where BRC duplicated - if in same site/plot/nest
grndlong = grndlong.drop_duplicates(['year','Site','Plot','Nest','Cove','BRC'],keep= 'last')


#get species names
grndfloraspecies = pd.merge(grndlong,BRC, on = 'BRC', how = "left")

#now grndflora long is same length after join with species, hooray.

#save this
grndfloraspecies.to_csv('../data/grndfloralongspecies.csv')

