# -*- coding: utf-8 -*-
"""
Created on Fri Apr 24 10:47:46 2020
Allocate plant types and habits from Plantatt
@author: petra
"""
#Use plantatt to allocate herb, perennial etc.

import pandas as pd

#need these two functions to tidy up merged df because it has duplicate columns
def drop_y(df):
    # list comprehension of the cols that end with '_y'
    # from https://stackoverflow.com/questions/19125091/pandas-merge-how-to-avoid-duplicating-columns
    to_drop = [x for x in df if x.endswith('_y')]
    df.drop(to_drop, axis=1, inplace=True)

def drop_prefix(self, suffix):
    self.columns = self.columns.str.rstrip(suffix)
    return self

####

#get plantatt data
plantatt = pd.read_csv('../data/PlantattAmmended.csv')

#rename cols for merge
plantatt.rename(columns = {'BRCcode': 'BRC', 'Taxon name': 'BRC names'}, inplace = True)

#get ground flora
ground = pd.read_csv('../data/groundspecieslist.csv')

#merge these to get plantatt data for bunce ground flora
merged = pd.merge(ground,plantatt, on = 'BRC',how = 'left')

#tidy up
drop_y(merged)
drop_prefix(merged,'_x')

merged.to_csv('../data/grndspeciesfullinfo.csv')
