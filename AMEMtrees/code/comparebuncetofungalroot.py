# -*- coding: utf-8 -*-
"""
Created on Tue Apr 21 13:12:04 2020
Comparing species in Bunce with Fungal Root
@author: petra
"""
# import the fungalroot and bunce data

import pandas as pd

bnc = pd.read_csv("../data/TreesInBunce.csv")# change this if you want by hand trees or from dbh
fr = pd.read_csv("../data/FungalRoot.csv")

bnc_in_fr = pd.merge(fr,bnc, on = 'species', how = 'right')
bnc_in_fr_wodups = bnc_in_fr.drop_duplicates()

#repeated rows for same species with different mycor status, so make wide so you can see
#how many actual species are in there
bnc_in_fr_wodups_wide = bnc_in_fr_wodups.pivot(index='species', columns='status',values = 'status')

#some of the rows are all nan
bnc_in_fr_wodups_wide.dropna(axis = 0, how = 'all', inplace = True)

#now you can see there are 82 species in FungalRoot that occur in Bunce, 
#and I could use Fungal Root references

bnc_in_fr_wodups_wide_wonan.to_csv('../data/BunceInFungalRoot.csv')