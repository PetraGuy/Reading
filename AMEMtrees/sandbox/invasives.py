# -*- coding: utf-8 -*-
"""
Created on Thu Sep 10 10:35:42 2020

@author: petra
"""

import pandas as pd

allflora = pd.read_csv('../data/allfloracomp.csv')
herbs = allflora[['species_x','cover_x','alpha']].drop_duplicates()
shrubs = allflora[['species_y','cover_y','alpha']].drop_duplicates()
herbs = herbs.rename(columns={'species_x':'species','cover_x':'cover'})
shrubs = shrubs.rename(columns={'species_y':'species','cover_y':'cover'})

herbs_and_shrubs = pd.concat([herbs,shrubs],axis = 'rows')
herbs_and_shrubs.to_csv('../data/herbs_and_shrubs.csv', index = False)
