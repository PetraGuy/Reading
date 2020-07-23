# -*- coding: utf-8 -*-
"""
Created on Wed Jul 22 11:47:19 2020

@author: petra
"""

#are all invasives the same

import pandas as pd

#invasives are herbs and shrubs that are flagged as invasive
invasives =  pd.read_csv('../data/invasives.csv')

#this is all herbs and shrubs so can select plots with high abundance of one species and see if invasive
allfloracomp = pd.read_csv('../data/allfloracomp.csv')

#filter plots where cover > 50% of either herb or shrub
invadedall = allfloracomp.query('cover_x > 50')

#group by species and look at herb richness
invadedall['species_x'].nunique()

