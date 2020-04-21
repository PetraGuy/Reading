# -*- coding: utf-8 -*-
"""
Created on Tue Apr 21 13:12:04 2020
Comparing species in Bunce with Fungal Root
@author: petra
"""
# import the fungalroot and bunce data

import pandas as pd

bunce = pd.read_csv("../data/TreesInBunce.csv")
fungalroot = pd.read_csv("../data/FungalRoot.csv")

bunceinfungalroot = pd.merge(fungalroot,bunce, on = 'species', how = 'right')
bunceinfungalroot_woduplicates = bunceinfungalroot.drop_duplicates()
