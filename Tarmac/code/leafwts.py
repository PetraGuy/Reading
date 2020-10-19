# -*- coding: utf-8 -*-
"""
Created on Fri Oct  9 15:44:38 2020

@author: petra
"""

import pandas as pd
import matplotlib.pyplot as plt

leaf = pd.read_csv('../data/TarmacLeafWts.csv')
leaf.boxplot(column='totalleafwt',by='treatment')
title_boxplot = 'awesome title'
plt.title( 'Leaf Weights' )
plt.suptitle('') # that's what you're after
plt.show()
