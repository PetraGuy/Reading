# -*- coding: utf-8 -*-
"""
Created on Wed Apr 22 13:37:36 2020
Get list of herbaceous species in Bunce
@author: petra
"""

#There is need to see what herbs are in grnd flora, might to to remove some
#like brambles
import pandas as pd

BRC = pd.read_csv('../data/BRCCodes.csv')
grndflora = pd.read_csv('../data/GroundFlora.csv')

grndflora.fillna(0, inplace =True) #d
grndflora = grndflora.astype('int64')

 #just take out species for now, dont need site,plot, nest, cover
 
justflora = grndflora[['AmalgamsYr1','AmalgamsYr2']]

#make long
df1 = pd.DataFrame(justflora['AmalgamsYr1'])
df2 = pd.DataFrame(justflora['AmalgamsYr2'])
justfloralong = pd.concat([df1,df2], ignore_index = True)

 #get rid of emty second column
 del justfloralong['AmalgamsYr2']
 
 #now delete duplicates in amalgams
 flora = justfloralong.drop_duplicates('AmalgamsYr1')
 flora.columns =['BRC number']
 
 #now merge with BRC codes to get names of the flora
floraspieces = pd.merge(BRC,flora, on = 'BRC number', how = 'right')
