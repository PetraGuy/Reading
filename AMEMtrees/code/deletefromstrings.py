# -*- coding: utf-8 -*-
"""
Created on Thu May 14 17:00:32 2020
Delet last character from element in list
@author: petra
"""
#NVC = pd.read_csv('../data/NVC.csv')

#i need to delete the non numeric character at the end of the NVC column
#So W10a becomes W10, but W8 just stays as W8

#turn the column into a string

#nvc = list(NVC['NVC'])

def deletelast(alist):
    shortnvc = []
    for codes in alist:
        last = codes[-1]
        if last.isnumeric():
            shortnvc.append(codes)
        else:
            shortnvc.append(codes[:-1])
    return(shortnvc)
    
#shortcodes = deletelast(nvc)        
        



