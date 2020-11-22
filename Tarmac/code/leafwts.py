# -*- coding: utf-8 -*-
"""
Created on Fri Oct  9 15:44:38 2020

@author: petra

"""

import pandas as pd
import matplotlib.pyplot as plt

####leaves
leaf = pd.read_csv('../data/TarmacLeafWts.csv')

leaf.boxplot(column='totalleafwt',by='treatment')
title_boxplot = 'awesome title'
plt.title( 'Leaf Weights' )
plt.suptitle('') # that's what you're after
plt.show()

####soil density



#pHdown
soilpHCN.boxplot(column='MSpHDnStrata',by='MSDnStata')
title_boxplot = 'awesome title'
plt.title( 'Mountsorrel pH down soil strata for different locations' )
plt.suptitle('') # that's what you're after
plt.show()
soilpHCN.boxplot(column='BDpHDnStrata',by='BDDnStrata')
title_boxplot = 'awesome title'
plt.title( 'Buddon Wood pH down soil strata for different locations' )
plt.suptitle('') # that's what you're after
plt.show()

#N down
soilpHCN.boxplot(column='MSNDnStrata',by='MSDnStata')
title_boxplot = 'awesome title'
plt.title( 'Mountsorrel N down soil strata for different locations' )
plt.suptitle('') # that's what you're after
plt.show()
soilpHCN.boxplot(column='BDNDnStrata',by='BDDnStrata')
title_boxplot = 'awesome title'
plt.title( 'Buddon Wood N down soil strata for different locations' )
plt.suptitle('') # that's what you're after
plt.show()

#C down
soilpHCN.boxplot(column='MSCDnStrata',by='MSDnStata')
title_boxplot = 'awesome title'
plt.title( 'Mountsorrel C down soil strata for different locations' )
plt.suptitle('') # that's what you're after
plt.show()
soilpHCN.boxplot(column='BDCDnStrata',by='BDDnStrata')
title_boxplot = 'awesome title'
plt.title( 'Buddon Wood C down soil strata for different locations' )
plt.suptitle('') # that's what you're after
plt.show()

#C:N down
soilpHCN.boxplot(column='MSCNDnStrata',by='MSDnStata')
title_boxplot = 'awesome title'
plt.title( 'Mountsorrel C:N down soil strata for different locations' )
plt.suptitle('') # that's what you're after
plt.show()
soilpHCN.boxplot(column='BDCNDnStrata',by='BDDnStrata')
title_boxplot = 'awesome title'
plt.title( 'Buddon Wood C:N down soil strata for different locations' )
plt.suptitle('') # that's what you're after
plt.show()



############### start ###
#redoing some charts to correct labelling, but now using new neater
#data file 
data =  pd.read_csv('../data/AllSoilData.csv')

#for fig 5 of report, use only MSs and BDs

siteMS = data[data['site']=='MSs']
siteBD = data[data['site']=='BDs']
#boxplot for each layer
siteMS.boxplot(column='pH',by='LAYER')
title_boxplot = 'awesome title'
plt.title( 'pH' )
plt.suptitle('') # that's what you're after
plt.ylim(3,7)
plt.show()

siteBD.boxplot(column='pH',by='LAYER')
title_boxplot = 'awesome title'
plt.title( 'pH' )
plt.suptitle('') 
plt.ylim(3,7)
plt.show()

siteMS.boxplot(column='N',by='LAYER')
title_boxplot = 'awesome title'
plt.title( 'N' )
plt.suptitle('') 
plt.ylim(0,2)
plt.show()
siteBD.boxplot(column='N',by='LAYER')
title_boxplot = 'awesome title'
plt.title( 'N' )
plt.suptitle('')
plt.ylim(0,2)
plt.show()

siteMS.boxplot(column='C',by='LAYER')
title_boxplot = 'awesome title'
plt.title( 'C' )
plt.suptitle('')
plt.ylim(0,40)
plt.show()
siteBD.boxplot(column='C',by='LAYER')
title_boxplot = 'awesome title'
plt.title( 'C' )
plt.suptitle('') 
plt.ylim(0,40)
plt.show()

siteMS.boxplot(column='CN',by='LAYER')
title_boxplot = 'awesome title'
plt.title( 'C:N' )
plt.suptitle('') 
plt.ylim(5,20)
plt.show()
siteBD.boxplot(column='CN',by='LAYER')
title_boxplot = 'awesome title'
plt.title( 'C:N' )
plt.suptitle('')
plt.ylim(5,20)
plt.show()

####


##fig 6

#sites = ['BDs','MSs']
#stratasites = data[data.site.isin(sites)]

siteMS.boxplot(column = 'pH', by = 'location')
#title_boxplot = 'awesome title'
plt.title( 'pH' )
plt.suptitle('')
plt.ylim(3,7)
plt.show()

siteMS.boxplot(column = 'C', by = 'location')
#title_boxplot = 'awesome title'
plt.title( 'C' )
plt.suptitle('')
plt.ylim(0,40)
plt.show()

siteMS.boxplot(column = 'N', by = 'location')
#title_boxplot = 'awesome title'
plt.title( 'N' )
plt.suptitle('')
plt.ylim(0,2)
plt.show()

siteMS.boxplot(column = 'CN', by = 'location')
#title_boxplot = 'awesome title'
plt.title( 'CN' )
plt.suptitle('')
plt.show()


siteBD.boxplot(column = 'pH', by = 'location')
#title_boxplot = 'awesome title'
plt.title( 'pH' )
plt.suptitle('')
plt.ylim(3,7)
plt.show()

siteBD.boxplot(column = 'C', by = 'location')
#title_boxplot = 'awesome title'
plt.title( 'C' )
plt.suptitle('')
plt.ylim(0,40)
plt.show()

siteBD.boxplot(column = 'N', by = 'location')
#title_boxplot = 'awesome title'
plt.title( 'N' )
plt.suptitle('')
plt.ylim(0,2)
plt.show()

siteBD.boxplot(column = 'CN', by = 'location')
#title_boxplot = 'awesome title'
plt.title( 'CN' )
plt.suptitle('')
plt.ylim(5,20)
plt.show()

################################################
#redo plotdatas for fig 7, all sites

data.boxplot(column='pH',by='site')
title_boxplot = 'awesome title'
plt.title( 'pH' )
plt.suptitle('') # that's what you're after
plt.show()
data.boxplot(column='N',by='site')
title_boxplot = 'awesome title'
plt.title( 'N' )
plt.suptitle('') # that's what you're after
plt.show()
data.boxplot(column='C',by='site')
title_boxplot = 'awesome title'
plt.title( 'C' )
plt.suptitle('') # that's what you're after
plt.show()
data.boxplot(column='CN',by='site')
title_boxplot = 'awesome title'
plt.title( 'C:N' )
plt.suptitle('') # that's what you're after
plt.show()

#redo figure 8

data.boxplot(column='pH')
title_boxplot = 'awesome title'
plt.title( 'pH' )
plt.suptitle('') 
plt.plot(1,5.3, marker = 'o',markersize = 20, markerfacecolor='red')
plt.show()


data.boxplot(column='N')
title_boxplot = 'awesome title'
plt.title( 'N' )
plt.suptitle('') # that's what you're after
plt.plot(1,0.3,marker = 'o',markersize = 20, markerfacecolor='red')
plt.show()

data.boxplot(column='C')
title_boxplot = 'awesome title'
plt.title( ' C' )
plt.suptitle('') # that's what you're after
plt.plot(1,4.8,marker = 'o',markersize = 20, markerfacecolor='red')
plt.show()

data.boxplot(column='CN')
title_boxplot = 'awesome title'
plt.title( 'C:N' )
plt.suptitle('') # that's what you're after
plt.plot(1,15.1,marker = 'o',markersize = 20, markerfacecolor='red')
plt.show()

data.boxplot(column='wt_g')
title_boxplot = 'awesome title'
plt.title( 'Weight' )
plt.suptitle('') 
plt.plot(1,114.6,marker = 'o',markersize = 20, markerfacecolor='red')# that's what you're after
plt.show()

######################
