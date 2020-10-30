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
soilden = pd.read_csv('../data/soil_density.csv')

soilden.boxplot(column='normalised_wt',by='Site')
title_boxplot = 'awesome title'
plt.title( 'Soil Weights' )
plt.suptitle('') # that's what you're after
plt.show()

soilden = pd.read_csv('../data/soil_density.csv')
soilden.boxplot(column='normalised_wt',by='Site')
title_boxplot = 'awesome title'
plt.title( 'Soil Weights' )
plt.suptitle('') # that's what you're after
plt.show()


####soil pH,C,N
soilpHCN = pd.read_csv('../data/soilpHCN.csv')

#PH across
soilpHCN.boxplot(column='MSpHAcSite',by='MSAcSite')
title_boxplot = 'awesome title'
plt.title( 'Mountsorrel pH across site for different soil strata' )
plt.suptitle('') # that's what you're after
plt.show()
soilpHCN.boxplot(column='BDpHAcSite',by='BDAcSite')
title_boxplot = 'awesome title'
plt.title( 'Buddon Wood pH across site for different soil strata' )
plt.suptitle('') # that's what you're after
plt.show()

#N across
soilpHCN.boxplot(column='MSNAcSite',by='MSAcSite')
title_boxplot = 'awesome title'
plt.title( 'Mountsorrel N across site for different soil strata' )
plt.suptitle('') # that's what you're after
plt.show()
soilpHCN.boxplot(column='BDNAcSite',by='BDAcSite')
title_boxplot = 'awesome title'
plt.title( 'Buddon Wood N across site for different soil strata' )
plt.suptitle('') # that's what you're after
plt.show()

#C across
soilpHCN.boxplot(column='MSCAcSite',by='MSAcSite')
title_boxplot = 'awesome title'
plt.title( 'Mountsorrel C across site for different soil strata' )
plt.suptitle('') # that's what you're after
plt.show()
soilpHCN.boxplot(column='BDCAcSite',by='BDAcSite')
title_boxplot = 'awesome title'
plt.title( 'Buddon Wood C across site for different soil strata' )
plt.suptitle('') # that's what you're after
plt.show()

#C:N across
soilpHCN.boxplot(column='MSpHAcStrata',by='MSAcStata')
title_boxplot = 'awesome title'
plt.title( 'Mountsorrel C:N across site for different soil strata' )
plt.suptitle('') # that's what you're after
plt.show()
soilpHCN.boxplot(column='BDCNAcSite',by='BDAcSite')
title_boxplot = 'awesome title'
plt.title( 'Buddon Wood C:N across site for different soil strata' )
plt.suptitle('') # that's what you're after
plt.show()

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

#soil comparison with all other sites
soilpHCNAll= pd.read_csv('../data/allsoils.csv')

soilpHCNAll.boxplot(column='pH',by='SITE')
title_boxplot = 'awesome title'
plt.title( 'Change in soil pH in  organic soil layer across all sites' )
plt.suptitle('') # that's what you're after
plt.show()
soilpHCNAll.boxplot(column='N',by='SITE')
title_boxplot = 'awesome title'
plt.title( 'Change in soil N in  organic soil layer across all sites' )
plt.suptitle('') # that's what you're after
plt.show()
soilpHCNAll.boxplot(column='C',by='SITE')
title_boxplot = 'awesome title'
plt.title( 'Change in soil C in  organic soil layer across all sites' )
plt.suptitle('') # that's what you're after
plt.show()
soilpHCNAll.boxplot(column='CN',by='SITE')
title_boxplot = 'awesome title'
plt.title( 'Change in soil C:N in  organic soil layer across all sites' )
plt.suptitle('') # that's what you're after
plt.show()

means = soilpHCNAll.groupby('SITE').mean()

soilpHCNAll.boxplot(column='pH')
title_boxplot = 'awesome title'
plt.title( 'Change in soil pH in  organic soil layer across all sites' )
plt.suptitle('') # that's what you're after
plt.plot(1,5.12, marker = '*')
plt.show()


soilpHCNAll.boxplot(column='N')
title_boxplot = 'awesome title'
plt.title( 'Change in soil N in  organic soil layer across all sites' )
plt.suptitle('') # that's what you're after
plt.plot(1,0.46,marker = '*')
plt.show()

soilpHCNAll.boxplot(column='C')
title_boxplot = 'awesome title'
plt.title( 'Change in soil C in  organic soil layer across all sites' )
plt.suptitle('') # that's what you're after
plt.plot(1,7.48,marker = '*')
plt.show()

soilpHCNAll.boxplot(column='CN')
title_boxplot = 'awesome title'
plt.title( 'Change in soil C:N in  organic soil layer across all sites' )
plt.suptitle('') # that's what you're after
plt.plot(1,15.99,marker = '*')
plt.show()

soilpHCNAll.boxplot(column='weight')
title_boxplot = 'awesome title'
plt.title( 'Change in soil C:N in  organic soil layer across all sites' )
plt.suptitle('') 
plt.plot(1,107.21,marker = '*')# that's what you're after
plt.show()

###############
#redoing some charts to correct labelling, but now using new neater
#data file 
data =  pd.read_csv('../data/AllSoilData.csv')
#for fig 4 of report, use only MSs and BDs

#sites = ['BDs','MSs']
#sites = data[data.site.isin(sites)]

siteMS = data[data['site']=='MSs']
siteBD = data[data['site']=='BDs']
#boxplot for each layer
siteMS.boxplot(column='pH',by='LAYER')
title_boxplot = 'awesome title'
plt.title( 'Mountsorrel pH across soil core' )
plt.suptitle('') # that's what you're after
plt.show()
siteBD.boxplot(column='pH',by='LAYER')
title_boxplot = 'awesome title'
plt.title( 'Buddon Wood pH across soil core' )
plt.suptitle('') # that's what you're after
plt.show()

siteMS.boxplot(column='N',by='LAYER')
title_boxplot = 'awesome title'
plt.title( 'Mountsorrel N across soil core' )
plt.suptitle('') # that's what you're after
plt.show()
siteBD.boxplot(column='N',by='LAYER')
title_boxplot = 'awesome title'
plt.title( 'Buddon Wood N across soil core' )
plt.suptitle('') # that's what you're after
plt.show()

siteMS.boxplot(column='C',by='LAYER')
title_boxplot = 'awesome title'
plt.title( 'Mountsorrel C across soil core' )
plt.suptitle('') # that's what you're after
plt.show()
siteBD.boxplot(column='C',by='LAYER')
title_boxplot = 'awesome title'
plt.title( 'Buddon Wood C across soil core' )
plt.suptitle('') # that's what you're after
plt.show()

siteMS.boxplot(column='CN',by='LAYER')
title_boxplot = 'awesome title'
plt.title( 'Mountsorrel C:N across soil core' )
plt.suptitle('') # that's what you're after
plt.show()
siteBD.boxplot(column='CN',by='LAYER')
title_boxplot = 'awesome title'
plt.title( 'Buddon Wood C:N across soil core' )
plt.suptitle('') # that's what you're after
plt.show()

####
#redo plotdatas for fig 6, all sites

data.boxplot(column='pH',by='site')
title_boxplot = 'awesome title'
plt.title( 'Change in soil pH in  organic soil layer for all sites' )
plt.suptitle('') # that's what you're after
plt.show()
data.boxplot(column='N',by='site')
title_boxplot = 'awesome title'
plt.title( 'Change in soil N in  organic soil layer for all sites' )
plt.suptitle('') # that's what you're after
plt.show()
data.boxplot(column='C',by='site')
title_boxplot = 'awesome title'
plt.title( 'Change in soil C in  organic soil layer for all sites' )
plt.suptitle('') # that's what you're after
plt.show()
data.boxplot(column='CN',by='site')
title_boxplot = 'awesome title'
plt.title( 'Change in soil C:N in  organic soil layer for all sites' )
plt.suptitle('') # that's what you're after
plt.show()

#redo figure 7

data.boxplot(column='pH')
title_boxplot = 'awesome title'
plt.title( 'pH in  upper soil layer across all sites' )
plt.suptitle('') # that's what you're after
plt.plot(1,5.3, marker = 'o',markersize = 20, markerfacecolor='red')
plt.show()


data.boxplot(column='N')
title_boxplot = 'awesome title'
plt.title( 'N in  upper soil layer across all sites' )
plt.suptitle('') # that's what you're after
plt.plot(1,0.3,marker = 'o',markersize = 20, markerfacecolor='red')
plt.show()

data.boxplot(column='C')
title_boxplot = 'awesome title'
plt.title( ' C in  upper soil layer across all sites' )
plt.suptitle('') # that's what you're after
plt.plot(1,4.8,marker = 'o',markersize = 20, markerfacecolor='red')
plt.show()

data.boxplot(column='CN')
title_boxplot = 'awesome title'
plt.title( 'C:N in  upper soil layer across all sites' )
plt.suptitle('') # that's what you're after
plt.plot(1,15.1,marker = 'o',markersize = 20, markerfacecolor='red')
plt.show()

data.boxplot(column='wt_g')
title_boxplot = 'awesome title'
plt.title( 'Weight of fixed volume of soil in  upper soil layer across all sites' )
plt.suptitle('') 
plt.plot(1,114.6,marker = 'o',markersize = 20, markerfacecolor='red')# that's what you're after
plt.show()