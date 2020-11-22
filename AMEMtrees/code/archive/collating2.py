#start again with wrangling
# 1 get proportions for am/total and invasives/total
#start with Trees.csv, tidy it up a bit

import pandas as pd

#treespecieslist has ms and flags
trees = pd.read_csv('../data/treespecieslist.csv')#tree species details

#TreesByPlot has all the site/plot details
treesinplots = pd.read_csv('../data/TreesByPlot.csv')#trees by plot

#merge to add mycor to trees list
treescomplete = pd.merge(treesinplots,trees, left_on = 'BRC',
                         right_on = 'BRC', how = 'left')

#tidy
treescomplete = treescomplete.drop(['Unnamed: 2','BRC names'], axis =1)
treescomplete = treescomplete.rename(columns={'SITE': 'site','PLOT':'plot'})
treescomplete.to_csv('../data/Trees.csv', index = False)#now trees by plot have details

###############################

#from trees, group by site lot and sum dbh x count for invasives,ams and all trees
#split trees into years
#treescomplete1 = treescomplete[treescomplete['Yr']==1]
#treescomplete2 = treescomplete[treescomplete['Yr']==2]
#create col for dbhxcounts for all trees
treescomplete['dbhcounts'] = treescomplete['DBH_class']*treescomplete['Count']
totaldbhcounts = treescomplete.groupby(['site','plot','Yr'])['dbhcounts'].sum().to_frame().reset_index()

#create dfs with dbhxcount totals per plot for invasinves/am and all
treesam = treescomplete[treescomplete['ms']=='am'] 
amtreesdbhcounts = treesam.groupby(['site','plot','Yr'])['dbhcounts'].sum().to_frame().reset_index()

#repeat for invasives
treesinv = treescomplete[treescomplete['flag']==1] 
invtreesdbhcounts = treesinv.groupby(['site','plot','Yr'])['dbhcounts'].sum().to_frame().reset_index()

#join the above 3 dfs.
dbhcounts = pd.merge(totaldbhcounts, amtreesdbhcounts, on = ['site','plot','Yr'],how ='left')
dbhcounts = dbhcounts.rename(columns={'dbhcounts_x':'totaldbh','dbhcounts_y':'amdbhcounts'})

dbhcounts = pd.merge(dbhcounts, invtreesdbhcounts, on = ['site','plot','Yr'],how ='left')
dbhcounts = dbhcounts.rename(columns={'dbhcounts':'invdbhcounts'})

#convert to relative abundance
dbhcounts['percentam']=dbhcounts['amdbhcounts']/dbhcounts['totaldbh']
dbhcounts['percentinv']=dbhcounts['invdbhcounts']/dbhcounts['totaldbh']

##############################################################################
# now get percent am cover from the shrubs, import ground cover and separate in herbs and shrubs

floralist = pd.read_csv('../data/grndspeciesfullinfoWS.csv')# species details
groundflora = pd.read_csv('../data/GroundFlora.csv')#flora by plot

#split ground flora into years because its a bit weird with repeats and nas
groundflora1 = groundflora.loc[:,['Site', 'Plot', 'Nest', 'CoverYr1','AmalgamsYr1']].drop_duplicates().dropna()
groundflora2 = groundflora.loc[:,['Site', 'Plot', 'Nest', 'CoverYr2','AmalgamsYr2']].drop_duplicates().dropna()

#rename cols
groundflora1 = groundflora1.rename(columns={'CoverYr1':'cover','AmalgamsYr1':'Amalgams'})
groundflora2 = groundflora2.rename(columns={'CoverYr2':'cover','AmalgamsYr2':'Amalgams'})

#now add year column
groundflora1['Yr'] = 1
groundflora2['Yr'] = 2

#now stick them back together as long df
groundfloranew = pd.concat([groundflora1,groundflora2])

#split into herbs and shrubs
herbs = floralist.loc[floralist['woodiness'] == 'h']
shrubs = floralist.loc[floralist['woodiness'].isin (['ws','wt','sw'])]

herbflora = pd.merge(groundfloranew,herbs, left_on = 'Amalgams', right_on = 'BRC',how = 'right')

shrubflora = pd.merge(groundfloranew,shrubs, left_on = 'Amalgams', right_on = 'BRC',how = 'right')

#select required columns
herbflora = herbflora.drop(['Unnamed: 0','preneation', 'life form', 'BRC','ms','woodiness', 
                            'Tjan', 'Tjul','Prec', 'Br Habitats',
                            'L', 'F', 'R', 'N', 'S'], axis = 1)

shrubflora = shrubflora.drop(['Unnamed: 0','preneation', 'life form', 'BRC','woodiness', 
                            'Tjan', 'Tjul','Prec', 'Br Habitats',
                            'L', 'F', 'R', 'N', 'S'], axis = 1)

#herbflora and ground flora are site/plot/nest flora.
#need cover of invasives as percentage of total cover.
#need to come from herbs and shrub because some invasives - bracken - are in herbs
#others - bramble - are in shrubs

totalcover = shrubflora.groupby(['Site','Plot','Yr'])['cover'].sum().to_frame().reset_index()
amshrub = shrubflora[shrubflora['ms']=='am'] 
amcover = amshrub.groupby(['Site','Plot','Yr'])['cover'].sum().to_frame().reset_index()
invshrub = shrubflora[shrubflora['flag']==1]
invcover = invshrub.groupby(['Site','Plot','Yr'])['cover'].sum().to_frame().reset_index()

shrubcover = pd.merge(totalcover, amcover, on = ['Site','Plot','Yr'],how ='left')
shrubcover = shrubcover.rename(columns={'cover_x':'totashrublcover','cover_y':'amcover'})

shrubcover = pd.merge(shrubcover, invcover, on = ['Site','Plot','Yr'],how ='left')
shrubcover = shrubcover.rename(columns={'cover':'incover'})
####################################################################

#herb richness and abundance
herbrichness = herbflora.groupby(['Site','Plot','Yr']).size().to_frame().reset_index()
herbrichness = herbrichness.rename(columns = {0:'alpha'})
herbcover = herbflora.groupby(['Site','Plot','Yr'])['cover'].sum().to_frame().reset_index()
herbcover = herbcover.rename(columns = {'cover':'herbcover'})
###################################################################

#the rest of the data, tidy and longify
som = pd.read_csv('../data/soil_som.csv')
pH = pd.read_csv('../data/soil_ph.csv')

som1 = som[['SITE','PLOT','SOM1971']]
som2 = som[['SITE','PLOT','SOM resurvey']]
som1 = som1.rename(columns={'SITE':'Site','PLOT':'Plot','SOM1971':'som'})
som1['Yr'] = 1
som2 = som2.rename(columns={'SITE':'Site','PLOT':'Plot','SOM resurvey':'som'})
som2['Yr'] = 2
som = pd.concat([som1,som2])

pH1 = pH[['SITE','PLOT','pH1971']]
pH2 = pH[['SITE','PLOT','pH resurvey']]
pH1 = pH1.rename(columns={'SITE':'Site','PLOT':'Plot','pH1971':'pH'})
pH2 = pH2.rename(columns={'SITE':'Site','PLOT':'Plot','pH resurvey':'pH'})
pH1['Yr']=1
pH2['Yr']=2
pH = pd.concat([pH1,pH2])

nvc = pd.read_csv('../data/NVCshort.csv')
nvc = nvc.drop(['Unnamed: 0'], axis = 1)
nvc = nvc.rename(columns={'SITE':'Site','PLOT':'Plot'})

lba = pd.read_csv('../data/Live_basal_area.csv')
lba1 = lba[['SITE','PLOT','y71']]
lba2 = lba[['SITE','PLOT','y03']]
lba1 = lba1.rename(columns={'SITE':'Site','PLOT':'Plot','y71':'lba'})
lba2 = lba2.rename(columns={'SITE':'Site','PLOT':'Plot','y03':'lba'})
lba1['Yr']=1
lba2['Yr']=2
lba = pd.concat([lba1,lba2])

################################################################
#collate all the data

alldata = pd.merge(herbrichness,herbcover, on = ['Site','Plot','Yr'])

dbhcounts = dbhcounts.rename(columns = {'site':'Site','plot':'Plot'})
alldata = pd.merge(alldata,dbhcounts, on = ['Site','Plot','Yr'], how = 'left')
alldata = pd.merge(alldata,shrubcover, on = ['Site','Plot','Yr'], how = 'left')
alldata = pd.merge(alldata,lba, on = ['Site','Plot','Yr'], how = 'left')
alldata = pd.merge(alldata,som,on = ['Site','Plot','Yr'], how = 'left')
alldata = pd.merge(alldata,pH, on = ['Site','Plot','Yr'], how = 'left')
alldata = pd.merge(alldata,nvc, on = ['Site','Plot','Yr'], how = 'left')

#######################################################################

#amcover and inv cover need to be %

alldata['percentamcover'] = alldata['amcover']/alldata['totashrublcover']
alldata['percentinvcover']= alldata['incover']/alldata['totashrublcover']
 #tidy by removing unneeded cols
 
alldata = alldata.drop(['totaldbh', 'amdbhcounts','invdbhcounts','totashrublcover', 'amcover','incover','NVC'], axis =1)
alldata= alldata.rename(columns = {'percentam':'amtreeprop','percentinv':'invtreeprop'})

alldata.to_csv('../data/alldata.csv', index = False)
