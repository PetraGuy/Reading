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
dbhcounts['percentamtree']=dbhcounts['amdbhcounts']/dbhcounts['totaldbh'] #amount am trees
dbhcounts['percentinvtree']=dbhcounts['invdbhcounts']/dbhcounts['totaldbh'] #amount inv trees
dbhcounts = dbhcounts.rename(columns = {'site':'Site','plot':'Plot'})

#drop cols not required

treedata = dbhcounts.drop(['totaldbh','amdbhcounts','invdbhcounts'], axis = 1) #final df of tree data, %inv and %am

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
groundfloranew.to_csv('../data/groundfloranew.csv', index = False)


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

###########################################################################
#herbflora and ground flora are site/plot/nest flora.
#need cover of invasives as percentage of total cover of shrubs
#need to come from herbs and shrub because some invasives - bracken - are in herbs
#others - bramble - are in shrubs

totalshrubcover = shrubflora.groupby(['Site','Plot','Yr'])['cover'].sum().to_frame().reset_index()

amshrub = shrubflora[shrubflora['ms']=='am'] 
amshrubcover = amshrub.groupby(['Site','Plot','Yr'])['cover'].sum().to_frame().reset_index() #amount am shrub cover per plot

herbcover = herbflora.groupby(['Site','Plot','Yr'])['cover'].sum().to_frame().reset_index() #amount am shrub cover per plot

invshrub = shrubflora[shrubflora['flag']==1]
invshrubcover = invshrub.groupby(['Site','Plot','Yr'])['cover'].sum().to_frame().reset_index() #amount inv shrub per plot

invherb = herbflora[herbflora['flag']==1]
invherbcover = invherb.groupby(['Site','Plot','Yr'])['cover'].sum().to_frame().reset_index() #amount inv herb per plot

cover = pd.merge(totalshrubcover, amshrubcover, on = ['Site','Plot','Yr'],how ='left')
cover = cover.rename(columns={'cover_x':'totalshrubcover','cover_y':'amshrubcover'})

cover = pd.merge(cover, invshrubcover, on = ['Site','Plot','Yr'],how ='left')
cover = cover.rename(columns={'cover':'invshrubcover'})

cover = pd.merge(cover, herbcover, on = ['Site','Plot','Yr'],how ='left')
cover = cover.rename(columns={'cover':'herbcover'})

cover = pd.merge(cover, invherbcover, on = ['Site','Plot','Yr'],how ='left')
cover = cover.rename(columns={'cover':'invherbcover'}) # df of cover for amshrubs, inv shrubs and inv herbs

#convert to relative abundance
cover['percentamshrub'] = cover['amshrubcover']/cover['totalshrubcover']
cover['percentinvshrub'] = cover['invshrubcover']/cover['totalshrubcover']
cover['percentinvherb'] = cover['invherbcover']/cover['herbcover']


#drop cols not needed
coverdata = cover.drop(['totalshrubcover','amshrubcover','invshrubcover','invherbcover'],axis=1)
####################################################################

#for invasives analysis
invherbcoverspecies = invherb.groupby(['Site','Plot','Yr','Amalgams','species'])['cover'].sum().to_frame().reset_index()
invasiveshrubcoverspecies = invshrub.groupby(['Site','Plot','Yr','Amalgams','species'])['cover'].sum().to_frame().reset_index()
invasives = pd.merge(invherbcoverspecies,invasiveshrubcoverspecies, on=['Site','Plot','Yr'], how = 'left')
#now see below, will merge to get NVC, richness and herb abundance before saving this file info

#but also want to look at how many "invasives" that I havent flagged as invasive
#these two now have evrything in, including flag = 1
herbcoverinvasive = herbflora.groupby(['Site','Plot','Yr','species'])['cover'].sum().to_frame().reset_index()
shrubcoverinvasive = shrubflora.groupby(['Site','Plot','Yr','species'])['cover'].sum().to_frame().reset_index()

#combine the herb and shrub
allflora = pd.merge(herbcoverinvasive,shrubcoverinvasive, on = ['Site','Plot','Yr'], how = 'left')

#see below for merge to get NVC's etc
#########################

#herb richness and abundance
herbrichness = herbflora.groupby(['Site','Plot','Yr']).size().to_frame().reset_index()
herbrichness = herbrichness.rename(columns = {0:'alpha'})

#save for later
herbrichness.to_csv('../data/herbrichness.csv', index = False)

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
nvc = nvc.drop(['NVC'],axis=1)

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

alldata = pd.merge(herbrichness,treedata, on = ['Site','Plot','Yr'])
alldata = pd.merge(alldata,coverdata, on = ['Site','Plot','Yr'], how = 'left')
alldata = pd.merge(alldata,lba, on = ['Site','Plot','Yr'], how = 'left')
alldata = pd.merge(alldata,som,on = ['Site','Plot','Yr'], how = 'left')
alldata = pd.merge(alldata,pH, on = ['Site','Plot','Yr'], how = 'left')
alldata = pd.merge(alldata,nvc, on = ['Site','Plot','Yr'], how = 'left')

#######################################################################


alldata.to_csv('../data/all2data.csv', index = False)

##########################################################################

#finalise the invasives data for further analysis

invasivescomp = pd.merge(invasives,alldata, on = ['Site','Plot','Yr'], how = 'left')
invasivescomp.to_csv('../data/invasives.csv', index = False)
allfloracomp = pd.merge(allflora,alldata, on = ['Site','Plot','Yr'], how = 'left')
allfloracomp.to_csv('../data/allfloracomp.csv', index = False)

