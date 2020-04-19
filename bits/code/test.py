# -*- coding: utf-8 -*-
"""
Created on Mon Mar 23 14:11:05 2020

@author: petra
"""



things = [1,"a",4]
print(things[0:2])
things[0]

for num in things:
    print(num)

things.append(6)

things
things.count()

atup = (1,2,4)
things[1] = 5
atup[0]

translation = {'one': 'first', 'two': 'second'}
translation['one']

def add_function(a, b):
    result = a + b
    return result

z = add_function(1,2)
print(z)

import pandas as pd
surveys_df = pd.read_csv("../data/surveys.csv")
surveys_df.head()
type(surveys_df)
surveys_df.dtypes
surveys_df.columns

surveys.df.shape
surveys_df.shape
surveys_df.tail(5)
surveys_df.columns

pd.unique(surveys_df['species_id'])
species_names = pd.unique(surveys_df['species_id'])
site_names = pd.unique(surveys_df['plot_id'])

len(site_names)
surveys_df['plot_id'].nunique()

len(site_names)

surveys_df['weight'].describe()
surveys_df['weight'].min()

grouped_data = surveys_df.groupby('sex')

grouped_data.describe()
grouped_data.mean()
grouped_data2 = surveys_df.groupby(['plot_id', 'sex'])
grouped_data2.mean()

grouped_data3 = surveys_df.groupby(['plot_id'])
grouped_data3['weight'].describe()
surveys_df.columns

grouped_data3 = surveys_df.groupby(['plot_id', 'weight'])
grouped_data3.describe()

species_counts = surveys_df.groupby('species_id')['record_id'].count()
print(species_counts)

surveys_df.groupby(['plot_id'])['weight'].describe()
surveys_df.groupby('species_id')['record_id'].count()['DO']



surveys_df.groupby('plot_id')['species_id'].count()


surveys_df.groupby('species_id').min()

species_counts.plot(kind='bar')

total_count = surveys_df.groupby('plot_id')['record_id'].nunique()

total_count.plot(kind='bar')

wts = surveys_df.groupby(['plot_id'])['weight'].mean()
wts.plot(kind = 'bar')


sex = surveys_df.groupby(['sex'])['record_id'].count()
sex.plot(kind = 'bar')


data = {}
data.plot(kind='bar', stacked=True, title="The title of my graph")

grouped_data3 = surveys_df(['plot_id'],['sex'])

string = 'abcdc'
string.find('c')

text1 = "Earth"
text2 = "Venus"
text3 = "Mars"
text4 = "Mercury"
print(text2.upper())
print(text4)
print(text3.lower())
print(text1)

daily_sales = \
"""Edith Mcbride   ;,;$1.21   ;,;   white ;,; 
09/15/17   ,Herbert Tran   ;,;   $7.29;,; 
white&blue;,;   09/15/17 ,Paul Clarke ;,;$12.52 
;,;   white&blue ;,; 09/15/17 ,Lucille Caldwell   
;,;   $5.13   ;,; white   ;,; 09/15/17,
Eduardo George   ;,;$20.39;,; white&yellow 
;,;09/15/17   ,   Danny Mclaughlin;,;$30.82;,;   
purple ;,;09/15/17 ,Stacy Vargas;,; $1.85   ;,; 
purple&yellow ;,;09/15/17,   Shaun Brock;,; 
$17.98;,;purple&yellow ;,; 09/15/17 , 
Erick Harper ;,;$17.41;,; blue ;,; 09/15/17, 
Michelle Howell ;,;$28.59;,; blue;,;   09/15/17   , 
Carroll Boyd;,; $14.51;,;   purple&blue   ;,;   
09/15/17   , Teresa Carter   ;,; $19.64 ;,; 
white;,;09/15/17   ,   Jacob Kennedy ;,; $11.40   
;,; white&red   ;,; 09/15/17, Craig Chambers;,; 
$8.79 ;,; white&blue&red   ;,;09/15/17   , Peggy Bell;,; $8.65 ;,;blue   ;,; 09/15/17,   Kenneth Cunningham ;,;   $10.53;,;   green&blue   ;,; 
09/15/17   ,   Marvin Morgan;,;   $16.49;,; 
green&blue&red   ;,;   09/15/17 ,Marjorie Russell 
;,; $6.55 ;,;   green&blue&red;,;   09/15/17 ,
Israel Cummings;,;   $11.86   ;,;black;,;  
09/15/17,   June Doyle   ;,;   $22.29 ;,;  
black&yellow ;,;09/15/17 , Jaime Buchanan   ;,;   
$8.35;,;   white&black&yellow   ;,;   09/15/17,   
Rhonda Farmer;,;$2.91 ;,;   white&black&yellow   
;,;09/15/17, Darren Mckenzie ;,;$22.94;,;green 
;,;09/15/17,Rufus Malone;,;$4.70   ;,; green&yellow 
;,; 09/15/17   ,Hubert Miles;,;   $3.59   
;,;green&yellow&blue;,;   09/15/17   , Joseph Bridges  ;,;$5.66   ;,; green&yellow&purple&blue 
;,;   09/15/17 , Sergio Murphy   ;,;$17.51   ;,;   
black   ;,;   09/15/17 , Audrey Ferguson ;,; 
$5.54;,;black&blue   ;,;09/15/17 ,Edna Williams ;,; 
$17.13;,; black&blue;,;   09/15/17,   Randy Fleming;,;   $21.13 ;,;black ;,;09/15/17 ,Elisa Hart;,; $0.35   ;,; black&purple;,;   09/15/17   ,
Ernesto Hunt ;,; $13.91   ;,;   black&purple ;,;   
09/15/17,   Shannon Chavez   ;,;$19.26   ;,; 
yellow;,; 09/15/17   , Sammy Cain;,; $5.45;,;   
yellow&red ;,;09/15/17 ,   Steven Reeves ;,;$5.50   
;,;   yellow;,;   09/15/17, Ruben Jones   ;,; 
$14.56 ;,;   yellow&blue;,;09/15/17 , Essie Hansen;,;   $7.33   ;,;   yellow&blue&red
;,; 09/15/17   ,   Rene Hardy   ;,; $20.22   ;,; 
black ;,;   09/15/17 ,   Lucy Snyder   ;,; $8.67   
;,;black&red  ;,; 09/15/17 ,Dallas Obrien ;,;   
$8.31;,;   black&red ;,;   09/15/17,   Stacey Payne 
;,;   $15.70   ;,;   white&black&red ;,;09/15/17   
,   Tanya Cox   ;,;   $6.74   ;,;yellow   ;,; 
09/15/17 , Melody Moran ;,;   $30.84   
;,;yellow&black;,;   09/15/17 , Louise Becker   ;,; 
$12.31 ;,; green&yellow&black;,;   09/15/17 ,
Ryan Webster;,;$2.94 ;,; yellow ;,; 09/15/17 
,Justin Blake ;,; $22.46   ;,;white&yellow ;,;   
09/15/17,   Beverly Baldwin ;,;   $6.60;,;   
white&yellow&black ;,;09/15/17   ,   Dale Brady   
;,;   $6.27 ;,; yellow   ;,;09/15/17 ,Guadalupe Potter ;,;$21.12   ;,; yellow;,; 09/15/17   , 
Desiree Butler ;,;$2.10   ;,;white;,; 09/15/17  
,Sonja Barnett ;,; $14.22 ;,;white&black;,;   
09/15/17, Angelica Garza;,;$11.60;,;white&black   
;,;   09/15/17   ,   Jamie Welch   ;,; $25.27   ;,; 
white&black&red ;,;09/15/17   ,   Rex Hudson   
;,;$8.26;,;   purple;,; 09/15/17 ,   Nadine Gibbs 
;,;   $30.80 ;,;   purple&yellow   ;,; 09/15/17   , 
Hannah Pratt;,;   $22.61   ;,;   purple&yellow   
;,;09/15/17,Gayle Richards;,;$22.19 ;,; 
green&purple&yellow ;,;09/15/17   ,Stanley Holland 
;,; $7.47   ;,; red ;,; 09/15/17 , Anna Dean;,;$5.49 ;,; yellow&red ;,;   09/15/17   ,
Terrance Saunders ;,;   $23.70  ;,;green&yellow&red 
;,; 09/15/17 ,   Brandi Zimmerman ;,; $26.66 ;,; 
red   ;,;09/15/17 ,Guadalupe Freeman ;,; $25.95;,; 
green&red ;,;   09/15/17   ,Irving Patterson 
;,;$19.55 ;,; green&white&red ;,;   09/15/17 ,Karl Ross;,;   $15.68;,;   white ;,;   09/15/17 , Brandy Cortez ;,;$23.57;,;   white&red   ;,;09/15/17, 
Mamie Riley   ;,;$29.32;,; purple;,;09/15/17 ,Mike Thornton   ;,; $26.44 ;,;   purple   ;,; 09/15/17, 
Jamie Vaughn   ;,; $17.24;,;green ;,; 09/15/17   , 
Noah Day ;,;   $8.49   ;,;green   ;,;09/15/17   
,Josephine Keller ;,;$13.10 ;,;green;,;   09/15/17 ,   Tracey Wolfe;,;$20.39 ;,; red   ;,; 09/15/17 ,
Ignacio Parks;,;$14.70   ;,; white&red ;,;09/15/17 
, Beatrice Newman ;,;$22.45   ;,;white&purple&red 
;,;   09/15/17, Andre Norris   ;,;   $28.46   ;,;   
red;,;   09/15/17 ,   Albert Lewis ;,; $23.89;,;   
black&red;,; 09/15/17,   Javier Bailey   ;,;   
$24.49   ;,; black&red ;,; 09/15/17   , Everett Lyons ;,;$1.81;,;   black&red ;,; 09/15/17 ,   
Abraham Maxwell;,; $6.81   ;,;green;,;   09/15/17   
,   Traci Craig ;,;$0.65;,; green&yellow;,; 
09/15/17 , Jeffrey Jenkins   ;,;$26.45;,; 
green&yellow&blue   ;,;   09/15/17,   Merle Wilson 
;,;   $7.69 ;,; purple;,; 09/15/17,Janis Franklin   
;,;$8.74   ;,; purple&black   ;,;09/15/17 ,  
Leonard Guerrero ;,;   $1.86   ;,;yellow  
;,;09/15/17,Lana Sanchez;,;$14.75   ;,; yellow;,;   
09/15/17   ,Donna Ball ;,; $28.10  ;,; 
yellow&blue;,;   09/15/17   , Terrell Barber   ;,; 
$9.91   ;,; green ;,;09/15/17   ,Jody Flores;,; 
$16.34 ;,; green ;,;   09/15/17,   Daryl Herrera 
;,;$27.57;,; white;,;   09/15/17   , Miguel Mcguire;,;$5.25;,; white&blue   ;,;   09/15/17 ,   
Rogelio Gonzalez;,; $9.51;,;   white&black&blue   
;,;   09/15/17   ,   Lora Hammond ;,;$20.56 ;,; 
green;,;   09/15/17,Owen Ward;,; $21.64   ;,;   
green&yellow;,;09/15/17,Malcolm Morales ;,;   
$24.99   ;,;   green&yellow&black;,; 09/15/17 ,   
Eric Mcdaniel ;,;$29.70;,; green ;,; 09/15/17 
,Madeline Estrada;,;   $15.52;,;green;,;   09/15/17 
, Leticia Manning;,;$15.70 ;,; green&purple;,; 
09/15/17 ,   Mario Wallace ;,; $12.36 ;,;green ;,; 
09/15/17,Lewis Glover;,;   $13.66   ;,;   
green&white;,;09/15/17,   Gail Phelps   ;,;$30.52   
;,; green&white&blue   ;,; 09/15/17 , Myrtle Morris 
;,;   $22.66   ;,; green&white&blue;,;09/15/17"""

daily_sales_replaced = daily_sales.replace(';,;',';')
daily_transactions = daily_sales_replaced.split(',')
print(daily_transactions)

daily_transactions_split = []
for items in daily_transactions:
 daily_transactions_split.append(items.split(';'))

transactions_clean = []
for items in daily_transactions_split:
  for item in items:
    transactions_clean.append(item.strip())

print(transactions_clean)
customers = []
sales = []
thread_sold = []
date = []

l = len(transactions_clean)
print(l)
customers = transactions_clean[::4]
sales = transactions_clean[1::4]
thread_sold = transactions_clean[2::4]

total_sales = 0

for item in sales:
    total_sales += float(item.strip('$'))
    


sale = sales[0]
float(sale.strip('$'))

thread_sold_split = []


for threads in thread_sold:
    if '&' in threads:
        split_colours = threads.split('&')
        thread_sold_split.append(split_colours[0])
        thread_sold_split.append(split_colours[1])
    else:
        thread_sold_split.append(threads)

thread = thread_sold[2]
thread.split('&')[0,1]
thread_sold_split.append(thread.split('&')[0])


thread_sold_split.count('white')
colours = set(thread_sold_split)

def count_colours(colour):
    return(thread_sold_split.count(colour))

count_colours('white')


print(colours)

num_sold = []
for colour in colours:
    num_sold.append(count_colours(colour))

colnums = list(zip(num_sold,colours))

for items in colnums:
    print('{items})

data = pd.DataFrame(surveys_df.groupby(['plot_id','sex'])['weight'].count())
newdf = data.reset_index()
newnewdf = newdf.pivot(index = 'plot_id', columns = 'sex', values = 'weight')
newnewdf.plot(kind='bar', stacked=True, title="The title of my graph")


surveys_df.sex.dtype
surveys_df['record_id'].dtype
surveys_df.record_id.dtype
surveys_df.sex.dtype
surveys_df.dtypes

print(5+5)
print(24-4)
print(5/9)
a = 7.33
int(a)
a = 7.8
int(a)
surveys_df['record_id'] = surveys_df['record_id'].astype('float64')
surveys_df['plot_id'].dtype
surveys_df['plot_id'] = surveys_df['plot_id'].astype('float64')

surveys_df.weight.astype("int")
surveys_df.weight.mean()

len(surveys_df[pd.isnull(surveys_df.weight)])
len(surveys_df[surveys_df.weight > 0])

df1 = surveys_df.copy()
df1.weight = df1.weight.fillna(0)
df1.weight.mean()
df1.weight = surveys_df.weight.fillna(surveys_df.weight.mean())

surveys_df.weight.isna().sum()
surveys_df.weight.isnull().values.sum()
surveys_df.isnull().sum()

surveys_df = pd.read_csv("../data/surveys.csv")
df_na = surveys_df.dropna()
df_na.to_csv('../data/surveys_complete.csv', index=False)

surveys_df = pd.read_csv("../data/surveys.csv", keep_default_na=False, na_values=[""])


species_df = pd.read_csv("../data/species.csv",keep_default_na=False, na_values=[""])

survey_sub = surveys_df.head(10)
surveys_sub_last = surveys_df.tail(10)
surveys_sub_last = surveys_sub_last.reset_index(drop=True)

vertical_stack = pd.concat([survey_sub,surveys_sub_last], axis=0)
horizontal_stack = pd.concat([survey_sub, surveys_sub_last], axis=1)

vertical_stack = vertical_stack.reset_index()
vertical_stack.to_csv('../data/out.csv', index = False)

new_output = pd.read_csv('../data/out.csv', keep_default_na=False, na_values=[""])

surevy2001 = pd.read_csv('../data/survey2001.csv')

species_sub = pd.read_csv('../data/speciesSubset.csv', keep_default_na=False, na_values=[""])

Index([u'species_id', u'genus', u'species', u'taxa'], dtype='object')

merged_inner = pd.merge(left=survey_sub, right = species_sub, left_on = 'species_id', right_on = 'species_id')
merged_inner2 = pd.merge(left=species_sub, right = survey_sub, left_on = 'species_id', right_on = 'species_id')
merged_inner.shape


merged_left = pd.merge(left=survey_sub, right = species_sub, how ='left',left_on = 'species_id',right_on='species_id')
merged_left

merged_left[ pd.isnull(merged_left.genus) ]


merged1 = pd.merge(left=survey_sub, right = species_sub, how ='right',left_on = 'species_id',right_on='species_id')
merged2 = pd.merge(left=species_sub, right = survey_sub, how ='left',left_on = 'species_id',right_on='species_id')

merged_outer = pd.merge(left=survey_sub, right = species_sub, how ='outer',left_on = 'species_id',right_on='species_id')
merged_outer2 = pd.merge(left=species_sub, right = survey_sub, how ='outer',left_on = 'species_id',right_on='species_id')

alldat = pd.merge(left = surveys_df, right = species_df, how = 'left', left_on = 'species_id', right_on = 'species_id')

total_count = surveys_df.groupby('plot_id')['record_id'].nunique()

total_count = alldat.groupby('plot_id')['taxa'].count()
total_count.plot(kind='bar')

%matplotlib inline

total_count = alldat.groupby('plot_id')['taxa'].count()
total_count.plot(kind='bar')


total_count2 = alldat.groupby(['sex','plot_id'])['taxa'].count()
total_count2.plot(kind='bar')


data = pd.DataFrame(surveys_df.groupby(['plot_id','sex'])['weight'].count())
new_df = data.reset_index()
newnewdf = new_df.pivot(index = 'plot_id', columns = 'sex', values = 'weight')
newnewdf.plot(kind='bar', stacked=True, title="The title of my graph")

animals = ['lion', 'tiger', 'crocodile', 'vulture', 'hippo']
print(animals)

for creature in animals:
    print(creature)

animals = ['lion', 'tiger', 'crocodile', 'vulture', 'hippo']
for creature in animals:
    print(creature  - '\n' + ',')
   
   
print('The loop variable is now: ' + creature)


converted_list = []
for creature in animals:
    converted_list.append(creature.strip())

print(converted_list)

import os

surveys2002 = surveys_df[surveys_df.year == 2002]
surveys2002.to_csv('../data/surveys2002.csv')
surveys_df.year


surveys_df['year'].unique()

for year in surveys_df['year'].unique():
   filename='data/surveys' + str(year) + '.csv'
   print(filename)
surveys_df = pd.read_csv('../data/surveys.csv')


for year in surveys_df.year.unique():
    surveys_year = surveys_df[surveys_df.year == year].dropna()
    filename = '../data/surveys'+str(year)+'.csv'
    surveys_year.to_csv(filename)
    
    
 test= pd.read_csv('../data/surveys2002.csv')
 test.count('na')
 
 l = len(transactions_clean)
print(l)
customers = transactions_clean[::4]
sales = transactions_clean[1::4]
thread_sold = transactions_clean[2::4]

for year in surveys_df.year.unique():
    surveys_year = surveys_df[surveys_df.year == year].dropna()
    filename = '../data/surveys'+str(year)+'.csv'
    surveys_year.to_csv(filename)
    
 
 len(os.walk('../data/').next()[2])
files = os.listdir('../data/')
len(files)    
len( os.listdir('../data/'))


    files = os.listdir('data/yearly_files')
for f in files:
    os.remove(path = 'data/yearly_files/' + f)

years =  surveys_df.year.unique()

years = years[::5]

for year in years:
    surveys_year = surveys_df[surveys_df.year == year].dropna()
    filename = '../data/surveys'+str(year)+'.csv'
    surveys_year.to_csv(filename)
#################
    

def this_is_the_function_name(input_argument1, input_argument2):

    print('The function arguments are:', input_argument1, input_argument2, '(this is done inside the function!)')
    x = 3
    return input_argument1 * input_argument2


this_is_the_function_name(1,2,3)

x = 5

########################################################################

def one_year_csv_writer(this_year, all_data,path,root):
    """
    Writes a csv file for data from a given year.
    this_year -- year for which data is extracted
    all_data -- DataFrame with multi-year data
    """

    # Select data for the year
    surveys_year = all_data[all_data.year == this_year]

    # Write the new DataFrame to a csv file
    filename = path + root +  str(this_year) + '.csv'
    surveys_year.to_csv(filename)
    


def yearly_data_csv_writer(start_year, end_year, all_data,path,root):
    """
    Writes separate CSV files for each year of data.

    start_year -- the first year of data we want
    end_year -- the last year of data we want
    all_data -- DataFrame with multi-year data
    """

    # "end_year" is the last year of data we want to pull, so we loop to end_year+1
    for year in range(start_year, end_year+1):
        one_year_csv_writer(year, all_data,path,root)



path = '../data/'
root = 'surveybydate'
start_year = 1977
end_year = 2002
all_data = surveys_df

yearly_data_csv_writer(start_year, end_year, all_data,path,root)

###############################################################
























