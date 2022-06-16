# -*- coding: utf-8 -*-
"""
Created on Wed Jun  8 11:12:52 2022

@author: Patri
"""


#Python Script for St2195 - Practise Assigment 6

#1. Load and merge the datasets keeping all information available 
#for the dates in which there is a measurement in “fx.csv”

#libraries

import pandas as pd
import numpy
import nltk
import string
import collections
#Get stopwords from nltk package
nltk.download('stopwords')
from nltk.corpus import stopwords
import csv

#names= ['date','speakers','title','subtitle','contents'],
#Load datasets, fix colnames
ecb_speeches = pd.read_csv("N:\\Uni_Bigdata\\all_ECB_speeches.csv",skiprows=23, sep="|",header=None,names= ['date','speakers','title','subtitle','contents'],index_col=0)
fx = pd.read_csv("N:\\data\\fx.csv",skiprows = 6,names = ['date','rate','obs.status','obs.comment'],header=None)

ecb_speeches.dropna(inplace=True)
ecb_speeches = ecb_speeches.groupby("date")['contents'].apply(lambda x: " ".join(x.astype(str))).reset_index()
#Change '-' values to NaN
fx['rate'] = fx['rate'].replace("-",numpy.NaN)

#Change rate to float
fx['rate'] = fx['rate'].astype('float')

#Merge them to one set

df = pd.merge(fx, ecb_speeches, how = 'left')

#Set date to datetime format, make it an index
df['date'] = pd.to_datetime(df['date'])
df.set_index('date', inplace= True)

#some info about df
df
df.plot();
df.describe()

df.isna().sum()

#2. Remove entries with obvious outliers or mistakes, if any.
#Some data analyses and getting var's for extreme outliers
min(df['rate'])
max(df['rate'])
#Looks like there are no outliers, but for completeness (3x for extreme outliers):

def iqr(_df):
    """This function returns the iqr of a df"""
    return (_df.quantile(0.75) - _df.quantile(0.25))

iqr(df['rate'])

Tmin = df['rate'].quantile(0.25) - 3 * iqr(df['rate'])
Tmax = df['rate'].quantile(0.75) + 3 * iqr(df['rate'])

#Remove anything above or below Tmin / Tmax
df.loc[((df.rate < Tmax) & (df.rate > Tmin)) | ((df.rate.isna()))]


#3. Replace  missing exchange rate with the latest information available.
#If there is none, remove entry

# Check every single value for NAs
# If found, put the mean of the surrounding vars in the new dataframe df_n
df.rate.fillna(method='bfill', inplace=True)

#4. Calculate the exchange rate return. Extend the original dataset with the
#following variables: “good_news” (equal to 1 when the exchange rate return is
#larger than 0.5 percent, 0 otherwise) and “bad_news” (equal to 1 when the
#exchange rate return is lower than -0.5 percent, 0 otherwise).

df['return'] = df.rate.diff(-1)/df.rate

df['return']

#Add column good_news
df['good_news'] = (df['return'] > 0.5/100).astype(int)
df['bad_news'] = (df['return'] < -0.5/100).astype(int)

#5. Remove the entries for which contents column has NA values. Generate and
#store in csv the following tables:
#a. “good_indicators” – with the 20 most common words (excluding articles,prepositions,etc
#associated with entries wherein “good_news” is equal to 1;
#b. “bad_indicators” – with the 20 most common words (excluding articles,prepositions,etc
#associated with entries wherein “bad_news” is equal to 1;


#Remove content = NA
df = df[df['contents'].notna()]

df_good_news = df.contents[df.good_news==1].str.cat(sep=' ')
df_bad_news = df.contents[df.bad_news==1].str.cat(sep=' ')

#Function to calc word freq. and remove stopwords

def get_word_freq(contents, stop_words, num_words):
    freq = dict()
    for word in contents.split():
        word = word.strip(string.punctuation+'–')
        word = word.lower()
        if word not in stop_words and len(word):
            if word in freq:
                freq[word] += 1
            else:
                freq[word] = 1
    freq = dict(sorted(freq.items(), key = lambda item: -item[1]))
    return list(freq.keys())[:num_words]

stopwords.words('english')


#Select words
df_good_news_n = get_word_freq(df_good_news, stopwords.words('english'), num_words = 20)
df_bad_news_n = get_word_freq(df_bad_news, stopwords.words('english'), num_words = 20)

#Finally, write .csv

fgn = open('good_indicators.csv','w')
fbn = open('bad_indicators.csv','w')

fgn.write('Words\n\n')
for word in df_good_news_n:
        fgn.write('%s\n' % word)
fgn.close()

fbn.write('Words\n\n')
for word in df_bad_news_n:
        fbn.write('%s\n' % word)
fbn.close()
