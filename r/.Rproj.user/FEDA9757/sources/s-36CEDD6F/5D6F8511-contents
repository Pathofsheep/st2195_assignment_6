#R Script for St2195 - Practise Assigment 6

#1. Load and merge the datasets keeping all information available 
#for the dates in which there is a measurement in “fx.csv”

#libraries
if (!("data.table" %in% installed.packages()))
{
  install.packages("data.table")
}
if (!("dplyr" %in% installed.packages()))
{
  install.packages("data.table")
}
if (!("tidytext" %in% installed.packages()))
{
  install.packages("tidytext")
}
if (!("stopwords" %in% installed.packages()))
{
  install.packages("stopwords")
}
library("data.table")
library("dplyr")
library("tidytext")
library("stopwords")


#Load datasets, fix colnames

ecb_speeches = read.csv("N:\\Uni_Bigdata\\all_ECB_speeches.csv",skip=22, quote = '',sep='|',col.names = c('date','speakers','title','subtitle','contents')) 
fx = read.csv("N:\\data\\fx.csv",skip = 5,col.names = c('date','rate','obs.status','obs.comment'))

#Change rate to numeric
fx$rate = as.double(fx$rate)

#Merge them to one set
df = merge(fx,ecb_speeches, by.x="date",by.y="date", all=TRUE)

#Remove row if NA in date column, order by date
df = df[!is.na(df$date),]
df = df[order(df$date),]

#2. Remove entries with obvious outliers or mistakes, if any.
#Some data analyses and getting var's for extreme outliers
summary(df$rate)
plot(df$rate)
#Looks like there are no outliers, but for dfness (3x for extreme outliers):

Tmin = quantile(df$rate,0.25,na.rm = TRUE) - 3 * IQR(df$rate,na.rm = TRUE)
Tmax = quantile(df$rate,0.75,na.rm = TRUE) + 3 * IQR(df$rate, na.rm = TRUE)

#Remove anything above or below Tmin / Tmax
df = df[(df$rate < Tmax & df$rate > Tmin) | is.na(df$rate),]

#3. Replace  missing exchange rate with the latest information available.
#If there is none, remove entry

# Check every single value for NAs
# If found, put the mean of the surrounding vars in the new dataframe df_n
df_n = df
for(i in 1:length(df$rate))
{
  #Check if element has NA
  if(is.na(df$rate[i]))
  {
    #Get Value from the next element (out of bounds check)
    if(i+1 <= length(df$rate))
      nextVal = df$rate[i+1]
    else      
      nextVal = NA
    #Get Value from the prev. element (out of bounds check)
    if(i-1 >= 1)
      prevVal = df$rate[i-1]
    else
      prevVal = NA
    #If nextVal or prevVal are not NA, replace the value
    if(!is.na(prevVal) | !is.na(nextVal))
    {
      #If one is NA, set them equal
      if(is.na(prevVal))
        prevVal = nextVal
      if(is.na(nextVal))
        nextVal = prevVal
      #Temporary vector so we dont have to calc on our own
      c_temp = c(nextVal,prevVal)
      df_n$rate[i] = mean(c_temp)
    }
  }
}

#remove any leftover NAs
df_n = df_n[!is.na(df_n$rate),]

#put original df back in place, remove temp one from memory
df = df_n
rm(df_n)

#4. Calculate the exchange rate return. Extend the original dataset with the
#following variables: “good_news” (equal to 1 when the exchange rate return is
#larger than 0.5 percent, 0 otherwise) and “bad_news” (equal to 1 when the
#exchange rate return is lower than -0.5 percent, 0 otherwise).

fx_rate_return = function(fx,fx_prev)
{
  return((fx-fx_prev)/fx_prev)
}

#Add a new column with the previous return rate
for(i in 1:length(df$rate))
{
  df$prev_rate[i] = df$rate[i-1]
}

#Add column good_news
df$good_news = ifelse(fx_rate_return(df$rate,df$prev_rate) > 0.005,1,0)
df$bad_news = ifelse(fx_rate_return(df$rate,df$prev_rate) < -0.005,1,0)

#Remove prev_rate with dplyr select
df = select(df, -c(prev_rate))

#5. Remove the entries for which contents column has NA values. Generate and
#store in csv the following tables:
#a. “good_indicators” – with the 20 most common words (excluding articles,prepositions,etc
#associated with entries wherein “good_news” is equal to 1;
#b. “bad_indicators” – with the 20 most common words (excluding articles,prepositions,etc
#associated with entries wherein “bad_news” is equal to 1;



#Remove content = NA
df = df[!is.na(df$contents),]

df_good_news = df[df$good_news == 1,]
df_bad_news = df[df$bad_news == 1,]

#Using Tidytext to count words, stopwords to filter out prepositions:
#select(df_good_news, c(contents))

#Select only contents
df_good_news_n = select(df_good_news, c(contents))
df_bad_news_n = select(df_bad_news, c(contents))
#Split it by words
df_good_news_n = unnest_tokens(df_good_news_n,word,contents)
df_bad_news_n = unnest_tokens(df_bad_news_n,word,contents)
#Get stopwords in dataframe
stop_words = data.frame(stopwords::stopwords("english")) 
#Rename column to word for easier join
colnames(stop_words) = c("word")
#Remove stop words from dataframe
df_good_news_n = anti_join(df_good_news_n, stop_words)
df_bad_news_n = anti_join(df_bad_news_n, stop_words)
#Count top 20 words
count_good_news = head(count(df_good_news_n,word,sort=TRUE),n=20)
count_bad_news = head(count(df_bad_news_n,word,sort=TRUE),n=20)
#Finally, write .csv
write.table(count_good_news,file="good_indicators.csv",quote=FALSE,col.names=FALSE, row.names=TRUE, sep = ",")
write.table(count_bad_news,file="bad_indicators.csv",quote=FALSE,col.names=FALSE, row.names=TRUE, sep = ",")

