# -*- coding: utf-8 -*-
'''
Python script created to collect daily Google Trends data for MA
thesis: 'Using Machine Learning and Google Searches to Predict Stock
Market Changes'. See paper for more details.

Uses python module pytrends: https://github.com/GeneralMills/pytrends
All credit for pytrends goes to GeneralMills
'''

from pytrends.request import TrendReq
import datetime
import pandas as pd
import matplotlib.pyplot as plt
import time
from random import randint

class InvalidMonthorYear(Exception):
    pass

# Setup Pytrends environment
pytrends = TrendReq(hl='en-US', tz=360)

def time_list(year,month,year_back):
    '''
    Creates a list of strings 1 month apart going back a 
    number of years, set by year_back, form the start year
    and month specified
    '''
    time_list = []
    try:
        month = int(month)  
        year = int(year)
    except ValueError:
        raise InvalidMonthorYear ("Error: Month and Year must be either an int or a string that is convertable to an int")
    
    while True:
        if month >= 1:
            month_str = str(month)
            if len(month_str) > 1:
                time_str = "{}-{}-01".format(year,month_str)
                time_list.append(time_str)
            else:
                time_str = "{}-0{}-01".format(year,month_str)
                time_list.append(time_str)
            month -= 1
        else:
            break
    
    year_num = int(year) - 1
    
    year_list = []
    for i in range(year_back + 1):
        year_list.append(year_num - i)
    
    for y in year_list:
        for m in range(12,0,-1):
            month_str = str(m)
            if len(month_str) > 1:
                month_time_str = "{}-01".format(month_str)
            else:
                month_time_str = "0{}-01".format(month_str)
            final_time_str = "{}-{}".format(y,month_time_str)
            time_list.append(final_time_str)
    return time_list

def get_kw_web(key_word,category=''):
    '''
    Gets the Google Trends search value for the specified
    keyword. Set category to desired medium(ex: 'news'), keep
    blank for web search.
    '''
    data = pd.DataFrame(columns=[key_word])
    for i in range(len(time_list)):
        month_1 = time_list[i]
        try:
            month_2 = time_list[i+1]
        except:
            break
        pytrends.build_payload([key_word], cat=0, timeframe="{} {}".format(month_2,month_1), geo='US', gprop=category)
        interest = pytrends.interest_over_time()
        interest = interest.drop(interest.index[len(interest)-1])
        data = pd.concat([interest,data])
        # Create a random cooldown so the API doesn't reject request
        cooldown = randint(1,5) 
        if cooldown == 3:
            time.sleep(3)
    data.index.name = 'date'
    data.reset_index(inplace=True)
    return data

def normalize_data(data, key_word, category=''):
    '''
    Normalizes the daily search data to be relative over 
    the entire time frame. Fill keyword and category
    with the same keyword and category used for the
    daily data.
    '''
    pytrends.build_payload([key_word], cat=0, timeframe="{} {}".format(time_list[-1],time_list[0]), geo='US', gprop=category)
    interest = pytrends.interest_over_time()
    interest.index.name = 'date'
    interest.reset_index(inplace=True)

    result = pd.merge(data[['date',key_word]],interest[['date',key_word]],left_on='date',right_on='date',how='left')

    names = ["date",
             "{}_daily".format(key_word).replace(" ","_"),
             "{}_weekly".format(key_word).replace(" ","_"),
             "{}_Normalized_Daily".format(key_word).replace(" ","_")]
    result.columns = names[0:3]
    
    week = 1
    result["Adjustment_Factor"] = 0
    adj = 0
    for index,row in result.iterrows():
        if not pd.isnull(row[names[2]]):
            if row[names[1]] != 0:
                week = row[names[2]]
                adj = week/row[names[1]]
            else:
                adj = row[names[1]] / 1
        result.loc[index,"Adjustment_Factor"] = adj
    
    result[names[3]] = result[names[1]] * result["Adjustment_Factor"]
    result[names[3]] = (result[names[3]] / max(result[names[3]])) * 100
    result = result.iloc[:,[0,1,4]]
    return result

if __name__ == "__main__":
    # Build time list
    year = "2018"
    month = "04"
    # Uncomment these for most current date
    #year = now.strftime("%Y")
    #month = now.strftime("%m")
    
    year_back = 1 #Number of years to go back
    time_list = time_list(year,month,year_back)
    
    # Define list of terms to collect for Google web search
    web_list = ["Amazon Stock","Apple Stock","Google Stock","Facebook Stock"]
    # Define list of terms to collect for Google news Search
    news_list = ["Amazon","Apple","Google","Facebook"]
    final = pd.DataFrame()
    
    # Run main loop
    for i in range(len(web_list)):
        dataframe_web = get_kw_web(web_list[i])
        dataframe_news = get_kw_web(news_list[i],category='news')     
        dataframe_web_cleaned = normalize_data(dataframe_web, web_list[i])
        dataframe_news_cleaned = normalize_data(dataframe_news, news_list[i],category='news')
        dataframe_cleaned = pd.concat([dataframe_web_cleaned,
                                       dataframe_news_cleaned],
                                       axis=1)
        if i == 0:
            final = pd.concat([final,dataframe_cleaned.iloc[:,[0,2,5]]],axis=1)
        else:
            final = pd.concat([final,dataframe_cleaned.iloc[:,[2,5]]],axis=1)
        print("Finished {}".format(web_list[i]))
        
        time.sleep(5)
        
        #Save csv of data at each run
        final.to_csv("{}_save.csv".format(web_list[i]).replace(" ","_"))
    
    # Plot data
    plt.figure(figsize=(15,10))
    
    plt.subplot(211)
    for i in web_list:
        plt.plot(final["date"],final["{}_Normalized_Daily".format(i).replace(" ","_")])
    
    plt.legend()
    
    plt.subplot(212)
    for i in news_list:
        plt.plot(final["date"],final["{}_Normalized_Daily".format(i).replace(" ","_")])
        
    plt.legend()
    plt.show()

    final.to_csv("Final_Data.csv")





