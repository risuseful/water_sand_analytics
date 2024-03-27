# -*- coding: utf-8 -*-
"""
Created on Wed Mar 27 09:05:56 2024

@author: akmalaulia
"""




# ---------------------------------------
#
# Library calls
#
# ---------------------------------------
import pandas as pd
from prophet import Prophet
import pyodbc
import matplotlib
import matplotlib.pyplot as plt
from matplotlib.backends.backend_pdf import PdfPages
from matplotlib.pyplot import figure
import numpy as np

# median filter parameter
window_size = 5

# min data size
min_data = 10

# forecast mode
mode = 'logistic' # option: linear, logistic

# max water capacity handling
max_wat = 40000


# ---------------------------------------
#
# Pull CKP water rate data from SQL Server
#
# ---------------------------------------

# build query
query = "SELECT cast([DATE_STAMP] as Date) as ds, [TotalWater] as y "
query = query + "FROM [DataMining].[dbo].[vTopSide] "
query = query + "where DATE_STAMP > '2020-06-01' and DATE_STAMP < '2022-12-01' " 

# execute query
conn = pyodbc.connect(driver='SQL Server', server='xxxx', user='xxxx', password='xxxx', database='xxxx')
cursor = conn.cursor()
df = pd.read_sql_query(query, conn)
df['ds'] = pd.to_datetime(df['ds'], format="%Y-%m-%d") # format the date



# ---------------------------------------
#
# Execute prediction using Facebook's library
#
# ---------------------------------------

# define cap
if mode=='logistic':
    # df['cap'] = max(df['y']) # predicted value must not exceed max of historical value
    df['cap'] = max_wat
    
# apply median filter
#y_ori = df['y'] # currently, just for bookkeeping
#df['y']= df['y'].rolling(window_size).median()
df = df.dropna() # remove rows with NaN values
    
# construct and fit model
if mode=='logistic':
    m = Prophet(growth='logistic')
else:
    m = Prophet() # i.e. linear growth
m.fit(df)

# make dates for model to predict
future = m.make_future_dataframe(periods=2000)

if mode=='logistic':
    # future['cap'] = max(df['cap']) # predicted value must not exceed max of historical value
    future['cap'] = max_wat


# do prediction
forecast = m.predict(future)
#forecast[['ds', 'yhat', 'yhat_lower', 'yhat_upper']].tail()

# store matrix
x = forecast[['ds', 'trend']]
x.columns = ['ds', 'y_pred'] # give proper column names

# merge data
df = df.merge(x, how="inner", on='ds')





# ---------------------------------------
#
# Plotting
#
# ---------------------------------------
plt.plot(df['ds'], df['y'], 'r--', df['ds'], df['y_pred'], 'b.',)

fig1 = m.plot(forecast)
fig2 = m.plot_components(forecast)
