#This script will do two things:
#Filter the data for the UK only
#Compute the daily, weekly, and monthly maximum

#Load required libraries
import numpy as np
import pandas as pd
import geopandas as geopd
import shapely as shply
import matplotlib.pyplot as plt
import scipy as sp
import xarray as xr
import netCDF4
import os
import zipfile
import re
from datetime import datetime


#Define the path where the all the data is stored
data_path = os.path.join(os.getcwd(), 'data', 'ozone')
#Look for the available years
years = os.listdir(data_path)
#years
#For each year, look for the available months
m_re = re.compile(r'\d*[_]\d*.zip')#Regular expression to filter non-desired files
#months = os.listdir(os.path.join(data_path, years[0]))
#months

#Read the UK shapefile to filter the data
uk_gdf = geopd.read_file(os.path.join(os.getcwd(), 'data', 'uk.shp'))
#uk_gdf.plot()
#plt.show()

#Function to take a datetime.date object and extract the week
def get_week(x):
    return(x.isocalendar().week)

#Function to take a datetime.date object and extract the month
def get_month(x):
    return(x.month)

#Function to take a datetime.date object and extract the year
def get_year(x):
    return(x.year)

for y in years:
    #Keep the filenames of zips associated to monthly data
    months = [month for month in os.listdir(os.path.join(data_path, y)) if m_re.search(month)]
    for m in months:
        with zipfile.ZipFile(os.path.join(data_path, y, m), 'r') as zf:
            file_name = zf.namelist()[0] #Read the file inside the zip
            #Define the path to extract the file
            ncpath = os.path.join(data_path, y, file_name)
            zf.extract(file_name, ncpath) #Unzip the file
            ozone = xr.open_dataset(os.path.join(ncpath, file_name), engine='netcdf4') #Read the nc file
            daily_maxima = ozone.groupby('time.date').max().to_dataframe().reset_index() #Compute daily maxima and convert to df
            #Convert to GeoDataFrame to allow for filtering using geographic locations
            daily_maxima_gdf = geopd.GeoDataFrame(data = daily_maxima[["date", "o3"]], 
                                            geometry=shply.points(daily_maxima[["lon", "lat"]]),
                                            crs=uk_gdf.crs)
            #Filter the elements of the grid that lie inside the UK
            daily_maxima_gdf = daily_maxima_gdf.sjoin(uk_gdf, how='inner', predicate='within')[['o3', 'date', 'geometry']].reset_index()
            #Add week information using data (Note that this is done after filtering the data to avoid using too much memory)
            daily_maxima_gdf['week'], daily_maxima_gdf['month'], daily_maxima_gdf['year'] = daily_maxima_gdf['date'].map(get_week), daily_maxima_gdf['date'].map(get_month) , daily_maxima_gdf['date'].map(get_year)
            #Compute weekly maxima for each year
            weekly_id = daily_maxima_gdf.groupby(["year", "week"])["o3"].agg(np.argmax)
            weekly_maxima_gdf = daily_maxima_gdf.loc[weekly_id]
            #Compute monthly maxima for each year
            monthly_id = daily_maxima_gdf.groupby(["year", "month"])["o3"].agg(np.argmax)
            monthly_maxima_gdf = daily_maxima_gdf.loc[monthly_id]
            #------We need to create folder for each shape file because it will output several files, not only the shp
            daily_folder = os.path.join(data_path, 'daily', y + '-' + m[5:7])
            weekly_folder = os.path.join(data_path, 'weekly', y + '-' + m[5:7])
            monthly_folder = os.path.join(data_path, 'monthly', y + '-' + m[5:7])
            os.makedirs(daily_folder, exist_ok=True)
            os.makedirs(weekly_folder, exist_ok=True)
            os.makedirs(monthly_folder, exist_ok=True)
            #Write a shape file to a folder that contains the information
            daily_maxima_gdf.to_file(filename=os.path.join(daily_folder, y + '-' + m[5:7] + '.shp'))
            weekly_maxima_gdf.to_file(filename=os.path.join(weekly_folder, y + '-' + m[5:7] + '.shp'))
            monthly_maxima_gdf.to_file(filename=os.path.join(monthly_folder, y + '-' + m[5:7] + '.shp'))


#daily_maxima_gdf.plot()
#plt.show()



#This is how we can extract the coordinates of each cell in the netcdf file
#pts = shply.points(ozone.drop_dims('time').coords.to_dataset().to_dataframe().reset_index())
#daily_maxima = daily_maxima.to_dataframe().reset_index()

#Here we create a geodataframe (similar to sfc)
#geo_pts = geopd.GeoDataFrame(geometry=pts, crs=uk_gdf.crs)
#geo_pts.plot()
#plt.show()

#Filtering the points
#uk_pts = geo_pts.sjoin(uk_gdf, how='inner', predicate='within')
#uk_pts.plot()
#plt.show()

#ozone.to_dataframe()

#Maybe it is more convenient to save the daily maxima with date in isodate format too.
#This might make easier to do data processing in R

#--------------------------
#I need to unzip the file 
#Note that for each month, there will be a single file (apparently)
#with zipfile.ZipFile(os.path.join(data_path, years[0], months[9]), 'r') as zf:
#    file_name = zf.namelist()[0] #Read the file inside the zip
#    #Define the path to extract the file
#    ncpath = os.path.join(data_path, years[0], file_name)
#    zf.extract(file_name, ncpath) #Unzip the file
#    ozone = xr.open_dataset(os.path.join(ncpath, file_name), engine='netcdf4') #Read the nc file
#    daily_maxima = ozone.groupby('time.date').max().to_dataframe().reset_index() #Compute daily maxima and convert to df
#    #Convert to GeoDataFrame to allow for filtering using geographic locations
#    daily_maxima_gdf = geopd.GeoDataFrame(data = daily_maxima[["date", "o3"]], 
#                                      geometry=shply.points(daily_maxima[["lon", "lat"]]),
#                                      crs=uk_gdf.crs)
#    #Filter the elements of the grid that lie inside the UK
#    daily_maxima_gdf = daily_maxima_gdf.sjoin(uk_gdf, how='inner', predicate='within')[['o3', 'geometry']]
#    #Write a shape file that contains the points
#    daily_maxima_gdf.to_file(filename=os.path.join(data_path, years[0],years[0]+'-'+months[9][5:7]+'.shp'))