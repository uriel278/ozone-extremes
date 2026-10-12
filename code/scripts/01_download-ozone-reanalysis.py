import cdsapi
import os
import time
#The dataset to fetch
dataset = "cams-europe-air-quality-reanalyses"

#The path of the data folder
data_path = os.path.join(os.getcwd(), 'data', 'ozone')

#The available years and months (all months of the year)
years = [str(i) for i in range(2013, 2023)]
months = ['0'+str(j) if j<10 else str(j) for j in range(1, 13)]

#Start a new client for the request
#client = cdsapi.Client()
for y in years:
    #Make sure the path for the year exists, otherwise create it
    year_path = os.path.join(data_path, y)
    os.makedirs(year_path, exist_ok=True)
    for m in months:
        #Start a new client for the request
        client = cdsapi.Client()
        request = {
            "variable": ["ozone"],
            "model": ["ensemble"],
            "level": ["0"],
            "type": ["validated_reanalysis"],
            "year": [y],
            "month": [m]
        }
        client.retrieve(dataset, request).download(os.path.join(year_path, y+'_'+m+'.zip'))
        time.sleep(0.5)
