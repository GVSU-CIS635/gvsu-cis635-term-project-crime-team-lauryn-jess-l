# imports
import Macxlsxdata as mac
import pandas as pd
import geoData 

#file_name = "/Users/lauryndavis/"
file_name = "/Users/Ldettling/Documents/"


def mainRun(runType = '', readGeo = False, isMAC = True):
    if runType == 'Test':
        testdf = mac.smallTesterFile(file_name, isMAC)
        print(testdf)
        mac.nanColumns(testdf)
    elif runType == 'Main':
        crime_data = mac.fileReader(file_name, isMAC)
        print(crime_data)

    if readGeo:
        crimeData_geo = geoData.shpFile_brute_MAC(file_name, isMAC)
        #crimeData_geo = geoData.shpFileReader(file_name)
        print(crimeData_geo)


'''
runs a main function, has 3 params
string for the type of run: no run, Test, or Main
readGeo - whether you would like to read in geospatial data
isMAC - True if you are using a MAC, false is you are using pc. for file reading, defaults to True
'''
mainRun('Test', readGeo=True, isMAC=True)

#testing a new commit here!
