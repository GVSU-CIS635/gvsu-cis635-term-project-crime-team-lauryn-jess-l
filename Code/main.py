# import
import Macxlsxdata as mac
import pandas as pd
import geoData 

def mainRun(runType = '', file_name = '', readGeo = False, isMAC = True):
    if runType == 'Test':
        testdf = mac.smallTesterFile(file_name, isMAC)
        print(testdf)
        mac.nanColumns(testdf)
    elif runType == 'Main':
        crime_data = mac.fileReader(file_name, isMAC)
        print(crime_data)

    if readGeo:
        crimeData_geo = geoData.shpFile_brute_MAC(file_name, isMAC)
        crimeData_geo = geoData.cleaningGEO(crimeData_geo)
        #crimeData_geo = geoData.shpFileReader(file_name)
        print(crimeData_geo)
        #geoData.plotGeo(crimeData_geo)


'''
runs a main function, has 4 params
runTyp - no run (any string besides test and Main), Test, or Main
file_name - describes the file path. 
readGeo - whether you would like to read in geospatial data
isMAC - True if you are using a MAC, false is you are using pc. for file reading, defaults to True
'''
#file_name = "/Users/lauryndavis/"
file_name = "/Users/Ldettling/Documents/"

print("STARTING")
mainRun('no run', file_name, readGeo=True, isMAC=True)
print("DONE")
