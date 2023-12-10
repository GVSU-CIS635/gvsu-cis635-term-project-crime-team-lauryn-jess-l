# imports
import pandas as pd
import os
import numpy as np

 
'''
takes in a directory per machine (self-provided)
defines our empty dataframe to populate with
loops through the file path, and once we have an xlsx file, we take it and merge with our df
returns our crime df 
'''
def fileReader(directory, isMAC = True):
    #Creating an empty dataframes
    crimeDataDF = pd.DataFrame(columns = ['CATEGORY','CALL GROUPS','final_case_type','CASE DESC', 'occ_date', 
                                    'x-coordinate', 'y_coordniate', 'census_tract'])
    
    # if we are working on a pc, chage the addtional file path / to \
    addDir = 'CIS635/'
    if isMAC == False:
        addDir = 'CIS635\\'

    # for each file, if it is an xlsx file, read and merge it
    for dirpath, dirnames, filenames in os.walk(directory + '/CIS635'):
        for filename in filenames:
            if filename.endswith('.xlsx'):
                temp_file = pd.read_excel(dirpath + '/' + filename)
                crimeDataDF = crimeDataDF.merge(temp_file, how = 'outer')

    # code adds in two addtional nan-columns - deleting those
    crimeDataDF.drop(columns=crimeDataDF.columns[0:2], inplace=True)
    return crimeDataDF

'''
used to test a small version of our data so that we can ensure 
syntax/low level bugs are fixed before compliling actual dataset
handels mac or pc
'''
def smallTesterFile(file_name = "", isMAC = True):
    if isMAC:
        fullYear2012 = pd.read_excel(file_name + r"CIS635/Full Year/2012fullyear.xlsx")
        fullYear2013 = pd.read_excel(file_name + r"CIS635/Full Year/2013fullyear.xlsx")
        testset = pd.merge(fullYear2012, fullYear2013, how = 'outer')
    else:
        fullYear2012 = pd.read_excel(file_name + r"CIS635\Full Year\2012fullyear.xlsx")
        fullYear2013 = pd.read_excel(file_name + r"CIS635\Full Year\2013fullyear.xlsx")
        testset = pd.merge(fullYear2012, fullYear2013, how = 'outer')

    return testset

# prints the nan's per column nicely
def nanColumns(df):
    new_df = df.copy()
    for column in new_df.columns:
        sum = new_df[column].isna().sum()
        print(column + " has " + str(sum) + " nans.")

        
