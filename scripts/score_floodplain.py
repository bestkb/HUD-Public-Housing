import pandas as pd
import numpy as np
import seaborn as sns
import plotly.express 
import matplotlib.pyplot as plt
import csv
import os

#number of decimals to keep
NUM_DECIMAL = 2

#header for year
HEADER_YEAR = ['year', 'count', 'mean', 'std', 'min', '25%', '50%', '75%', 'max']


#fema region
FEMA_MAP = {
    #Connecticut | Maine | Massachusetts | New Hampshire | Rhode Island | Vermont
    'region1': ['CT', 'ME', 'MA', 'NH', 'RI', 'VT'], 
    #New Jersey | New York | Puerto Rico | Virgin Islands
    'region2': ['NJ', 'NY', 'PR', 'VI'],
    #Delaware | Maryland | Pennsylvania | Virginia | District of Columbia | West Virginia
    'region3': ['DE', 'MD', 'PA', 'VA', 'DC', 'WV'],
    #Alabama | Florida | Georgia | Kentucky |  Mississippi | North Carolina | South Carolina | Tennessee
    'region4': ['AL', 'FL', 'GA', 'KY', 'MS', 'NC', 'SC', 'TN'], 
    #Illinois | Indiana | Michigan | Minnesota | Ohio | Wisconsin
    'region5': ['IL', 'IN', 'MI', 'MN','OH', 'WI'],
    #Arkansas | Louisiana | New Mexico | Oklahoma | Texas
    'region6': ['AR', 'LA', 'NM', 'OK', 'TX'],
    #Iowa | Kansas | Missouri | Nebraska
    'region7': ['IA', 'KS', 'MO', 'NE'],
    #Colorado | Montana | North Dakota | South Dakota | Utah | Wyoming
    'region8': ['CO', 'MT', 'ND', 'SD', 'UT','WY'],
    #Arizona | California | Hawaii | Nevada | Guam | American Samoa | Commonwealth of Northern Mariana Islands | Republic of Marshall Islands | Federated States of Micronesia
    'region9': ['AZ', 'CA', 'HI', 'NV', 'GU', 'AS', 'MP'],
    #Alaska | Idaho | Oregon | Washington
    'region10':['AK', 'ID', 'OR', 'WA']
}


#header for state
HEADER_STATE = ['state', 'count', 'mean', 'std', 'min', '25%', '50%', '75%', 'max']


if __name__ == '__main__':
    #read the dataset
    df = pd.read_csv('data/locations_inspectionscores_forMeri_Feb.csv')

    
    #box plot -- overall
    floodplain_data_df = df.loc[df['in_floodplain'] == 1.0]
    floodplain_data = list(floodplain_data_df['INSPECTION_SCORE'])
    non_floodplain_data_df = df.loc[df['in_floodplain'] == 0.0]
    non_floodplain_data = list(non_floodplain_data_df['INSPECTION_SCORE']) 
    data = [floodplain_data, non_floodplain_data]
    sns.boxplot(data)
    
    plt.title('Is in floodplain or not vs. Inspection Score in U.S')
    plt.xlabel('Is in floodplain or not')
    plt.ylabel('Inspection score')
    plt.xticks([0, 1], ['in floodplain', 'not in floodplain'])
    plt.savefig('figures/correlation/floodplain/score_vs_floodplain_overall.png')

    #box plot -- fema region
    #update figure number from 0 to 1
    figure_number = 1

    for num in range(1, 11):
        region = f'region{num}'
        states = FEMA_MAP[region]
        
        #select the whole fema region
        temp_df = pd.DataFrame()
        for st in states:
            temp_df = pd.concat([temp_df, df.loc[df['STATE_NAME.x'] == st]], axis=0)
            
        temp_df = temp_df.dropna()

        floodplain_data_df = temp_df.loc[temp_df['in_floodplain'] == 1.0]
        floodplain_data = list(floodplain_data_df['INSPECTION_SCORE'])
        non_floodplain_data_df = temp_df.loc[temp_df['in_floodplain'] == 0.0]
        non_floodplain_data = list(non_floodplain_data_df['INSPECTION_SCORE']) 
        
        region_data = [floodplain_data, non_floodplain_data]
        print(region_data[0])

        

        #use different figures to avoid superimposing
        plt.figure(f'{figure_number}')
        figure_number += 1
        sns.boxplot(region_data)
        plt.title(f'Is in floodplain or not vs. Inspection Score in FEMA{region}')
        plt.xlabel('Is in floodplain or not')
        plt.ylabel('Inspection score')
        plt.xticks([0, 1], ['in floodplain', 'not in floodplain'])
        
        #save the plot
        plt.savefig(f'figures/correlation/floodplain/score_vs_floodplain_FEMA{region}.png')