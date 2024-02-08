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
    df = pd.read_csv('data/locations_inspectionscores_forMeri_Nov.csv')

    #only extract the middle 50 percent of the data based on distance_to_floodzone
    per_25 = list(df['distance_to_floodzone'].describe())[4]
    per_75 = list(df['distance_to_floodzone'].describe())[6]
    df = df.loc[df['distance_to_floodzone'] > per_25]
    df = df.loc[df['distance_to_floodzone'] < per_75]

    distance_data = df['distance_to_floodzone']
    score_data = df['INSPECTION_SCORE']

    #Perform linear regression using numpy
    slope, intercept = np.polyfit(distance_data, score_data, 1)

    #scatter plot -- overall
    plt.scatter(x=distance_data, y=score_data, s=1)
    
    #Plot regression line
    plt.plot(distance_data, slope*np.array(distance_data) + intercept, color='red', label='Regression Line')
    
    plt.title('Distance to Floodzone vs. Inspection Score in U.S')
    plt.xlabel('Distance to floodzone')
    plt.ylabel('Inspection score')
    plt.savefig('figures/correlation/distance/score_vs_distance_overall.png')

    #scatter plot -- fema region
    #update figure number from 0 to 1
    figure_number = 1

    for num in range(1, 11):
        region = f'region{num}'
        states = FEMA_MAP[region]
        
        #select the whole fema region
        temp_df = pd.DataFrame()
        for st in states:
            temp_df = pd.concat([temp_df, df.loc[df['STATE_NAME.x'] == st]], axis=0)

        distance_data = temp_df['distance_to_floodzone']
        score_data = temp_df['INSPECTION_SCORE']

        #use different figures to avoid superimposing
        plt.figure(f'{figure_number}')
        figure_number += 1
        
        #Perform linear regression using numpy
        slope, intercept = np.polyfit(distance_data, score_data, 1)

        #scatter plot -- overall
        plt.scatter(x=distance_data, y=score_data, s=1)

        #Plot regression line
        plt.plot(distance_data, slope*np.array(distance_data) + intercept, color='red', label='Regression Line')

        plt.title(f'Distance to Floodzone vs. Inspection Score in FEMA {region}')
        plt.xlabel('Distance to floodzone')
        plt.ylabel('Inspection score')
        plt.savefig(f'figures/correlation/distance/score_vs_distance_FEMA{region}.png')