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

#header for state
HEADER_REGION = ['region', 'count', 'mean', 'std', 'min', '25%', '50%', '75%', 'max']


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


if __name__ == '__main__':
    #read the dataset
    df = pd.read_csv('data/locations_inspectionscores_forMeri_Feb.csv')    
    
    #inspection score data
    inspection_score = df['INSPECTION_SCORE']
    
    #statistic CSV table -- by year
    stat_file = open('figures/stats_inspection_score_region.csv', 'w') 
    
    stat_writer = csv.writer(stat_file)
    
    stat_writer.writerow(HEADER_YEAR)

    #overall inspection score statistics
    stats = inspection_score.describe().round(NUM_DECIMAL).to_list()
    
    stats.insert(0, 'overall')
    
    stat_writer.writerow(stats)

    #get all available years and get them sorted
    all_years = sorted(list(set(df['inspection_year'].dropna().astype(np.int32))))

    all_years.remove(2005)

    #get all available states and get them sorted
    all_states = sorted(list(set(df['STATE_NAME.x'])))

    #statistic CSV table -- by state
    stat_file = open('figures/stats_inspection_score_region.csv', 'w') 
    
    stat_writer = csv.writer(stat_file)
    
    stat_writer.writerow(HEADER_REGION)

    #each-state inspection score statistics
    for num in range(1, 11):
        region = f'region{num}'
        states = FEMA_MAP[region]

        #select the whole fema region
        temp_df = pd.DataFrame()
        for st in states:
            temp_df = pd.concat([temp_df, df.loc[df['STATE_NAME.x'] == st]], axis=0)

        temp_df = temp_df.dropna()

        inspection_score_each = temp_df['INSPECTION_SCORE']

        region_each = inspection_score_each.describe().round(NUM_DECIMAL).to_list()
        
        region_each.insert(0, region)
        
        stat_writer.writerow(region_each)
    
    stat_file.close()