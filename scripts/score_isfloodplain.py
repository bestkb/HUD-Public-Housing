import pandas as pd
import numpy as np
import seaborn as sns
import plotly.express 
import matplotlib.pyplot as plt
import csv
import os
import math

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


def in_region(df, region_list):
    for region in region_list:
        if df == region:
            return True
    return False


if __name__ == '__main__':
    #read the dataset
    df = pd.read_csv('data/locations_inspectionscores_forMeri_Nov.csv')    
    
    #inspection score data
    inspection_score = df['INSPECTION_SCORE']
    
    #statistic CSV table -- by year
    stat_file = open('figures/stats_inspection_score_year.csv', 'w') 
    
    stat_writer = csv.writer(stat_file)
    
    stat_writer.writerow(HEADER_YEAR)

    #overall inspection score statistics
    stats = inspection_score.describe().round(NUM_DECIMAL).to_list()
    
    stats.insert(0, 'overall')
    
    stat_writer.writerow(stats)

    #get all available years and get them sorted
    all_years = sorted(list(set(df['inspection_year'].astype(np.int32))))

    all_years.remove(2005); 

    #each-year inspection score statistics
    for yr in all_years:
        inspection_score_each = df.loc[df['inspection_year'] == yr]['INSPECTION_SCORE']
        
        stats_each = inspection_score_each.describe().round(NUM_DECIMAL).to_list()
        
        stats_each.insert(0, yr)
        
        stat_writer.writerow(stats_each)
    
    #close the file
    stat_file.close()

    #get all available states and get them sorted
    all_states = sorted(list(set(df['STATE_NAME.x'])))

    #statistic CSV table -- by state
    stat_file = open('figures/stats_inspection_score_state.csv', 'w') 
    
    stat_writer = csv.writer(stat_file)
    
    stat_writer.writerow(HEADER_STATE)

    #each-state inspection score statistics
    for st in all_states:
        inspection_score_each = df.loc[df['STATE_NAME.x'] == st]['INSPECTION_SCORE']

        stats_each = inspection_score_each.describe().round(NUM_DECIMAL).to_list()
        
        stats_each.insert(0, st)
        
        stat_writer.writerow(stats_each)
    
    stat_file.close()

    #single point control of figure number
    figure_number = 0

    #inspection scores changed over years -- in_floodplain
    score_vs_floodplain = open('figures/correlation/floodplain/score_vs_floodplain_overall.csv', 'w') 
    writer = csv.writer(score_vs_floodplain)
    writer.writerow(['year', 'infloodplain', 'NOTinfloodplain'])
    for yr in all_years:
        
        #extract data
        year_df = df.loc[(df['inspection_year'] == yr)]
        inspection_score_flood = round(year_df.loc[year_df['in_floodplain'] == 1]['INSPECTION_SCORE'].mean(), NUM_DECIMAL)
        inspection_score_nonflood = round(year_df.loc[year_df['in_floodplain'] == 0]['INSPECTION_SCORE'].mean(), NUM_DECIMAL)
        
        #drop NAN data
        if math.isnan(inspection_score_flood)  or math.isnan(inspection_score_nonflood):
            continue
        
        #write in the score
        writer.writerow([yr, inspection_score_flood, inspection_score_nonflood])
    
    score_vs_floodplain.close()

    for num in range(1, 11):    
        region = f'region{num}'
        states = FEMA_MAP[region]
        score_vs_floodplain = open(f'figures/correlation/floodplain/score_vs_floodplain_FEMA{region}.csv', 'w') 
        writer = csv.writer(score_vs_floodplain)
        writer.writerow(['year', 'infloodplain', 'NOTinfloodplain'])

        #select the whole fema region
        temp_df = pd.DataFrame()
        for st in states:
            temp_df = pd.concat([temp_df, df.loc[df['STATE_NAME.x'] == st]], axis=0)

        #ensure year data is available
        all_years_state = sorted(list(set(temp_df['inspection_year'].astype(np.int32))))

        for yr in all_years_state:
            #extract data
            year_df = temp_df.loc[temp_df['inspection_year'] == yr]
            inspection_score_flood = round(year_df.loc[year_df['in_floodplain'] == 1]['INSPECTION_SCORE'].mean(), NUM_DECIMAL)
            inspection_score_nonflood = round(year_df.loc[year_df['in_floodplain'] == 0]['INSPECTION_SCORE'].mean(), NUM_DECIMAL)
        
            #drop NAN data
            if math.isnan(inspection_score_flood)  or math.isnan(inspection_score_nonflood):
                continue
        
            #write in the score
            writer.writerow([yr, inspection_score_flood, inspection_score_nonflood])

        score_vs_floodplain.close()