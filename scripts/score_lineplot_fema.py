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

    #inspection scores changed over years
    mean_years = pd.DataFrame()
    for yr in all_years:
        inspection_score_each = df.loc[df['inspection_year'] == yr]['INSPECTION_SCORE']
        another_year = pd.DataFrame({f'{yr}': [inspection_score_each.mean()]})
        mean_years = pd.concat([mean_years, another_year], axis=1)

    plt.figure(f'{figure_number}')
    plt.plot(mean_years.columns.to_list(), mean_years.iloc[0, ], label="mean")
    plt.xlabel('time in years')
    plt.ylabel('inspection score')
    plt.title('inspection score over time in U.S.')

    #inspection scores changed over years  -- median as reference 
    median_years = pd.DataFrame()
    for yr in all_years:
        inspection_score_each = df.loc[df['inspection_year'] == yr]['INSPECTION_SCORE']
        another_year = pd.DataFrame({f'{yr}': [inspection_score_each.median()]})
        median_years = pd.concat([median_years, another_year], axis=1)

    plt.plot(median_years.columns.to_list(), median_years.iloc[0, ], label="median")
    plt.legend()
    plt.savefig('figures/dist_by_year/line_plot/US.png')

    #update figure number from 0 to 1
    figure_number += 1

    for num in range(1, 11):
        region = f'region{num}'
        states = FEMA_MAP[region]
        
        #select the whole fema region
        temp_df = pd.DataFrame()
        for st in states:
            temp_df = pd.concat([temp_df, df.loc[df['STATE_NAME.x'] == st]], axis=0)

        #ensure year data is available
        all_years_state = sorted(list(set(temp_df['inspection_year'].astype(np.int32))))
        
        #inspection scores changed over years
        mean_years = pd.DataFrame()
        for yr in all_years_state:
            inspection_score_each = temp_df.loc[temp_df['inspection_year'] == yr]['INSPECTION_SCORE']
            another_year = pd.DataFrame({f'{yr}': [inspection_score_each.mean()]})
            mean_years = pd.concat([mean_years, another_year], axis=1)

        #use different figures to avoid superimposing
        plt.figure(f'{figure_number}')
        figure_number += 1
        
        plt.plot(mean_years.columns.to_list(), mean_years.iloc[0, ], label="mean")
        plt.xlabel('time in years')
        plt.ylabel('inspection score')
        plt.title(f'inspection score over time in FEMA {region}')

        #inspection scores changed over years  -- median as reference 
        median_years = pd.DataFrame()
        for yr in all_years_state:
            inspection_score_each = temp_df.loc[temp_df['inspection_year'] == yr]['INSPECTION_SCORE']
            another_year = pd.DataFrame({f'{yr}': [inspection_score_each.median()]})
            median_years = pd.concat([median_years, another_year], axis=1)

        plt.plot(median_years.columns.to_list(), median_years.iloc[0, ], label="median")
        plt.legend()
        #footnote_text = f'{region} {[s for s in FEMA_MAP[region]]}'
        #plt.annotate(footnote_text, (0.5, -0.1), xycoords='axes fraction', ha='right', fontsize=8, color='gray')
        plt.savefig(f'figures/dist_by_year/line_plot/FEMA{region}.png')
