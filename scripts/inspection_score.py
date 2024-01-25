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
HEADER_STATE = ['state', 'count', 'mean', 'std', 'min', '25%', '50%', '75%', 'max']


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

    #FIXME: remove "2005" outlier value
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

    #histograms of inspection scores with each individual year
    i = 0
    for yr in all_years:
        inspection_score_each = df.loc[df['inspection_year'] == yr]['INSPECTION_SCORE']
        plt.figure(f'{i}')
        sns.histplot(inspection_score_each)
        plt.title(f'inspection score in {yr}')
        plt.savefig(f'figures/dist_by_year/histagram/hist_{yr}.png')
        i += 1

    #inspection scores changed over years
    mean_years = pd.DataFrame()
    for yr in all_years:
        inspection_score_each = df.loc[df['inspection_year'] == yr]['INSPECTION_SCORE']
        another_year = pd.DataFrame({f'{yr}': [inspection_score_each.mean()]})
        mean_years = pd.concat([mean_years, another_year], axis=1)

    plt.figure(f'{i}')
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
    i += 1

    #inspection scores changed over years -- for each state
    i = 0
    for st in all_states:
        #select state-specific dataframe
        temp_df = df.loc[df['STATE_NAME.x'] == st]

        #ensure year data is available
        all_years_state = sorted(list(set(temp_df['inspection_year'].astype(np.int32))))

        #inspection scores changed over years
        mean_years = pd.DataFrame()
        for yr in all_years_state:
            
            inspection_score_each = temp_df.loc[temp_df['inspection_year'] == yr]['INSPECTION_SCORE']
            another_year = pd.DataFrame({f'{yr}': [inspection_score_each.mean()]})
            mean_years = pd.concat([mean_years, another_year], axis=1)

        #use different figures to avoid superimposing
        plt.figure(i)
        i += 1
        
        plt.plot(mean_years.columns.to_list(), mean_years.iloc[0, ], label="mean")
        plt.xlabel('time in years')
        plt.ylabel('inspection score')
        plt.title(f'inspection score over time in {st}')


        #inspection scores changed over years  -- median as reference 
        median_years = pd.DataFrame()
        for yr in all_years_state:
            inspection_score_each = temp_df.loc[temp_df['inspection_year'] == yr]['INSPECTION_SCORE']
            another_year = pd.DataFrame({f'{yr}': [inspection_score_each.median()]})
            median_years = pd.concat([median_years, another_year], axis=1)

        plt.plot(median_years.columns.to_list(), median_years.iloc[0, ], label="median")
        plt.legend()
        plt.savefig(f'figures/dist_by_year/line_plot/{st}.png')
        
