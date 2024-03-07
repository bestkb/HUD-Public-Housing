"""
Source code: 
Python notebook from 
https://gist.github.com/sachinsdate/5d353cce29284748f9a0143d2111b87d#file-fixed_effects_regression_model-py

"""

import pandas as pd
import scipy.stats as st
import statsmodels.api as sm
import statsmodels.formula.api as smf
from matplotlib import pyplot as plt
import seaborn as sns
from sklearn import preprocessing
from sklearn.preprocessing import OneHotEncoder
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

def retrive_region(state):
    for region in FEMA_MAP:
        states = FEMA_MAP[region]  
        for s in states:
            if s == state:
                return region
    return 0

#TODO: adapt it to our inspection score dataset
if __name__ == '__main__':
    
    """
    initialization.
    """
    fe_regions = ['region1', 'region2', 'region3', 'region4', 'region5', 
                    'region6', 'region7', 'region8', 'region9', 'region10']

    unit_col_name='FEREGION'
    time_period_col_name='YEAR'

    #Define the y and X variable names
    y_var_name = 'INSPECTION_SCORE'
    X_var_names = ['distance_to_floodzone']

    #Load the panel data set of World Bank published development indicators into a Pandas Dataframe
    #FIXME: ONLY a subset of the data
    df_panel = pd.read_csv('data/locations_inspectionscores_forMeri_Feb.csv').iloc[0:10000, :]


    """
    create and joia new column called FEMA_REGION
    """
    fema_region_col = []
    for i in range(len(df_panel)):
        fm_region = retrive_region(df_panel['STATE_NAME.x'][i])
        fema_region_col.append(fm_region)
    df_panel.insert(0, 'FEMA_REGION', fema_region_col)
    #print(df_panel.tail(10))

    """
    scatter plot
    """
    plot_against_X_index = 0
    sns.scatterplot(x=df_panel[X_var_names[plot_against_X_index]], y=df_panel[y_var_name], 
                    palette=fe_regions).set(title=
                'Distance to floodzone vs. housing inspection score')
    plt.show()


    """
    use OneHotEncoder for data preprocessing
    """
    #create the encoder and fit the value
    enc = OneHotEncoder()
    enc.fit(df_panel[['FEMA_REGION']])
    #encoded array
    one_hot = enc.transform(df_panel[['FEMA_REGION']]).toarray()
    #print(one_hot)
    #add the encoded array to the dataframe
    df_panel[['region1', 'region2', 'region3', 'region4', 'region5','region6', 'region7', 'region8', 'region9', 'region10']] = one_hot
    #print(df_panel.head(10))


    """
    Construct the fixed-effects regression model equation
    Note that we are leaving out one dummy variable
    so as to avoid perfect Multicollinearity between all dummy variables
    The regression model's intercept will hold the value of the coefficient for the omitted dummy variable.
    """
    #create the fixed-effects regression (FE regression) formula
    expr = y_var_name + ' ~ '
    i = 0
    for X_var_name in X_var_names:
        if i > 0:
            expr = expr + ' + ' + X_var_name
        else:
            expr = expr + X_var_name
        i += 1

    for dummy_name in fe_regions[0:5]:
        expr = expr + ' + ' + dummy_name
    for dummy_name in fe_regions[6:]:
        expr = expr + ' + ' + dummy_name
    
    #fit the model
    model = smf.ols(formula=expr, data=df_panel)
    model_results = model.fit()

    #get the results
    print('===============================================================================')
    print(model_results.summary())
    
    with open('figures/FE_regression/summary_distance_to_floodzone_6.txt', 'w') as file:
        file.write(str(model_results.summary()))
