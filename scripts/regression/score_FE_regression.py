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


#TODO: adapt it to our inspection score dataset

colors_master = ['blue', 'red', 'orange', 'lime', 'yellow', 'cyan', 'violet', 'yellow',
                 'sandybrown', 'silver']

#Define the units (countries) of interest
unit_names = ['Belgium', 'CzechRepublic', 'France', 'Ireland', 'Portugal', 'UK', 'USA']
unit_names.sort()

colors = colors_master[:len(unit_names)]

unit_col_name='COUNTRY'
time_period_col_name='YEAR'

#Define the y and X variable names
y_var_name = 'GDP_PCAP_GWTH_PCNT'
X_var_names = ['GCF_GWTH_PCNT']

#Load the panel data set of World Bank published development indicators into a Pandas Dataframe
df_panel = pd.read_csv('wb_data_panel_2ind_7units_1992_2014.csv', header=0)

print(df_panel.corr())

plot_against_X_index=0

#Use Seaborn to plot GDP growth over all time periods and across all countries versus gross
# capital formation growth:
sns.scatterplot(x=df_panel[X_var_names[plot_against_X_index]], y=df_panel[y_var_name],
                hue=df_panel[unit_col_name], palette=colors).set(title=
                'Y-o-Y % Change in per-capita GDP versus Y-o-Y % Change in Gross capital formation')
plt.show()

#Print out the first 30 rows
print(df_panel.head(30))


#Create the dummy variables, one for each country
df_dummies = pd.get_dummies(df_panel[unit_col_name])

#Join the dummies Dataframe with the panel data set
df_panel_with_dummies = df_panel.join(df_dummies)

print(df_panel_with_dummies)


#Construct the regression equation. Note that we are leaving out one dummy variable so as to
# avoid perfect multi-colinearity between the 7 dummy variables. The regression model's intercept
# will contain the value of the coefficient for the omitted dummy variable.
lsdv_expr = y_var_name + ' ~ '
i = 0
for X_var_name in X_var_names:
    if i > 0:
        lsdv_expr = lsdv_expr + ' + ' + X_var_name
    else:
        lsdv_expr = lsdv_expr + X_var_name
    i = i + 1
for dummy_name in unit_names[:-1]:
    lsdv_expr = lsdv_expr + ' + ' + dummy_name

print('Regression expression for OLS with dummies=' + lsdv_expr)

lsdv_model = smf.ols(formula=lsdv_expr, data=df_panel_with_dummies)
lsdv_model_results = lsdv_model.fit()
print('===============================================================================')
print('============================== OLSR With Dummies ==============================')
print(lsdv_model_results.summary())
print('LSDV='+str(lsdv_model_results.ssr))

#Compare the goodness-of-fit of the LSDV FE model with that of the Pooled OLSR model

#First build and fit a Poole OLSR model on the panel data set so that we can access it's SSE
pooled_y=df_panel[y_var_name]
pooled_X=df_panel[X_var_names]
pooled_X = sm.add_constant(pooled_X)
pooled_olsr_model = sm.OLS(endog=pooled_y, exog=pooled_X)
pooled_olsr_model_results = pooled_olsr_model.fit()

#Setup the variables for calculating the F-test

#n=number of groups
n=len(unit_names)

#T=number of time periods per unit
T=df_panel.shape[0]/n

#N=total number of rows in the panel data set
N=n*T

#k=number of regression variables of the Pooled OLS model
k=len(X_var_names)+1

#Get the Residual Sum of Squares for the Pooled OLS model
ssr_restricted_model = pooled_olsr_model_results.ssr

#Get the Residual Sum of Squares for the Fixed Effects model
ssr_unrestricted_model = lsdv_model_results.ssr

#Get the degrees of freedom of the Pooled OLSR model
k1 = len(pooled_olsr_model_results.params)


#Get the degrees of freedom of the Fixed Effects model
k2 = len(lsdv_model_results.params)

#Calculate the F statistic
f_statistic = ((ssr_restricted_model - ssr_unrestricted_model)/ssr_unrestricted_model)*((N-k2)/(
        k2-k1))
print('F-statistic for FE model='+str(f_statistic))

#Calculate the critical value at alpha=.05
alpha=0.05
f_critical_value=st.f.ppf((1.0-alpha), (k2-k1), (N-k2))
print('F test critical value at alpha=0.05='+str(f_critical_value))