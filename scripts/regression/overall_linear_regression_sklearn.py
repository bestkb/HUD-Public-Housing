import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
import sklearn
from sklearn.linear_model import LinearRegression


if __name__ == "__main__":
    df1 = pd.read_csv('data/locations_inspectionscores_w_tracts.csv')
    df2 = pd.read_csv('data/raw_tract_data.csv')
    df_merged = pd.merge(df1, df2, on=['GEOID','inspection_year'], how='inner')

    #linear regression:
    #regressors:
    #distance to floodzone, age, percent white, income, renter
    df_merged["white_percent"] = df_merged["total_white"] / df_merged["total_pop"]
    
    #set up the regressors and target
    final_df = df_merged[["distance_to_floodzone", "age", "white_percent", "income", "renter", "INSPECTION_SCORE"]]
   
    #data cleaning
    final_df = final_df.dropna()

    X = final_df.iloc[: , 0:5]
    y= final_df.iloc[:, 5]
    
    model =LinearRegression()
    model.fit(X, y)

    #save 4 decimal digits for coefficients and intercepts
    coef = list(map(lambda x: f'{x:.4e}', model.coef_))
    intercept = round(model.intercept_, 4)

    #format the equation
    equa_file = open('figures/linear_regression/sklearn_equation.txt','w')
    equation = "INSPECTION_SCORE = "
    i = 0
    for feature in X.columns:
        equation += f"{coef[i]}*{feature} + "
        i += 1
    equation += f"{str(intercept)}\n"
    equa_file.write("overall inspection score:\n")
    equa_file.write(equation)
    equa_file.close()

    #visualization: correlation matrix
    sns.heatmap(final_df.corr(), cmap='Blues', annot=True)
    plt.title('correlation matrix')
    plt.show()