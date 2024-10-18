import pandas as pd
import statsmodels.api as sm
from sklearn.preprocessing import StandardScaler

if __name__ == "__main__":
    # Load the data
    df = pd.read_csv('data/inspection_scores_w_flood_blocktract.csv')
    
    # Create the necessary percentage columns
    df["white_percent_b"] = df["total_white_b"] / df["total_pop_b"]
    df["renter_percent_b"] = df["owner_b"] / df["total_pop_b"] 
    
    # Set up the regressors and target
    final_df = df[["FloodDistM", "in_floodplain", "age_b", "white_percent_b", "income_b", "renter_percent_b", "public_assistance_b","INSPECTION_SCORE"]]
    
    # Data cleaning: Drop rows with any NaN values
    final_df = final_df.dropna()

    # Separating features and target variable
    X = final_df.iloc[:, :7]  # Select all columns except the target
    y = final_df["INSPECTION_SCORE"]

    # Z-normalization
    scaler = StandardScaler()
    X_scaled = scaler.fit_transform(X)

    # Create a DataFrame for scaled features with variable names
    X_scaled_df = pd.DataFrame(X_scaled, columns=X.columns, index=X.index)  # Preserve the index
    
    # Add intercept to the scaled features
    X_with_intercept = sm.add_constant(X_scaled_df)

    # Fit the OLS model
    model = sm.OLS(y, X_with_intercept)
    results = model.fit()

    # Save the results to a text file
    with open('figures/linear_regression/BLOCK_summarytable.txt', 'w') as equa_file:
        equa_file.write("Overall inspection score (BLOCK data):\n")
        equa_file.write(str(results.summary()))
