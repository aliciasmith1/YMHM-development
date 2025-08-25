#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Sep 24 15:30:46 2024

@author: aliciasmith
"""
# Import the necessary packages
    
import matplotlib.pyplot as plt
import pandas as pd
import os
import pingouin as pg

from facsimile.eval import FACSIMILEOptimiser
from facsimile.plotting import plot_predictions
from facsimile.utils import (
    check_directories,
    set_style,
    train_validation_test_split,
)

# Set matplotlib dpi
plt.rcParams["figure.dpi"] = 100


# Load data
items = pd.read_csv('full_data_reduced.csv') # This file has 'duplicate' items removed
factor_scores = pd.read_csv('facscores.csv')



# Split data into test and training sets

X_train, X_val, X_test, y_train, y_val, y_test = train_validation_test_split(
    items.iloc[:, 1:],  # Drop the first column, which is ID
    factor_scores.iloc[:, 1:],  # Drop the first column, which is just IDs
    train_size=0.6,
    test_size=0.2,
    val_size=0.2,
)
    

## Run the optimiser
# Initialise the optimiser
optimiser = FACSIMILEOptimiser(n_iter=100, n_jobs=10)

# Fit
optimiser.fit(
    X_train, y_train, X_val, y_val, target_names=["F1", "F2", "F3", "F4"]
)

# View results
optimiser.results_.head()

# Plot
optimiser.plot_results(figsize=(6, 4))




## Choose classifier
# Get the best classifier
best_clf = optimiser.get_best_classifier_max_items(36, metric='min_r2')

# Fit
best_clf.fit(X_train, y_train)

# Print number of included items
print("Number of included items: {}".format(best_clf.included_items.sum()))




## Predict factor scores using the predict method
# Get predictions
y_pred = best_clf.predict(X_test)

# Plot
plot_predictions(y_test, y_pred, ["F1", "F2", "F3", "F4"], scale=1)




# Alternatively plot using a dataframe with only the selected items (predict_reduced method)
# Select only included items
X_reduced = X_test[
    [i for i in X_test.columns if i in best_clf.included_item_names]
]

# Get predictions
y_pred = best_clf.predict_reduced(X_reduced)

# Plot
plot_predictions(y_test, y_pred, ["F1", "F2", "F3", "F4"], scale=1)



# Weights saved and used to predict factor scores
# Plot weights
best_clf.plot_weights()

# Output the weights
weights = best_clf.get_weights()
weights.to_csv("factor_weights.csv")

# Get predictions (note that we have to use the values from the dataframe here)
y_pred = X_reduced @ weights.values[:-1] + weights.values[-1] # only 66 subjects' data here as only analysing test set




