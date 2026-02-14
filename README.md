# Spotify Song Popularity Analysis

## Overview

An analysis of what drives song popularity on Spotify, comparing multiple regression approaches including OLS, Box-Cox transformed OLS, and a quasi-Poisson GLM. Models are evaluated for both inferential and predictive performance using ANOVA, residual diagnostics, and cross-validation.

## Data

The dataset contains Spotify track-level data with audio features (tempo, loudness, energy, duration), metadata (genre, mode), and artist metrics. The response variable is a song's popularity score (0–100).

Link: https://www.kaggle.com/datasets/bwandowando/spotify-songs-with-attributes-and-lyrics<img width="661" height="35" alt="image" src="https://github.com/user-attachments/assets/9f0e8016-c130-4869-a02b-1cd042d61a2a" />


## Methods

**Exploratory Data Analysis**: Summary statistics, scatterplots of audio features vs. popularity, and a correlation matrix of predictor variables.

**OLS Model 1**: Baseline model using tempo, energy, loudness, and duration. Compared to the full model via ANOVA.

**OLS Model 2**: Adds average artist popularity, mode, and genre. Residual diagnostics via studentized deleted residuals, QQ plots, skewness/kurtosis. Box-Cox transformation applied to identify optimal power transformation.

**Fourth-Root Model**: Power transformation (λ ≈ 0.25) suggested by Box-Cox. Improved predictive accuracy relative to untransformed OLS.

**Quasi-Poisson GLM**: GLM to better reflect the bounded, right-skewed nature of popularity scores. Pseudo R-Squared via deviance ratio, coefficients interpreted via exponentiation, Cook's distance for influential observations.

**Model Comparison via Cross Validation**: 5-fold CV with MAE on the original popularity scale across all three model families. Fourth-root predictions back-transformed to ensure comparability.

## Key Findings

- Average artist popularity is the dominant predictor across all specifications
- The fourth-root model achieved the lowest cross validated MAE (~5.0), outperforming both untransformed OLS (~13.0) and quasi-Poisson (~13.0)
- While the fourth-root model is the strongest predictor, the quasi-Poisson GLM is best suited for inference due to its alignment with the data generating process and interpretable coefficients

## Tech Stack

R, dplyr, tidyr, gt, corrplot, moments, car, boot
