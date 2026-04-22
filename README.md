# COVID-19-Bangladesh-EDA-and-Daily-Cases-Prediction

- Engineered a robust COVID-19 data pipeline in R, automating the scraping and cleaning of real-time Wikipedia datasets to produce high-integrity inputs for predictive modelling.
- Conducted exploratory data analysis (EDA) on COVID-19 time-series data, visualising daily case trends, 12-day moving averages, and train/validation/test splits to identify seasonality and outbreak patterns.
- Benchmarked four forecasting models — ARIMA (statistical baseline, RMSE: 514), Prophet (RMSE: 209, +59.3%), H2O AutoML (RMSE: 200, +61%), and LSTM (RMSE: 158, +69.2%) — with LSTM delivering the best performance using recursive one-step-ahead forecasting on a 20-day prediction window.
