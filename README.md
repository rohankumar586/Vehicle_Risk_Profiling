# Vehicle Risk Profiling - MGSC 661 Project

## Introduction
This project aim to apply data science in the insurance domain, offering tangible benefits in terms of risk management and financial forecasting. It involves developing a predictive model for an insurance company with the aim of assessing the risk associated with new vehicles entering the market. The focal point of the assessment is the 'symboling' attribute, a risk indicator inherent to each car model, which ranges from +3 (signifying high risk) to -3 (indicating low risk). This indicator is pivotal for insurance companies as it influences insurance premium calculations. The development of a reliable risk prediction model stands as a testament to the power of machine learning in transforming industry practices and enhancing decision-making processes.

## Project Description
Objective: To build a model that predicts the insurance risk rating ('symboling') of vehicles, ranging from high risk (+3) to low risk (-3).
Data Source: The dataset is available on [Kaggle](https://www.kaggle.com/datasets/toramky/automobile-dataset?resource=download)

## Technical Details
- Data Cleaning and Imputation: Employed regression modelling and other techniques to impute missing values and prepare the data for analysis.
- Feature Selection and Multicollinearity Analysis: Utilized Variance Inflation Factor (VIF) and Random Forest algorithm for feature selection, and removed highly collinear variables to assess feature importance.
- Understanding Variable Distribution: Analyzed the distribution of variables through various graphical methods to gain insights into their characteristics and relationships.
- Model Selection: Linear Discriminant Analysis (LDA) was used as the primary model due to the categorical nature of the target variable. 
- Clustering Analysis: Applied hierarchical and K-means clustering to identify distinct groups of vehicles, aiding in risk assessment.

## Results and Discussion
- Model Performance: The model achieved an accuracy of 70% demonstrating strong capability in predicting vehicle risk ratings with high specificity and balanced accuracy.
- Clustering Insights: Provided valuable insights into different vehicle groupings, which can be used for targeted insurance product development.

## Acknowledgments
- This project was a part of a course at McGill University. Special thanks to the course instructor for their support and guidance.

