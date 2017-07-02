# Data Analysis on Ebola Virus epidemics in 2014

The Ebola virus epidemic in 2014 is the largest in history. Multiple countries in West Africa were affected. As of December 17, 2014, there are totally 19065 cases and 7388 deaths as reported at CDC (Centers for Disease Control and Prevention). The demographic characteristics of the population can help identifying the pattern of spread and determining the effective way for disease control. In this project, a dataset containing Ebola subjects by gender and age will be used to identify the statistics in terms of age and gender in different regions. 

The dataset can be downloaded from https://data.hdx.rwlabs.org/dataset/sub-national-data-of-confirmed-cumulative-ebola-by-gender. This dataset stores the sub-national data for African countries (mainly in Liberia, Guinea and Sierra Leone) with subjects' age and gender. It contains 847 rows and 6 columns (Country, District, EpiCaseDef, Gender, Age and Value). The dataset is provided as .xlsx format with only one sheet. The last update was on 15 October 2014. 

The data was imported by read_excel() in pandas, with xlrd engine.

The exploratory analysis was performed in python and visualized using matplotlib.