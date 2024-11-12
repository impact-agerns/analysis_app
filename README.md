### Aggregation & Analysis app for KI-level data

Created by: Global Emergencies Unit - Impact Initiatives.

The app can be accessed here: <https://impact-initiatives.shinyapps.io/geuki_analysis_app/>

Purpose: The app is intended to simplify and standardize the aggregation and analysis process of Key informant information. It allows a less technical audience to run the app autonomsly. Through the app the data gets transformed into the desired output with very few steps and can then be used for other products.

<br>

### Method:

<br>

####    1. Data aggregation

When working with KI-level data, the information is often not comparable between regions, and requires aggregation of responses to a higher level of analysis.

Example: In area A there is one 1 KI and in area B there are 5 KIs. When looking at the results of the assessment in terms of percentage response of the KIs area A often reaches 100% as there is only one KI, and area B never reaches 100%. To be able to compare the responses between both areas, the data has to be transformed. One approach is by aggregating the data up to the desired level, for example area-level.

Depending on the type of indicator the information is aggregated differently:

`select_one` : Mode (Most common choice)

`select_multiple` : All choices of all KIs are kept

`integer` : Mean (arithmetic)

`text` : Mode

By aggregating the data up to area-level area A is now showing 1 response, and area B also is showing 1 response, this is shown in the dataset as 1 row for each area instead of previously being 1 row per KI.

<br>

####    2. Data analysis

For reporting and analysis purposes the data has to be summarised to identify the number of responses given, the total number of people/areas that have been given that question and the resulting percentages. This transformation is happening in the data analysis step. This step does not require the data to be aggregated previously.

In the data analysis part, 2 analyses can be run at the same time, with the second one being optional.

Each analysis can be run with multiple variables.

Example: Analysis using the following variables - admin2, KI-type

In this case the analysis script will set the 2 selected variables as dimensions of the analysis and identify for each distinct combination of the 2 variables all response statistics for all indicators/choices.

<br>

For any questions contact Alvaro Gerns (alvaro.gerns\@impact-initiatives.org)
