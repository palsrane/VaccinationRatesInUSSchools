# VaccinationRatesInUSSchools
Analysis of reports of MMR and Overall Vaccination rates in US schools between 2017-2019.

Go to measles_immunization_exploration.pdf for an explanation of the R code.

If you would like to directly download R code, that is provided as well. 


This data comes from [#tidytuesday](https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-02-25/readme.md) and is originally from [The Wallstreet Journal](https://github.com/WSJ/measles-data). They recently published an [article](https://www.wsj.com/graphics/school-measles-rate-map/) around 46,412 schools across 32 US States. 

"This repository contains immunization rate data for schools across the U.S., as compiled by [The Wall Street Journal](https://github.com/WSJ/measles-data). The dataset includes the overall and MMR-specific vaccination rates for 46,412 schools in 32 states. As used in ["What's the Measles Vaccination Rate at Your Child's School?"](https://www.wsj.com/graphics/school-measles-rate-map/).  
  
Vaccination rates are for the 2017-18 school year for Colorado, Connecticut, Minnesota, Montana, New Jersey, New York, North Dakota, Pennsylvania, South Dakota, Utah and Washington. Rates for other states are 2018-19."  
  
The cleaning process produces a .csv file with followig columns:  
  
|variable |class     |description |
|:--------|:---------|:-----------|
|index    |double    | Index ID |
|state    |character | School's state |
|year     |character | School academic year|
|name     |character | School name|
|type     |character | Whether a school is public, private, charter |
|city     |character | City |
|county   |character | County |
|district |character | School district |
|enroll   |double    | Enrollment |
|mmr      |double    | School's Measles, Mumps, and Rubella (MMR) vaccination rate |
|overall  |double    | School's overall vaccination rate|
|xrel     |double    | Percentage of students exempted from vaccination for religious reasons |
|xmed     |double    | Percentage of students exempted from vaccination for medical reasons |
|xper     |double    | Percentage of students exempted from vaccination for personal reasons |
|lat      |double    | Lattitude  |
|lng      |double    | Longitude  |


<u>Data cleaning performed in this study:</u>

The initial cleaning code from [tidytuesday](https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-02-25/readme.md) had to be modified because   
  
 * It was resulting in an error, possibly because the page where [the list of URLs for individual states](https://github.com/WSJ/measles-data/tree/master/individual-states) was coming from has changed since the code was published.  

 * When we were adding the latitude and longitude data from the states to the original vaccination file, it was being done only with school name and if one state had multiple schools with the same name, that was leading to a many to many matching, resulting in a cartesian matching and duplication.)  

 * Finally, 86 records, 3 from Florida, and 83 from Vermont were noted to have incorrect longitudes. 1 record from Florida merely had the longitude marked as +ve instead of -ve as per google search, and that was corrected as so. However, rest of the records had their own latitudes inputted as longitudes at the very source. Those longitude values were replaced with -72.7678 for Vermont records, and -81.55 for Florida records, the central logitudes for the states that are generated by Tableau.
