# Diversity in Economics Research Project

I used this .R code to clean a dataset of undergraduate students at a top public state university, and merge this file with their grades, demographics of their TAs (GSIs), demographics of their professors, as well as their course and section time and day. 

Ultimate goal of the study was to see casual impact of TA demographics on student outcomes by exploiting fact that TAs and students both do not know who will be in class when they choose one. Analysis on the final dataset was performed in Stata. Results were presented at the Undergraduate Women in Economics conference in April 2017

Datasets used to prepare the ultimate table are not in this repo for privacy reasons.

## Data
* course.enroll is a 66270 x 13 dataframe from a  .csv is a student by class level dataset containing class information for every class a student took
* course.enroll.grades is a 78239 x 17 dataframe from a .csv that student by class level grade data
* students is a 15822 x 108 dataframe student level dataframe from a .csv containing student demographic data (except for name of student). Demographic data is from original college application submitted by student.
* act_sat_conversion.csv 

### Econ_times.R

Ran first. Cleaned econ course schedules which were then added to the student/course dataset in Merge.R

### Merge.R

Does a number of things including 
* Cleaning and merging course.enroll and course.enroll.grades dataset to students dataset - ultimately regressions were run on student as well as student by course level.
* Merging student level dataset with gsi information for lower-division courses that were studied.
* Manipulating the student level dataset to be a student by class level dataset for analyzing the results with a different regression