# Crustacean_Sensation
Instructions for using Crab Maturity Analysis Code in R. 
This is an R-language program designed to separate Mature from Immature crabs based on allometric growth of body parts. Code logic is based on Somerton (1980) Program MATURE. The program separates two overlapping groups of points by minimizing the RSS of two regression lines, and compares the remaining RSS to that of a single line. The program was developed by Brad Stevens, Univ. of MD Eastern Shore. Suggestions for improvements are welcome.  This program is divided into multiple modules, several of which require R package(dplyr).  
All modules read and modify your data prior to analysis. Initial Input required for all modules is: 
1.	Path for reading input data file
2.	Define your input variable names
3.	Input Sex code as requested ("M" or 1 for males, "F or 2 for females)
4.	Select log transformation (or not).
5.	Select the range of data to be tested by defining the minimum and maximum X values. For Type A allometry, this is the range that contains the breakpoint; For other types, this is the range containing crabs of unknown maturity. 

Crab Maturity for Allometry Type A
This module analyzes data that fit allometry type A as defined by on Somerton (1980).  It is most often used to fit abdominal width of female crabs. Points should lie along a regression line that bends at a point.  This is often described as a “broken stick” model, and is fitted using a piece-wise regression equation.  There are two versions of this module
“Crab Maturity Type A v2.R” uses a brute force method to determine the SM50. After defining the test range, every observed value of X is tested as a breakpoint iteratively, the regression lines are recalculated, and the one giving the lowest residual sum of squares (RSS) is defined as the SM50.  The two-line regression model is then tested against a single line model.  
The second portion of this module uses a piecewise regression model to find the x value that gives the lowest mean square error (MSE), by iteratively testing each observed x value; the regression is then recalculated using that value. HOWEVER, the two lines defined by this method can overlap at that point. Alternatively, the SM50 could be defined as the point at which the two lines actually intercept, i.e. the regression equations predict the same y value, and this point may be less than the previously determined breakpoint. The user can select which one to use.

“Crab Maturity Type A pw v2.1.R” uses only a piecewise regression model to find the x value that gives the lowest mean square error (MSE); in this version, the x values are evenly spaced points within the test range as defined above, and may not equal actual observed values. The number and interval between points is user-defined. The regression is then recalculated using that value, and compared to a single line model.  As above, the SM50 is defined as the point at which the two lines actually intercept, i.e. the regression equations predict the same y value, and this point may be less than the minimum MSE breakpoint. 
The results from the two versions of piecewise regression will be similar if the test range is identical, and the number of points tested is similar to the number of observations within the test range. 

Crab Maturity for Allometry Types B-D
This module analyzes data that fit allometry types B, C, and D as defined by on Somerton (1980).  It is most often used to fit chela allometry for male crabs.  Points should lie in two clouds that overlap each other in the middle range.
Module A: Data Preparation 
This Module reads and modifies data prior to analysis.  Input required: 
1.	Path for reading input data file
2.	Define your input variable names
3.	Input Sex code as requested ("M" or 1 for males, "F or 2 for females)
4.	Select log transformation (or not).
After input initial settings, or after changing any settings, the module can be run as source.  Output is a file ready for analysis by Parts B and C.
Part B: Maturity assignment
Run Part A before running this module. This Module assigns maturity to unknown crabs.  Data are ln-transformed prior to regression analysis and maturity assignment. User will select a range of values for the lower and upper ends of the unknown region. Points to the left of the lower bound are assumed to be immature, and points to the right of the upper bound are assumed to be mature. The exact location of these bounds is not critical, because they will be adjusted by the program, but they should encompass most of the unknowns.  If in doubt, make the bounds as wide as possible, but there must be at least three points in both the immature and mature categories.
Output includes three plots; a plot of X and Y variables used to select the boundaries of the unknown data not saved); a plot of log-transformed X and Y variables showing single regression line (“Maturity Plot 1”); and a plot of the final assigned maturity and associated regression lines, with number of iterations and F value compared to a single line regression (saved as “Maturity Plot 2”).  
Instructions: 
1.	Run Section B1. Stop after first plot and input lower and upper bounds for unknowns
2.	Run Section B2. 
3.	Run Sections B3 and B4.
Part C: SM50 Estimation
This Module estimates SM50 using logistic regression. No user input is required because it runs on data created as a result of Module B.  You must run Parts A and B before running this module. Analysis is run on raw data vs maturity, and plotted within 10 mm groups. A maturity function is then bootstrapped 1000 times to estimate the standard error of the estimate. No additional input is required, so this module can be run as a single block. Output includes a table of SM50 estimates with bounds and confidence intervals, and a plot of the maturity ogive overlaid with proportion mature by 10 mm intervals (“Maturity Plot 3”).

