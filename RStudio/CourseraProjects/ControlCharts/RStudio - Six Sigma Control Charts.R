# SIX SIGMA - CONTROL CHARTS
# Coursera Guided Project
# Instructor - Moses Gummadi

# This is an R Script file
# You can open this in R Studio
# Use # symbol for comments in R Script file

# R Studio is already downloaded in your Rhyme Desktop
# To download on your own desktop
# Use this link https://rstudio.com/products/rstudio/
# Choose the RStudio Desktop Free Option

# In this Guided Project You Will Learn
# How To Plot & Interpret Control Charts Using RStudio

# Approach:
#    1. Data Types - Variables, Attribute (Type I and II)
#    2. Control Chart Selecting Based On Data Type
#    3. Common Cause & Speacial Cause Variability
#    4. Rules For Stability - Western Electric, Nelson's 
#    5. Plot & Interpret Variables Charts (XmR / IMR; Xbar-R, Xbar-S)
#    6. Plot & Interpret Attribute Charts (NP, P, U, C)
#
# Data Types Flow Chart
#
#    1. "Variables" Data (aka "Continuous" Data)
#        Criterion 1: Measure, not Count
#        Criterion 2: Avg of any two values must make sense
#        Examples: Length, Temperature, Weight, Time, etc
#     
#    2. "Attribute" Data Type I (aka "Discrete" Type I)
#        Criterion 1: Count, not Measure
#        Criterion 2: Able to count non-occurrences
#        Examples: # of defective units in a lot of 100
#                  # of business class seats occupied in the plane
#
#    3. "Attribute" Data Type II (aka "Discrete" Type II)
#        Criterion 1: Count, not Measure
#        Criterion 2: CANNOT count non-occurrences
#        Examples: # of scratches on the car
#                  # of customers in a day
#                  # of complaints per week

# Control Chart Selection Algorithm
#   
#     Variables Data      Individuals           : IMR / XmR Chart
#                         Groups (2 to 5)       : Xbar - R Chart
#                         Groups (6+   )        : Xbar - S Chart
#
#     Attribute Type-I    Sample Size Const     : NP Chart
#                         Sample Size Variable  : P Chart
#
#     Attribute Type-II   Const. Opportunity    : C Chart
#                         Variable Opportunity  : U Chart
#----------------------------------------------------------

# Link To Introductory Video
browseURL("https://youtu.be/N62qUfMksIQ")


#----------------------------------------------------------
# Load Libraries & Functions
source("https://pastebin.com/raw/d1UpgBxi") 
#----------------------------------------------------------



#----------------------------------------------------------
# Task 1 - Plot XMR Chart
#----------------------------------------------------------

dt = fread("XMR Chart Dataset.csv")
print(dt)

dt = dt[order(FiscalWeek)]   # Ensure Data Is Time Ordered

x = dt$England
x = dt$Scotland
x = dt$England + dt$Scotland +dt$Wales +dt$N_Ireland

out = XMRChartX(x, label = "Sales")    # X in the X-MR Chart
out = XMRChartMR(x, label = "Sales")   # MR in the X-MR Chart

out  # view control chart data

out[rule1 > 0]   

# Exercise - Plot & Interpret XMR Charts 
#           (X and MR) for N_Ireland

dt = fread("XMR Chart Dataset.csv")


#----------------------------------------------------------
# Task 2 - Plot & Interpret Xbar-R Chart, Xbar-S Chart
#----------------------------------------------------------

dt = fread("XbarRS Chart Dataset.csv")
print(dt)

x = dt$England
g = dt$Week

out = XbarRChartX(x, g, label = "Sales", group = "Week")  
out = XbarRChartR(x, g, label = "Sales", group = "Week")  

x = dt$England
g = dt$Month

out = XbarSChartX(x, g, label = "Sales", group = "Month")  
out = XbarSChartS(x, g, label = "Sales", group = "Month") 

# Exercise, Plot Xbar-R & Xbar-S for N_Ireland by Week

dt = fread("XbarRS Chart Dataset.csv")


#----------------------------------------------------------
# Task 3 - Plot & Interpret NP Chart and P Chart
#----------------------------------------------------------

dt = fread("NP & P Chart Dataset.csv")
View(dt)

# NP Chart (for constant sample size)

mean(dt$Units_Eng)

out = npChart(400, dt$Defects_Eng, label = "Defects")

# What happens when n is different? (Inspect "out")
out = npChart(300, dt$Defects_Eng, label = "Defects")

# P Chart (for variable sample size)

out = pChart(dt$Units_Eng, dt$Defects_Eng, label = "Defects/ Unit")

# Exercise: NP Chart (Use N = 157) & P Chart for Scotland
dt = fread("NP & P Chart Dataset.csv")


#----------------------------------------------------------
# Task 4 - Plot & Interpret C Chart and U Chart
#----------------------------------------------------------

dt = fread("C & U Chart Dataset.csv")
View(dt)

# C Chart (for constant opportunity size)

out = cChart(dt$PetrolFills, label = "PetrolFills")

# Let's check what happens with NP chart
out = npChart(1000, dt$PetrolFills)

# U Chart (for variable opportunity size)

out = uChart(dt$Dispensers, dt$PetrolFills, label = "PetrolFills")


# Exercise: Plot C & U Charts for DieselFills
dt = fread("C & U Chart Dataset.csv")

#----------------------------------------------------------
# End of Guided Project
# Summary At: https://youtu.be/N62qUfMksIQ
#----------------------------------------------------------

