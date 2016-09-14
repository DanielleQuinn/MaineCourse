# ---- Description ----
# This is part II of the code along
# script for the September 14th R 
# Workshop at University of Maine,
# presented by Dr. Trevor Avery and
# Danielle Quinn
# Enjoy!

# ---- Set Working Directory ----
# The working directory is the location (folder)
# that R is going to communicate with when
# importing files, exporting figures, etc.

# Hint: To check your current working directory:
# Hint: Use the tab key to easily navigate folders

# ---- Load Packages ----

# ---- Import Data ----
# Single flat file saved as a .txt

# ---- Data Frames: Basics ----
 # Spreadsheet of data frame in new tab
 # Structure of each variable
 # Summary of each variable
 # Dimensions (rows, columns)
 # Output te first six rows
 # Variable (column) names

# ---- Data Frames: Each Column is a Vector ----
 # Outputs the column length as a vector
 # Outputs the 5th element of the vector

# Output a subset of the mydata$length vector
# where length > 10

# Note: To omit NAs, add a secondary argument

# ---- Data Frames: Data Frames are Two Dimensional ----
 # Outputs the value of row 1, column 2

 # Outputs the values of rows 1 to 5
  # and columns 1, 2, 3, and 6

# You can leave on value blank to select all
 # Outputs all rows and column 3
 # Outputs rows 1 to 5 and all columns

# Output a subset of the mydata data frame
# where length > 10
mydata[mydata$length>10] # Why does this give you an error?

# Correct:
 # Need to include the 2nd dimension (column)

# ---- Data Frames: Modifying Variables ----
# Add a new column (variable)

# Make the variable eggs represent presence/absence
# Where eggs is greater than 0, change to 1

# Males should all have NA values for eggs

# ---- Exercise 1.6 ----
# (A) Create a subset that contains males
# captured in 2013

# (B) Find the average length of females

# ---- Exercise Solutions ----
# Exercise 1.6
# (A)
male_2013<-mydata[mydata$sex=="m" & mydata$year==2013,]

# (B)
female_meanlength<-mean(mydata$length[mydata$sex=="f" & !is.na(mydata$length)])
# or
female_meanlength<-mean(mydata$length[mydata$sex=="f"], na.rm=TRUE)
