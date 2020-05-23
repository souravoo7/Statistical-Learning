#PRINCIPAL COMPONENTS ANALYSIS
#---------------------------------------------------------------------------------
library(ISLR)
#---------------------------------------------------------------------------------
# Description
# This data set contains statistics, in arrests per 100,000 residents for assault, murder, and rape in each of the 50 US states in 1973. Also given is the percent of the population living in urban areas.
# 
# Usage
# USArrests
# Format
# A data frame with 50 observations on 4 variables.
# 
# [,1]	Murder	numeric	Murder arrests (per 100,000)
# [,2]	Assault	numeric	Assault arrests (per 100,000)
# [,3]	UrbanPop	numeric	Percent urban population
# [,4]	Rape	numeric	Rape arrests (per 100,000)
# Note
# USArrests contains the data as in McNeil's monograph. For the UrbanPop percentages, a review of the table (No. 21) in the Statistical Abstracts 1975 reveals a transcription error for Maryland (and that McNeil used the same "round to even" rule that R's round() uses), as found by Daniel S Coven (Arizona).
# 
# See the example below on how to correct the error and improve accuracy for the '<n>.5' percentages.
# 
# Source
# World Almanac and Book of facts 1975. (Crime rates).
# 
# Statistical Abstracts of the United States 1975, p.20, (Urban rates), possibly available as https://books.google.ch/books?id=zl9qAAAAMAAJ&pg=PA20.
#---------------------------------------------------------------------------------

states =  row.names(USArrests) # get the row names of the dataframe

column_names = names(USArrests) #get the column names of the data frame

#check the mean and variance to see if we need to standardize the variables
get_means = apply(USArrests, #the data
                  2,         #on columns 1 for rows
                  mean)      #the function

get_var   = apply(USArrests, #the data
                  2,         #on columns 1 for rows
                  var)      #the function

pr.out = prcomp(USArrests,  #data
                scale = TRUE)#scaling 
summary(pr.out)

biplot(pr.out, scale=0)
#proportion of variance
PVE = pr.out$sdev^2/sum(pr.out$sdev^2)
plot(PVE, 
     xlab = "Princpal Component",
     ylab = "Proportion of Variance Explained",
     ylim = c(0,1),
     type = 'b')

#cumulative PVE
plot(cumsum(PVE), 
     xlab = "Princpal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     ylim = c(0,1),
     type = 'b')
