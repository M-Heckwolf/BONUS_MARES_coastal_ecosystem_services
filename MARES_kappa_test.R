#load package
library(irr)

## Load data table for 20 randomly selected papers, which were evaluated by everyone involved in the subsequent systematic review assessment (Person 01-05).
## Numbers indicate the decision (1 = include paper, 0 = exclude paper) after full text assessment.
table <- read.csv2("\\\\helmholtz/users/fb3/ev/mheckwolf/Daten/Promotion/MARES/systematic review/trial_Kappa/MARES_kappa_data.csv")

#conduct kappa test
kappam.fleiss(table[,-1]) # remove paper_ID
#Subjects = 20 
#Raters = 5 
#Kappa = 0.793 
#
#z = 11.2 
#p-value = 0 