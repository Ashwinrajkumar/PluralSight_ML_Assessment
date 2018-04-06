
# Installing the plumber package.
install.packages('plumber')
# Loading the plumber package
library(plumber)


# Make sure that the working directory is pointing to the foldr where the R script is located.
# Pointing to the file and running the endpoints in localhost port = 8000
r <- plumb("PluralSight_ML_Similarity_Script.R")  
r$run(port=8000)



