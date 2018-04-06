# Installing the required packages.

# install.packages('dplyr')
# install.packages('data.table')
# install.packages('ggplot2')
# install.packages('reshape2')
#install.packages('RSQLite')


# Loading the installed packages.
library(dplyr)
library(data.table)
library(ggplot2)
library(reshape2)
library(RSQLite)


# Connecting to a sqlite server
con = dbConnect(drv = SQLite(), dbname = "similarusers.sqlite")

# Loading the .csv files provided in the R environment. Make sure that the current working directory 
# is pointing to the location of files.
course_tags <- read.csv("course_tags.csv", header = TRUE)
user_assessment_scores <- read.csv("user_assessment_scores.csv", header = TRUE)
user_course_views <- read.csv("user_course_views.csv", header = TRUE)
user_interests <- read.csv("user_interests.csv", header = TRUE)

# Finding the number of unique items in each category.
length(unique(course_tags$course_tags))
length(unique(user_assessment_scores$assessment_tag))
length(unique(user_course_views$course_id))
length(unique(user_interests$interest_tag))

# Removing the duplicate entries.
user_interests <- user_interests[-which(duplicated(user_interests) == TRUE),]
 
# Removing users with less than 5 views.
matches <- data.frame(table(user_course_views$user_handle))
colnames(matches) <- c('user_handle', 'counts')
user_course_views <- merge(user_course_views,matches)
user_course_views <- user_course_views[-which(user_course_views$counts <= 5),]
user_course_views <- user_course_views[,-ncol(user_course_views)]

# Removing users with less than 3 assessments.
matches1 <- data.frame(table(user_assessment_scores$user_handle))
colnames(matches1) <- c('user_handle', 'counts')
user_assessment_scores <- merge(user_assessment_scores,matches1)
user_assessment_scores <- user_assessment_scores[-which(user_assessment_scores$counts <= 3),]
user_assessment_scores <- user_assessment_scores[,-ncol(user_assessment_scores)]

# Exploration of the data

# Distribution of number of views per user.
user_course_views %>%
    group_by(user_handle) %>%
    summarize(number_of_views_per_user = n()) %>%
    ggplot(aes(number_of_views_per_user)) +
    geom_bar(fill = "cadetblue3", color = "grey20") + coord_cartesian(c(3, 50))

# Distribution of number of assessments per user.
user_assessment_scores %>%
    group_by(user_handle) %>%
    summarize(number_of_assessments_per_user = n()) %>%
    ggplot(aes(number_of_assessments_per_user)) +
    geom_bar(fill = "cadetblue3", color = "grey20") + coord_cartesian(c(3, 50))

# Distribution of mean time viewed.
user_course_views %>%
    group_by(user_handle) %>%
    summarize(mean_user_view_time = mean(view_time_seconds)) %>%
    ggplot(aes(mean_user_view_time)) +
    geom_histogram(fill = "cadetblue3", color = "grey20")

# Distribution of mean assessment score.

user_assessment_scores %>%
    group_by(user_handle) %>%
    summarize(mean_user_assessment_score = mean(user_assessment_score)) %>%
    ggplot(aes(mean_user_assessment_score)) +
    geom_histogram(fill = "cadetblue3", color = "grey20")

# Distribution of number of views per course.
user_course_views %>%
    group_by(course_id) %>%
    summarize(number_of_views_per_course = n()) %>%
    ggplot(aes(number_of_views_per_course)) +
    geom_bar(fill = "orange", color = "grey20", width = 1) + coord_cartesian(c(0,40))

# Distribution of number of assessments per tag.
user_assessment_scores %>%
    group_by(assessment_tag) %>%
    summarize(number_of_assessments_per_tag = n()) %>%
    ggplot(aes(number_of_assessments_per_tag)) +
    geom_bar(fill = "orange", color = "grey20", width = 1) + coord_cartesian(c(0,40))


# This is the function to calculate the similar users to the current user based on the courses viewed
# and calculating the simialrity score and writing into a table in the database. 

    
#' Return the similar users to the given current user
#' @param user Enter the current user.
#' @get /similar_users_views   
similar_user_views <- function(user)
{

# Assigning the current user handle.

current_user <- user

# Checking if the provided user handle is valid.
if (current_user %in% unique(user_course_views$user_handle))
{
 
# Getting a list of unique user handles and course ids .  
dimension_names <- list(user_handle = sort(unique(user_course_views$user_handle)), course_id = sort(unique(user_course_views$course_id)))

# Subsetting only the required columns and removing date, author handle and level
# as I am trying to find similar users only based on course views.
user_course_views_sub <- user_course_views[,c(1,3,6)]

# Creating a wide matrix that would be helpful in finding similarity score.
viewsecmat1 <- dcast(user_course_views_sub, user_handle ~ course_id, value.var = "view_time_seconds", fun = sum)

# Making the data more readable and processable.
viewsecmat1[viewsecmat1 == 0] <- NA
viewsecmat2 <- viewsecmat1[,-1]
rownames(viewsecmat2) <- viewsecmat1[,1]
viewsecmat2 <- as.matrix(viewsecmat2)
dimnames(viewsecmat2) <- dimension_names


# Finiding the courses viewed by the current user.
viewed_items <- which(!is.na((as.data.frame(viewsecmat2[current_user, ]))))
#  Selecting the users who have viewed atleast 3 courses as the current_user.
selected_users <- names(which(apply(!is.na(viewsecmat2[ ,viewed_items]), 1, sum) >= 3))

# Subtracting each row with their means to make the view seconds more consistent.

viewmat <- viewsecmat2[selected_users, ]
user_mean_views <- rowMeans(viewmat,na.rm = T)
viewmat <- viewmat - user_mean_views

# using Pearson's correlation to calculate the similarity score between users and 
# putting the results in a data frame.
similarities <- cor(t(viewmat[rownames(viewmat) != current_user, ]),viewmat[current_user, ], use = 'pairwise.complete.obs')
sim <- as.vector(similarities)
names(sim) <- rownames(similarities)
res <- sort(sim, decreasing = TRUE)
df <- data.frame(names(res), res)
colnames(df) <- c('Similar_User','Similarity_Score')
df$current_user <- current_user

# Writing the results into a table called similarusers_views and removing all the duplicate entries.
dbWriteTable(con, "similarusers_views", df, append = TRUE)
dbGetQuery(con, "CREATE TABLE copied AS SELECT * FROM similarusers_views WHERE 0")
dbGetQuery(con, " INSERT INTO copied SELECT * FROM similarusers_views GROUP BY Similar_User,current_user")
dbGetQuery(con, "DROP TABLE similarusers_views")
dbGetQuery(con,"ALTER TABLE copied RENAME TO similarusers_views")
return(df)
}
else
{
    return("User handle not found in list")
}

}

similar_user_views('2')


# Function to find similar users depending upon the assessment scores and 
# calculating similarity score and writing it in a table in a database.

#' Return the similar users to the given current user
#' @param user Enter the current user.
#' @get /similar_users_assessment 

similar_user_assessment <- function(user)
{
current_user <- user

# Checking if the current user is in the available list of user handles.
if (current_user %in% unique(user_assessment_scores$user_handle))
{

# Creating a unique list of user handles and assessment tag.
dimension_names1 <- list(user_handle = sort(unique(user_assessment_scores$user_handle)), assessment_tag = sort(unique(user_assessment_scores$assessment_tag)))
# Subsetting only the required columns as I am finding similarity score only 
# based on assessment scores and tags.
user_assessment_scores_sub <- user_assessment_scores[,c(1,2,4)]
# Creating a wide matrix to make it easier for calculation.
assessmentscoremat1 <- dcast(user_assessment_scores_sub, user_handle ~ assessment_tag, value.var = "user_assessment_score", fun = sum)
# Making the data more readable and processable.
assessmentscoremat1[assessmentscoremat1 == 0] <- NA
assessmentscoremat2 <- assessmentscoremat1[,-1]
rownames(assessmentscoremat2) <- assessmentscoremat1[,1]
assessmentscoremat2 <- as.matrix(assessmentscoremat2)
dimnames(assessmentscoremat2) <- dimension_names1

# Getting the assessed items for the current user.
assessed_items <- which(!is.na((as.data.frame(assessmentscoremat2[current_user, ]))))
# Selecting the users who have atleast 3 assessment tags in common.
selected_users_assessment <- names(which(apply(!is.na(assessmentscoremat2[ ,assessed_items]), 1, sum) >= 3))

# Subtracting each row by their means to make assessment score more consistent.
assessmentscoremat <- assessmentscoremat2[selected_users_assessment, ]
user_mean_assessment_score <- rowMeans(assessmentscoremat,na.rm = T)
assessmentscoremat <- assessmentscoremat - user_mean_assessment_score

# Calcuating the Pearson's similarity score between two users and putting into a data frame.
similarities_assessment <- cor(t(assessmentscoremat[rownames(assessmentscoremat)!=current_user, ]),assessmentscoremat[current_user, ], use = 'pairwise.complete.obs')
sim_assessment <- as.vector(similarities_assessment)
names(sim_assessment) <- rownames(similarities_assessment)
res_assessment <- sort(sim_assessment, decreasing = TRUE)
df1 <- data.frame(names(res_assessment), res_assessment)
colnames(df1) <- c('Similar_User','Similarity_Score')
df1$current_user <- current_user

# Writing the results in a table called similarusers_assessment and 
# removing the duplicate entries.
dbWriteTable(con, "similarusers_assessment", df1, append = TRUE)
dbGetQuery(con, "CREATE TABLE copied AS SELECT * FROM similarusers_assessment WHERE 0")
dbGetQuery(con, " INSERT INTO copied SELECT * FROM similarusers_assessment GROUP BY Similar_User,current_user")
dbGetQuery(con, "DROP TABLE similarusers_assessment")
dbGetQuery(con,"ALTER TABLE copied RENAME TO similarusers_assessment")
return(df1)
}
else
{
    return("User handle not in list")
}
}

similar_user_assessment('1')











