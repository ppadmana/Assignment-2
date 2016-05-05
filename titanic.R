# Task 1 - Correct the "embarked" column by replacing the missing value with "S"

func1<-function(y){if(nchar(y)==0){print("S")} else {print(y)}}
titanic1<-bind_cols(select(titanic3, -embarked), transmute(titanic3,embarked_new=lapply(as.character(titanic3$embarked), func1)))

# Task 2 - Correct the "Age" column by filling in the missing values with the mean age of the remaining values.

func2 <-function(y){if(is.na(y)){print(round(mean(titanic1$age, na.rm=TRUE)))} else print(y)}
titanic2<-bind_cols(select(titanic1, -age), transmute(titanic1, age_new=lapply(titanic1$age, func2)))

# The mean is not always very reliable especialy if we have outliers in the data, values far away from the mean. The median is a better indicator in such situations which can be obtained after sorting the age data. We could also use a trimmed mean to remove the outliers before computing the mean.
# However this depends on the kind of data we are dealing with which is age of passengers here. We do not expect many outliers in this data given the human life span and the fact not too many old or very young passengers could have travelled alone, so the mean age may very well do the job in this case.


#Task 3 - Correct the boat coulmn by filling in the missing values with "NA".

func3<-function(y){if(nchar(y)==0){print("NA")} else print(y)}
titanic4<-bind_cols(select(titanic2, -boat), transmute(titanic2, boat_new=lapply(as.character(titanic2$boat), func3)))

# Task 4 - Add a binary column to indicate which passengers have a cabin number and which don't. This is a good indicator of survival as only those who survived could have reported their cabin number. This means that it makes sense to add this binary column to make inferences about the survival.

func4<-function(y){if(nchar(y)==0){print(0)} else print(1)}
titanic5<-mutate(titanic4, has_cabin_number=lapply(as.character(titanic4$cabin), func4))

#Task 5 - Write cleaned data into .csv file.

write.csv(as.character(titanic5), file="titanic_clean.csv")

