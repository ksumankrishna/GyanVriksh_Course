ReadData <- read.csv("D:/suman/DataScience/GyanVriksh_Course-master (1)/GyanVriksh_Course-master/Assignment/Assignment/MBA Starting Salaries Data.csv")

#mode calculation
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#histogram of age
hist(ReadData$age)

#as per age observation it is right skewed data since mean> median > mode
summary(ReadData$age)
getmode(ReadData$age)

#mean median mode of gmat total it is slightly left skewed
summary(ReadData$gmat_tot)
getmode(ReadData$gmat_tot)
hist(ReadData$gmat_tot)

#histogram of salary after eliminating 999 and 999 and 0 records
totalSalary <- ReadData$salary[ReadData$salary>1000]
hist(ReadData$salary[ReadData$salary>1000])

#building FD of salary
classbreaks <- seq(min(totalSalary),max(totalSalary), by = (max(totalSalary)-min(totalSalary))/(1 + 3.322*(log(length(totalSalary)))))
frequencyTable <- table(cut(totalSalary, classbreaks))
#asper above observation modal class of the salary is 92500 and 102000 which has 42 records


#getting students who has satis as 7
satis7 <- ReadData$salary[ReadData$satis==7]
#removing 0 and 999 and 998
satis7remove0 <- satis7[satis7>1000]
mean(satis7remove0, trim = 10)
mean(totalSalary, trim = 10)
median(satis7remove0)
median(totalSalary)
getmode(satis7remove0)
getmode(totalSalary)
#as per the observation satis7 users are not highest paid members as their mean,media, mode less than total salary
#deviation is 2000, difference of trimmed mean

