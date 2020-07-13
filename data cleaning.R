library("dplyr")
library(mice)
cv =  read.csv("covid.csv")

cv = na.omit(cv)

cv= as.numeric(cv$total_cases)

#do it for all numeric null valued columns else just do cv = na.omit(cv)
mean(cv , na.rm = TRUE)

cv= as.numeric(cv$total_deaths)
mean(cv , na.rm = TRUE)


cv= as.numeric(cv$total_deaths)
mean(cv , na.rm = TRUE)


cv= as.numeric(cv$total_cases_per_million)

mean(cv , na.rm = TRUE)


cv= as.numeric(cv$total_deaths_per_million)
mean(cv , na.rm = TRUE)



cv= as.numeric(cv$total_deaths)
mean(cv , na.rm = TRUE)


cv= as.numeric(cv$total_tests_per_thousand)
mean(cv , na.rm = TRUE)


cv= as.numeric(cv$new_deaths_per_million)
mean(cv , na.rm = TRUE)


cv= as.numeric(cv$new_tests_smoothed)
mean(cv , na.rm = TRUE)


cv = cv[order(cv$location),]


write.csv("C:\\Users\\KOMAL\\Documents\\semester-2\\covid3.csv")
