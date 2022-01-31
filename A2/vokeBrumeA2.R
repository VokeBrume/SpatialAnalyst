#Name:  Voke Brume
#Assignment:  2
#Date: 02/25/2021

#Read file in
cdata <- read.csv("A2/functions_loops/classification_data.csv")

#Load dplyr library
library(dplyr)

#T1:
#Created function to generate contingency table and return the calculation of overall accuracy
overall_accuracy <- function(corr_class, pred_class){
  cont_table <- table(corr_class, pred_class)
  overll_accu <- (sum(diag(cont_table))/sum(cont_table)) * 100
  overall_accuracy_combined = c(overll_accu)
  return (overll_accu)
}


#T2: 
#Called overall accuracy function on the appropriate parameters
overall_accuracy(cdata$class, cdata$spec)
overall_accuracy(cdata$class, cdata$spec_lidar)


#T3:
#Answer: The model of the class and the spectral + LiDAR yielded the best classification performance with 83%.


#T4:
#Generated 10 random numbers between 1 - 20, with replacement enabled
random_values <- sample(1:20, 10, replace = TRUE)
random_values

#Used for-loop to determine if the numbers above are even(TRUE) or odd(FALSE)
for (i in random_values){
  if (i%%2 == 0){
    print("TRUE")
  } else {
    print ("FALSE")
  }
}


#T5:
#Hard-coded sample text to text for-loop
sample_text <- c("young", "old", "small", "big", "tall", "short")
sample_text

#Declared variable to store uppercase versions of sample texts
all_upper_case = c()
for (j in sample_text){
  all_upper_case = c(all_upper_case, toupper(j))
}
all_upper_case

#T6:
#Declared data-frames
correctly_predicted <- data.frame()
incorrectly_predicted <- data.frame()

#Used for-loop to loop through each row and compare the appropriate values, then save the rows to seperate data-frames depending on correctness
#of prediction.
for (k in 1:nrow(cdata)){
  if (cdata[k, 1] == cdata[k, 3]){
    correctly_predicted = rbind(correctly_predicted, c(cdata[k, ]))
  } else {
    incorrectly_predicted = rbind(incorrectly_predicted, c(cdata[k, ]))
  }
}

correctly_predicted
incorrectly_predicted