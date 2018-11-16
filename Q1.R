#Q1 Majority Vote 
p = c(0.1, 0.1, 0.15, 0.2, 0.2, 0.2, 0.55, 0.55, 0.55, 0.6, 0.6, 0.65, 0.65, 0.7, 0.75)  

length(which(p>=0.5))
length(which(p<0.5))

#Comment: Based on Majority vote, the classification is red

#Average Probability
mean(p)

#Comment: Since the average probability is less then 0.5, the classification is black.