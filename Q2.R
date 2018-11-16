#Boosting

#Function for computing alpha
alpha_fun <- function(epsilon){
  (1/2)*log((1-epsilon)/epsilon)
}

#Number of observations
n = 10

#x values
x = seq(0,9)

#y values
y <- c(1, 1, 1, -1, -1, -1, 1, 1, 1, -1)

#Set initial weight
w0 = rep(0.10, 10)

# These are the x values that we classified incorrectly in round 1
index_incor <- c(7:9) 
epsilon1 <- sum(w0[index_incor])
epsilon1

#epsilon1 = 0.3
# we plug our epsilon value  into our alpha function
alpha1<-alpha_fun(epsilon1) # 0.4236489
alpha1

alpha1_high1 <- exp(alpha1)  # reweighting term of 1.527 for those initially misclassified
alpha1_high1
alpha1_low1 <- exp(-alpha1) # reweighting of 0.654 for those correctly classified on round 1
alpha1_low1

w_new <-w0
# new weight for those classified incorrectly; weighting up
w_new[index_incor]<- alpha1_high1*w0[index_incor]
# new weights for those classified correctly; weighting down
# minus sign before "index_incor" in coding
w_new[-index_incor]<- alpha1_low1*w0[-index_incor]
w_new

# We use Z_t to normalize
Z_1 <- sum(w_new)
Z_1

w1<- w_new/Z_1
w1

sum(w1)

#Q2.1 Epsilon and alpha 

#For t=2, the threshold set is between 8 and 9

# These are the x values that we classified incorrectly in round 2
index_incor <- c(4:6) 
epsilon2 <- sum(w1[index_incor])
epsilon2 #0.21

#Alternate epsilons

#If threshold is set between 5 and 6 with all values below 5.5 = -1 and above 5.5 = +1

alternate.index_incor = c(1,2,3,10)
epsilon.alternate <- sum(w1[alternate.index_incor])
epsilon.alternate #0.28

#If threshold is set between 2 and 3 with all values below 2.5 = +1 and above 2.5 = -1
alternate.index_incor = c(7:9)
epsilon.alternate <- sum(w1[alternate.index_incor])
epsilon.alternate #0.5

#Best epsilon is 0.21 and corresponds to threshold between 8 and 9

#epsilon2 = 0.21
# we plug our epsilon value  into our alpha function
alpha2<-alpha_fun(epsilon2)
alpha2  # 0.6496415

#Q2.2
alpha2_high2 <- exp(alpha2)  # reweighting term of 1.915 for those initially misclassified
alpha2_high2
alpha2_low2 <- exp(-alpha2) # reweighting of 0.522 for those correctly classified on round 1
alpha2_low2

#Q2.3 h2(x) = 0.6496*I(x < 8.5)

#Q2.4 Reweighting
w_new <-w1
# new weight for those classified incorrectly; weighting up
w_new[index_incor]<- alpha2_high2*w1[index_incor]
# new weights for those classified correctly; weighting down
# minus sign before "index_incor" in coding
w_new[-index_incor]<- alpha2_low2*w1[-index_incor]
w_new

# We use Z_t to normalize
Z_1 <- sum(w_new)
Z_1

w2<- w_new/Z_1
w2

sum(w2)

#Q2.5 Third Round (t = 3)
#For t = 3 the threshold is set between 5 and 6
index_incor <- c(1,2,3,10)
epsilon3 <- sum(w2[index_incor])
epsilon3 #0.18

# we plug our epsilon value  into our alpha function
alpha3<-alpha_fun(epsilon3) # 0.7520387
alpha3


alpha3_high3 <- exp(alpha3)  # reweighting term of 2.121 for those initially misclassified
alpha3_high3
alpha3_low3 <- exp(-alpha3) # reweighting of 0.471 for those correctly classified on round 2
alpha3_low3

#Q2.3 h3(x) = 0.7520*(x > 5.5)

#Q2.4 Reweighting
w_new <-w2
# new weight for those classified incorrectly; weighting up
w_new[index_incor]<- alpha3_high3*w2[index_incor]
# new weights for those classified correctly; weighting down
# minus sign before "index_incor" in coding
w_new[-index_incor]<- alpha3_low3*w2[-index_incor]
w_new

# We use Z_t to normalize
Z_1 <- sum(w_new)
Z_1

w3<- w_new/Z_1
w3

sum(w3)

#Q2.6 Strong Classifier
#sign(0.4236489*I(x < 2.5) + 0.6496*I(x < 8.5) + 0.7520*I(x > 5.5))

#Q2.7 Results
pred = sign(alpha1*((x < 2.5) - (x > 2.5)) + alpha2*((x < 8.5) - (x > 8.5)) + alpha3*((x > 5.5) - (x < 5.5)))
print(pred)

#The strong classifier made no mistakes in classification


