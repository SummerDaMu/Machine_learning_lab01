#read_pima-indians-diabetes.csv
library(tidyverse)
# ----------------------------------------------------------Assignment 3.1----------------------------------------------------------
#1.read
O_pima <- read_delim("pima-indians-diabetes.csv",",", col_names = FALSE)
#2.rename columns
pima <-
  O_pima |>
    rename('Pregnancies' = X1, #Number_of_times_pregnant
         "Glucose" = X2, # Plasma glucose concentration a 2 hours in an oral glucose tolerance test.
         "BP" = X3, # Diastolic blood pressure (mm Hg)
         "Skinthickness" = X4, # Triceps skinfold thickness (mm)
         "Insulin" = X5, # 2-Hour serum insulin (mu U/ml).
         "BMI" = X6, # Body mass index (weight in kg/(height in m)^2)
         "Pedigree" = X7, # Diabetes pedigree function
         "Age" = X8, # Age (years)
         "Diabetes"= X9 # Diabetes (0=no or 1=yes)
         ) 
#3.scatterplot
# a on b , a versus b; a is y, b is x ; plasma glucose on age. 
pima |>
  ggplot(aes(x = Age, y = Glucose, color = factor(Diabetes))) +
  geom_point() +
  labs(color = 'Diabetes' )  # use diabetes as factor with 0 or 1.
#comment
#NO. I think Diabetes is not easy to classify.
#Because the pots of Diabetes which is 0 and 1 are mixed together , and there are extremely datas which age is 40 but diabetes is 1,
#so we could not find a clearly Decision Boundary for all datas.
#So the result of regression will not be good.

# ----------------------------------------------------------Assignment 3.2----------------------------------------------------------
#1.build a model
train_model <- glm(Diabetes ~ Glucose + Age, family = binomial, data = pima)
#2. use r = 0.5 as threshold to predict and classification.
pima$Prob_tm <- predict(train_model, type = 'response') # use response to predict probability(0-1).
pima$Pred_class <- ifelse(pima$Prob_tm > 0.5, 1, 0) #use r=0.5 as the standard of prediction.
#3. report the probabilistic equation
#summary(train_model)  
#Log-odds equation : Logit(P(Diabetes=1)) = -5.912449 + 0.035644 * Glucose + 0.024778 * Age   
#4. compute misclassification error
miserror <- mean( pima$Diabetes != pima$Pred_class) #use sum to calculate 'number', use mean to calculate 'rate'.
print(miserror)
#5. scatter plot 
pima |>
  ggplot(aes(x = Age, y = Glucose, color = factor(Pred_class))) +
  geom_point() +
  labs(color = 'Prediction Class')
#COMMENT on Model Quality:
#The training misclassification error is 0.263 (approximately 26.3%), which demonstrates that the model has some classification capability. However, this error rate leaves room for improvement, as a notable portion of observations are still misclassified.
#From the scatter plot colored by predicted classes, we can observe a general distribution difference. The classification boundary generally tilts from the top-left to the bottom-right. Nevertheless, there is significant overlap between the two classes in the region where Glucose is approximately 125–150 and Age is 20–50. This indicates that the model fails to fully distinguish between the two classes.
#Additionally, the presence of Glucose = 0 observations in the dataset likely represents missing or invalid measurements. This data quality issue requires further inspection and appropriate preprocessing in future steps.
#Overall, while the current model exhibits basic classification capability, both the linear boundary limitation and the underlying data quality issues indicate that further optimization is required.

# ----------------------------------------------------------Assignment 3.3----------------------------------------------------------
#3.3.a setting the log-odds to 0, 0 = -5.912449 + 0.035644 * Glucose + 0.024778 * Age   
#the equation of the decision boundary is : Glucose = 5.912449/0.035644 - 0.024778/0.035644 *Age, Glucose= 165.875 - 0.695*Age
#3.3.b
b0 = coef(train_model)[1] #beta0
b1 = coef(train_model)[2] #beta1
b2 = coef(train_model)[3] #beta2
pima |>
  ggplot(aes(x = Age, y = Glucose, color = factor(Pred_class))) +
    geom_point() +
    geom_abline(
      intercept =  - b0/ b1, # - beta0/beta1
      slope = - b2/ b1) + # - beta2/beta1
    labs(color = 'Prediction Class')
#COMMENT
#From the figure, it can be seen that the model divides the two classes using a linear decision boundary. 
#Observations above and to the right of the boundary are predicted as Class 1, while those below and to the left are predicted as Class 0. 
#However, there is significant overlap between the two types of observations in certain areas, indicating that the model cannot fully distinguish between the two classes.

# ----------------------------------------------------------Assignment 3.4----------------------------------------------------------
#the probability is Prob_tm.
#use r = 0.2 as thresholds.
pima$Pred_class_r2 <- ifelse(pima$Prob_tm > 0.2, 1, 0)
pima |>
  ggplot(aes(x = Age, y = Glucose, color = factor(Pred_class_r2))) +
  geom_point() +
  labs(color = "Prediction Class\n(r=0.2)")
#use r = 0.8 as thresholds.
pima$Pred_class_r8 <- ifelse(pima$Prob_tm > 0.8, 1, 0) 
pima |>
  ggplot(aes(x = Age, y = Glucose, color = factor(Pred_class_r8))) +
  geom_point() +
  labs(color = 'Prediction Class\n(r=0.8)')  
#COMMENT
#From the results, we can observe that a lower threshold (r=0.2) makes the model more sensitive to predicting Class 1, thereby reducing Class 0 predictions. 
#Conversely, a higher threshold (r=0.8) makes it stricter, leading to more Class 0 predictions.

# ----------------------------------------------------------Assignment 3.5----------------------------------------------------------
#1.computing features z1,z2,z3,z4,z5, adding to the data set
pima_z <- 
  O_pima |>
  rename('Pregnancies' = X1, #Number_of_times_pregnant
         "Glucose" = X2, # Plasma glucose concentration a 2 hours in an oral glucose tolerance test.
         "BP" = X3, # Diastolic blood pressure (mm Hg)
         "Skinthickness" = X4, # Triceps skinfold thickness (mm)
         "Insulin" = X5, # 2-Hour serum insulin (mu U/ml).
         "BMI" = X6, # Body mass index (weight in kg/(height in m)^2)
         "Pedigree" = X7, # Diabetes pedigree function
         "Age" = X8, # Age (years)
         "Diabetes"= X9 # Diabetes (0=no or 1=yes)
  ) 
pima_z$z1 = pima_z$Glucose ^ 4
pima_z$z2 = pima_z$Glucose ^ 3 * pima_z$Age
pima_z$z3 = pima_z$Glucose ^ 2 * pima_z$Age ^ 2
pima_z$z4 = pima_z$Glucose * pima_z$Age ^ 3
pima_z$z5 = pima_z$Age ^4
#2.perform a basis function expansion trick to compute a new model.
train_model_z <- glm(Diabetes ~ Glucose + Age + z1 + z2 + z3 + z4 + z5, 
                     family = binomial, 
                     data = pima_z)
pima_z$Pred_tm <- predict(train_model_z, type = 'response') # use response to predict probability 0-1.
pima_z$Pred_class_z <- ifelse(pima_z$Pred_tm > 0.5, 1, 0) # use r = 0.5
#3.create a scatterplot
pima_z |>
  ggplot(aes(x = Age, y = Glucose, color = factor(Pred_class_z))) +
  geom_point() +
  labs(color = 'Prediction Class\n(r=0.5)')
#4.compute misclassification rate.
miserror_z <- mean(pima_z$Diabetes != pima_z$Pred_class_z)
print(miserror_z)
#COMMENT
#After introducing the five new basis function features, the model underwent a significant change: 
#the decision boundary changed from a straight line (tilting from top-left to bottom-right) to a curved (non-linear) boundary. This allows the model to more flexibly separate the regions for Class 1 and Class 0.
#At the same time, the model's misclassification error dropped from approximately 0.263 to 0.245, 
#indicating a reduction in prediction errors compared to the standard logistic regression model.