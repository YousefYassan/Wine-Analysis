# 1)DATA CLEANING
library(dplyr)
library(shiny)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(scatterplot3d)
# import data
df_red <- read.csv('C:\\Users\\Laser\\Downloads\\project\\winequality-red.csv')
df_white <- read.csv('C:\\Users\\Laser\\Downloads\\project\\winequality-white.csv')
# dataframe dimensions before
dim(df_red)
dim(df_white)
# check duplicates
sum(duplicated(df_red))
sum(duplicated(df_white))
# remove duplicates
library(dplyr)
df_red <- distinct(df_red)
df_white <- distinct(df_white)
# check datatype of each column in red wine --> all ok
column_names <- names(df_red)
for (col in column_names) {
  print(is.numeric(df_red[[col]]))
}
# check datatype of each column in white wine --> all ok
column_names <- names(df_white)
for (col in column_names) {
  print(is.numeric(df_white[[col]]))
}
# check for null values
sum(is.na(df_red))
sum(is.na(df_white))
# dataframe dimensions after
dim(df_red)
dim(df_white)

whitewine <- df_white
redwine <- df_red





# 2)BASIC VISUALS

df_red$color <- 'red'
df_red

df_white$color <- 'white'
df_white

total_wine <- rbind(df_red, df_white)
total_wine %>% sample_frac(0.2)


fixed_acidity <- data.frame(total_wine %>% group_by(color) %>% summarize(fixed.acidity = mean(fixed.acidity)))
volatile_acidity <- data.frame(total_wine %>% group_by(color) %>% summarize(volatile.acidity = mean(volatile.acidity)))
citric_acid <- data.frame(total_wine %>% group_by(color) %>% summarize(citric.acid = mean(citric.acid)))
residual_sugar <- data.frame(total_wine %>% group_by(color) %>% summarize(residual.sugar = mean(residual.sugar)))
chlorides <- data.frame(total_wine %>% group_by(color) %>% summarize(chlorides = mean(chlorides)))
free_sulfur_dioxide <- data.frame(total_wine %>% group_by(color) %>% summarize(free.sulfur.dioxide = mean(free.sulfur.dioxide)))
total_sulfur_dioxide <- data.frame(total_wine %>% group_by(color) %>% summarize(total.sulfur.dioxide = mean(total.sulfur.dioxide)))
density <- data.frame(total_wine %>% group_by(color) %>% summarize(density = mean(density)))
pH <- data.frame(total_wine %>% group_by(color) %>% summarize(pH = mean(pH)))
sulphates <- data.frame(total_wine %>% group_by(color) %>% summarize(sulphates = mean(sulphates)))
alcohol <- data.frame(total_wine %>% group_by(color) %>% summarize(alcohol = mean(alcohol)))
quality <- data.frame(total_wine %>% group_by(color) %>% summarize(quality = mean(quality)))


barplot(fixed_acidity$fixed.acidity, names.arg = fixed_acidity$color, col = c("red", "blue"), main = "Mean Fixed Acidity by color", xlab = "Wine color", ylab = 'Mean Fixed Acidity')
barplot(volatile_acidity$volatile.acidity, names.arg = volatile_acidity$color, col = c("red", "blue"), main = "Mean Volatile Acidity by color", xlab = "Wine color", ylab = 'Mean Volatile Acidity')
barplot(citric_acid$citric.acid, names.arg = citric_acid$color, col = c("red", "blue"), main = "Mean Citric Acid by color", xlab = "Wine color", ylab = 'Mean Citric Acid')
barplot(residual_sugar$residual.sugar, names.arg = residual_sugar$color, col = c("red", "blue"), main = "Mean Residual Sugar by color", xlab = "Wine color", ylab = 'Mean Residual Sugar')
barplot(chlorides$chlorides, names.arg = chlorides$color, col = c("red", "blue"), main = "Mean chlorides by color", xlab = "Wine color", ylab = 'Mean chlorides')
barplot(free_sulfur_dioxide$free.sulfur.dioxide, names.arg = free_sulfur_dioxide$color, col = c("red", "blue"), main = "Mean Free Sulfur Dioxide by color", xlab = "Wine color", ylab = 'Mean Free Sulfur Dioxide')
barplot(total_sulfur_dioxide$total.sulfur.dioxide, names.arg = total_sulfur_dioxide$color, col = c("red", "blue"), main = "Mean Total Sulfur Dioxide by color", xlab = "Wine color", ylab = 'Mean Total Sulfur Dioxide')
barplot(density$density, names.arg = density$color, col = c("red", "blue"), main = "Mean Density by color", xlab = "Wine color", ylab = 'Mean Density')
barplot(pH$pH, names.arg = pH$color, col = c("red", "blue"), main = "Mean pH by color", xlab = "Wine color", ylab = 'Mean pH')
barplot(sulphates$sulphates, names.arg = sulphates$color, col = c("red", "blue"), main = "Mean Sulphates by color", xlab = "Wine color", ylab = 'Mean Sulphates')
barplot(alcohol$alcohol, names.arg = alcohol$color, col = c("red", "blue"), main = "Mean Alcohol by color", xlab = "Wine color", ylab = 'Mean Alcohol')
barplot(quality$quality, names.arg = quality$color, col = c("red", "blue"), main = "Mean Quality by color", xlab = "Wine color", ylab = 'Mean Quality')

##############################################################

wtree<-rpart(quality ~ chlorides + volatile.acidity + fixed.acidity + alcohol+sulphates+pH+ density+total.sulfur.dioxide+free.sulfur.dioxide +residual.sugar +citric.acid, data =whitewine)

rpart.plot(wtree)

whitewine_rules <- rpart.rules (wtree)

whitewine_rules

wtree

##############################################################

Rtree<-rpart (quality ~ chlorides + volatile.acidity + fixed.acidity + alcohol+sulphates+pH+density+total.sulfur.dioxide+free.sulfur.dioxide +residual.sugar +citric.acid, data = redwine)

rpart.plot(Rtree)

redwine_rules <- rpart.rules (Rtree)

redwine_rules

Rtree

##############################################################

wtree<-rpart(quality ~ chlorides + volatile.acidity + fixed.acidity + alcohol+sulphates+pH+density+ total.sulfur.dioxide+free.sulfur.dioxide + residual.sugar +citric.acid, data= whitewine)

variable_importance_white <- wtree$variable.importance

variable_importance_white

if (is.data.frame(variable_importance_white)) {  variable_importance_white$importance_scores <- as.numeric(as.character(variable_importance_white$importance_scores));

} else { # If 'variable_importance' is a vector, create a data frame
  
  variable_importance_white <- data.frame(variables = names (variable_importance_white), importance_scores = as.numeric(variable_importance_white));
}
variable_importance_white$importance_scores <- abs (variable_importance_white$importance_scores)

colors <- rainbow (nrow(variable_importance_white));

S<-sum(variable_importance_white$importance_scores)

pie(variable_importance_white$importance_scores,labels = paste (variable_importance_white$variables, "(", round((variable_importance_white$importance_scores/S) * 100, 2), "%)")  ,col = colors , main = "Variable Importance")

################################################################

Rtree <- rpart(quality ~ chlorides + volatile.acidity + fixed.acidity + alcohol+sulphates+pH+density+total.sulfur.dioxide+free.sulfur.dioxide +residual.sugar +citric.acid, data = redwine)

variable_importance_red <- Rtree$variable.importance

variable_importance_red

if (is.data.frame(variable_importance_red)) { # Convert importance_scores to numeric
  
  variable_importance_red$importance_scores <- as.numeric(as.character(variable_importance_red$importance_scores))
  
} else { # If 'variable_importance' is a vector, create a data frame
  
  variable_importance_red <- data.frame(variables = names (variable_importance_red), importance_scores = as.numeric(variable_importance_red));  }

variable_importance_red$importance_scores <- abs (variable_importance_red$importance_scores)

colors <- rainbow(nrow(variable_importance_red));

pie(variable_importance_red$importance_scores,
    labels = paste (variable_importance_red$variables, "(", round((variable_importance_red$importance_scores/ S) * 100, 2), "%)"), col = colors,
    main = "Variable Importance")

###################################################################

ALLWINES<-rbind(whitewine, redwine)

winetree<-rpart (quality ~ chlorides + volatile.acidity + fixed.acidity + alcohol+sulphates+pH+density+total.sulfur.dioxide+free.sulfur.dioxide +residual.sugar +citric.acid, data = ALLWINES)

variable_importance <- winetree$variable.importance

variable_importance


if ( is.data.frame(variable_importance)) { # Convert importance_scores to numeric
  
  variable_importance$importance_scores <- as.numeric(as.character(variable_importance$importance_scores));
  
} else { # If 'variable_importance' is a vector, create a data frame
  
  variable_importance <- data.frame(variables = names (variable_importance), importance_scores = as.numeric(variable_importance));
  
}

variable_importance$importance_scores <- abs (variable_importance$importance_scores)

colors <- rainbow (nrow (variable_importance)); S<-sum(variable_importance$importance_scores)
pie(variable_importance$importance_scores,
    labels = paste (variable_importance$variables, "(", round((variable_importance$importance_scores/S)* 100, 2), "%)"), col = colors,
    main = "Variable Importance")

############################################################################################

data <- ALLWINES[, c("alcohol", "density", "quality")]

kmeans_result <- kmeans (data, centers = 3, nstart = 20)
library(ggplot2)

ggplot(ALLWINES, aes (x = alcohol, y = density, color = factor (kmeans_result$cluster)))+

geom_point()+
  
  labs (title = "Cluster Assignment Based on Alcohol and Density", x = "Alcohol",
        
        y = "Density") +
  
  theme_minimal()


############################################################################################
ALLWINES <- rbind(whitewine, redwine)

 filtered_data <- subset (ALLWINES, ALLWINES$alcohol <= 14.5 & ALLWINES$density < 1.01)

 kmeans_result <- kmeans (filtered_data, centers = 3, nstart = 20)

ggplot(filtered_data, aes (x = alcohol, y = density, color = factor (kmeans_result$cluster))) +


geom_point() +

labs (title = "Cluster Assignment Based on Alcohol and Density",

x = "Alcohol",

y = "Density") +

theme_minimal()
############################################################################################
plot(x = redwine$residual.sugar,
     
     y = redwine$quality ,
     
     main = "effect of residual suger on Quality",
     
     
     xlab = "residual suger",
     
     ylab = "Quality",
     
     col="red" );

 plot(x = whitewine$residual.sugar ,
       
       
       y = whitewine$quality ,
       
       main = "effect of residual suger on Quality",
       
       xlab = "residual suger",
       
       ylab = "Quality",
       
       col="blue" )

############################################################################################
plot(x = redwine$alcohol,
     y = redwine$quality,
     
     main = "effect of Alcohol on Quality",
     
     xlab = "Alcohol",
     
     ylab = "Quality",
     
     col="red" )

plot(x = whitewine$alcohol,
     y = whitewine$quality,
     
     main = "effect of Alcohol on Quality",
     
     xlab = "Alcohol",
     
     ylab = "Quality",
     
     col="blue" )
############################################################################################
boxplot(quality ~ sulphates, data = whitewine, main = "Boxplot of Quality by sulphates",
        
        xlab = "Sulphates",
        
        ylab = "Quality",
        
        col = "blue");

boxplot(quality ~ sulphates, data = redwine,
        
        main = "Boxplot of Quality by sulphates",
        
        xlab = "Sulphates",
        
        ylab = "Quality",
        
        col = "red")
############################################################################################
plot(redwine$density, redwine$quality,
     
     type = "l",
     
     main = "effect of density on Quality", 
       
     xlab = "Density",
     
     ylab = "Quality",
     
     col = "red",
     
     lwd = 2);

plot(whitewine$density, whitewine$quality,
     type = "l",
     
     main = "effect of density on Quality",
     
     xlab = "Density",
     
     ylab = "Quality",
     
     col ="lightblue", 
     lwd = 2)
##############################################################################################
attributes_for_clustering <- ALLWINES[,!(names (ALLWINES) %in% c("quality"))]

kmeans_result <- kmeans (attributes_for_clustering, centers = 3)

ALLWINES$cluster <- kmeans_result$cluster

scatterplot3d(attributes_for_clustering$pH, attributes_for_clustering$volatile.acidity, ALLWINES$quality,
                                     
pch= 16, main = "3D Scatter Plot with Clustering",

xlab = "pH", ylab = "Volatile Acidity", zlab = "Quality", color = ALLWINES$cluster)

##############################################################################################

attributes_for_clustering <- ALLWINES[, ! (names (ALLWINES) %in% c("quality"))]
kmeans_result <- kmeans (attributes_for_clustering, centers = 3)

ALLWINES$cluster <- kmeans_result$cluster

scatterplot3d(attributes_for_clustering$pH, attributes_for_clustering$volatile.acidity, ALLWINES$quality,
                                     
pch= 16, main = "3D Scatter Plot with Clustering",
                                     
xlab= "pH", ylab = "Volatile Acidity", zlab = "Quality",
                                     
color = ALLWINES$cluster )

 scatterplot3d(attributes_for_clustering$free.sulfur.dioxide, attributes_for_clustering$fixed.acidity, ALLWINES$quality,
                                                                              
 pch = 16, main = "3D Scatter Plot with Clustering",
                                                                              
 xlab = "free sulphur dioxide", ylab = "fixed Acidity", zlab = "Quality",
                                                                              
 color = ALLWINES$cluster)

