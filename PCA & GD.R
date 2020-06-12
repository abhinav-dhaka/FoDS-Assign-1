#### PCA & GD on mtcars dataset ####
mtcars # to get and load this dataset
dim(mtcars) #gives dim of this dataset
str(mtcars) #gives structure of this dataset
mtcars.pca <-prcomp(mtcars[,c(1:7,10,11)], center = TRUE, scale. = TRUE) 
#performs PCA on the datset #columns 8 & 9 are not taken
summary(mtcars.pca) # gives result of prcomp function carried out in line 5
#PC1 and PC2 together make up for almost 86% variance in the data  
#for most cases the cars can be categorised just on the basis of these two attributes instead of all 11 
#This drastically reduces computational effort and time
mtcars_red <- mtcars[,c(1:7,10,11)]
dim(mtcars_red)
cor_var <- cor(mtcars_red, use="pairwise.complete.obs") 
print(cor_var) # a high corr filter. There is also a low var filter
# cyl and disp, wt and disp are highly corel one of them can be dropped in eda
mtcars_new = tibble(x1 = mtcars$disp, x2 = mtcars$hp, x3 = mtcars$wt, y = mtcars$mpg)
#tibble creates new data set of predictor and target variables 
gradDesc=GD(mtcars_new, alpha = 0.0000001, maxIter = 10000, seed = NULL)
print(gradDesc)
minigrad=MBGD(mtcars_new, alpha = 0.0000001, maxIter = 10000, seed = NULL)
print(minigrad)
sgd=SGD(mtcars_new, alpha = 0.0000001, maxIter = 10000, seed = NULL)
print(sgd)
#to compare which on the above three algorithms works the best RMSE function is used
GD_pred = prediction(gradDesc, mtcars_new[,-4])[,4]# gives value of mpg predicted by GD
GD_RMSE = RMSE(mtcars_new$y, GD_pred)# calculates RMSE btw true and predicted value of mpg
MiniGD_pred = prediction(minigrad, mtcars_new[,-4])[,4] 
MiniGD_RMSE = RMSE(mtcars_new$y, MiniGD_pred)
SGD_pred = prediction(sgd, mtcars_new[,-4])[,4] 
SGD_RMSE = RMSE( mtcars_new$y , SGD_pred )
print(GD_RMSE)
print(MiniGD_RMSE) 
print(SGD_RMSE)