library(lavaan)
#Importing the csv data 

setwd("path/folder")

data<-read.csv("Folder/workingdataO.csv")

#Count of Rows and columns
dim(data)

View(data)

# Print all variable names in a data frame
names(data)

df1 <- data[,c("ET","CC","CI","OE","OW","NI","SI","RI","FI")]

# remove the first column from data matrix
dfN <- df[, -1]

# perform principal component analysis
dfN.pca <- prcomp(dfN,center = TRUE, scale = TRUE)
par(mar= c(0.1,0.1,0.1,0.1))
plot(dfN.pca, type ="l", main="PCA Variance Explained", cex.main = 0.7) # this is called scree plot
summary(dfN.pca)  # gives the data from the scree plot

df1 <- apply(df1,  2, scale)
summary(df1)

#Correlation matrix
round(cor(df1[,]),2)


#Principal component Analysis can be done on numerical data only
data.pca <- prcomp(workingdata[,c(1:5)], center = TRUE,scale. = TRUE)
summary(data.pca)

# Let's call str() to have a look at your PCA object.
str(data.pca)

require(lavaan)

model1 <- '
             ATC =~ CC + CI
            IDC =~ OE +  OW
            ITC =~ NI + FI + SI 
            RI ~  CI + SI
            ET ~~ OW
            RI ~~  CC
            ITC ~~ CC
            NI ~~ FI
            ET ~  AC +  IDC + ITC + RI
            '

#View model           
cat(model1) 
fit1 <- sem(model1, data=df1, std.lv=TRUE , estimator="DWLS")

summary(fit1, standardize = T, fit.measures = TRUE)
fitMeasures(fit1, c("chisq","df","cfi", "rmsea", "srmr"))


inspect(fit1,'r2') # variance explained

lavInspect(fit1, "cov.lv")

lavInspect(fit1, "theta")

inspect(fit1,"sampstat")# sample means and covariance matrix

library(MASS)
options(digits=3)
resid(fit1, type ="cor")

library("semPlot")
library("psych")
par(mar= c(0.1,0.1,0.1,0.1))
semPaths(fit1,what='std',
         rotation = 4, equalizeManifests = FALSE, optimizeLatRes = TRUE, node.width = 1.5,
         edge.width = 1, shapeMan = "rectangle", shapeLat = "ellipse", 
         shapeInt = "triangle", sizeMan = 6, sizeInt = 6, sizeLat = 10, 
         unCol = "#070b8c",
         
         node.label.cex=5,
         edge.label.cex=1.25, curvePivot = TRUE, 
         fade=FALSE)


