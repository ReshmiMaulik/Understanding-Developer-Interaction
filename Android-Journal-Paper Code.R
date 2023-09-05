library(lavaan)
#Importing the csv data 

setwd(" ") #Set the path of Working Directory

data<-read.csv("workingdataA.csv " )
#Set the path for the csv (database) file
#Count of Rows and columns
dim(data)
View(data)
# Print all variable names in a data frame
names(data)

df1 <- data[,c("ET","CC","CI","OE","OW","NI","SI","RI","FI")]

# remove the first column from data matrix
dfN <- df[, -1]

#The difference in the magnitude between the variables is huge and it is suggested to scale the variables. 

df1 <- apply(df1,  2, scale)


summary(df1)

modelA <- '
             ATC =~  CC + CI 
            IDC =~   OE + 1* OW 
            ITC =~   SI + NI + FI 
            NI ~~ SI
            SI ~~ FI
            IDC ~~ AC
            ITC ~~ CC
            RI~  CI + SI
            ET ~ AC + IDC + ITC + RI
            '
fitA <- sem(modelA, data=df1, std.lv=TRUE , estimator="DWLS")


summary(fitA, standardized = TRUE, fit.measures = TRUE)

fitMeasures(fitA, c("chisq","df","cfi", "rmsea", "srmr"))
#chisq     df    cfi  rmsea   srmr 


resid(fitA, "cor")

library("semPlot")

semPaths(fitA,what='std',
         rotation = 4, equalizeManifests = FALSE, optimizeLatRes = TRUE, node.width = 1.5,
         edge.width = 1, shapeMan = "rectangle", shapeLat = "ellipse", 
         shapeInt = "triangle", sizeMan = 6, sizeInt = 6, sizeLat = 10, 
         unCol = "#070b8c",
         
         node.label.cex=5,
         edge.label.cex=1.25, curvePivot = TRUE, 
         fade=FALSE)

lavCor(fitA) #residuals

inspect(fitA,'r2') # variance explained

lavInspect(fitA, "cov.lv") #Covariance matrix of latent variables

inspect(fitA,"sampstat")# sample means and covariance matrix
