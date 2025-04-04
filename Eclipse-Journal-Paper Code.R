library(lavaan)
#Importing the csv data 

setwd(" ") #Set the path of Working Directory

data<-read.csv("workingdataE.csv " )
#Set the path for the csv (database) file
#Count of Rows and columns
dim(data)
View(data)
# Print all variable names in a data frame
names(data)

df1 <- data[,c("ET","CC","Pr","CI","OE","OW","NI","SI","RI","FI")]

# remove the first column from data matrix
dfN <- df[, -1]

#The difference in the magnitude between the variables is huge and it is suggested to scale the variables. 

df1 <- apply(df1,  2, scale)


summary(df1)

modelE<-'
          ATC =~  CC + CI 
          IDC =~   OE + OW + PR
          ITC =~   SI + NI + FI 
          RI ~ CI + SI 
          ET ~~ OW
          ATC ~~ Pr
          OE ~~ CI
          ITC ~~ PR
          FI ~~ NI
          ET ~  ATC + IDC + ITC + RI'

fitE <- sem(modelE, data=df1, std.lv=TRUE , estimator="DWLS")

summary(fitE, standardized = TRUE, fit.measures = TRUE)

fitMeasures(fitE, c("chisq","df","cfi", "rmsea", "srmr"))
#chisq     df    cfi  rmsea   srmr 


resid(fitE, "cor")

library("semPlot")
library("psych")
semPaths(fitE,what='std',
         rotation = 4, equalizeManifests = FALSE, optimizeLatRes = TRUE, node.width = 1.5,
         edge.width = 1, shapeMan = "rectangle", shapeLat = "ellipse", 
         shapeInt = "triangle", sizeMan = 6, sizeInt = 6, sizeLat = 10, 
         unCol = "#070b8c",
         
         node.label.cex=5,
         edge.label.cex=1.25, curvePivot = TRUE, 
         fade=FALSE)

lavCor(fitE) #residuals

inspect(fitE,'r2') # variance explained

lavInspect(fitE, "cov.lv") #Covariance matrix of latent variables

inspect(fitE,"sampstat")# sample means and covariance matrix

parameterEstimates(fitE)  # for relationship estimation

#_________Indirect Effect _______

modelM<-'
          ATC =~  CC + CI + Pr
          IDC =~   OE + OW + Pr
          ITC =~   SI + NI + FI 
          RI ~  a1 *CI + a2 *SI 
          ET ~~ OW
          ATC ~~ Pr
          OE ~~ CI
          ITC ~~ PR
          FI ~~ NI
          ET ~  b1 *ATC + b2* IDC + b3* ITC + b4*RI
          indirect1:=a1*b4
          indirect2:=a2*b4
          overallindirect:= indirect1 + indirect2
          total:=overallindirect + b1 + b2 + b3
'

fitmod <- sem(modelM, data=df1)
#summarize Sobel test or Delta
summary(fitmod,fit.measures=TRUE, rsquare= T)
