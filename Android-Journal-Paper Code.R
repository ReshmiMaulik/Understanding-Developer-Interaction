library(lavaan)
#Importing the csv data 

setwd(" ") #Set the path of Working Directory
#url=""https://raw.githubusercontent.com/ReshmiMaulik/Understanding-Developer-Interaction/main/Data/workingdataA.csv"

data<-read.csv("workingdataA.csv " )
#data <- read.csv(url)
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
            IDC =~   OE + Pr + OW 
            ITC =~   SI + NI + FI 
            CI ~~ Pr
            SI ~~ RI            
            RI~  CC + CI + OE
            ET ~ ATC + IDC + ITC + RI
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


#----Mediation analysis---

#Indirect Effect
modelM <- '
            ATC =~    CC + CI 
            IDC =~ OE + 1 * OW
            ITC =~ NI + FI + SI 
            RI ~ a1* CI + a2*SI
            NI ~~ SI
            SI ~~ FI
            IDC ~~ ATC
            ITC ~~ CC
            ET ~  b1* ATC + b2* IDC + b3* ITC + b4*RI
            
           indirect1:=a1*b4
           indirect2:=a2*b4
           overallindirect:= indirect1 + indirect2
           total:=overallindirect + b1 + b2 + b3
            '

fitmod <- sem(modelM, data=df1)
#summarize Sobel test or Delta
summary(fitmod,fit.measures=TRUE, rsquare= T)
