library(lavaan)
#Importing the csv data 
#data<-read.csv(file.choose())
data<-read.csv("C:/Users/rmaul/Downloads/SQJ/new_Working_Data_E.csv")
#Count of Rows and columns
dim(data)
View(data)

# Print all variable names in a data frame
names(data)
df <- data[,c("PR","ET","CC","CI", "OE","RI","OW","NI","SI","FI","PF")]

cov_mat <- cov(df, use = "complete.obs")
det(cov_mat)

cov_mat <- cov(df, use = "pairwise.complete.obs")
det(cov_mat)

#sample variance-covariance matrix 
cov(df)
#The difference in the magnitude between the variables is huge and it is suggested to scale the variables. 
## Scale the whole data.frame
df <- apply(df,  2, scale)
summary(df)

#cov_mat <- cov(df[sapply(df, is.numeric)])
#det(cov_mat)

# determinant of covariance matrix
#det(cov(df))

#Correlation matrix
round(cor(df[,]),2)

#PR (Priority) is missing (“Undecided”)
colSums(is.na(df))                                   # missing values per column
sapply(df, function(x) sd(x, na.rm = TRUE))          # 0 (or NA) = constant column

#visual correlation matrix
#library(corrplot)
#par(mar= c(0.1,0.1,0.1,0.1))
#corrplot(cor(df[,]), order = "original", tl.col='black', tl.cex=.5) 

#Pairwise-complete observations is the same approach used for the manuscript’s correlation tables: bugs 
#without a priority are left out only of the correlations that involve PR.

library(corrplot)
while (!is.null(dev.list())) dev.off()          # close any device left open by earlier plots

d <- read.csv("C:/Users/rmaul/Downloads/SQJ/new_Working_Data_E.csv")
d <- as.data.frame(d)
keep <- vapply(d, function(v) is.numeric(v) && isTRUE(sd(v, na.rm = TRUE) > 0), logical(1))
print(names(d)[!keep])                           # columns dropped (should be none for Eclipse)

x <- d[, keep, drop = FALSE]
M <- cor(x, method = "spearman", use = "pairwise.complete.obs")
print(dim(M))                                    # 11 x 11

par(mar = c(0.1, 0.1, 0.1, 0.1))
corrplot(M, order = "original", tl.col = "black", tl.cex = 0.7)

# # it creates the share variables and renames PR to Pr.
prep <- function(f){
  d <- read.csv(f); names(d)[names(d)=="PR"] <- "Pr"; pairs <- d$CC*(d$CC-1)/2
  d$sNI <- d$NI/pairs; d$sFI <- d$FI/pairs; d$sSI <- d$SI/pairs; d$sRI <- d$RI/pairs; d$PFpp <- d$PF/pairs
  tr <- function(x){ q <- quantile(x,.99,na.rm=TRUE); log1p(pmin(pmax(x,0),q)) }
  for (v in c("ET","CC","CI","OW","OE","RI","PF","PFpp")) d[[v]] <- tr(d[[v]])
  as.data.frame(scale(d[,c("ET","Pr","CC","CI","OW","OE","PFpp","sNI","sFI","sSI","sRI")]))
}

df <- prep("C:/Users/rmaul/Downloads/SQJ/new_Working_Data_E.csv")     
names(df)                                    # check: ET Pr CC CI OW OE PFpp sNI sFI sSI sRI

m <- '
ATC =~ CC + CI
IDC =~ l*OW + l*OE
ITC =~ sNI + sFI
sRI ~ a*PFpp + ITC
ET  ~ ATC + IDC + ITC + b*sRI + c*PFpp + Pr
ind := a*b
total := c + a*b
ATC ~~ PFpp + Pr
IDC ~~ PFpp + Pr
ITC ~~ PFpp + Pr
PFpp ~~ Pr
'

fit <- sem(m, data = df, estimator = "MLR", missing = "ml", fixed.x = FALSE) #missing = "ml" is important for OpenStack, Without it, lavaan silently drops every bug with a missing priority: 21% of OpenStack.
#fixed.x = FALSE is needed so that FIML can use cases where the exogenous Pr is missing.

summary(fit, standardized = TRUE, fit.measures = TRUE)
fitMeasures(fit, c("chisq.scaled", "df.scaled", "cfi.robust", "tli.robust", "rmsea.robust", "srmr"))
standardizedSolution(fit)                    # standardised paths, ind and total, with CIs
lavInspect(fit, "cor.lv")                    # latent correlations (cov.lv gives covariances)

#install.packages("semPlot")   # only needed once
library(semPlot)              # needed in every new R session, before semPaths()

semPaths(fit, "std", edge.label.cex = 1.0, curvePivot = FALSE, rotation = 3)

library(semPlot)

# fixed positions (x, y): ET top left, RI bottom left, PF between them,
# constructs in the middle, Priority and the indicators on the right
pos <- list(ET   = c(-1, 1),  PFpp = c(-0.55, -0.05), sRI = c(-1, -1),
            ATC  = c(0, 0.62), IDC = c(0.32, 0.02),      ITC = c(0, -0.58),
            Pr   = c(1, 1),    CC  = c(1, 0.72),  CI  = c(1, 0.46),
            OW   = c(1, 0.12), OE  = c(1, -0.14),
            sNI  = c(1, -0.5), sFI = c(1, -0.76))

# semPaths orders the nodes in its own way, so read that order first
g0 <- semPaths(fit, what = "std", intercepts = FALSE, residuals = TRUE,
               thresholds = FALSE, nCharNodes = 0, DoNotPlot = TRUE)

nodes <- g0$graphAttributes$Nodes$names
if (is.null(nodes) || length(nodes) == 0) nodes <- g0$graphAttributes$Nodes$labels
nodes <- as.character(nodes)
print(nodes)        # should now show PFpp, not PFp

missing <- setdiff(nodes, names(pos))
if (length(missing)) stop("No position given for: ", paste(missing, collapse = ", "))
lay <- do.call(rbind, lapply(nodes, function(n) pos[[n]]))                              # should list all 13: CC CI OW OE PFpp Pr ET sRI sNI sFI ATC IDC ITC



#lay <- do.call(rbind, lapply(nodes, function(n) pos[[n]]))   # always a (number of nodes) x 2 matrix
stopifnot(nrow(lay) == length(nodes), ncol(lay) == 2)

# short labels for the share variables
lab <- c(sNI = "NI", sFI = "FI", sRI = "RI", PFpp = "PF")
nodeLab <- ifelse(nodes %in% names(lab), lab[nodes], nodes)

png("fig_sem_Eclipse.png", width = 6.5, height = 5, units = "in", res = 300)   # or pdf("fig_sem.pdf", 6.5, 5)
semPaths(fit, what = "std", whatLabels = "std", layout = lay, nodeLabels = nodeLab,
         style = "lisrel", intercepts = FALSE, residuals = TRUE, thresholds = FALSE, exoCov = TRUE,
         posCol = "darkgreen", negCol = "red", fixedStyle = 1, freeStyle = 1, fade = FALSE,
         edge.label.cex = 0.55, edge.label.position = 0.6, edge.label.bg = TRUE,
         sizeMan = 5.5, sizeMan2 = 3.2, sizeLat = 8, sizeLat2 = 5, nCharNodes = 0, label.cex = 0.9,
         residScale = 6, curvature = 1.5, mar = c(4, 6, 4, 6), edge.width = 0.9, esize = 4)
dev.off()

getwd()                      # the folder where fig_sem.png was written
file.exists("fig_sem_Eclipse.png")   # TRUE if it was saved

f <- "C:/Users/rmaul/OneDrive/Documents/fig_sem_Eclipse.png"
file.exists(f)          # should print TRUE
shell.exec(f)           # opens it in the Windows Photos app

#install.packages("png")   # once
library(png)
img <- readPNG("C:/Users/rmaul/Downloads/SQJ/fig_sem_Eclipse.png")
plot.new(); par(mar = c(0, 0, 0, 0))
rasterImage(img, 0, 0, 1, 1)

## all three: files <- c(Eclipse = "new_Working_Data_E.csv", OpenStack = "new_Working_Data_O.csv",
#                       Qt = "new_Working_Data_Qt.csv")

files <- c(Eclipse = "new_Working_Data_E.csv")

P <- list(); FIT <- list(); LV <- list(); REL <- list(); LOAD <- list(); LCOR <- list()
data_dir <- "C:/Users/rmaul/Downloads/SQJ"


stopifnot(all(file.exists(file.path(data_dir, files))))

## 1. reset the result lists (run every time before the loop)
P <- list(); FIT <- list(); LV <- list(); REL <- list(); LOAD <- list(); LCOR <- list()

## 2. the loop (unchanged, with data_dir instead of dir)
for (ds in names(files)) {
  cat("Estimating", ds, "...\n")
  z <- prep(file.path(data_dir, files[ds]))
  f <- sem(m, data=z, estimator="MLR", missing="ml", fixed.x=FALSE)
  s <- standardizedSolution(f)
  s <- subset(s, op %in% c("~",":="))
  s$path <- ifelse(s$op=="~", paste(s$rhs,"->",s$lhs), s$lhs)
  s$z <- s$est.std/s$se
  P[[ds]] <- data.frame(dataset=ds, path=s$path, beta=s$est.std, se=s$se, z=s$z, p=s$pvalue, lo=s$ci.lower, hi=s$ci.upper)
  fm <- fitMeasures(f, c("chisq.scaled","df.scaled","pvalue.scaled","cfi.robust","tli.robust","rmsea.robust",
                         "rmsea.ci.lower.robust","rmsea.ci.upper.robust","srmr"))
  r2 <- lavInspect(f,"rsquare")
  FIT[[ds]] <- data.frame(dataset=ds, N=nobs(f), converged=lavInspect(f,"converged"),
                          admissible=isTRUE(tryCatch(lavInspect(f,"post.check"), warning=function(w) FALSE)),
                          as.list(round(fm,3)), R2_ET=round(unname(r2["ET"]),3), R2_sRI=round(unname(r2["sRI"]),3))
  ss <- standardizedSolution(f)
  ld <- subset(ss, op == "=~")
  LOAD[[ds]] <- data.frame(dataset=ds, construct=ld$lhs, indicator=ld$rhs, lambda=ld$est.std, se=ld$se,
                           z=ld$z, p=ld$pvalue, lo=ld$ci.lower, hi=ld$ci.upper)
  lvn <- c("ATC","IDC","ITC")
  lc <- subset(ss, op == "~~" & lhs %in% c(lvn,"PFpp","Pr") & rhs %in% c(lvn,"PFpp","Pr") & lhs != rhs)
  LCOR[[ds]] <- data.frame(dataset=ds, a=lc$lhs, b=lc$rhs, r=lc$est.std, se=lc$se, p=lc$pvalue)
  cl <- lavInspect(f,"cor.lv"); LV[[ds]] <- c(ATC_IDC=cl["ATC","IDC"], ATC_ITC=cl["ATC","ITC"], IDC_ITC=cl["IDC","ITC"])
  st <- lavInspect(f,"std"); th <- diag(st$theta)
  for (k in c("ATC","IDC","ITC")) { l <- st$lambda[st$lambda[,k]!=0,k]; e <- th[names(l)]
  REL[[paste(ds,k)]] <- data.frame(dataset=ds, construct=k, l1=l[1], l2=l[2], CR=sum(l)^2/(sum(l)^2+sum(e)), AVE=mean(l^2)) }
  cat("  done:", ds, "\n")
}

## 3. check that the loop produced results, then combine ONCE

if (length(P) == 0) stop("The loop produced no results - names(files) is: ", paste(names(files), collapse = ", "))

P    <- do.call(rbind, P);    FIT  <- do.call(rbind, FIT);  REL <- do.call(rbind, REL)
LOAD <- do.call(rbind, LOAD); LCOR <- do.call(rbind, LCOR); LV  <- do.call(rbind, LV)

print(FIT); print(P, digits = 3, row.names = FALSE); print(LOAD, digits = 3, row.names = FALSE)
print(REL, digits = 3, row.names = FALSE); print(LCOR, digits = 3, row.names = FALSE); print(round(LV, 3))

out <- file.path(data_dir, "eclipse_")
write.csv(P,    paste0(out, "paths.csv"),       row.names = FALSE)
write.csv(FIT,  paste0(out, "fit.csv"),         row.names = FALSE)
write.csv(LOAD, paste0(out, "loadings.csv"),    row.names = FALSE)
write.csv(REL,  paste0(out, "reliability.csv"), row.names = FALSE)
write.csv(LCOR, paste0(out, "exog_cor.csv"),    row.names = FALSE)
write.csv(LV,   paste0(out, "latent_cor.csv"))