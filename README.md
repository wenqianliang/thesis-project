# thesis-project
#############area_segmentation+fpca+SDR##########################
##################################################################
##################################################################
setwd("~/R data")
load("fpcascore118.rdata")
load("newstatus118.rdata")
#rename#
X <- fpcascore118
Y <- newstatus118
rownames(X) <- paste('obs', 1:dim(X)[1])
colnames(X) <- paste('feature', 1:dim(X)[2])

####################install all the package required##############################
#SDR#
setwd("~/R code")
source("SDR.class.NET.R")
#Support Vector Machines#
install.packages("e1071")
library(e1071)
#permute#
install.packages("permute")
require(permute)
#stringr#
install.packages("stringr", repos='http://cran.us.r-project.org')
library(stringr)


###########5-folders###################
######Compute the obs for each fold

install.packages("ipred")
require(ipred)
##kfoldcv(5, 118, nlevel=NULL)
tableY <-as.numeric(table(Y))
Y0<-kfoldcv(5, tableY[1], nlevel=NULL)
Y1<-kfoldcv(5, tableY[2], nlevel=NULL)

###################### 5 folder----stratified CV#########################
#stratify and order the obs#
tableY <-as.numeric(table(Y))
SX <- X
SY <- Y
Stotal <- as.data.frame(cbind(SX, SY))
Stotal$SY <- as.factor(Stotal$SY)
Stotalsort <- Stotal[order(SY),]
Stotal0 <- Stotalsort[1:tableY[1], ]
Stotal1 <- Stotalsort[(tableY[1]+1):dim(X)[1], ]
#shuffle the obs#
set.seed(1234)
S0 <- shuffle(tableY[1])
S1 <- shuffle(tableY[2])
Stotal0 <- as.data.frame(cbind(Stotal0,S0))
Stotal1 <- as.data.frame(cbind(Stotal1,S1))
Stotal0sort <- Stotal0[order(S0),]
Stotal1sort <- Stotal1[order(S1),]
##########delete the sorted column##
Stotal0sort <- Stotal0sort[,1:(dim(X)[2]+1)]
Stotal1sort <- Stotal1sort[,1:(dim(X)[2]+1)]
#rename1sort to fit the following combination
#names(Stotal1sort)[98] <- "Sa" 
##############CV dataset#
Slisttest <- list()
Slistvalida <- list()
Slisttrain <- list()

Slisttest[[1]] <- as.data.frame(rbind(Stotal0sort[1:9,],Stotal1sort[1:15,]))
Slistvalida[[1]] <- as.data.frame(rbind(Stotal0sort[10:18,],Stotal1sort[16:30,]))
Slisttrain[[1]] <- as.data.frame(rbind(Stotal0sort[19:45,],Stotal1sort[31:73,]))

Slisttest[[2]] <- as.data.frame(rbind(Stotal0sort[10:18,],Stotal1sort[16:30,]))
Slistvalida[[2]] <- as.data.frame(rbind(Stotal0sort[19:27,],Stotal1sort[31:45,]))
Slisttrain[[2]] <- as.data.frame(rbind(Stotal0sort[28:45,],Stotal1sort[46:73,],
                                       Stotal0sort[1:9,],Stotal1sort[1:15,]))

Slisttest[[3]] <- as.data.frame(rbind(Stotal0sort[19:27,],Stotal1sort[31:45,]))
Slistvalida[[3]] <- as.data.frame(rbind(Stotal0sort[28:36,],Stotal1sort[46:59,]))
Slisttrain[[3]] <- as.data.frame(rbind(Stotal0sort[37:45,],Stotal1sort[60:73,],
                                       Stotal0sort[1:18,],Stotal1sort[1:30,]))

Slisttest[[4]] <- as.data.frame(rbind(Stotal0sort[28:36,],Stotal1sort[46:59,]))
Slistvalida[[4]] <- as.data.frame(rbind(Stotal0sort[37:45,],Stotal1sort[60:73,]))
Slisttrain[[4]] <- as.data.frame(rbind(Stotal0sort[1:27,],Stotal1sort[1:45,]))

Slisttest[[5]] <- as.data.frame(rbind(Stotal0sort[37:45,],Stotal1sort[60:73,]))
Slistvalida[[5]] <- as.data.frame(rbind(Stotal0sort[1:9,],Stotal1sort[1:15,]))
Slisttrain[[5]] <- as.data.frame(rbind(Stotal0sort[10:36,],Stotal1sort[16:59,]))

#####verify the number in each list#####
dimtest<-matrix(NA,nrow=5,ncol=2)
dimvalida<-matrix(NA,nrow=5,ncol=2)
dimtrain<-matrix(NA,nrow=5,ncol=2)
for (i in 1:5)
{
  dimtest[i,]<-dim(Slisttest[[i]])
  dimvalida[i,]<-dim(Slistvalida[[i]])
  dimtrain[i,]<-dim(Slisttrain[[i]])
}
dimtest
dimvalida
dimtrain

############save datafile##############
Slisttest118all <-Slisttest
Slisttrain118all <-Slisttrain
Slistvalida118all <-Slistvalida

save(Slisttest118all,file="Slisttest118all.rdata")
save(Slisttrain118all,file="Slisttrain118all.rdata")
save(Slistvalida118all,file="Slistvalida118all.rdata")
###########run data in server##################
####################install all the package required##############################
#SDR#
install.packages("e1071")
#Support Vector Machines#
setwd("~/R code")
source("SDR.class.NET.R")
#stringr#
install.packages("stringr", repos='http://cran.us.r-project.org')
library(e1071)
library("stringr")

################read data##################
setwd("~/R data")
load("Slisttest118.rdata")
load("Slisttrain118.rdata")
load("Slistvalida118.rdata")
load("fpcascore118.rdata")

X <- fpcascore118
Slisttest <-Slisttest118
Slisttrain <-Slisttrain118
Slistvalida <-Slistvalida118

######################################################################
#######################################################
b <- array(NA,c(length(lambda),length(delta)))
b<-list(b,b,b,b,b)

for (a in 1:5)
{
  for (l in 1:length(lambda))
  {
    for (d in 1:length(delta))
    {
      if (class(Ssortblist_1[[a]][[l]][[d]])== "data.frame")
        b[[a]][l,d]<-"ture"
    }
  }
}
b


b <- array(NA,c(length(lambda),length(delta)))
b<-list(b,b,b,b,b)

for (a in 1:5)
{
  for (l in 1:length(lambda))
  {
    for (d in 1:length(delta))
    {
      if (class(v[[a]][[l]][[d]])== "matrix")
        b[[a]][l,d]<-"ture"
    }
  }
}
b
class(Ssortblist_1[[1]][[1]][[1]])

###check 2/5#####
dim(Slisttrain[[5]])
dim(listdata)
##########################try different parameters@##############
###################################################

lambda <- c(0.01,0.02,0.03)
delta <- c(1,3)
length(lambda)
length(delta)
lambda
delta

v<-list()####number of delta
v<-list(v,v,v)###number of lambda
v<-list(v,v,v,v,v)

for (a in 1:5)
{
  for (l in 1:length(lambda))
  {
    for (d in 1:length(delta))
    {
      listdata <- Slisttrain[[a]]
      X <- listdata[ ,1:dim(X)[2]]
      Y <- listdata$SY
      #SDR#
      v[[a]][[l]][[d]]<- try(class_SDR(X, Y, lambda=lambda[l],delta=delta[d], max.diff=1e-6, iteration=1000))
    }
  }
}
#####save####
v_dwtpcascore354<-v
setwd("~/R data")
save(v_dwtpcascore354,file="v_dwtpcascore354.rdata")

#####################set sortblist##########################
################################################################
S<- vector("list",2)##length of delta###
S<- list(S,S,S) ##length of lambda###
Ssortblist_1<- list(S,S,S,S,S)

for (a in 1:5)
{
  for (l in 1:length(lambda))
  {
    for (d in 1:length(delta))
    {
      if (class(v[[a]][[l]][[d]]) == "matrix")
      {
        zb <- v[[a]][[l]][[d]]
        z.name <- rownames(zb)
        z.beta <-cbind(z.name,zb[,1])
        z.beta <- as.data.frame(z.beta, stringsAsFactors=FALSE)
        Ssortblist_1[[a]][[l]][[d]] <- z.beta[rev(order(abs(as.numeric(z.beta[,2])))),]
        Ssortblist_1[[a]][[l]][[d]]$feature <- str_extract(string=Ssortblist_1[[a]][[l]][[d]]$z.name, pattern="\\d+")
        Ssortblist_1[[a]][[l]][[d]]$feature <- as.numeric(Ssortblist_1[[a]][[l]][[d]]$feature)
        Ssortblist_1[[a]][[l]][[d]]$lambda <- lambda[l]
        Ssortblist_1[[a]][[l]][[d]]$delta <- delta[d]
      }
    }
  }
}


na <- rep(0, 30)###########change the feature???
sr <- data.frame(accvalida1=na, senvalida1=na, spevalida1=na, feature=na, lambda=na, delta=na)
sr <- list(sr,sr)###number of delta
sr <- list(sr,sr,sr)###number of lambda
Sresultvalida <- list(fold1=sr, fold2=sr, fold3=sr, fold4=sr, fold5=sr)

##############################################################################
for (a in 1:5)
{
  for (l in 1:length(lambda))
  {
    for (d in 1:length(delta))
    {
      if(!is.null(Ssortblist_1[[a]][[l]][[d]]))
      {
        for (p in 1:30)###########
        {
          zsort11 <- Ssortblist_1[[a]][[l]][[d]][1:p,]
          f <- c(zsort11$feature)
          f <- c(f,(dim(X)[2]+1))
          trainsvm1 <- Slisttrain[[a]][ ,f]
          set.seed(1234)
          tune.out <- tune(svm, SY~., data=trainsvm1,  kernel = "linear", ranges=list(cost = c(0.001,0.01,0.1,1,10,100)))
          bestmodel <- tune.out$best.model
          #predict function
          predvalida <- predict(bestmodel, Slistvalida[[a]][ ,f])
          tablevalida1 <- as.matrix(table(predvalida, true=Slistvalida[[a]]$SY))
          accvalida1 <- (tablevalida1[1,1]+tablevalida1[2,2])/sum(tablevalida1)
          spevalida1 <- (tablevalida1[1,1]/(tablevalida1[1,1] + tablevalida1[2,1]))
          senvalida1 <- (tablevalida1[2,2]/(tablevalida1[1,2] + tablevalida1[2,2]))
          feature <- p
          lam <- lambda[l]
          del <- delta[d]
          Sresultvalida[[a]][[l]][[d]][p, ] <- as.data.frame(cbind(accvalida1,senvalida1, spevalida1,feature,lam,del)) 
        }
      }
    }
  }
}


############save file#########
Sresultvalida_30_dwtpcascore354<-Sresultvalida################change the name#########
save(Sresultvalida_30_dwtpcascore354,file="Sresultvalida_30_dwtpcascore354.rdata")

##################################################################################################
###########################get the maximum parameters##############################################

li <- rep(0, 2)####change 2 to number of delta###############
li <- data.frame(accvalida1=li, senvalida1=li, spevalida1=li, feature=li, lambda=li, delta=li)
Sdelta<-list(li,li,li)###########number of lambda###################
Sdelta<-list(Sdelta,Sdelta,Sdelta,Sdelta,Sdelta)
for (a in 1:5)
{
  for(l in 1:length(lambda))
  {
    for (d in 1:length(delta))
    {
      Sdelta[[a]][[l]][d,]<-Sresultvalida[[a]][[l]][[d]][which.max(Sresultvalida[[a]][[l]][[d]]$accvalida1),]
    }
  }
}


li <- rep(0, 3)####change 3 to number of lambda###############
li <- data.frame(accvalida1=li, senvalida1=li, spevalida1=li, feature=li, lambda=li, delta=li)
Slambda<-list(li,li,li,li,li)
for (a in 1:5)
{
  for (l in 1:length(delta))
  {
    sort<-order(Sdelta[[a]][[l]]$feature)
    Sdelta[[a]][[l]]<-Sdelta[[a]][[l]][sort,]
    Slambda[[a]][l,]<-Sdelta[[a]][[l]][which.max(Sdelta[[a]][[l]]$accvalida1),]
  }
}

li <- rep(0, 5)
Spara<-data.frame(accvalida1=li, senvalida1=li, spevalida1=li, feature=li, lambda=li, delta=li)
for (a in 1:5)
{
  sort<-order(Slambda[[a]]$feature)
  Slambda[[a]]<-Slambda[[a]][sort,]
  Spara[a,]<-Slambda[[a]][which.max(Slambda[[a]]$accvalida1),]
}

Spara



###########################################use para in trainning and test###########################
Ssortb<-list()
for (a in 1:5)
{
  Ssortb[[a]]<-Ssortblist_1[[a]][[which(lambda==Spara[a,]$lambda)]][[which(delta==Spara[a,]$delta)]]
}

#####################################################################
############################fixed feature##################################################
na <- rep(0, 5)
Sresulttest <- data.frame(acctest1=na, sentest1=na, spetest1=na, feature=na, lambda=na, delta=na)
Sresulttrain <- data.frame(acctrain1=na, sentrain1=na, spetrain1=na, feature=na, lambda=na, delta=na)

for(a in 1:5)
{
  zsort11 <- Ssortb[[a]][1:Spara$feature[a],]
  f <- c(zsort11$feature)
  f <- c(f,(dim(X)[2]+1))
  trainsvm1 <- Slisttrain[[a]][ ,f]
  set.seed(1234)
  tune.out <- tune(svm, SY~., data=trainsvm1,  kernel = "linear", ranges=list(cost = c(0.001,0.01,0.1,1,10,100)))
  bestmodel <- tune.out$best.model
  #predict on train#
  predtrain <- predict(bestmodel, Slisttrain[[a]][ ,f])
  tabletrain1 <- as.matrix(table(predtrain, true=Slisttrain[[a]]$SY))
  acctrain1 <- (tabletrain1[1,1]+tabletrain1[2,2])/sum(tabletrain1)
  spetrain1 <- (tabletrain1[1,1]/(tabletrain1[1,1] + tabletrain1[2,1]))
  sentrain1 <- (tabletrain1[2,2]/(tabletrain1[1,2] + tabletrain1[2,2]))
  feat <- Spara$feature[a]
  lam <- Spara$lambda[a]
  del <-Spara$delta[a]
  Sresulttrain[a, ] <- as.data.frame(cbind(acctrain1, sentrain1, spetrain1, feat,lam,del))
  #predict on test data#
  predtest <- predict(bestmodel, Slisttest[[a]][ ,f])
  tabletest1 <- as.matrix(table(predtest, true=Slisttest[[a]]$SY))
  acctest1 <- (tabletest1[1,1]+tabletest1[2,2])/sum(tabletest1)
  spetest1 <- (tabletest1[1,1]/(tabletest1[1,1] + tabletest1[2,1]))
  sentest1 <- (tabletest1[2,2]/(tabletest1[1,2] + tabletest1[2,2]))
  feat <- Spara$feature[a]
  lam <- Spara$lambda[a]
  del <-Spara$delta[a]
  Sresulttest[a, ] <- as.data.frame(cbind(acctest1,sentest1, spetest1, feat,lam,del))
}

round(Sresulttrain,digits=3)
round(Sresulttest,digits=3)
round(Spara,digits=3)


colMeans(Sresulttrain)
colMeans(Sresulttest)
colMeans(Spara)


















###########################LDA################################################

library(MASS)

na <- rep(0, 30)###########change the feature???
sr <- data.frame(accvalida1=na, senvalida1=na, spevalida1=na, feature=na, lambda=na, delta=na)
sr <- list(sr,sr)###number of delta
sr <- list(sr,sr,sr)###number of lambda
Sresultvalida <- list(fold1=sr, fold2=sr, fold3=sr, fold4=sr, fold5=sr)

##############################################################################
for (a in 3:5)
{
  for (l in 1:length(lambda))
  {
    for (d in 1:length(delta))
    {
      if(!is.null(Ssortblist_1[[a]][[l]][[d]]))
      {
        for (p in 1:30)###########
        {
          zsort11 <- Ssortblist_1[[a]][[l]][[d]][1:p,]
          f <- c(zsort11$feature)
          f <- c(f,(dim(X)[2]+1))
          trainsvm1 <- Slisttrain[[a]][ ,f]
          set.seed(1234)
          fit<-lda(SY~.,data=trainsvm1)
          #predict function
          predvalida <- predict(fit, Slistvalida[[a]][ ,f])$class
          tablevalida1 <- as.matrix(table(predvalida, true=Slistvalida[[a]]$SY))
          accvalida1 <- (tablevalida1[1,1]+tablevalida1[2,2])/sum(tablevalida1)
          spevalida1 <- (tablevalida1[1,1]/(tablevalida1[1,1] + tablevalida1[2,1]))
          senvalida1 <- (tablevalida1[2,2]/(tablevalida1[1,2] + tablevalida1[2,2]))
          feature <- p
          lam <- lambda[l]
          del <- delta[d]
          Sresultvalida[[a]][[l]][[d]][p, ] <- as.data.frame(cbind(accvalida1,senvalida1, spevalida1,feature,lam,del)) 
        }
      }
    }
  }
}


li <- rep(0, 2)####change 2 to number of delta###############
li <- data.frame(accvalida1=li, senvalida1=li, spevalida1=li, feature=li, lambda=li, delta=li)
Sdelta<-list(li,li,li)###########number of lambda###################
Sdelta<-list(Sdelta,Sdelta,Sdelta,Sdelta,Sdelta)
for (a in 1:5)
{
  for(l in 1:length(lambda))
  {
    for (d in 1:length(delta))
    {
      Sdelta[[a]][[l]][d,]<-Sresultvalida[[a]][[l]][[d]][which.max(Sresultvalida[[a]][[l]][[d]]$accvalida1),]
    }
  }
}


li <- rep(0, 3)####change 3 to number of lambda###############
li <- data.frame(accvalida1=li, senvalida1=li, spevalida1=li, feature=li, lambda=li, delta=li)
Slambda<-list(li,li,li,li,li)
for (a in 1:5)
{
  for (l in 1:length(delta))
  {
    sort<-order(Sdelta[[a]][[l]]$feature)
    Sdelta[[a]][[l]]<-Sdelta[[a]][[l]][sort,]
    Slambda[[a]][l,]<-Sdelta[[a]][[l]][which.max(Sdelta[[a]][[l]]$accvalida1),]
  }
}

li <- rep(0, 5)
Spara<-data.frame(accvalida1=li, senvalida1=li, spevalida1=li, feature=li, lambda=li, delta=li)
for (a in 1:5)
{
  sort<-order(Slambda[[a]]$feature)
  Slambda[[a]]<-Slambda[[a]][sort,]
  Spara[a,]<-Slambda[[a]][which.max(Slambda[[a]]$accvalida1),]
}

Spara
###########################################use para in trainning and test###########################
Ssortb<-list()
for (a in 1:5)
{
  Ssortb[[a]]<-Ssortblist_1[[a]][[which(lambda==Spara[a,]$lambda)]][[which(delta==Spara[a,]$delta)]]
}

na <- rep(0, 5)
Sresulttest <- data.frame(acctest1=na, sentest1=na, spetest1=na, feature=na, lambda=na, delta=na)
Sresulttrain <- data.frame(acctrain1=na, sentrain1=na, spetrain1=na, feature=na, lambda=na, delta=na)

for(a in 1:5)
{
  zsort11 <- Ssortb[[a]][1:Spara$feature[a],]
  f <- c(zsort11$feature)
  f <- c(f,(dim(X)[2]+1))
  trainsvm1 <- Slisttrain[[a]][ ,f]
  set.seed(1234)
  fit<-lda(SY~.,data=trainsvm1)
  #predict on train#
  predtrain <- predict(fit, Slisttrain[[a]][ ,f])$class
  tabletrain1 <- as.matrix(table(predtrain, true=Slisttrain[[a]]$SY))
  acctrain1 <- (tabletrain1[1,1]+tabletrain1[2,2])/sum(tabletrain1)
  spetrain1 <- (tabletrain1[1,1]/(tabletrain1[1,1] + tabletrain1[2,1]))
  sentrain1 <- (tabletrain1[2,2]/(tabletrain1[1,2] + tabletrain1[2,2]))
  feat <- Spara$feature[a]
  lam <- Spara$lambda[a]
  del <-Spara$delta[a]
  Sresulttrain[a, ] <- as.data.frame(cbind(acctrain1, sentrain1, spetrain1, feat,lam,del))
  #predict on test data#
  predtest <- predict(fit, Slisttest[[a]][ ,f])$class
  tabletest1 <- as.matrix(table(predtest, true=Slisttest[[a]]$SY))
  acctest1 <- (tabletest1[1,1]+tabletest1[2,2])/sum(tabletest1)
  spetest1 <- (tabletest1[1,1]/(tabletest1[1,1] + tabletest1[2,1]))
  sentest1 <- (tabletest1[2,2]/(tabletest1[1,2] + tabletest1[2,2]))
  feat <- Spara$feature[a]
  lam <- Spara$lambda[a]
  del <-Spara$delta[a]
  Sresulttest[a, ] <- as.data.frame(cbind(acctest1,sentest1, spetest1, feat,lam,del))
}

round(Sresulttrain,digits=3)
round(Sresulttest,digits=3)
round(Spara,digits=3)


colMeans(Sresulttrain)
colMeans(Sresulttest)
colMeans(Spara)




##################################without FPCA#################


rawdata <- function(img,seg,id){
  ###generate the segment label
  marker <- which(seg==id);
  ###generate the mask to filter out the segment for each individual
  mask <- array(0,dim=dim(seg));
  mask[marker] <- 1;
  
  
  ###compute the image of segments##########
  img_mask <- array(0,dim=dim(img));
  for (i in 1:dim(img)[4]){
    img_mask[,,,i] <- img[,,,i] * mask; 
  }
  img_mask <- img_mask + 0.0001;
  
  rlt_score <- FPCA_3D_score(img_mask)[[2]];
  rlt_value <- FPCA_3D_score(img_mask)[[1]];
  return(list("score"=rlt_score,"value"=rlt_value))
}


img<-sarray[,,,20]
marker<-which(seg_8==8)
mask<-array(0,dim=dim(seg_8))
mask[marker]<-1
img_mask<-array(0,dim=dim(img))
img_mask<-img*mask
s<-as.vector(which(img_mask!=0))
head(s)
length(s)
length(img)

length(marker)

###################################wavelets PCA############
install.packages("wavelets")
library("wavelets")

dwtscore118<-matrix(ncol=62010,nrow=118)
for (r in 1:118)
{
  img<-sarray[,,1,r]
  dwt<-dwt(img,n.levels=3)
  dwt.align<-align(dwt)
  dwtscore_45<-t(dwt.align@V[[3]])
  for (i in 2:45)
{
  img<-sarray[,,i,1]
  dwt<-dwt(img,n.levels=3)
  dwt.align<-align(dwt)
  dwtscore<-t(dwt.align@V[[3]])
  dwtscore_45<-cbind(dwtscore_45,dwtscore)
  }
  dwtscore118[r,]<-dwtscore_45
}

save(dwtscore118,file="dwtscore118.rdata")

pca118<-prcomp(dwtscore118)
pcavector118<-pca118$rotation
pcascore118<-dwtscore118%*%pcavector118

save(pcascore118,file="pcascore118.rdata")

#########add segmentation##############
mask_dwtpca <- function(img,seg,id){
  mask <- array(0,dim=dim(seg));
  marker <- which(seg==id)
  mask[marker] <- 1
  ###compute the image of segments##########
  img_mask <- array(0,dim=dim(img));
  for (i in 1:dim(img)[4]){
    img_mask[,,,i] <- img[,,,i] * mask; 
  }
  
  dwtscore118<-matrix(ncol=62010,nrow=118)
  for (r in 1:118)
  {
    img<-sarray[,,1,r]
    dwt<-dwt(img,n.levels=3)
    dwt.align<-align(dwt)
    dwtscore_45<-t(dwt.align@V[[3]])
    for (i in 2:45)
    {
      img<-sarray[,,i,1]
      dwt<-dwt(img,n.levels=3)
      dwt.align<-align(dwt)
      dwtscore<-t(dwt.align@V[[3]])
      dwtscore_45<-cbind(dwtscore_45,dwtscore)
    }
    dwtscore118[r,]<-dwtscore_45
  }
  pca118<-prcomp(dwtscore118)
  pcavector118<-pca118$rotation
  pcascore118<-dwtscore118%*%pcavector118
  return(pcascore118)
}

dwtpca4<-mask_dwtpca(sarray,seg_8,4)
dwtpca5<-mask_dwtpca(sarray,seg_8,5)
dwtpca8<-mask_dwtpca(sarray,seg_8,8)

dwtpcascore354 <- cbind(dwtpca4,dwtpca5,dwtpca8)
dim(dwtpcascore354)
save(dwtpcascore354,file="dwtpcascore354.rdata")
#######scaled 118
dwtscore118<-matrix(ncol=62010,nrow=118)
for (r in 1:118)
{
  img<-sarray[,,1,r]
  dwt<-dwt(img,n.levels=3)
  dwt.align<-align(dwt)
  dwtscore_45<-t(dwt.align@V[[3]])
  for (i in 2:45)
  {
    img<-sarray[,,i,1]
    dwt<-dwt(img,n.levels=3)
    dwt.align<-align(dwt)
    dwtscore<-t(dwt.align@V[[3]])
    dwtscore_45<-cbind(dwtscore_45,dwtscore)
  }
  dwtscore118[r,]<-dwtscore_45
}

pca118<-prcomp(dwtscore118)
pcavector118<-pca118$rotation
dwtpcascore118<-dwtscore118%*%pcavector118
save(dwtpcascore118,file="dwtpcascore118.rdata")

summary(tpca)
dim(tpca$rotation)

pca118$scores[1,]
summary(pca118)
print(pca118)
plot(pca118)
dim(pca118$rotation)


######################without Sparse SDR##############################
setwd("~/R data")
load("Slisttest_354_sorted.rdata")
load("Slisttrain_354_sorted.rdata")
load("Slistvalida_354_sorted.rdata")
load("fpcascore8_region_354_sortedfeature.rdata")

X <- fpcascore8_region_354_sortedfeature
Slisttest <-Slisttest_354_sorted
Slisttrain <-Slisttrain_354_sorted
Slistvalida <-Slistvalida_354_sorted


na <- rep(0, 354)###########change the feature???
sr <- data.frame(accvalida1=na, senvalida1=na, spevalida1=na, feature=na)
Sresultvalida <- list(fold1=sr, fold2=sr, fold3=sr, fold4=sr, fold5=sr)
train <- data.frame(acctrain1=na, sentrain1=na, spetrain1=na, feature=na)
Sresulttrain <- list(fold1=train, fold2=train, fold3=train, fold4=train, fold5=train)

##############################################################################
for (a in 1:5)
{
        for (p in 1:354)###########
        {
          f <- c(1:p,(dim(X)[2]+1))
          trainsvm1 <- Slisttrain[[a]][ ,f]
          set.seed(1234)
          tune.out <- tune(svm, SY~., data=trainsvm1,  kernel = "linear", ranges=list(cost = c(0.001,0.01,0.1,1,10,100)))
          bestmodel <- tune.out$best.model
          #predict function
          predvalida <- predict(bestmodel, Slistvalida[[a]][ ,f])
          tablevalida1 <- as.matrix(table(predvalida, true=Slistvalida[[a]]$SY))
          accvalida1 <- (tablevalida1[1,1]+tablevalida1[2,2])/sum(tablevalida1)
          spevalida1 <- (tablevalida1[1,1]/(tablevalida1[1,1] + tablevalida1[2,1]))
          senvalida1 <- (tablevalida1[2,2]/(tablevalida1[1,2] + tablevalida1[2,2]))
          feature <- p
          Sresultvalida[[a]][p, ] <- as.data.frame(cbind(accvalida1,senvalida1, spevalida1,feature)) 
          }
}


Sresulttrain[[1]]
          #predict on train#
          predtrain <- predict(bestmodel, Slisttrain[[a]][ ,f])
          tabletrain1 <- as.matrix(table(predtrain, true=Slisttrain[[a]]$SY))
          acctrain1 <- (tabletrain1[1,1]+tabletrain1[2,2])/sum(tabletrain1)
          spetrain1 <- (tabletrain1[1,1]/(tabletrain1[1,1] + tabletrain1[2,1]))
          sentrain1 <- (tabletrain1[2,2]/(tabletrain1[1,2] + tabletrain1[2,2]))
          feat <- Sresultvalidamax$feature[a]
          Sresulttrain[[a]][p, ] <- as.data.frame(cbind(acctrain1, sentrain1, spetrain1, feat))

          
          
li <- rep(0, 5)
Sresultvalidamax <- data.frame(accvalida1=li, senvalida1=li, spevalida1=li, feature=li)
for (a in 1:5)
{
      Sresultvalidamax[a,]<-Sresultvalida[[a]][which.max(Sresultvalida[[a]]$accvalida1),]
}
Sresultvalidamax


na <- rep(0, 5)
Sresulttest <- data.frame(acctest1=na, sentest1=na, spetest1=na, feature=na)
Sresulttrain <- data.frame(acctrain1=na, sentrain1=na, spetrain1=na, feature=na)

for(a in 1:5)
{
  f <- c(Sresultvalidamax$feature[a],(dim(X)[2]+1))
  trainsvm1 <- Slisttrain[[a]][ ,f]
  set.seed(1234)
  tune.out <- tune(svm, SY~., data=trainsvm1,  kernel = "linear", ranges=list(cost = c(0.001,0.01,0.1,1,10,100)))
  bestmodel <- tune.out$best.model
  #predict on train#
  predtrain <- predict(bestmodel, Slisttrain[[a]][ ,f])
  tabletrain1 <- as.matrix(table(predtrain, true=Slisttrain[[a]]$SY))
  acctrain1 <- (tabletrain1[1,1]+tabletrain1[2,2])/sum(tabletrain1)
  spetrain1 <- (tabletrain1[1,1]/(tabletrain1[1,1] + tabletrain1[2,1]))
  sentrain1 <- (tabletrain1[2,2]/(tabletrain1[1,2] + tabletrain1[2,2]))
  feat <- Sresultvalidamax$feature[a]
  Sresulttrain[a, ] <- as.data.frame(cbind(acctrain1, sentrain1, spetrain1, feat))
  #predict on test data#
  predtest <- predict(bestmodel, Slisttest[[a]][ ,f])
  tabletest1 <- as.matrix(table(predtest, true=Slisttest[[a]]$SY))
  acctest1 <- (tabletest1[1,1]+tabletest1[2,2])/sum(tabletest1)
  spetest1 <- (tabletest1[1,1]/(tabletest1[1,1] + tabletest1[2,1]))
  sentest1 <- (tabletest1[2,2]/(tabletest1[1,2] + tabletest1[2,2]))
  feat <- Sresultvalidamax$feature[a]
  Sresulttest[a, ] <- as.data.frame(cbind(acctest1,sentest1, spetest1, feat))
}


round(Sresulttrain,digits=3)
round(Sresulttest,digits=3)
round(Sresultvalidamax,digits=3)

colMeans(Sresulttrain)
colMeans(Sresulttest)
colMeans(Sresultvalidamax)

##################without Sparse SDR and validation######################
setwd("~/R data")
load("Slisttest_354_cv.rdata")
load("Slisttrain_354_cv.rdata")
load("fpcascore8_region_354.rdata")

X <- fpcascore8_region_354
Slisttest <-Slisttest_354_cv
Slisttrain <-Slisttrain_354_cv

na <- rep(0, 30)###########change the feature???
train <- data.frame(acctrain1=na, sentrain1=na, spetrain1=na, feature=na)
Sresulttrain <- list(fold1=train, fold2=train, fold3=train, fold4=train, fold5=train)

##############################################################################
for (a in 1:5)
{
  for (p in 1:30)###########
  {
    f <- c(1:p,(dim(X)[2]+1))
    trainsvm1 <- Slisttrain[[a]][ ,f]
    set.seed(1234)
    tune.out <- tune(svm, SY~., data=trainsvm1,  kernel = "linear", ranges=list(cost = c(0.001,0.01,0.1,1,10,100)))
    bestmodel <- tune.out$best.model
    #predict on train#
    predtrain <- predict(bestmodel, Slisttrain[[a]][ ,f])
    tabletrain1 <- as.matrix(table(predtrain, true=Slisttrain[[a]]$SY))
    acctrain1 <- (tabletrain1[1,1]+tabletrain1[2,2])/sum(tabletrain1)
    spetrain1 <- (tabletrain1[1,1]/(tabletrain1[1,1] + tabletrain1[2,1]))
    sentrain1 <- (tabletrain1[2,2]/(tabletrain1[1,2] + tabletrain1[2,2]))
    feat <- p
    Sresulttrain[[a]][p, ] <- as.data.frame(cbind(acctrain1, sentrain1, spetrain1, feat))
  }
}
Sresulttrain[[1]]

li <- rep(0, 5)
Sresulttrainmax <- data.frame(acctrain1=li, sentrain1=li, spetrain1=li, feature=li)
for (a in 1:5)
{
  Sresulttrainmax[a,]<-Sresulttrain[[a]][which.max(Sresulttrain[[a]]$acctrain1),]
}
Sresulttrainmax


na <- rep(0, 5)
Sresulttest <- data.frame(acctest1=na, sentest1=na, spetest1=na, feature=na)

for(a in 1:5)
{
  f <- c(Sresultvalidamax$feature[a],(dim(X)[2]+1))
  trainsvm1 <- Slisttrain[[a]][ ,f]
  set.seed(1234)
  tune.out <- tune(svm, SY~., data=trainsvm1,  kernel = "linear", ranges=list(cost = c(0.001,0.01,0.1,1,10,100)))
  bestmodel <- tune.out$best.model
  #predict on test data#
  predtest <- predict(bestmodel, Slisttest[[a]][ ,f])
  tabletest1 <- as.matrix(table(predtest, true=Slisttest[[a]]$SY))
  acctest1 <- (tabletest1[1,1]+tabletest1[2,2])/sum(tabletest1)
  spetest1 <- (tabletest1[1,1]/(tabletest1[1,1] + tabletest1[2,1]))
  sentest1 <- (tabletest1[2,2]/(tabletest1[1,2] + tabletest1[2,2]))
  feat <- Sresulttrainmax$feature[a]
  Sresulttest[a, ] <- as.data.frame(cbind(acctest1,sentest1, spetest1, feat))
}


round(Sresulttrainmax,digits=3)
round(Sresulttest,digits=3)

colMeans(Sresulttrainmax)
colMeans(Sresulttest)



