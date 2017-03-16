
f <- as.formula(paste('garisktot ~', paste(colnames(HHdata)[20:43], collapse='+')))
modelAllHexSubscales <- lm(f, HHdata)





mydata<-mtcars #mtcars is the data in R

dep<-c("mpg~","cyl~","disp~") # list of unique dependent variables with ~ 
indep1<-c("hp","drat","wt")  # list of first unique independent variables 
indep2<-c("qsec","vs","am") # list of second unique independent variables 
myvar<-cbind(dep,indep1,indep2) # matrix of variables
myvar
 



for (i in 1:dim(myvar)[1]){
  print(paste("This is", i, "regression", "with dependent var",gsub("~","",myvar[i,1])))
  k[[i]]<-lm(as.formula(paste(myvar[i,1],paste(myvar[i,2:3],collapse="+"))),mydata)
  print(k[[i]])
}


dep<-list("mpg~","cyl~","disp~") # list of unique dependent variables with ~ 
indep1<-list("hp","drat","wt")  # list of first unique independent variables 
indep2<-list("qsec","vs","am") # list of second unique independent variables
Map(function(x,y,z) lm(as.formula(paste(x,paste(list(y,z),collapse="+"))),data=mtcars),dep,indep1,indep2)
