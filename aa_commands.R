library(tidyverse)

#beef amino-acid data is copied from an online source to a text file, and formatted into an
#R data frame
beef_dat<-read.table("beef_amino acid.txt", sep="")
names(beef_dat)<-c("amino_acid","quantity")
beef_dat$quantity<-gsub("mg","", beef_dat$quantity)
beef_dat$quantity<-as.numeric(beef_dat$quantity)
beef_dat$proportion<-beef_dat$quantity/77300
beef_dat$percentage<-beef_dat$proportion*100
beef_dat[,c(3,4)]<-lapply(beef_dat[,c(3,4)],round,2)

colnames(beef_dat)[2]<-"quantity (mg)"
