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

#sources for other the amino acid distribution of other types of protein (egg, whey, soy,
#collagen) are manually entered and coerced into vectors, then data frames.

amino_acid<-c("Glutamin_acid","Lysine", "Serine","Aspartic_acid","Leucine","Arginine","Valine","Tyrosine","Alanine","Isoleucine","Phenylalanine","Threonine","Glycine","Methionine","Histidine","Proline","Cystine","Tryptothan")
egg_amino_acid<-c(13.04,7.19, 7.44, 10.03, 8.53, 6.02, 6.1, 4.1, 5.6, 5.43, 5.35, 4.76, 3.34, 3.09, 2.34, 4.01, 2.34, 1.25)
egg_dat<-data.frame(amino_acid,egg_amino_acid)

#data for whey and soy were gathered from same source
amino_acids_2<-c("Aspartic_acid","Serine","Glutamic_acid","Histidine","Glycine","Threonine","Arginine","Alanine","Tyrosine","Valine","Methionine","Isoleucine","Phenylalanine","Tryptophan","Leucine","Lysine","Proline","Cysteine")
whey_amino_acids<-c(11,5.7,18.6,1.8,1.8,7.4,2.3,5.3,2.6,5.1,1.5,5.7,2.7,1.4,9.8,9.5,6.6,1.4)
soy_amino_acids<-c(12.1,5.2,19.7,2.5,4.3,3.9,7.4,4.4,3.1,3.9,1.2,3.7,4.9,1.3,7.5,6.6,6.4,2.2)
whey.soy_dat<-data.frame(amino_acids_2,whey_amino_acids,soy_amino_acids)
