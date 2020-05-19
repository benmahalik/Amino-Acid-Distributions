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


amino_acids_3<-c("Aspartic_acid","Serine","Glutamic_acid","Glycine","Arginine","Threonine","Alanine","Proline","Hydroxyproline","Tyrosine","Valine","Methionine","Lysine","Isoleucine","Leucine","Phenylalanine")
collagen_amino_acids<-c(4.97,3.9,9.27,26.67,9.4,2.64,7.35,13,9,.5,2.53,.61,3.5,.94,2.39,1.84)
collagen_dat<-data.frame(amino_acids_3,collagen_amino_acids)


##checks to see if the sum of proportions adds to 100. 
#sum(egg_amino_acid)
#sum(soy_amino_acids)
#sum(whey_amino_acids)


#standardizing the variables with anino acid factors to "amino_acid"
#This will allow the observations to be matched and the separate datasets to be merged together
collagen_dat<-rename(collagen_dat,  amino_acid=amino_acids_3)
whey.soy_dat<-whey.soy_dat%>%
  rename(amino_acid=amino_acids_2)%>%
  rename(whey_perc=whey_amino_acids)%>%
  rename(soy_perc=soy_amino_acids)

#other recodings
collagen_dat<-rename(collagen_dat,   "collagen_perc"= collagen_amino_acids)

beef_dat<-rename(beef_dat, "b_quantity (mg)"="quantity (mg)")

egg_dat<-rename(egg_dat, "egg_perc"=egg_amino_acid)


#combining separate dataframes into list and then merging them together into dataframe "dat"
aa_list<-list(beef_dat=beef_dat, collagen_dat=collagen_dat, egg_dat=egg_dat, whey.soy_dat=whey.soy_dat)

dat<-aa_list[[1]]
for(i in 2:length(aa_list)){
  dat<-merge(dat,aa_list[[i]], all.x =T, all.y=T)
  
}

#data cleaning and fixing data entry errors
dat<-dat%>%
  select(-"b_quantity (mg)",-"proportion")%>%
  rename(beef_perc=percentage)

dat<-dat[!dat$amino_acid=="Glutamin_acid",]
dat[is.na(dat$egg_perc) & dat$amino_acid=="Glutamic_acid","egg_perc"]<-13.04

dat<-dat[!dat$amino_acid=="Tryptothan",]
##egg_perc/Tryptophan=1.25
dat[is.na(dat$egg_perc) & dat$amino_acid=="Tryptophan","egg_perc"]<-1.25

##Cysteine(beef_perc,egg_perc)=c(2.9,2.34)
dat<-dat[!dat$amino_acid=="Cystine",]
dat[is.na(dat$beef_perc) & dat$amino_acid=="Cysteine","beef_perc"]<-2.9
dat[is.na(dat$egg_perc) & dat$amino_acid=="Cysteine","egg_perc"]<-2.34

#resets observations, to account for ommitted observations
rownames(dat)<-1:nrow(dat)

#altering source of beef amino acids with new data frame "protein
protein<-data.frame(amino_acid=c(140,849,967,1691,1797,554,274,840,677, 1055, 1375, 678, 1292, 1936, 3191, 1294,1013,837,223))


protein$perc<-protein$amino_acid/213 #this factor is a combination of converting to percentage, and change of unit to milligram
protein<-rename(protein, quantity=amino_acid)
protein$amino_acid<-c("Tryptophan","Threonine","Isoleucine","Leucine","Lysine","Methionine","Cysteine","Phenylalanine","Tyrosine","Valine","Arginine","Histidine","Alanine","Aspartic_acid","Glutamic_acid","Glycine","Proline","Serine","Hydroxyproline")
protein<-protein[,c(3,1,2)]
protein$beef2_perc<-protein$perc
protein$beef2_perc<-round(protein$beef2_perc,2)
protein$quantity<-NULL

dat<-merge(dat,protein)
dat[,c("beef_perc","perc")]<-NULL
dat<-rename(dat, beef_perc=beef2_perc)


dat$amino_acid_sum<-rowSums(cbind(dat$collagen_perc,dat$egg_perc,dat$whey_perc,dat$soy_perc,dat$beef_perc),na.rm = T)


write.csv(dat, "DATA SETS/amino acid composition.csv")

