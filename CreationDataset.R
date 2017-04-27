#Kindergarden without aide
STAR_Students<-NULL
STAR_Students <- read.delim("C:/Users/Aliba/Dropbox/0_Uni (Privat)/Master Thesis/Data/STAR_Students_Full_Dataset.txt", 
                  as.is=TRUE, na.strings=c(NA,"NA", " NA"))

#Delete NA of Kindergarten
STAR_Students1<-NULL
#STAR_Students1<-subset(STAR_Students, g1treadss>0 & race>0 & g1tmathss<0)
#View(STAR_Students1)


#Create variables for Kindergarten
STAR_G1<-NULL
STAR_G1$stdntid<-STAR_Students$stdntid
STAR_G1$flagsg1<-STAR_Students$flagsg1
STAR_G1$g1classtype<-STAR_Students$g1classt
STAR_G1$schoolid<-STAR_Students$g1schid
STAR_G1$g1surban<-STAR_Students$g1surban
STAR_G1$tgender<-STAR_Students$g1tgen
STAR_G1$trace<-STAR_Students$g1trace
STAR_G1$tdegree<-STAR_Students$g1thighd
STAR_G1$tcareer<-STAR_Students$g1tcaree
STAR_G1$tyears<-STAR_Students$g1tyears
STAR_G1$g1classtype<-STAR_Students$g1classt
STAR_G1$race<-STAR_Students$race
STAR_G1$gender<-STAR_Students$gender
STAR_G1$birthmonth<-STAR_Students$birthmonth
STAR_G1$birthyea<-STAR_Students$birthyea
STAR_G1$freelu<-STAR_Students$g1freelunch
STAR_G1$promo<-STAR_Students$g1promote
STAR_G1$present<-STAR_Students$g1present
STAR_G1$read<-STAR_Students$g1treadss
STAR_G1$math<-STAR_Students$g1tmathss
STAR_G1$lang<-STAR_Students$g1wordskillss
STAR_G1$list<-STAR_Students$g1tlistss
#STAR_G1$yearssm<-STAR_Students$yearssmall

#View(STAR_G1)
write.csv(STAR_G1, file="STAR_G1.csv")

#omit na
STAR_G1<-NULL
STAR_G1 <- read.csv("C:/Users/Aliba/Documents/STAR_G1.csv", na.strings=c(NA,"NA", " NA"))
STAR_G1_clean<-na.omit(STAR_G1)

#eliminate aide class
STAR_G1_clean<-STAR_G1_clean[!(STAR_G1_clean$g1classtype=="3"),]


#Combine birthyear and birthmonth: Divide by 13 to get months in terms of years
STAR_G1_clean$birth<-NULL
STAR_G1_clean$birth<-with(STAR_G1_clean, birthyea+birthmonth/13)

#Create a dummy for classtype
STAR_G1_clean$g1classtype<-with(STAR_G1_clean, g1classtype-1)

#Create ground truth
MeanSmallClass_G1<-mean(STAR_G1_clean$g1classtype)
STAR_G1_clean$g1treadss_1<-ifelse(STAR_G1_clean$g1classtype>0,-1/(1-MeanSmallClass_G1)*STAR_G1_clean$read,
                                  1/MeanSmallClass_G1*STAR_G1_clean$read)
STAR_G1_clean$g1tmathss_1<-ifelse(STAR_G1_clean$g1classtype>0,-1/(1-MeanSmallClass_G1)*STAR_G1_clean$math,
                                  1/MeanSmallClass_G1*STAR_G1_clean$math)
STAR_G1_clean$g1tlistss_1<-ifelse(STAR_G1_clean$g1classtype>0,-1/(1-MeanSmallClass_G1)*STAR_G1_clean$list,
                                  1/MeanSmallClass_G1*STAR_G1_clean$list)
STAR_G1_clean$g1wordskillss_1<-ifelse(STAR_G1_clean$g1classtype>0,-1/(1-MeanSmallClass_G1)*STAR_G1_clean$lang,
                                  1/MeanSmallClass_G1*STAR_G1_clean$lang)

STAR_G1_clean$read_2<-STAR_G1_clean$g1treadss_1
STAR_G1_clean$math_2<-STAR_G1_clean$g1tmathss_1
STAR_G1_clean$list_2<-STAR_G1_clean$g1tlistss_1
STAR_G1_clean$word_2<-STAR_G1_clean$g1wordskillss_1

View(STAR_G1_clean)

#Save as csv
write.csv(STAR_G1_clean, file="STAR_G1_clean.csv")
