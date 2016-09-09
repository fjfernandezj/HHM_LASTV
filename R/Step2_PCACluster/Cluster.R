### Read Data
All.Data<-read.table(file.choose("DataForCluster.csv"), header = TRUE, sep =",")

#Checking Data
AllData<-na.omit(All.Data)
save(AllData, file='AllData.Rda')
## Matrix of x-y plots for the variable
par("mar")
par(mar=c(1,1,1,1))

# Selecting first variables from AllData
AllData1 <- select(AllData, X, folio, commune, hhsize, Farm_size, Hrd_ratio, Ttl_Lab, Ttl_Rev, Ttl_Cst, Grn_shr, SV_shr, SC_shr, Consumption)
save(AllData1, file='AllData1.Rda')
pairs(AllData1[,-c(1,2,3,4)], panel=panel.smooth)


##Distribution of the variables
par(mfrow=c(3,4))
h1<-hist(AllData$Farm_size, main="Farm size")
h2<-hist(AllData$hhsize, main="Household size")
h3<-hist(AllData$Hrd_ratio, main="Hired ratio")
h4<-hist(AllData$Ttl_Lab, main="Total Labour")
h5<-hist(AllData$Ttl_Rev, main="Total Revenue")
h6<-hist(AllData$Ttl_Cst, main="Total Cost")
h7<-hist(AllData$Grn_shr, main="Share of grain crops")
h8<-hist(AllData$SV_shr, main="Share of spring vegetables")
h9<-hist(AllData$SC_shr, main="Share of spring crops")
h10<-hist(AllData$OC_shr, main="Share of other crops")
h11<-hist(AllData$SD_shr, main="Share of seed crops")
h12<-hist(AllData$Consumption, main="Consumption")


## Study of outliers with more details
par(mfrow=c(3,4))
boxplot(AllData$Farm_size, main="Farm size")
boxplot(AllData$Ttl_Lab, main="Tot_Lab")
boxplot(AllData$Ttl_Rev, main="Tot_Rev")
boxplot(AllData$Ttl_Cst, main="Tot_Cst")
boxplot(AllData$Grn_shr, main="Shr_grn")
boxplot(AllData$SV_shr, main="Shr_sprVeg")
boxplot(AllData$SC_shr, main="Shr_sprCrs")
boxplot(AllData$OC_shr, main="Shr_other")
boxplot(AllData$SD_shr, main="Shr_seeds")
boxplot(AllData$Consumption, main="Consum")

#Removing outliers
AllData2<- AllData[AllData$Farm_size < 200,]

par(mfrow=c(3,4))
boxplot(AllData2$Farm_size, main="Farm size")
boxplot(AllData2$Ttl_Lab, main="Tot_Lab")
boxplot(AllData2$Ttl_Rev, main="Tot_Rev")
boxplot(AllData2$Ttl_Cst, main="Tot_Cst")
boxplot(AllData2$Grn_shr, main="Shr_grn")
boxplot(AllData2$SV_shr, main="Shr_sprVeg")
boxplot(AllData2$SC_shr, main="Shr_sprCrs")
boxplot(AllData2$OC_shr, main="Shr_other")
boxplot(AllData2$SD_shr, main="Shr_seeds")
boxplot(AllData2$Consumption, main="Consum")

AllData2$Farm_size2 <- log10(AllData2$Farm_size)
AllData2$Ttl_Lab2 <- log10(1+AllData2$Ttl_Lab)
AllData2$Hrd_ratio2 <- log10(1+AllData2$Hrd_ratio)
AllData2$Ttl_Rev2 <- log10(AllData2$Ttl_Rev)
AllData2$Ttl_Cst2 <- log10(AllData2$Ttl_Cst)
AllData2$Grn_shr2 <- log10(1+AllData2$Grn_shr)
AllData2$SV_shr2 <- log10(1+AllData2$SV_shr)
AllData2$SC_shr2 <- log10(1+AllData2$SC_shr)
AllData2$Consumption2 <- sqrt(AllData2$Consumption)

par(mfrow=c(3,4))
newh1<-hist(AllData2$Farm_size, main="Farm size 1")
newh2<-hist(AllData2$Farm_size2, main="Farm size 2")
newh3<-hist(AllData2$Ttl_Lab, main="Ttl Lab 1")
newh4<-hist(AllData2$Ttl_Lab2, main="Ttl Lab 2")
newh5<-hist(AllData2$Hrd_ratio, main="Hrd ratio 1")
newh6<-hist(AllData2$Hrd_ratio2, main="Hrd ratio 2")
newh7<-hist(AllData2$Ttl_Rev, main="Ttl Rev 1")
newh8<-hist(AllData2$Ttl_Rev2, main="Ttl Rev 2")
newh9<-hist(AllData2$Ttl_Cst, main="Ttl Cst 1")
newh10<-hist(AllData2$Ttl_Cst2, main="Ttl Cst 2")
newh11<-hist(AllData2$Grn_shr, main="Grn Share 1")
newh12<-hist(AllData2$Grn_shr2, main="Grn Share 2")
newh13<-hist(AllData2$SV_shr, main="SV Share 1")
newh14<-hist(AllData2$SV_shr2, main="SV Share 2")
newh15<-hist(AllData2$SC_shr, main="SC Share 1")
newh16<-hist(AllData2$SC_shr2, main="SC Share 2")
newh17<-hist(AllData2$Consumption, main="Consumption 1")
newh18<-hist(AllData2$Consumption2, main="Consumption 2")



h2<-hist(AllData$hhsize, main="Household size")
h3<-hist(AllData$Hrd_ratio, main="Hired ratio")
h4<-hist(AllData$Ttl_Lab, main="Total Labour")
h5<-hist(AllData$Ttl_Rev, main="Total Revenue")
h6<-hist(AllData$Ttl_Cst, main="Total Cost")
h7<-hist(AllData$Grn_shr, main="Share of grain crops")
h8<-hist(AllData$SV_shr, main="Share of spring vegetables")
h9<-hist(AllData$SC_shr, main="Share of spring crops")
h10<-hist(AllData$OC_shr, main="Share of other crops")
h11<-hist(AllData$SD_shr, main="Share of seed crops")
h12<-hist(AllData$Consumption, main="Consumption")



# Scale
AllData3 <- data.frame(scale(AllData2))


##Distribution of the variables
par(mfrow=c(3,4))
nh1<-hist(AllData3$Farm_size2, main="Farm size")
nh2<-hist(AllData3$hhsize, main="Household size")
nh3<-hist(AllData3$Hrd_ratio2, main="Hired ratio")
nh4<-hist(AllData3$Ttl_Lab2, main="Total Labour")
nh5<-hist(AllData3$Ttl_Rev2, main="Total Revenue")
nh6<-hist(AllData3$Ttl_Cst2, main="Total Cost")
nh7<-hist(AllData3$Grn_shr2, main="Share of grain crops")
nh8<-hist(AllData3$SV_shr2, main="Share of spring vegetables")
nh9<-hist(AllData3$SC_shr2, main="Share of spring crops")
nh10<-hist(AllData3$OC_shr, main="Share of other crops")
nh11<-hist(AllData3$SD_shr, main="Share of seed crops")
nh12<-hist(AllData3$Consumption2, main="Consumption")

??s.corcircle 

#PCA on the trasformed data
install.packages("ade4")
library(ade4)

par(mfrow=c(1,1))
DataT <- AllData2[,match(c("Farm_size2", "commune", "hhsize", "Hrd_ratio2", "Ttl_Lab2", "Ttl_Rev2", "Ttl_Cst2",
                             "Grn_shr2", "SV_shr2" ,"SC_shr2", "Consumption2"), dimnames(AllData2)[[2]])]
Data.pca <- dudi.pca(DataT,  center=T, scale=T, scannf=T, nf=5)

save(Data.pca, file='Data.pca2.Rda')

Data.pca$eig

cumsum(Data.pca$eig) / sum(Data.pca$eig)

par(mfrow=c(1,2))
s.corcircle(Data.pca$co, xax=1, yax=2 )
s.corcircle(Data.pca$co, xax=1, yax=3 )
s.corcircle(Data.pca$co, xax=1, yax=4 )
s.corcircle(Data.pca$co, xax=1, yax=5 )

Data.pca$co

#Correlatons
cor.test(AllData3$Farm_size, AllData2$Ttl_Lab, method=c("pearson"))
cor.test(AllData3$Ttl_Rev2, AllData2$Ttl_Cst2, method=c("pearson"))
cor.test(AllData3$Ttl_Rev2, AllData2$Ttl_Lab2, method=c("pearson"))

par(mfrow=c(1,2))
s.label(Data.pca$li, xax=1, yax=2)
s.label(Data.pca$li, xax=1, yax=3)
s.label(Data.pca$li, xax=1, yax=4)
s.label(Data.pca$li, xax=1, yax=5)

#To delete the farms from  dataset and selecting variables:

AllData4 <- AllData2 %>%
        filter(Farm_size < 500) 
AllData4<- AllData2[AllData2$Consumption <= 1,]


#Checking Data
AllData4<-na.omit(AllData4)

AllData4 <- select(AllData4, X, folio, commune, hhsize, Farm_size2, Hrd_ratio2, Ttl_Lab2, Ttl_Rev2, Grn_shr2, SV_shr2, SC_shr2, Consumption2)

# Scale
AllData5 <- data.frame(scale(AllData4))

#To create a new DataT without the variable  and run again the PCA:
par(mfrow=c(1,1))
DataT2 <- AllData2[,match(c("Farm_size2","commune", "Hrd_ratio2", "Ttl_Lab2", "Ttl_Rev2",
                           "Grn_shr2", "SV_shr2" ,"SC_shr2", "Consumption2"), dimnames(AllData2)[[2]])]
Data.pca2 <- dudi.pca(DataT2, center=T, scale=T, scannf=T, nf=5)

save(Data.pca2, file='Data.pca3.Rda')

dev.off()
Data.pca2$eig

cumsum(Data.pca2$eig) / sum(Data.pca2$eig)

par(mfrow=c(1,2))
s.corcircle(Data.pca2$co, xax=1, yax=2 )
s.corcircle(Data.pca2$co, xax=1, yax=3 )
#s.corcircle(Data.pca$co, xax=1, yax=4 )
#s.corcircle(Data.pca$co, xax=1, yax=5 )


Data.pca2$co


s.label(Data.pca2$li, xax=1, yax=2)
s.label(Data.pca2$li, xax=1, yax=3)
s.label(Data.pca$li, xax=1, yax=4)
s.label(Data.pca$li, xax=1, yax=5)

AllData6 <- AllData5 %>%
        filter(Farm_size2 < 5.5) 


################ Final PCA run


#To create a new DataT without the variable  and run again the PCA:

DataT <- DataT %>%
        filter(Farm_size2 < 5.5) 

par(mfrow=c(1,1))
DataT <- AllData6[,match(c("Farm_size2","commune", "hhsize", "Hrd_ratio2", "Ttl_Lab2", "Ttl_Rev2",
                           "Grn_shr2", "SV_shr2" ,"SC_shr2", "Consumption2"), dimnames(AllData6)[[2]])]
Data.pca <- dudi.pca(DataT, center=T, scale=T, scannf=T, nf=5)



dev.off()
Data.pca$eig

cumsum(Data.pca$eig) / sum(Data.pca$eig)

par(mfrow=c(2,2))
s.corcircle(Data.pca$co, xax=1, yax=2 )
s.corcircle(Data.pca$co, xax=1, yax=3 )
s.corcircle(Data.pca$co, xax=1, yax=4 )
s.corcircle(Data.pca$co, xax=1, yax=5 )


Data.pca$co


s.label(Data.pca$li, xax=1, yax=2)
s.label(Data.pca$li, xax=1, yax=3)
s.label(Data.pca$li, xax=1, yax=4)
s.label(Data.pca$li, xax=1, yax=5)






scatter(Data.pca2)

scatter(Data.pca2, xax=1, yax=2)
scatter(Data.pca2, xax=1, yax=3)
#3rd and final PCA run --> WITHOUT OC_shr variable:
#par(mfrow=c(1,1))
#DataT <- AllnewData[,match(c("Farm_size", "hhsize", "Hrd_ratio", "Ttl_Lab", "Ttl_Rev",
#                             "Grn_shr", "SV_shr" ,"SC_shr"), dimnames(AllnewData)[[2]])]
#Data.pca <- dudi.pca(DataT, center=T, scale=T, scannf=T, nf=5)
#
#Data.pca$eig

#cumsum(Data.pca$eig) / sum(Data.pca$eig)

#par(mfrow=c(1,2))
#s.corcircle(Data.pca$co, xax=1, yax=2 )
#s.corcircle(Data.pca$co, xax=1, yax=3 )

#Data.pca$co

#s.label(Data.pca$li, xax=1, yax=2)
#s.label(Data.pca$li, xax=1, yax=3)

##cluster analysis
Data.cah <- hclust(dist(Data.pca2$li), method="ward.D")

par(mfrow=c(1,2))

barplot(Data.cah$height)
plot(Data.cah)

??s.class   

Data.type <- cutree(Data.cah, k=5)

?par
par(mfrow=c(2,2))
s.corcircle(Data.pca2$co, xax=1, yax=2)
s.class(Data.pca2$li, fac = as.factor(Data.type),col=sample(colors(),10))

s.corcircle(Data.pca2$co, xax=1, yax=3)
s.class(Data.pca2$li, xax=1, yax=3, fac = as.factor(Data.type), col=sample(colors(),8))

s.corcircle(Data.pca$co, xax=1, yax=4)
s.class(Data.pca$li, xax=1, yax=4, fac = as.factor(Data.type))

FinalData <- AllData2 %>%
        filter(folio != 9) %>% 
        filter(folio != 92) %>%
        filter(folio != 154) 

FinalData$typo <- Data.type

write.csv(FinalData, file="csv_files/Final_cluster.csv") 
