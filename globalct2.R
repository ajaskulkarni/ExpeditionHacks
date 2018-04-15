library(readr)
ctd_ht <- read_csv("~/ctd_ht.csv")


master<-ctd_ht[,-1]
master.m<-replace(master, master[,1:58]=="-99", NA)
master.m = as.data.frame(master.m)
#master.m$citizenship[is.na(master.m$citizenship)]<-"NAM"
#master.new<-na.omit(master.m)
na_count <-sapply(master.m, function(y) sum(length(which(is.na(y)))))
cl = c("majorityStatusAtExploit", "majorityEntry",
       "meansOfControlDebtBondage","meansOfControlTakesEarnings",
       "meansOfControlRestrictsFinancialAccess", "meansOfControlThreats",
       "meansOfControlPsychologicalAbuse" , "meansOfControlSexualAbuse",
       "meansOfControlFalsePromises", "meansOfControlPsychoactiveSubstances",
       "meansOfControlRestrictsMovement", "meansOfControlRestrictsMedicalCare",
       "meansOfControlExcessiveWorkingHours", "meansOfControlUsesChildren",
       "meansOfControlThreatOfLawEnforcement", "meansOfControlWithholdsNecessities",
       "meansOfControlWithholdsDocuments", "meansOfControlOther", "isForcedMarriage",
       "isForcedMilitary", "isOrganRemoval", "typeOfSexPrivateSexualServices", "isAbduction",
       "meansOfControlPhysicalAbuse","typeOfSexPornography","typeOfSexRemoteInteractiveServices","yearOfRegistration",
       "CountryOfExploitation"
)
#na_count <-sapply(master.new, function(y) sum(length(which(is.na(y)))))
#remove column names with more than 30,000 NAs
master.new = master.m[ , !names(master.m) %in% cl, drop=F]
colnames(master.new)
master.new1<-na.omit(master.new)
dim(master.new1)
master.new1$gender<-factor(master.new1$gender, levels=c("Female","Male"),
                           labels=c(0,1))
master.new1$Datasource<-factor(master.new1$Datasource, levels=c("Case Management","Hotline"),
                               labels=c(0,1))
master.new1$ageBroad<-factor(master.new1$ageBroad, levels=c("0--8","9--17","18--20","21--23","24--26","27--29",
                                                            "30--38","39--47","48+","Unknown"),
                             labels=c(1,2,3,4,5,6,7,8,9,10))
master.new1$majorityStatus<-factor(master.new1$majorityStatus, levels=c("Adult","Minor", "Unknown"),
                                   labels=c(1,0,2))

master.new1$citizenship<-factor(master.new1$citizenship, levels=c("MD","UA","RO","HT","BG","BY","KG","AF",
                                                                  "KH","MM","GH","ID","GN","SN","AL","GW", 
                                                                  "UZ","UG","LK","MX","ZZ",
                                                                  "US","PH","CN","KR"),
                                labels=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,
                                         16,17,18,19,20,21,22,23,24,25))
master.new1$RecruiterRelationship<-factor(master.new1$RecruiterRelationship, levels=c("Unknown","Family/Relative","Other",
                                                                                      "Friend","Family/Relative; Intimate Partner","Intimate Partner",                 
                                                                                      "Friend; Intimate Partner","Friend; Other","Family/Relative; Other",           
                                                                                      "Family/Relative; Friend","Friend; Intimate Partner; Other","Family/Relative; Unknown",         
                                                                                      "Intimate Partner; Unknown","Intimate Partner; Other","Family/Relative; Friend; Other"),
                                          labels=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15
                                          ))

data_new<-cbind(master.new1$RecruiterRelationship,master.new1[,-25])
colnames(data_new)[1]<-c("RecruiterRelationship")

data_new$Forced_Labour = as.factor(data_new$Forced_Labour)
data_new$Datasource = as.factor(data_new$Datasource)
data_new$gender = as.factor(data_new$gender)
data_new$ageBroad = as.factor(data_new$ageBroad)
data_new$majorityStatus = as.factor(data_new$majorityStatus)
data_new$citizenship = as.factor(data_new$citizenship)
data_new$meansOfControlNotSpecified = as.factor(data_new$meansOfControlNotSpecified)
data_new$isSexualExploit = as.factor(data_new$isSexualExploit)
data_new$isOtherExploit = as.factor(data_new$isOtherExploit)
data_new$isSexAndLabour = as.factor(data_new$isSexAndLabour)
data_new$typeOfLabourAgriculture = as.factor(data_new$typeOfLabourAgriculture)
data_new$typeOfLabourAquafarming = as.factor(data_new$typeOfLabourAquafarming)
data_new$typeOfLabourBegging = as.factor(data_new$typeOfLabourBegging)
data_new$typeOfLabourConstruction = as.factor(data_new$typeOfLabourConstruction)
data_new$typeOfLabourDomesticWork = as.factor(data_new$typeOfLabourDomesticWork)
data_new$typeOfLabourHospitality = as.factor(data_new$typeOfLabourHospitality)
data_new$typeOfLabourIllicitActivities = as.factor(data_new$typeOfLabourIllicitActivities)
data_new$typeOfLabourManufacturing = as.factor(data_new$typeOfLabourManufacturing)
data_new$typeOfLabourMiningOrDrilling = as.factor(data_new$typeOfLabourMiningOrDrilling)
data_new$typeOfLabourPeddling = as.factor(data_new$typeOfLabourPeddling)
data_new$typeOfLabourTransportation = as.factor(data_new$typeOfLabourTransportation)
data_new$typeOfLabourOther = as.factor(data_new$typeOfLabourOther)
data_new$typeOfLabourNotSpecified = as.factor(data_new$typeOfLabourNotSpecified)
data_new$typeOfSexProstitution = as.factor(data_new$typeOfSexProstitution)
data_new$RecruiterRelationship = as.factor(data_new$RecruiterRelationship)
data_new$recruiterRelationIntimatePartner = as.factor(data_new$recruiterRelationIntimatePartner)
data_new$recruiterRelationFriend = as.factor(data_new$recruiterRelationFriend)
data_new$recruiterRelationFamily = as.factor(data_new$recruiterRelationFamily)
data_new$recruiterRelationOther = as.factor(data_new$recruiterRelationOther)
data_new$recruiterRelationUnknown = as.factor(data_new$recruiterRelationUnknown)

data_new = as.data.frame(data_new)

n<-nrow(data_new)
set.seed(1)
train<-sample(1:n, size=round(0.9*n),replace=FALSE)
train_data=data_new[train,]
test_data=data_new[-train,]

library(randomForest)
rf_model = randomForest(Forced_Labour~., data = train_data)
varImpPlot(rf_model)


#May Add Back:
##"meansOfControlNotSpecified", "typeOfLabourConstruction", "typeOfLabourNotSpecified",
sel_var = c("Datasource", "isSexualExploit", "gender", "citizenship", "ageBroad",
            "RecruiterRelationship", "recruiterRelationOther", "typeOfSexProstitution",
            "recruiterRelationUnknown","majorityStatus","Forced_Labour")
train_data2<-train_data[,names(train_data)%in% sel_var, drop=F]
test_data2<-test_data[,names(test_data)%in% sel_var, drop=F]
colnames(train_data2)
colnames(test_data2)

svm_model = svm(RecruiterRelationship~., data = train_data2)
pred = predict(svm_model, newdata = test_data2)
table(pred,test_data2$RecruiterRelationship)
confusionMatrix(test_data2$RecruiterRelationship,pred)

nb_model = naiveBayes(RecruiterRelationship~., data = train_data2)
pred = predict(nb_model, newdata = test_data2)
table(pred,test_data2$RecruiterRelationship)
confusionMatrix(test_data2$RecruiterRelationship,pred)

library(class)
knn_prediction <- knn(train = train_data2, test = test_data2,cl = train_data2$RecruiterRelationship, k=10)
table(knn_prediction,test_data2$RecruiterRelationship)
confusionMatrix(test_data2$RecruiterRelationship,knn_prediction)

histogram(master.new1$RecruiterRelationship, xlab=c("Unknown","Family/Relative","Other","Friend","Family/Relative; Intimate Partner","Intimate Partner","Friend; Intimate Partner","Friend; Other"),
          xlim=c(1:7), main="Percent Of Total for Recruiter's Relationship to Victim")
