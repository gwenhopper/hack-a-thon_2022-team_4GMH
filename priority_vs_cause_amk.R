#hackathon

#303D_18 is a shapefile- which are imparied ve meeting standards
#the other folder is a shape file, biological information , more detail 
library(tidyverse)
library(tidyverse)

#looking at impaired waters 
imp = read_csv("data/2018303d_final.csv") # SC Impaired Waters List 303d for 2018
summary(imp) # There are 861 empty rows

# Get rid of empty rows
dim(imp) #1979 rows
imp = imp %>%
  filter(priorityRank != "") 
dim(imp) # 1979-861 = 1118 rows

summary(imp)
names(imp) <-  c("priorityRank", "note", "basin", "huc_12", "county", "description", "station", "use", "cause")
 summary(imp)      

unique(imp$cause)

ggplot(data = imp)+
  geom_histogram(aes(x=cause, fill=priorityRank), stat="count", binwidth =5 )+
  coord_flip()

unique(imp$cause) 


# Seperating the causes 
imp_cause<- separate(imp,cause,
  into=c("cause1","cause2", "cause3", "cause4"),
  sep = ",")

tail(imp_cause, n=20)

imp_cause$totalCause <- 0

for(i in seq(dim(imp_cause)[1])){
  if(is.na(imp_cause$cause4)[i] == FALSE){
    imp_cause$totalCause[i] = 4
  }else if(is.na(imp_cause$cause3)[i] == FALSE){
        imp_cause$totalCause[i] = 3
    }else if(is.na(imp_cause$cause2)[i] == FALSE){
      imp_cause$totalCause[i] = 2
    }else if(is.na(imp_cause$cause1)[i] == FALSE){
      imp_cause$totalCause[i] = 1
    }
}



ggplot(data = imp_cause)+
  geom_histogram(aes(x=totalCause, fill=priorityRank), stat="count", binwidth =5 )+
  coord_flip()
#most only have 1, but the combo of all the different lists really clog up the image 


ggplot(data = imp_cause)+
  geom_histogram(aes(x=totalCause, fill=basin), stat="count", binwidth =5 )+
  coord_flip()
#seems distributed among the basins


#how to do a count of all 

table(imp_cause[,c("cause1", "cause2", "cause3", "cause4")])

table(imp_cause$cause1) + table(imp_cause$cause2) + table(imp_cause$cause3) + table(imp_cause$cause4)
