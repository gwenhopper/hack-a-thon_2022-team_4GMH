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


<- strsplit(imp$cause)= ',')
names(splitdat) = paste("trial", 1:4, sep = "")


#Normalize the area or number of water bodies per basin 



test <- separate(imp,cause,
  into=c("cause1","cause2", "cause3", "cause4"),
  sep = ",")

head(test)
