
# load data
read_file <- read.csv('C:/Users/Sheikh Afaan/Desktop/Project/2.our_feature_scaling/depression_nd_s.csv',header=TRUE,stringsAsFactors = F) 
str(read_file)
# rename dataset to keep code below generic
dataset <- read_file

#zscore standardization
dataset2 <- as.data.frame( scale(dataset[1:16] ))
str(dataset2)
head(dataset2)

write.csv(dataset2, "depression_nd_nor.csv")

# load data
read_file <- read.csv('C:/Users/Sheikh Afaan/Desktop/Project/2.our_feature_scaling/depression_without_nd_s.csv',header=TRUE,stringsAsFactors = F) 
str(read_file)
# rename dataset to keep code below generic
dataset <- read_file

#zscore standardization
dataset2 <- as.data.frame( scale(dataset[1:16] ))
str(dataset2)
head(dataset2)

write.csv(dataset2, "depression_without_nd_nor.csv")


