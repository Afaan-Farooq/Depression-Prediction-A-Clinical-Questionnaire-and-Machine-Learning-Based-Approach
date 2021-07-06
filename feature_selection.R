library(missForest)
library(reshape2)
library(ggplot2)
library(dplyr)
library(Amelia)
library(Boruta)

#read csv into a dataframe
read_file <- read.csv('C:/Users/Sheikh Afaan/Desktop/Project/1.our_feature_selection/depression_with_nd.csv',header=TRUE,stringsAsFactors = F) 
str(read_file)
summary(read_file)
depression.mis <- read_file
  
ggplot_missing <- function(x){
  
  x %>% 
    is.na %>%
    melt %>%
    ggplot(data = .,
           aes(x = Var2,
               y = Var1)) +
    geom_raster(aes(fill = value)) +
    scale_fill_grey(name = "",
                    labels = c("Present","Missing")) +
    theme_minimal() + 
    theme(axis.text.x  = element_text(angle=45, vjust=0.5)) + 
    labs(x = "Variables in Dataset",
         y = "Rows / observations")
}
ggplot_missing(depression.mis)

missmap(depression.mis)
amelia_depression <- amelia(depression.mis, m=5, parallel = "multicore",noms=NULL ,ords =c('Ofdist','Pwcomp','Loenjoy','Ractivity','Reffect'))
amelia_depression$imputations[[1]]
write.amelia(amelia_depression, file.stem = "imputed_depression_data_set")


#boruta code
set.seed(111)
boruta.depression_train <- Boruta(Depression~., data = amelia_depression$imputations[[5]] , doTrace = 2)
print(boruta.depression_train)
boruta.depression <- TentativeRoughFix(boruta.depression_train)
print(boruta.depression)
lz<-lapply(1:ncol(boruta.depression$ImpHistory),function(i)
  boruta.depression$ImpHistory[is.finite(boruta.depression$ImpHistory[,i]),i])
names(lz) <- colnames(boruta.depression$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
     at = 1:ncol(boruta.depression$ImpHistory), cex.axis = 0.7)
getSelectedAttributes(boruta.depression, withTentative = F)
depression_df <- attStats(boruta.depression)
print(depression_df)

