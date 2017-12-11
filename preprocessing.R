
sample_files <- dir("DNA_Methylation/JHU_USC__HumanMethylation27/Level_3")
sample_files_sorted <- sort(sample_files, decreasing = FALSE)
samples <- sample_files_sorted[1:100]

file_paths <- file.path("C:/Users/jacob/Desktop/DNA_Methylation/JHU_USC__HumanMethylation27/Level_3", samples)



data_list <- llply(file_paths, read.table, sep = "\t")




sample_files <- dir("DNA_Methylation/JHU_USC__HumanMethylation27/Level_3")
sample_files_sorted <- sort(sample_files, decreasing = FALSE)
samples <- sample_files_sorted[1:100]
file_paths <- file.path("C:/Users/jacob/Desktop/New, samples)
for(i in 1:100){
file_contents <- read.table(file_paths[i], sep = "\t")
df_file_contents<-data.frame(file_contents)
distance<- dist(datafr[4,], method = "euclidean")
clust<-hclust(distance, method = "ward")
plot(clust)
}

setwd ("C:\\Users\\jacob\\Desktop\\DNA_Methylation\\JHU_USC__HumanMethylation27\\Level_3")
getwd()
data<-read.delim("jhu-usc.edu__HumanMethylation27__TCGA-A1-A0SD-01A-11D-A112-05__methylation_analysis.txt")
newdata= as.matrix(data)

train<-which(newdata[,3]!="N/A")
finaldata<-newdata[train,]
NROW(finaldata)
t2<-which(finaldata[,4]!="N/A")
final2data<-finaldata[t2,]
NROW(final2data)

finaldata3<-(final2data[])

beta<-as.matrix(c(file_contents[,3]))
for(i in 1:length(beta)){
if(beta[i]!=NA)
print(beta[i])
return(TRUE)
}

data<-read.table("C:/Users/jacob/Desktop/New/1.txt",sep = "\t")
datafr<-data.frame(sample)
distance<- dist(datafr, method = "euclidean")
clust1<-hclust(distance, method = "ward")
plot(clust1)


