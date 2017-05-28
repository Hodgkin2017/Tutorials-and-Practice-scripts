m <- matrix(data=cbind(rnorm(30, 0), rnorm(30, 2), rnorm(30, 5)), nrow=30, ncol=3)
m

apply(m, 1, mean)
apply(m, 2, mean)

apply(m, 2, function(x) length(x[x<0]))
apply(m, 2, function(x) x[x<0])
apply(m, 2, function(x) is.matrix(x))

apply(m, 2, is.vector)

apply(m, 2, length(x[x<0]))


apply(m, 2, function(x) mean(x[x>0]))

sapply(1:3, function(x) x^2)
sapply(m, function(x) x^2)
m

lapply(1:3, function(x) x^2)

sapply(1:3, function(x) x^2, simplify=F)

unlist(lapply(1:3, function(x) x^2))

sapply(1:3, function(x) mean(m[,x]))
apply(m,2,function(x) mean(x))

sapply(1:3, function(x, y) mean(y[,x]), y=m)


###############

################
##Data

CNV.data<-cnv.list

CNV.data[[38]]<- CNV.all.table

##############
###Learning
lapply(1:3, function(x) c(x, x^2, x^3))

new.function<- function(x){
  (x+1)*2
}

lapply(1:3, function(x) new.function(x))

x<- matrix(rpois(1e4, 8), 1e3)
dim(x)

apply(x, 2, mean)
apply(x, 2, function(x) sd(x)/sqrt(length(x)))
apply(x, 2, function(x) {sd(x)/sqrt(length(x))})

GETCI<- function(x, level=0.95){
  if(level <= 0 || level >=1) {
    stop("The 'level' argument must be >- and <1")
  }
  if (level < 0.5) {
    warning("Confidence levels are often close to 1, e.g. 0.95")
  }
  
  m <-mean(x)
  n<- length(x)
  SE<- sd(x)
  upper<- 1 - (1-level)/2
  ci<- m+c(-1,1)*qt(upper, n-1)*SE
  return(list(mean=m, se=SE, ci=ci))
  
}

apply(x, 2, function(x) GETCI(x))


#####################
### Learning how to use lappy with my own function to speak up data processing(hopefully!)


### Function to process data

length(CNV.data)



new.function<- function(x,y){
  
  z<- dplyr::full_join(y[,1:8],x, by = "Locus.ID")
  cytoband.list<- events.per.cytoband(z, threshold = -1, cytoband_column = 10, column_data_start = 11, chromosome_interval = 0,  deletion = TRUE)
  return(cytoband.list[[2]]$proportion.of.deletions)
  
}

system.time(test5<- lapply(CNV.data, function(x) {new.function(x, acc.cnv.chr.location)}))

# ?do.call
# length(test5)
# str(test5)
test6<- do.call(cbind, test5) %>%as.matrix
# class(test6)
# dim(test6)
# all.equal(test6, heatmap.matrix.cytoband.del)
# identical(test6, heatmap.matrix.cytoband.del)
# head(heatmap.matrix.cytoband.del)
# class(heatmap.matrix.cytoband.del)


rownames(test6)<- cytoband.list[[2]]$cytoband
colnames(test6)<- c(cancer.type, "ALL")



pheatmap(test6,
         cluster_row = F,
         cluster_cols = F,
         show_rownames = TRUE,
         show_colnames = TRUE,
         color = col.pal,
         fontsize_row=1,
         #cellwidth = 10,
         annotation_row = annotation_row,
         annotation_legend = FALSE
)


##########
## Try multiple cores:

library(doMC)
options(cores = 7)
registerDoMC()

CNV.data<-cnv.list

CNV.data[[38]]<- CNV.all.table


new.function<- function(x,y){
  
  z<- dplyr::full_join(y[,1:8],x, by = "Locus.ID")
  cytoband.list<- events.per.cytoband(z, threshold = -1, cytoband_column = 10, column_data_start = 11, chromosome_interval = 0,  deletion = TRUE)
  return(cytoband.list[[2]]$proportion.of.deletions)
  
}

system.time(test5<- lapply(CNV.data, function(x) {new.function2(x, acc.cnv)}))


##########
##multi-core test 1:

output=vector(length=38)
output

output=foreach(i = CNV.data) %dopar% {
  i[,1:3]
}

length(output)
class(output)
dim(output[[1]])

##########
##multi-core test 2:

## Get chromosomal locations:
acc.cnv<-chromosomal_location(cnv.list[[1]])
head(acc.cnv[,1:12])

##multi-core loop:

output=vector(length=38)
#works:
output=foreach(i = CNV.data) %dopar% {
  acc.cnv[,1:8]}
output[[1]]
#works:
output=vector(length=38)
output=foreach(i = CNV.data) %dopar% {
  dplyr::full_join(acc.cnv[,1:8],acc.cnv[,1:3], by = "Locus.ID")}
output[[1]]
#does not work...error:
output=vector(length=38)
output=foreach(i = CNV.data) %dopar% {
  dplyr::full_join(acc.cnv[,1:8],i, by = "Locus.ID")}
output[[1]]

#does not work...error:
output=vector(length=38)
output=foreach(i = 1:38) %dopar% {
  dplyr::full_join(acc.cnv[,1:8],CNV.data[[i]], by = "Locus.ID")}
output[[1]]

#does not work...error:
output=vector(length=38)
output=foreach(i=CNV.data) %dopar% {
  i %>% dplyr::full_join(acc.cnv[,1:8],., by = "Locus.ID")}
output[[1]]

  ############
##rest of function:
  cytoband.list<- events.per.cytoband(z, threshold = -1, cytoband_column = 10, column_data_start = 11, chromosome_interval = 0,  deletion = TRUE)
  return(cytoband.list[[2]]$proportion.of.deletions)
  }

output=foreach(i = CNV.data) %dopar% {
  dplyr::full_join(acc.cnv[,1:8],i, by = "Locus.ID") }
#%>%
  #events.per.cytoband(., threshold = -1, cytoband_column = 10, column_data_start = 11, chromosome_interval = 0,  deletion = TRUE %>%
  #return(.[[2]]$proportion.of.deletions)
#}

length(output)
head(output[[1]][,1:12])

############
##multi-core test 3:
##ONly one that works:

new.function2<- function(x,y){
  
  z<- x %>% dplyr::full_join(y[,1:8],., by = "Locus.ID") %>%
  events.per.cytoband(., threshold = -1, cytoband_column = 10, column_data_start = 11, chromosome_interval = 0,  deletion = TRUE)
  return(z[[2]]$proportion.of.deletions)
  
}
new.function6<- function(x,y){
  
dplyr::full_join(y[,1:8],x, by = "Locus.ID")
  
}

new.function7<- function(x){
  
  z<- events.per.cytoband(x, threshold = -1, cytoband_column = 10, column_data_start = 11, chromosome_interval = 0,  deletion = TRUE)
  return(z[[2]]$proportion.of.deletions)
  
}

library(parallel)

# Calculate the number of cores
no_cores <- detectCores() - 1
no_cores
no_cores<- 4

# Initiate cluster
cl <- makeCluster(no_cores)
cl

clusterExport(cl, "acc.cnv")
clusterExport(cl, "new.function")
clusterExport(cl, "new.function2")
clusterExport(cl, "new.function6")
clusterExport(cl, "new.function7")
clusterExport(cl, "events.per.cytoband")
clusterEvalQ(cl, library(dplyr))

test5=vector(length=38)
system.time(test5<- parLapply(cl, CNV.data, function(x) {new.function(x, acc.cnv)}))
system.time(test5<- parLapply(cl, CNV.data, function(x) {new.function2(x, acc.cnv)}))
system.time(test5<- parLapply(cl, CNV.data, function(x) {new.function6(x, acc.cnv)}))
system.time(test5<- parLapply(cl, test5, function(x) {new.function7(x)}))

stopCluster(cl)

length(test5)
head(test5[[1]])
##Time = 157 (50s faster....not much better)

test6<- do.call(cbind, test5) %>%as.matrix



rownames(test6)<- CNV.data[[1]]$cytoband
colnames(test6)<- c(cancer.type, "ALL")



pheatmap(test6,
         cluster_row = F,
         cluster_cols = F,
         show_rownames = TRUE,
         show_colnames = TRUE,
         color = col.pal,
         fontsize_row=1
         #cellwidth = 10,
         #annotation_row = annotation_row,
         #annotation_legend = FALSE
)

############
##multi-core test 4:


new.function<- function(x){
  
  z<- dplyr::full_join(acc.cnv[,1:8],x, by = "Locus.ID")
  cytoband.list<- events.per.cytoband(z, threshold = -1, cytoband_column = 10, column_data_start = 11, chromosome_interval = 0,  deletion = TRUE)
  return(cytoband.list[[2]]$proportion.of.deletions)
  
}

new.function3<- function(x){
  
  dplyr::full_join(acc.cnv[,1:8],x, by = "Locus.ID") %>%
  events.per.cytoband(threshold = -1, cytoband_column = 10, column_data_start = 11, chromosome_interval = 0,  deletion = TRUE) %>%
  return(.[[2]]$proportion.of.deletions)
  
}

system.time(test7 <- mclapply(X=CNV.data, FUN=new.function, mc.cores=19))
system.time(test7 <- mclapply(X=CNV.data, FUN=new.function3, mc.cores=4))
#####

test8<- mclapply(X=CNV.data, FUN=function(x) dplyr::full_join(acc.cnv[,1:8],x, by = "Locus.ID"), mc.cores=4)
test9<- mclapply(X=test8, FUN=events.per.cytoband(x, threshold = -1, cytoband_column = 10, column_data_start = 11, chromosome_interval = 0,  deletion = TRUE),x, by = "Locus.ID"), mc.cores=4))
new.function4<-function(x) {dplyr::full_join(acc.cnv[,1:8],x, by = "Locus.ID")}
test8<- mclapply(CNV.data, new.function4, mc.cores=4)
new.function5<-function(x) {}
##############
##Old method:

#old.function<- function(cnv.list, acc.cnv.chr.location, CNV.all.table){}
old.function<- function(){
  cancer.type<- names(cnv.list)
  cancer.type
  
  heatmap.matrix.cytoband.del<- matrix(NA, ncol = length(cancer.type)+1, nrow = 806)
  
  
  for (i in 1:length(cancer.type)){
    
    x<-cnv.list[[i]]
    x<- dplyr::full_join(acc.cnv.chr.location[,1:8],x, by = "Locus.ID")
    cytoband.list<- events.per.cytoband(x, threshold = -1, cytoband_column = 10, column_data_start = 11, chromosome_interval = 0,  deletion = TRUE)
    heatmap.matrix.cytoband.del[,i]<- cytoband.list[[2]]$proportion.of.deletions
    #print(cancer.type[i])
  }
  
  #head(heatmap.matrix.cytoband.del)
  
  ##Final column contains all cancer types:
  
  ##Create one large dataframe with all CNV data in it:
  #x<-join.cnv.datasets(cnv.list, 4)
  
  ##Calculate proportion of deletions per cytoband and add to matrix
  x<- dplyr::full_join(acc.cnv.chr.location[,1:8],CNV.all.table, by = "Locus.ID")
  cytoband.list<- events.per.cytoband(x, threshold = -1, cytoband_column = 10, column_data_start = 11, chromosome_interval = 0,  deletion = TRUE)
  heatmap.matrix.cytoband.del[,38]<- cytoband.list[[2]]$proportion.of.deletions
}

system.time(old.function())
