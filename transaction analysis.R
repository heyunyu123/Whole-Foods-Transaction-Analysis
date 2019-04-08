## per transaction id is per customer id
wf<-read.csv("/Users/heyunyu/Downloads/Whole_Foods_Transaction_Data.csv")
View(wf)
list<-unique(wf$transaction_id)
re<-c()
aor<-c()
for (i in 1:length(list)){
  pos<-which(wf$transaction_id==list[i])
  aor[i]<-length(pos)
  re[i]<-sum(wf$reordered[pos])
} 
rate<-re/aor  ##acquire the reorder rate per customer
count(rate==1)
ratio_re<-1-sum(re==0)/length(re) ## percentage of repeat purchases
re

aor ##product number count for per customer

## all customer IDs with reorder rate==1 
list_new<-list[which(rate==1)]
aor_new<-aor[which(rate==1)] ## the number of product for per transaction/ID
kind<-c() ## the number of different department for per ID
for (i in 1:length(list_new)){
  pos<-which(wf$transaction_id==list_new[i])
  kind[i]<-length(unique(wf$department[pos]))
}  

wf_subset<-cbind(list_new,aor_new,kind)
write.table(wf_subset,"wf_subset.csv",sep=",",row.names = FALSE)

unique(wf$aisle_id)
unique(wf$aisle)
unique(wf$department_id)
unique(wf$department)
unique(wf$product_name)

m<-as.vector(unique(wf$department))
dic<-cbind(unique(wf$department_id),m)
write.table(dic,"dict.csv",sep=",",row.names = FALSE)

cate<-c()
for (i in 1:length(list)){
  pos1<-which(wf$transaction_id==list[i])
  cate[i]<-length(unique(wf$department[pos1]))
}
ratio_var<-sum(cate>5)/length(cate)
ratio_var ## percentage of purchases with more than three departments

unique(wf$product_name)

cate_matrix<-matrix(0,nrow=length(list),ncol = length(unique(wf$department_id)))
for (i in 1:length(list)){
  pos<-which(wf$transaction_id==list[i])
  for(j in 1:length(unique(wf$department_id))){
   ## i is the ith customer in the ith row, and j is the jth item
    cate_matrix[i,j]=sum(wf$department_id[pos]==j)>0
  }

}
cate_matrix
cor_matr = cor(cate_matrix)
symnum(cor_matr)

library(Hmisc)
output<-rcorr(cate_matrix)
p<-as.data.frame(output$P)
r<-as.vector(output$r)
pos2<-order(r)
pos22<-pos2[-(421:441)]
r[pos2]

## correlation coefficient ranking with significant p value
de_co<-c()
cor_col1<-c()
cor_row1<-c()
for (i in length(pos22):1){
  if(i%%2!=0){
    cor_row<-pos22[i]%%21
    cor_col<-ceiling(pos22[i]/21)
    if (cor_row==0){
      cor_row=21
    }
    if(is.na(output$P[cor_col,cor_row])==0 & output$P[cor_col,cor_row]<=0.05){
      cor_row1<-c(cor_row1,pos22[i]%%21)
      cor_col1<-c(cor_col1,ceiling(pos22[i]/21))
      de_co<-c(de_co,output$r[cor_col,cor_row])
    }
  }
}
  
rr<-cbind(cor_col1,cor_row1)
rr
rrr<-rbind(cor_col1,cor_row1)
rrr

table<-cbind(rr,de_co)
table
write.table(table,"fff.csv",sep=",",row.names=FALSE)
##
output$n
max(r)

##
r[r==1]<-0
r1<-r
which(r==0)
r2<-r1
max(r2)

## all transactions group by product name
sum_pro<-c()

list1<-unique(wf$product_name)
for (i in 1:length(list1)){
  rank_pro<-which(wf$product_name==list1[i])
  sum_pro[i]<-sum(new_or[rank_pro])
}
list1

pro_name<-c()
pro_name<-as.string(pro_name)
list1<-unique(wf$product_name)
for (i in 1:2){
  pro_name[i]=
   list1[i]
}

sum_prob<-c()
list1<-unique(wf$product_name)
for (i in 1:length(list1)){
  rank_pro<-which(wf$product_name==list1[i])
  sum_prob[i]<-length(rank_pro)
}

wf$department<-as.character(wf$department)
list1<-as.character(list1)
dep<-c()
orr<-c()
for (i in 1:length(list1)){
  rank_pro<-which(wf$product_name==list1[i])
  dep[i]<-wf$department[rank_pro[1]]
  orr[i]<-sum(wf$reordered[rank_pro])
}

na<-c()
for (i in 1:length(list1)){
  
  na[i]<-list1[i]
}

data_new<-read.csv("p1.csv")
View(data_new)
de<-unique(wf$department)

dep_list<-c()
dep_sum<-c()
de<-as.character(de)
for (i in 1:length(de)){
  ro<-which(data_new$dep==de[i])
  dep_sum[i]<-sum(data_new$x[ro])
}

for (i in 1:length(de)){
  ro<-which(data_new$dep==de[i])
  dep_sum[i]<-sum(data_new$sum[ro])
}

new_or<-ifelse(wf$reordered==0,1,2)


sum_dep<-c()
list2<-unique(wf$department)
for (i in 1:length(list2)){
  rank_pro<-which(wf$department==list2[i])
  sum_dep[i]<-length(rank_pro)
}


wf$department<-as.character(wf$department)
list2<-as.character(list2)
dep_name<-c()
orr<-c()
for (i in 1:length(list2)){
  rank_pro<-which(wf$department==list2[i])
  dep_name[i]<-list2[i]
  orr[i]<-sum(wf$reordered[rank_pro])
}

rank_pro<-which(wf$department=="other")
rank_pro

fff<-read.csv("fff.csv")
fff$Department_id_2<-ifelse(fff$Department_id_2==0,21,fff$Department_id_2)
table(fff$Department_id_2)
v1<-table(fff$Department_id_1)
v2<-table(fff$Department_id_2)
v1<-as.data.frame(v1)
v2<-as.data.frame(v2)
f_v<-merge(v1,v2,all=TRUE)
sort(f_v$Var1)
library(plyr)
ff_v<-rbind.fill(v1,v2)
ff_v
##frequency for each department
tot<-aggregate(Freq~Var1,data=ff_v,FUN="sum")

## dictionaty of department_id
dict<-c()
for(i in 1:21){
  dict[i]=as.character(wf$department[which(wf$department_id==i)[1]])
}

subset2<-cbind(tot,dict)
write.table(subset2,"subset2.csv",sep=",",row.names=FALSE) 
##product sale and retention rate
sale<-c()
ret<-c()
for (i in 1:length(list1)){
  pos<-which(wf$product_name==list1[i])
  sale[i]<-length(pos)
  ret[i]<-sum(wf$reordered[pos])/sale[i]

}
##normalization
sale<-(sale-min(sale))/(max(sale)-min(sale))
ret<-(ret-min(ret))/(max(ret)-min(ret))

sale<-scale(sale,center = TRUE,scale=TRUE) ##standardized
ret<-scale(ret,center = TRUE,scale=TRUE)
subset1<-cbind(list1,sale,ret)
write.table(subset1,"subset1.csv",sep=",",row.names = FALSE)
