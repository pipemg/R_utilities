
```{r, message=FALSE, warning=FALSE, error=FALSE}

list=c("NANOG", "POU5F1", "POU5F1B", "SOX2", "KLF4", "MYC",  "FOXC2" ,"PMS1", "TGFB3", "PAX6", "PARP1", "ZFX", "BRIP1", "WEE1", "CDKN2A", "TP63", "STAT3", "MMP7", "AGR2", "GDA" ,"CD63", "KRT7");

names_ids<-unique(names_df[names_df$symbol %in% list,c("symbol","entrez")])

colors_clasification<-c("blue","yellow","red")
samplecolors <- unlist(lapply(phenodata$samples[phenodata$samples %in% c("CervixCancer-SCC", "lesion-high","NormCervix")], function(mol.biol){colors_clasification[which(unique(phenodata$samples[phenodata$samples %in% c("CervixCancer-SCC", "lesion-high","NormCervix")])==mol.biol)]}))
  

data<- exprs(gset)[names_ids$entrez,phenodata$samples %in% c("CervixCancer-SCC", "lesion-high", "NormCervix")] 

df<-data.frame(row.names = c(1:60))
 k=1

for(i in 1:length(names_ids$symbol)){
  scaled<-scale(data[i,])
  for(j in 1:3){
    df$sample[k]<-as.character(unique(phenodata$samples)[j])
    df$gene[k]<-as.character(unlist(names_ids$symbol[i]))
    sury<-summary(scaled[phenodata$samples==unique(phenodata$samples)[j]])
    df$min[k]<-sury[1]
    df$FstQu[k]<-sury[2]
    df$median[k]<-sury[3]
    df$mean[k]<-sury[4]
    df$TrdQu[k]<-sury[5]
    df$max[k]<-sury[6]
    k=k+1
  }
}


# Change barplot fill colors by groups
p<-ggplot(df, aes(x=gene, y=median, fill=sample)) +
  geom_bar(stat="identity", position="dodge")
p
```
