

##################################
# VOLCANO PLOT per GENES HaCaT vs Keratinocyte

rv = rowVars(gset[rownames(gset) %in% names(entrezid[entrezid %in% recon1]),])
select = order(rv, decreasing=TRUE)#[seq_len(500)]

f <- factor(phenodata$samples)
samples <- as.factor(phenodata$samples)
design <- model.matrix(~0 + samples)
fit <- lmFit(gset, design)

contrast.matrix <- makeContrasts(keratinocyte_HaCaT <- samplesHaCaT-sampleskeratinocyte,  levels=design)
huvec_fits <- contrasts.fit(fit, contrast.matrix)
huvec_ebFit <- eBayes(huvec_fits)

#topTable(huvec_ebFit, number=10, coef=1)
nrow(topTable(huvec_ebFit, coef=1, number=10000, lfc=5))
probeset.list <- topTable(huvec_ebFit, coef=1, number=10000, lfc=4)


gene_list <- topTable(huvec_ebFit, coef=1, number=1000000, sort.by="logFC")
#head(gene_list$logFC)
#head(gene_list$P.Value)

#plot(gene_list$logFC, -log10(gene_list$P.Value),
#     xlim=c(-10, 10), ylim=c(0, 15), #Set limits
#     xlab="log2 fold change", ylab="-log10 p-value")

#sum(abs(gene_list$logFC) > 2 & gene_list$P.Value < 0.05)

#sum(abs(gene_list$logFC) > 2 & gene_list$P.Value < 0.05/no_of_genes)
#sum(abs(gene_list$logFC) > 2 & gene_list$adj.P.Val < 0.015)

no_of_genes = dim(probeset.list)[1]



gene_list$threshold = as.factor(abs(gene_list$logFC) > 2 & gene_list$P.Value < 0.01/no_of_genes)


gene_list_upreg = intersect (which(gene_list$logFC > 2),which( gene_list$P.Value < 0.01/no_of_genes))
gene_list_dwreg = intersect (which(gene_list$logFC < -2), which( gene_list$P.Value < 0.01/no_of_genes))


g = ggplot(data=gene_list, aes(x=logFC, y=-log10(P.Value),label="Keratinocyte_HaCaT", colour=threshold), ) +
  geom_point(alpha=0.4, size=1.75) +
  theme(legend.position = "none") +
  xlim(c(-10, 10)) + ylim(c(0, 15)) +
  xlab("log2 fold change") + ylab("-log10 Pvalue")


pdf("volcano_Keratinocyte_HaCaT_plot.pdf");
g
dev.off();




###
##CREATE KEGG LISTS
###


kegglink<-keggLink("hsa", "pathway");
Pathways<-unique(names(keggLink("hsa", "pathway")))
kegg<-data.frame(Pathways);

keggName<-function(x){return(keggGet(x)[[1]]$NAME);}
keggPathwaysNames<-unlist(lapply(Pathways,FUN="keggName"));
kegg$Names <- gsub(" - Homo sapiens (.+)+","",keggPathwaysNames,ignore.case=TRUE,perl=TRUE)

getGenesKegg<-function(x) {
	wh<-which(names(keggLink("hsa", "pathway")) %in% x)
	list<-keggLink("hsa", "pathway")[wh]
	return(as.vector(list));
}

kegg$KeggGenes<-lapply(Pathways,FUN="getGenesKegg")


keggConv2<-function(x){
	keggConv("ncbi-geneid",x)
}

for(i in 1:length(kegg$Names)){
	vec<-lapply(unlist(kegg$KeggGenes[i]),keggConv2);
	ret<-gsub("ncbi-geneid:","",vec,ignore.case=TRUE,perl=TRUE)
	kegg$EntrezGenes[i]=as.data.frame(as.integer(ret));
}


save(kegg, file = "kegg.RData")

save(list=ls(), file = "Backup.RData");

