

###################################
##VOLCANOPLOT


f <- factor(phenodata$cell_lines)
samples <- as.factor(phenodata$cell_lines)
design <- model.matrix(~0 + samples)
fit <- lmFit(exprset_biggest, design)

contrast.matrix <- makeContrasts(keratinocyte_HeLa <-  samplesHeLa-sampleskeratinocyte,  levels=design)
huvec_fits <- contrasts.fit(fit, contrast.matrix)
huvec_ebFit <- eBayes(huvec_fits)

nrow(topTable(huvec_ebFit, coef=1, number=10000, lfc=5))
probeset.list <- topTable(huvec_ebFit, coef=1, number=10000, lfc=4)
gene_list <- topTable(huvec_ebFit, coef=1, number=1000000, sort.by="logFC")


plot(gene_list$logFC, -log10(gene_list$P.Value),
     xlim=c(-10, 10), ylim=c(0, 15), #Set limits
     xlab="log2 fold change", ylab="-log10 p-value")

no_of_genes = dim(probeset.list)[1]

gene_list$threshold = as.factor(abs(gene_list$logFC) > 2 & gene_list$P.Value < 0.05/no_of_genes)

gene_list_upreg = intersect (which(gene_list$logFC > 2),which( gene_list$P.Value < 0.05/no_of_genes))
gene_list_dwreg = intersect (which(gene_list$logFC < -2), which( gene_list$P.Value < 0.05/no_of_genes))


  g = ggplot(data=gene_list, aes(x=logFC, y=-log10(P.Value),label="HeLa_Keratinocyte", colour=threshold), ) +
  geom_point(alpha=0.4, size=1.75) +
  theme(legend.position = "none") +
  xlim(c(-10, 10)) + ylim(c(0, 15)) +
  xlab("log2 fold change") + ylab("-log10 p-value")


pdf("volcano_plot.pdf");
g
dev.off();






