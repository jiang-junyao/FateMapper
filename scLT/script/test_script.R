#devtools::install_github("jiang-junyao/FateMapper")
#devtools::install_github("caleblareau/BuenColors")
library(FateMapper)
library(BuenColors)
library(ggplot2)
### load data
obj_metadata_list = readRDS('..\\metadata_rds/obj_metadata_list.rds')
metadata = obj_metadata_list[[1]]
colnames(metadata)[1:2] = c('UMAP_1','UMAP_2')
### UMAP
ggplot(metadata,aes(x=UMAP_1,y=UMAP_2,color=celltype))+geom_point()+
  theme_void()+scale_color_manual(values = jdb_palette("corona"))

### Barcode visualization on UMAP
plot_clone_embedding(c('394'),metadata,colors = c(rgb(200/255,200/255,200/255),
                                             rgb(239/255,153/255,81/255)),)

### clone analysis
cell_fate = metadata[,c('barcodes','celltype')]
cell_fate = cell_fate[!is.na(cell_fate$barcodes),]
barcode_freq = fate_mapping(cell_fate,show_row = F)

### cell type similarity
ct_similarity = cell_type_fate_similartiy(cell_fate)

### clone fate bias
bias_list = list()
for (i in unique(cell_fate[,2])) {
  bias_list[[i]] = clone_fate_bias(cell_fate,i)
}
