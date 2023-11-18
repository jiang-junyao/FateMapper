#devtools::install_github("jiang-junyao/FateMapper")
#devtools::install_github("caleblareau/BuenColors")
#setwd('F:\\scLT-seq db\\script')
library(FateMapper)
library(BuenColors)
library(ggplot2)
### load data
col<- rev(colorRampPalette(c("#cc0000", "#FFff00",'#66ccff','#000066'))(50))
save_dir = 'F:\\scLT-seq db\\out_result/'
obj_metadata_list = readRDS('..\\metadata_rds/obj_metadata_list.rds')
for (i in 1:length(obj_metadata_list)) {
  dataset_name = names(obj_metadata_list)[i]
  metadata = obj_metadata_list[[i]]
  colnames(metadata)[1:2] = c('UMAP_1','UMAP_2')
  ### UMAP
  if (nrow(metadata)>10000) {
    cell_size=0.5
  }else{
    cell_size=1
  }
  png(filename = paste0(save_dir,'dataset_umap/',dataset_name,'.png'),
      width = 3000,height = 2400,res = 400)
  print(ggplot(metadata,aes(x=UMAP_1,y=UMAP_2,color=celltype))+geom_point(size=cell_size)+
    theme_void()+scale_color_manual(values = c(jdb_palette("corona"),
                                               jdb_palette("FantasticFox"))))
  dev.off()
  ### clone analysis
  cell_fate = metadata[,c('barcodes','celltype')]
  cell_fate = cell_fate[cell_fate$barcodes!='',]
  cell_fate = cell_fate[!is.na(cell_fate$barcodes),]
  barcode_freq = fate_mapping(cell_fate,show_row = F,
                              cluster_rows = T)
  barcode_freq = apply(barcode_freq, 1, function(x){
    row_sum = sum(x)
    return(x/row_sum)
  })
  png(filename = paste0(save_dir,'clone_expression_fig/',dataset_name,'.png'),
      width = 1200,height = 1000,res = 200)
  print(pheatmap::pheatmap(t(barcode_freq),show_rownames =F,color = col
                     ,cluster_rows = T,cluster_cols = T,border_color=NA))
  dev.off()
  write.table(barcode_freq,paste0(save_dir,'clone_expression_table/',
                                  dataset_name,'.txt'),quote = F,sep = '\t')
  ### cell type similarity
  png(filename = paste0(save_dir,'cell_type_similarity/',dataset_name,'.png'),
      width = 1200,height = 1000,res = 200)
  print(cell_type_fate_similartiy(cell_fate))
  dev.off()
  ### clone fate bias
  dir.create(paste0(save_dir,'clone_fate_bias/',dataset_name))
  bias_list = list()
  for (j in unique(cell_fate[,2])) {
    ct_fate_bias = clone_fate_bias(cell_fate,j)
    bias_list[[j]] = ct_fate_bias
    # write.table(ct_fate_bias,paste0(save_dir,'clone_fate_bias/',
    #                                 dataset_name,'/',j,'.txt'),quote = F,
    #                                 row.names = F,sep = '\t')
    ct_name = gsub('/','_',j)
    dir.create(paste0(save_dir,'clone_fate_bias_summary/'
                      ,dataset_name))
    png(filename = paste0(save_dir,'clone_fate_bias_summary/'
                          ,dataset_name,'/',ct_name,'.png'),
        width = 1200,height = 1000,res = 200)
    print(fate_bias_summary(ct_fate_bias))
    dev.off()
  }
  saveRDS(bias_list,paste0(save_dir,'clone_fate_bias/',
                           dataset_name,'.rds'),compress = F)


}


