library(Seurat)
library(tidyverse)
dir1 = 'F:\\scLT-seq db\\data/'
fate_bias_dir = 'F:\\scLT-seq db\\out_result\\clone_fate_bias/'
all_data = dir(dir1)

all_fate_bias_marker = list()
for (i in all_data) {
  obj = readRDS(paste0(dir1,i))
  metadata = obj@meta.data
  fate_bias = readRDS(paste0(fate_bias_dir,i))
  sig_fate = c()
  all_ct = unique(metadata$celltype)
  ### retina sig
  for (j in 1:length(fate_bias)) {
    fate = fate_bias[[j]]
    fate = fate[fate$fdr<0.05,]
    fate = fate[fate$fate_ratio>0.1,]
    print(paste0(names(fate_bias)[j],' num:',nrow(fate)))
    if (nrow(fate)>0) {
      fate_bias[[names(fate_bias)[j]]] = fate
      sig_fate = c(sig_fate,names(fate_bias)[j])
    }
  }
  ### all cell fate combinations
  marker_list = list()
  combinations <- combn(sig_fate, 2)
    
  for (j in 1:ncol(combinations)) {
    fate1 = fate_bias[[combinations[1,j]]]$clone_name
    fate2 = fate_bias[[combinations[2,j]]]$clone_name
    fate1_specific = fate1[!fate1 %in% fate2]
    fate2_specific = fate2[!fate2 %in% fate1]
    ### call deg for each cell type based on fate bias group
    for (k in all_ct) {
      ct_meta = metadata[metadata$celltype==k,]
      fate1_cell = rownames(ct_meta[ct_meta$barcodes %in% fate1_specific,])
      fate2_cell = rownames(ct_meta[ct_meta$barcodes %in% fate2_specific,])
      if (length(fate2_cell)>0 & length(fate1_cell)>0) {
        data = subset(obj,cells = c(fate1_cell,fate2_cell))
        data$lineage = ifelse(colnames(data) %in% fate1_cell,combinations[1,j],
                           combinations[2,j])
        data@active.ident = as.factor(data$lineage)
        marker = FindAllMarkers(data,only.pos = T,test.use = 'negbinom')
        if (nrow(marker)>0){
            marker = marker[order(marker$p_val_adj),]
            #marker$cluster = paste(marker$cluster,'bias')
            index = paste(combinations[1,j],'bias','VS',combinations[2,j],
                          'bias','-',k)
            marker$index = index
            marker_list[[index]] = marker
        }else{
            marker_list[[index]] = NA
        }

      }
    }
  }
  marker_list = marker_list[!is.na(marker_list)]
  marker_df = do.call(bind_rows,marker_list)
  all_fate_bias_marker[[i]] = marker_df
}





