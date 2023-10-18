#' Title
#'
#' @param barcode_use vector, indicating barcodes for ploting
#' @param pbmc seurat object
#'
#' @return
#' @export
#'
#' @examples
plot_clone_embedding <- function(barcode_use,pbmc,colors=c(rgb(200/255,200/255,200/255),
                                                        rgb(230/255,230/255,230/255),
                                                        rgb(239/255,153/255,81/255),
                                                        rgb(91/255,166/255,218/255),
                                                        rgb(217/255,83/255,25/255)
)){
  ### pbmc is seurat object need cell_fate in metadata
  ### barcode_use is vector
  barcode_anno = rep('other barcode',ncol(pbmc))
  barcode_anno[is.na(pbmc$barcodes)] = 'no barcode'
  for (i in 1:length(colnames(pbmc))) {
    if (pbmc@meta.data$barcodes[i] %in% barcode_use) {
      barcode_anno[i] = paste0('select_barcode:',pbmc@meta.data$barcodes[i])
    }
  }
  pbmc[['Barcode_family']]=barcode_anno
  coor = as.data.frame(pbmc@reductions$umap@cell.embeddings)
  coor$barcode_type = pbmc@meta.data$Barcode_family
  coor1 = coor[coor$barcode_type %in% c('other barcode','no barcode'),]
  coor2 = coor[!coor$barcode_type %in% c('other barcode','no barcode'),]
  ggplot(coor1,aes(x=UMAP_1,y=UMAP_2,color=barcode_type))+geom_point(data=coor1,size=1)+
    geom_point(data=coor2,aes(x=UMAP_1,y=UMAP_2),size=2)+theme_void()+
    scale_color_manual(values = colors)
}

multi_relationship = function(pbmc){
  library(ggplot2)
  ### seurat object with cell_fate & cell_type
  cell_use = rownames(pbmc@meta.data)[!is.na(pbmc@meta.data$cell_fate)]
  pbmc_plot = subset(pbmc,cells=cell_use)
  ave_exp = AverageExpression(pbmc,slot = 'data')$SCT
  ave_atac = AverageExpression(pbmc,slot = 'data')$ATAC
  cor.exp = reshape2::melt(cor(ave_exp,method = 'spearman'))
  cor.atac = reshape2::melt(cor(ave_atac,method = 'spearman'))
  cell_fate = pbmc@meta.data[,c('cell_fate','cell_type')]
  cell_fate = cell_fate[!is.na(cell_fate[,1]),]
  colnames(cell_fate)[2] = 'celltype'
  cor.barcode = reshape2::melt(as.matrix(cell_type_hier_heatmap(cell_fate,plot=F)))
  multi_rela = cbind(cor.barcode,cor.exp[,3],cor.atac[,3])
  multi_rela = multi_rela[multi_rela$Var1!=multi_rela$Var2,]
  colnames(multi_rela)[3:5] = c('lineage','rna','atac')
  min_num = min(multi_rela[,4],multi_rela[,5])
  if (min_num-0.05 < 0) {
    min_num = 0
  }else{
    min_num = min_num-0.05
  }
  rl_slope = round(coef(lm(rna ~ lineage, data = multi_rela))[2],3)
  al_slope = round(coef(lm(atac ~ lineage, data = multi_rela))[2],3)
  ggplot(multi_rela, aes(x = rna, y = atac, color = lineage)) +
    geom_point(size = 4) +
    labs(x = "RNA correlation", y = "ATAC correlation", color = "Lineage similarity") +
    theme_minimal()+xlim(c(min_num,1))+ylim(c(min_num,1))+geom_smooth(method = 'lm')+
    ggtitle(paste0('rna+lineage slope: ',rl_slope,' \natac+lineage slope: ',al_slope))
  return(multi_rela)
}

circu_plot <- function(pbmc){
  ### seurat object with cell_fate and cell_type
  library(ggraph)
  library(igraph)
  meta = pbmc@meta.data

  meta = meta[!is.na(meta$cell_fate),]
  meta =  meta[,c('cell_fate','cell_type')]
  meta$cell_fate = as.character(meta$cell_fate)
  meta$cell_type = as.character(meta$cell_type)
  hierarchy = meta
  df2 = data.frame('from' = rep(' polylox',length(unique(hierarchy$from))),'to'=unique(hierarchy$from),
                   stringsAsFactors = F)
  hierarchy = rbind(hierarchy,df2)
  colnames(hierarchy) = c('from','to')

  # create a vertices data.frame. One line per object of our hierarchy, giving features of nodes.
  ver_barcode = data.frame(name = unique(meta$cell_fate),color = rep(' polylox',length(unique(meta$cell_fate))))
  ver_barcode = rbind(ver_barcode,c(' polylox',' polylox'))
  ver_ct = data.frame(name = unique(meta$cell_type),color = unique(meta$cell_type))
  vertices <- rbind(ver_barcode,ver_ct)

  mygraph <- graph_from_data_frame(hierarchy, vertices=vertices )

  ggraph(mygraph, layout = 'dendrogram', circular = TRUE) +
    geom_edge_diagonal(color=rgb(230/255,230/255,230/255)) +
    theme_graph() +
    geom_node_point(aes(color = color), size = 2) +scale_colour_manual(values = cols)
}

