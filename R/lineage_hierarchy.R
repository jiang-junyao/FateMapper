
#' fate mapping heatmap
#' @description plot heatmap for lineage tracing barcodes in each cell type
#' @param data data.frame, indicating lineage tracing data, first column should
#' be lineage tracing barcodes, second column should be related cell type
#' @param idx column name of cell type
#' @param order_use order cell type
#' @param show_rownames vector,indicating the order of cell types in the heatmap.
#' @param col heatmap colors
#' @param cluster_row boolean values determining if rows should be clustered
#' @param cluster_cols boolean values determining if cols should be clustered
#' @param ... other pheatmap parameters
#' @importFrom pheatmap pheatmap
#' @return
#' heatmap
#' @export
#'
#' @examples fate_ct_heatmap(fate_test)
fate_ct_heatmap <- function(data,idx='cell_type',order_use=NULL,
                            show_rownames = T,
                            col = rev(colorRampPalette(c("#cc0000"
                                                         , "#FFff00",
                                                         '#66ccff',
                                                         '#000066'))(50)),
                            cluster_row = F,cluster_cols = T,
                            ...){

  lineage_use = unique(data[,idx])
  freq_list = list()

  for (i in unique(data[,1])) {
    data_use = data[data[,1]==i,]
    freq = as.data.frame(table(data_use[,idx]))
    freq$Var1 = as.character(freq$Var1)
    freq = freq[freq$Freq!=0,]
    lineage_absent = lineage_use[!lineage_use %in% as.character(freq$Var1)]
    lineage_absent_df =data.frame(lineage_absent,rep(0,length(lineage_absent)))
    colnames(lineage_absent_df) = colnames(freq)
    freq = rbind(freq,lineage_absent_df)
    rownames(freq) = freq$Var1
    freq = freq[lineage_use,]
    freq_list[[i]] = as.data.frame(t(data.frame(freq$Freq)))
  }

  freq_df = do.call(dplyr::bind_rows,freq_list)
  rownames(freq_df) = names(freq_list)
  colnames(freq_df) = lineage_use

  if (!is.null(order_use)) {
    freq_df = freq_df[,order_use]
  }
  pheatmap::pheatmap(log10(freq_df+0.0001),show_rownames =show_rownames,color = col
                     ,cluster_rows = cluster_row,cluster_cols = cluster_cols,...)
}

#' Column plot for Composition of cell types of each barcode
#' @description make column plot to show Composition of cell types of each barcode
#' @param data data.frame, indicating lineage tracing data, first column should
#' be lineage tracing barcodes, second column should be related cell type
#' @param idx column name of cell type
#' @param barcode_use Vector of barcodes to plot
#'
#' @return
#' ggplot column plot
#' @export
#'
#' @examples fate_ct_col(fate_test)
fate_ct_col <- function(data,idx='cell_type',
                        barcode_use = NULL){

  lineage_use = unique(data[,idx])
  freq_list = list()

  for (i in unique(data[,1])) {
    data_use = data[data[,1]==i,]
    freq = as.data.frame(table(data_use[,idx]))
    freq$Var1 = as.character(freq$Var1)
    freq = freq[freq$Freq!=0,]
    lineage_absent = lineage_use[!lineage_use %in% as.character(freq$Var1)]
    lineage_absent_df =data.frame(lineage_absent,rep(0,length(lineage_absent)))
    colnames(lineage_absent_df) = colnames(freq)
    freq = rbind(freq,lineage_absent_df)
    rownames(freq) = freq$Var1
    freq = freq[lineage_use,]
    freq_list[[i]] = as.data.frame(t(data.frame(freq$Freq)))
  }

  freq_df = do.call(dplyr::bind_rows,freq_list)
  rownames(freq_df) = names(freq_list)
  colnames(freq_df) = lineage_use

  col<- rev(colorRampPalette(c("#cc0000", "#FFff00",'#66ccff','#000066'))(50))
  freq_df$freq_sum = rowSums(freq_df)
  freq_df = freq_df[order(freq_df$freq_sum,decreasing = T),]
  if (!is.null(barcode_use)) {
    freq_df_use = freq_df[barcode_use,]
  }else{
    freq_df_use = freq_df
  }
  bar_plot_input = reshape2::melt(as.matrix(freq_df_use)[,1:4])
  colnames(bar_plot_input) = c('barcode','sample','count')
  ggplot(bar_plot_input,aes(x=barcode,y=count,fill=sample))+geom_col()+
    theme_classic()+theme(text = element_text(size=15),axis.text.x =
                            element_text(angle = 45,size=12,hjust=1))
}

#' Title
#'
#' @param data data.frame, indicating lineage tracing data, first column should
#' be lineage tracing barcodes, second column should be related cell type
#' @param idx column name of cell type
#' @param method character, indicating which correlation coefficient
#' (or covariance) is to be computed. One of "pearson", "kendall",
#' or "spearman (default)": can be abbreviated.
#' @param plot whether plot the heatmap
#' @param ... other pheatmap parameters
#'
#' @return
#' barcodes correlation between cell type
#' @export
#'
#' @examples cell_type_hier_heatmap(fate_test)
cell_type_hier_heatmap <- function(data,idx='cell_type',method='spearman',
                                   plot = TRUE,...){
  lineage_use = unique(data[,idx])
  library(ggtree)
  sample_similarity_list = list()
  for (i in lineage_use) {
    for (j in lineage_use) {
      sample1 = data[data[,idx]==i,]
      sample2 = data[data[,idx]==j,]

      fre_all = as.data.frame(table(sample1[,1]))
      fre_all_all = as.data.frame(table(fre_all[,2]))
      fre_all1 = as.data.frame(table(sample2[,1]))
      fre_all_all1 = as.data.frame(table(fre_all1[,2]))

      overlapped_idx = intersect(sample1[,1],sample2[,1])
      all_idx = union(sample1[,1],sample2[,1])
      df_plot = data.frame(all_idx,rep(0,length(all_idx)),rep(0,
                                                              length(all_idx)))
      df_plot[,2] = fre_all[match(df_plot$all_idx,fre_all[,1]),2]
      df_plot[,3] = fre_all1[match(df_plot$all_idx,fre_all1[,1]),2]
      colnames(df_plot)[2:3] = c(i,j)
      df_plot[is.na(df_plot)] = 0

      sample_similarity = cor(df_plot[,2],df_plot[,3])
      sample_similarity_list[[paste0(i,'-',j)]] = data.frame(i
                                                             ,j,sample_similarity)
    }
  }
  sample_similarity_df = do.call(dplyr::bind_rows,sample_similarity_list)
  sample_similarity_df = reshape2::dcast(sample_similarity_df,i~j)
  rownames(sample_similarity_df) = sample_similarity_df[,1]
  sample_similarity_df = sample_similarity_df[,-1]
  sample_similarity_df[is.na(sample_similarity_df)]=0
  if (plot) {
    pheatmap::pheatmap(sample_similarity_df,...)
  }

  #ggtree(pheatmap::pheatmap(sample_similarity_df)$tree_col,
   #      layout = 'rectangular')+geom_tiplab(size=5,hjust = 1,vjust=1.2)
  return(sample_similarity_df)

}





