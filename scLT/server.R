obj_metadata_list <- readRDS('obj_metadata_list.rds')
coretable <- readxl::read_xlsx('scLTdb summary.xlsx')

fate_mapping2 <- function(data,idx='celltype',input_type = 'table',normalize_method='ratio',
                         order_use=NULL,show_row=T,
                         cluster_rows=F,cluster_cols=T,...){
  data_levels=NULL
  if (input_type == 'table') {
      if (class(data[,idx])== 'factor') {
      data_levels<-levels(data[,idx])
      }
      data[,idx]=as.character(data[,idx])
      data = data[!is.na(data[,'barcodes']),]
      lineage_use = unique(data[,idx])
      freq_list <- purrr::map(unique(data$barcodes),function(i){
        data_use = data[data$barcodes==i,]
        freq = as.data.frame(table(data_use$celltype))
        freq$Var1 = as.character(freq$Var1)
        freq = freq[freq$Freq!=0,]
        lineage_absent = lineage_use[!lineage_use %in% as.character(freq$Var1)]
        lineage_absent_df =data.frame(lineage_absent,rep(0,length(lineage_absent)))
        colnames(lineage_absent_df) = colnames(freq)
        freq = rbind(freq,lineage_absent_df)
        rownames(freq) = freq$Var1
        freq = freq[lineage_use,]
        return(as.data.frame(t(data.frame(freq$Freq))))
      })

      freq_df = do.call(dplyr::bind_rows,freq_list)
      rownames(freq_df) = unique(data$barcodes)
      colnames(freq_df) = lineage_use
  }else if(input_type == 'matrix'){
    freq_df = data
  }


  col<- rev(colorRampPalette(c("#cc0000", "#FFff00",'#66ccff','#000066'))(50))
  column_order <- if (!is.null(order_use)) order_use else if (!is.null(data_levels)) data_levels else NULL
  if (!is.null(column_order)) {
    for (column in column_order) {
        if (!(column %in% names(freq_df))) {
          freq_df[[column]] <- 0
        }
    }
    freq_df = freq_df[, column_order]
  }
  if (normalize_method=='ratio') {
    freq_df_ratio = apply(freq_df, 1, function(x){
      row_sum = sum(x)
      return(x/row_sum)
    })
    pheatmap::pheatmap(t(freq_df_ratio),show_rownames =show_row,color = col
                       ,cluster_rows = cluster_rows,cluster_cols = cluster_cols,
                       border_color = NA,...)
  }else if(normalize_method=='log'){
    freq_df_ratio <- log10(freq_df + 1)
    pheatmap::pheatmap(freq_df_ratio,show_rownames =show_row,color = col
                       ,cluster_rows = cluster_rows,cluster_cols = cluster_cols,
                       border_color = NA,...)
  }


  return(freq_df)
}
cell_type_fate_similartiy2 <- function(data,idx='celltype',input_type = 'table'
                                      ,method='spearman',
                                   plot = TRUE,out_similar_mt = FALSE,...){
  col<- rev(colorRampPalette(c("#cc0000", "#FFff00",'#66ccff','#000066'))(50))
  lineage_use = unique(data[,idx])
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

      sample_similarity = cor(df_plot[,2],df_plot[,3],method = method)
      if (i==j && is.na(sample_similarity)) {
         sample_similarity=1
      }
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
    pheatmap::pheatmap(sample_similarity_df,color = col,...)
  }
  if (out_similar_mt) {
    return(sample_similarity_df)
  }
}


dataset_cell_number_compare <- function(dataset1_name,dataset2_name,metdata_list){
    
    dataset1 = metdata_list[[dataset1_name]]
    dataset2 = metdata_list[[dataset2_name]]
    dataset1_ct_freq = as.data.frame(table(dataset1$celltype))
    dataset1_ct_freq$dataset = dataset1_name
    dataset2_ct_freq = as.data.frame(table(dataset2$celltype))
    dataset2_ct_freq$dataset = dataset2_name
    df_final = rbind(dataset1_ct_freq,dataset2_ct_freq)
    
    p1 <- ggplot(df_final,aes(x=Var1,y=Freq,fill=dataset))+
        geom_col(stat='identity', position='dodge',width = 0.8)+theme_classic()+
        xlab('cell state')+ylab('cell number') +scale_y_continuous(expand = c(0,0))+
        scale_fill_manual(values = c("#5E4FA2","#F88D51"))
    
    return(p1)
}




server <- function(input, output,session = session) {
    #HOME-------
    output$HOME_output_text <- renderUI({
        div(p("scLtDB is a database of single-cell multiomics lineage tracing.It encompasses a comprehensive collection of 30 manually curated datasets, each comprising well-annotated cell identities and barcodes.These datasets span across three distinct species, encompassing seven diverse tissues, and feature the utilization of 16 different lineage tracing technologies.scLTdb provides:"),
            p("1.Browse single cell multiomics lineage tracing data, including single cell embedding, clone expression profile, lineage similarity among cell types, and lineage tree"),
            p("2.Data search by species, tissue, and technologies"),
            p("3.Interactive data exploration, e.g., clone fate bias for different cell types, and visualize clone on single cell embedding"),
            p("4.Online tool to compare different datasets, and perform clone analysis"),
            p("5.Download well processed seurat object"),style ='display: inline;'
        )
    })




    #search-----
    output$coretable = renderDataTable(
        coretable,
        selection = 'single',
        rownames =FALSE,
        server = TRUE,
        options = list(
            pageLength = 10,
            searchHighlight = TRUE,
            lengthChange = FALSE)
    )
    #unique(coretable$Dataset)
    #core_table_rows_selected
    output$seleted_row = renderPrint({
        s = input$coretable_rows_selected
        if (length(s)) {
            cat('These rows were selected:\n\n')
            cat(coretable[s,]$Dataset, sep = ', ')
        }
    })
    observeEvent(input$go_to_panel, {
        
        updateTabsetPanel(session,"inTabset", selected =  "Results")
    })

    #Results-----------
    Select_dataseted <- reactive({
        s = input$coretable_rows_selected
        if (length(s)) {
            Select_dataset =  coretable[s,]$Dataset
        }else{
            Select_dataset = 'Biddy_2018_Nature'
        }
        obj_metadata_list[[Select_dataset]]
    })

    output$show_umap1 <- renderImage({
        s = input$coretable_rows_selected
        if (length(s)) {
            Select_dataset =  coretable[s,]$Dataset
        }else{
            Select_dataset = 'Biddy_2018_Nature'
        }

        list(
            src = file.path("dataset_umap/", paste0(Select_dataset, ".png")),
            contentType = "image/png",
            width = 800
        )
    }, deleteFile = FALSE)
    output$show_umap2 <- renderPlot({
        plot_clone_embedding2(input$input_barcode,metadata = Select_dataseted())
    },
    height = 700,
    width = 800
    )

    output$show_umap <- renderUI({
        if (input$input_barcode == 'Please input barcode...') {
            imageOutput("show_umap1")
        } else {
            plotOutput("show_umap2")
        }
    })


    cell_fate <- reactive({
        req(input$Select_dataset)
        cell_fate <-  Select_dataseted()[,c('barcodes','celltype')]
        cell_fate <- cell_fate[!is.na(cell_fate$barcodes),]
        return(cell_fate)
    })
    fate_bias <- reactive({
        s = input$coretable_rows_selected
        if (length(s)) {
            Select_dataset =  coretable[s,]$Dataset
        }else{
            Select_dataset = 'Biddy_2018_Nature'
        }
        fate_bias <-  readRDS(paste0('clone_fate_bias/',unique(Select_dataset),'.rds'))
        fate_bias <- as_tibble(purrr::reduce(fate_bias,bind_rows))
        fate_bias$fate_ratio <- round(as.numeric(fate_bias$fate_ratio),4)
        fate_bias$pvalue <- round(as.numeric(fate_bias$pvalue),4)
        fate_bias$fdr <- round(as.numeric(fate_bias$fdr),4)
        return(fate_bias)
    })

    ##plotly for interactive
    # output$barcode_count_plot <- renderPlotly({
    #   barcode_count_plot <- fate_mapping2(cell_fate())
    #   plot_ly(x=colnames(log10(barcode_count_plot + 1e-04)), color =  rev(colorRampPalette(c("#cc0000", "#FFff00", "#66ccff", "#000066"))(8)),
    #           z = as.matrix(log10(barcode_count_plot + 1e-04)), type = "heatmap")
    # })
    output$barcode_count_plot <- renderImage({
        s = input$coretable_rows_selected
        if (length(s)) {
            Select_dataset =  coretable[s,]$Dataset
        }else{
            Select_dataset = 'Biddy_2018_Nature'
        }
        list(
            src = file.path("clone_expression_fig/", paste0(Select_dataset, ".png")),
            contentType = "image/png",
            width = 800
        )
    }, deleteFile = FALSE)

    output$cell_type_similarity_plot <- renderImage({
        s = input$coretable_rows_selected
        if (length(s)) {
            Select_dataset =  coretable[s,]$Dataset
        }else{
            Select_dataset = 'Biddy_2018_Nature'
        }
        list(
            src = file.path("cell_type_similarity/", paste0(Select_dataset, ".png")),
            contentType = "image/png",
            width = 800
        )
    }, deleteFile = FALSE)

    output$lineage_tree_plot <- renderImage({
        s = input$coretable_rows_selected
        if (length(s)) {
            Select_dataset =  coretable[s,]$Dataset
        }else{
            Select_dataset = 'Biddy_2018_Nature'
        }
        list(
            src = file.path("lineage_tree_fig/", paste0(Select_dataset, ".png")),
            contentType = "image/png",
            width = 800
        )
    }, deleteFile = FALSE)

    output$look_fate_bias = renderDT(
        fate_bias()[,-5],
        extensions = c('Select', 'SearchPanes',"Buttons"),
        colnames=c("clone_name","fate_use","clone_size","fate_ratio", "fdr"),
        selection = 'none',
        server = FALSE,
        options = list(
            dom = 'Bftip',
            columnDefs = list(
                list(searchPanes = list(show = FALSE), targets = c(1,3,4,5)),
                list(targets = c(1:5), className = 'dt-center')
            ),
            buttons = list(
                'searchPanes'
            ),
            language = list(searchPanes = list(collapse = 'Select fata bias')),
            pageLength = 10,
            searchHighlight = TRUE,
            lengthChange = FALSE)
    )

    #Online tools-----
    upload_metadata <- reactive({
      req(input$upload_metadata)
      ext <- tools::file_ext(input$upload_metadata$name)
      switch(ext,
             csv = read.csv(input$upload_metadata$datapath),
             validate("Invalid file; Please upload a .csv file")
      )
    })

    output$cloneprofile_tools <- renderPlotly({
      cloneprofile <- fate_mapping2(upload_metadata())
        plot_ly(
          x = colnames(log10(cloneprofile + 1e-04)),
          color =  rev(colorRampPalette(
            c("#cc0000", "#FFff00", "#66ccff", "#000066")
          )(8)),
          z = as.matrix(log10(cloneprofile + 1e-04)),
          type = "heatmap"
        )
      })
    output$cell_type_fate_similartiy_tools <- renderPlotly({
      ctf <- cell_type_fate_similartiy(upload_metadata(),out_similar_mt = T,plot = FALSE)
      plot_ly(
        x = colnames(ctf),
        y = rownames(ctf),
        color =  rev(colorRampPalette(
          c("#cc0000", "#FFff00", "#66ccff", "#000066")
        )(8)),
        z = as.matrix(ctf),
        type = "heatmap"
      )
    })

    
    
    
    selected_value <- reactive({
        input$Compare_dataset
    })
    
    
    output$selected_option <- renderPlot({
        dataset_cell_number_compare(dataset1_name = selected_value()[1],
                                    dataset2_name = selected_value()[2],
                                    metdata_list = obj_metadata_list)
      
    })
}
