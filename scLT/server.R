server <- function(input, output,session = session) {

  # obj_metadata_list()读取metadata----
  obj_metadata_list <- reactive({
    selected_id <- getSelectedDataset()
      file_path <- paste0("obj_metadata_list/", selected_id, ".rds")
      readRDS(file_path)
  })
  #----
  
  # clone_fate_bias_list()创建clone_fate_bias----
  clone_fate_bias_list <- reactive({
    selected_id <- getSelectedDataset()
      file_path <- paste0("clone_fate_bias_list/", selected_id, ".rds")
      readRDS(file_path)
  })
  #----
  
  # deg_rds_list()deg_rds----
  deg_rds_list <- reactive({
    selected_id <- getSelectedDataset()
      file_path <- paste0("deg_rds_list/", selected_id, "_deg.rds")
      if (file.exists(file_path)) {
        readRDS(file_path)
      } else {
        NULL
      }
  })
  #----
  
  # deg_size_rds_list()deg_size_rds----
  deg_size_rds_list <- reactive({
    selected_id <- getSelectedDataset()
      file_path <- paste0("deg_size_rds_list/", selected_id, "_deg.rds")
      if (file.exists(file_path)) {
        readRDS(file_path)
      } else {
        NULL
      }
  })
  #----
  
  # 根据用户的行数锚定到数据集上的getSelectedDataset常用反应式----
  getSelectedDataset <- reactive({
    selected_rows <- input$coretable123_rows_selected
    data <- reactiveData()
    if (!is.null(selected_rows) && length(selected_rows) > 0) {
      selected_id_c  <- data[selected_rows, ]$Dataset
      selected_id_c <- gsub("<.*?>", "", selected_id_c)
      return(selected_id_c)
    } else {
      return('Weinreb_2020_Science')
    }
  })
  
  #----
  
   # home界面还没有函数
  
    # search界面相关函数设置 ----   
  
    filteredData <- reactive({
      req(coretable)  
      data <- coretable
      if (length(input$species) > 0){ data <- data[data$Species %in% input$species, ]}
      if (length(input$tissue) > 0){ data <- data[sapply(data$`Tissue source`, function(x) any(sapply(input$tissue, function(y) grepl(y, x)))), ]}
      if (length(input$technology) > 0){ data <- data[data$Technology %in% input$technology, ]}
      if (length(input$modality) > 0) {data <- data[data$Modality %in% input$modality, ]}
      if (length(input$barcodetype) > 0) {data <- data[data$`Barcode type` %in% input$barcodetype, ]}
      
      if (input$searchBtn > 0) {
        isolate({ 
          if (input$search != "") {
            search_term <- tolower(input$search)
            data <- data[apply(data, 1, function(row) any(grepl(search_term, tolower(row)))), ] } }) }
      return(data) })
    
    reactiveSearch <- eventReactive(input$searchBtn,{ input$search }, ignoreNULL = FALSE)
    
    reactiveData <- reactive({
      data <- filteredData()  
      search <- reactiveSearch() 
      if (nchar(search) > 0 ) {
        #使用 gsub 高亮匹配的文本
        data <- data %>% 
          mutate(across(everything(), ~ gsub(paste0('(', search, ')'), 
                        '<span style="background-color: yellow;">\\1</span>', 
                                             ., ignore.case = TRUE)))}
      return(data) })
  
    # output$coretable123 <- DT::renderDT({
    #   data <- reactiveData() 
    #   datatable(
    #     data,
    #     extensions = 'Buttons',
    #     class = 'cell-border stripe hover',
    #     selection = 'single',
    #     rownames = FALSE,
    #     escape = FALSE,
    #     options = list(
    #       dom = 'lBrtip',
    #       scrollX = TRUE,
    #       pageLength = 10,
    #       searchHighlight = TRUE,
    #       lengthChange = TRUE,
    #       lengthMenu = list(c(10, 25, 50, 100, 150), c('10', '25', '50', '100', '150')),
    #       buttons = list('copy', 'csv', 'excel', 'pdf'),
    #       responsive = TRUE,
    #       columnDefs = list(
    #         list(
    #           targets = which(names(data) == 'Species') - 1,
    #           render = JS("function(data, type, row) {
    #           return type === 'display' && data != null ? '<i>' + data + '</i>' : data;
    #         }") ))))})
    
    
    first_load <- reactiveVal(TRUE)
    
    observe({
      if (input$inTabset == "Search" && first_load() ) {
        showModal(modalDialog(
          title = NULL,
          "Loading......Please wait a second",
          footer = NULL
        ))

        # 使用延迟来模拟加载时间
        delay(1, {  # 1毫秒的延迟，确保modal显示

          output$coretable123 <- DT::renderDT({
            data <- reactiveData()

            data <- data %>%
              mutate(Species = paste0("<i>", Species, "</i>"))

            datatable(
              data,
              extensions = 'Buttons',
              class = 'cell-border stripe hover',
              selection = 'single',
              rownames = FALSE,
              escape = FALSE,
              options = list(
                dom = 'lBrtip',
                scrollX = TRUE,
                pageLength = 10,
                searchHighlight = TRUE,
                lengthChange = TRUE,
                lengthMenu = list(c(10, 25, 50, 100, 150), c('10', '25', '50', '100', '150')),
                #buttons = list('copy', 'csv', 'excel', 'pdf'),
                buttons = list('excel'),
                responsive = TRUE
            #     columnDefs = list(
            #       list(
            #         targets = which(names(data) == 'Species') - 1,
            #         render = JS("function(data, type, row) {
            #   return type === 'display' && data != null ? '<i>' + data + '</i>' : data;
            # }") ))
                ))})

          first_load(FALSE)
          shinyjs::delay(3000, {
            removeModal()
          })
        })
      }
    })
   

           
    
    
    
    
    
    
    
    
    
    #----

    # 设置全界面url，同时设置search界面通过url跳转results界面----
    
    # observeEvent(input$go_to_panel, {
    #   selected_id <- getSelectedDataset()
    #   updateQueryString(paste0("?tab=", 'Results','&dataset=',selected_id), mode = "push")
    # } ,ignoreInit = TRUE   )
    # 
    # observeEvent(input$coretable123_rows_selected, {
    #   if (input$inTabset == 'Results') {
    #     selected_id <- getSelectedDataset()
    #     updateQueryString(paste0("?tab=", input$inTabset, '&dataset=', selected_id), mode = "push")
    #   }else {
    #     updateQueryString(paste0("?tab=", input$inTabset), mode = "push")
    #   }
    # } ,ignoreInit = TRUE )

    # observe({
    #   query <- parseQueryString(session$clientData$url_search)
    #   if (!is.null(query$tab)) {
    #     showModal(
    #       modalDialog(
    #         title = NULL,
    #         "Loading...",
    #         footer = NULL,
    #         easyClose = FALSE
    #       )
    #     )
    #     Sys.sleep(1)
    #     updateNavbarPage(session, "inTabset", selected = query$tab)
    #     removeModal()
    #   }
    # })
    observe({
      updateQueryString(input$inTabset, mode = "replace")
    })
    # observeEvent(input$go_to_panel, { updateNavbarPage(session, "inTabset", selected = 'Results') } ,ignoreInit = TRUE )
    # 
    # observeEvent(input$tutorial_link, { updateNavbarPage(session, "inTabset", selected = "Tutorial")} ,ignoreInit = TRUE)
    # 
    #----    
    
    # 设置results界面标题----
    output$dynamicTitle <- renderUI({
      selected_id <- getSelectedDataset()
      HTML(paste0("<span>", selected_id, "  -- Results</span>")) })
    #----
    
    sample_data <- reactive({
      selected_id <- getSelectedDataset()
      
      metadata <- obj_metadata_list()
      
      celltype_num <- length(as.character(unique(metadata$celltype)))
      
      filtered_data <- data_summary_table[data_summary_table$Dataset == selected_id, ]
      data.frame(
      col1 = c("Dataset name", "Unique barcode number", "Cell number","Cell type (state) number", "Number of cell with paired barcode", "Detection rate"), 
      col2 = c(filtered_data$Dataset, filtered_data$`Unique barcode number`, filtered_data$`Cell number`, celltype_num,
               filtered_data$`Number of cell with barcode`, filtered_data$`Detection rate`),check.names = F
      )  })
    
    # 输出表格
    output$sampleInfo <- renderTable({
      sample_data()
    }, colnames = FALSE,striped = FALSE)
    
    
    
# 设置Single cell module
    
 # 设置Cell annotation(Pseudotime) embedding ----
    
    observe({
      if (input$inTabset == "Results") {
        showModal(modalDialog(
          title = NULL,
          "Loading......Please wait a second",
          footer = NULL
        ))
        
        # 使用延迟来模拟加载时间
        delay(1000, {  # 1毫秒的延迟，确保modal显示
          
          output$pseudotime_or_celltype <- renderUI({
            if (input$change_svg == 'Pseudotime') {
              selected_id <- getSelectedDataset()
              file_path <- paste0("pseudotime_new/", selected_id, ".csv")
              if( file.exists(file_path) ){
                girafeOutput('girafe_output_umap_pseudotime', width = "100%",height= "100%")
              }else{ tagList( img(src = "Caution_pseudotime.png", alt = "Error Image",width = "100%", height = "100%") ) }
            }else{
              girafeOutput('girafe_output_umap', width = "100%",height= "100%")
            } })
          
          removeModal()
        })
      }
    })
    
    
    # output$pseudotime_or_celltype <- renderUI({
    #   if (input$change_svg == 'Pseudotime') {
    #     selected_id <- getSelectedDataset()
    #     file_path <- paste0("pseudotime_new/", selected_id, ".csv")
    #     if( file.exists(file_path) ){
    #           girafeOutput('girafe_output_umap_pseudotime', width = "100%",height= "100%")
    #     }else{ tagList( img(src = "Caution_pseudotime.png", alt = "Error Image",width = "100%", height = "100%") ) }
    #   }else{
    #     girafeOutput('girafe_output_umap', width = "100%",height= "100%")
    #     } })
  
    
    output$girafe_output_umap_pseudotime <- renderGirafe({
      selected_id <- getSelectedDataset()
      metadata <- obj_metadata_list()
      generatePseudotimeSvg(selected_id,metadata) })
    
    output$girafe_output_umap <- renderGirafe({
      req(input$selectPage)
      req(input$change_svg)
      metadata = obj_metadata_list()
      dataset_umap( metadata, as.numeric(input$selectPage), input$change_svg)
      
      
    })
 
 #---- 
  
 # 设置gene expression module----
    
    #读取matrix.rds
    matrix_q <- reactive({
      selected_id <- getSelectedDataset()
      file_path <- paste0("data_matrix_data/", selected_id, "_expression_matrix.rds")
      if (file.exists(file_path)) { readRDS(file_path) } else { data.frame() } })
    
    
    # output$download_list <- downloadHandler(
    #   filename = function() {
    #     paste("gene_list", Sys.Date(), ".csv", sep = "")
    #   },
    #   content = function(file) {
    #     # 调用 matrix_q 函数并提取行名
    #     matrix_qq <- matrix_q()
    #     gene_names <- rownames(matrix_qq)
    #     
    #     # 将基因名称写入 CSV 文件
    #     write.csv(data.frame(Gene = gene_names), file, row.names = FALSE)
    #   }
    # )
    
    
    observeEvent(getSelectedDataset(),{
      matrix_qq <- matrix_q()
      if ( nrow(matrix_qq)>0 ) {
        gene_names <- rownames(matrix_qq)
        if ("Gata2" %in% gene_names) {
          # 移除 'Gata2' 并将其插入到向量的第一位
          gene_names <- c("Gata2", gene_names[gene_names != "Gata2"])
        }
        
        updateSelectizeInput(session, "gene_expression", choices = gene_names, selected = gene_names[1], server = TRUE)
        
      selected_id <- getSelectedDataset()
      umap_data <- obj_metadata_list()
      common_cells <- intersect(rownames(umap_data), colnames(matrix_qq))
      umap_data <- umap_data[common_cells, ]
      umap_data$gene <- as.vector(matrix_qq[gene_names[1], common_cells, drop = FALSE])
      umap_data <- umap_data[order(umap_data$gene, decreasing = FALSE), ]
   #   umap_data$tooltip = paste0("Cell name: ", rownames(umap_data), "\nCell type: ", umap_data$celltype , "\nCell barcode: ", umap_data$barcodes , "\nUMAP coordinates: ", umap_data$UMAP_1, ", ", umap_data$UMAP_2,"\nGene expression: ",umap_data$gene)

      if (nrow(umap_data)>10000) { cell_size=0.15 }else{ cell_size=0.2 }

      colors <- c(
        rgb(225/255, 225/255, 225/255), # 浅灰色
        rgb(185/255, 138/255, 133/255), # 中间色
        rgb(185/255, 138/255, 133/255)  # 深色
      )
      plot_obj <- ggplot() +
        geom_point(data = umap_data,  aes(x = UMAP_1, y = UMAP_2, color = gene ), size = cell_size) +
        theme_void() +
        scale_color_gradientn(colors = colors) +
        labs(x = "UMAP_1", y = "UMAP_2", color = paste("Expression of", gene_names[1]))

      pp <- girafe(ggobj = plot_obj,options = list( opts_selection(type = "none"),opts_tooltip(offx = 0, offy = 0) ),width_svg = 8,height_svg = 6)

      output$plot_gene <- renderGirafe({ pp })

      output$dynamic_plot_gene <- renderUI({ girafeOutput("plot_gene",width = "100%",height= "100%") }) }else{
        output$dynamic_plot_gene <- renderUI({
          img(src = "Caution_gene_expression.png", alt = "Error Image", width = "100%", height = "100%")  }) } })
   
    observeEvent(input$submit, {
      matrix_qq <- matrix_q()
      if (nrow( matrix_qq ) > 0 ) {
        if (input$gene_expression %in% rownames( matrix_qq ) ) {
          selected_id <- getSelectedDataset()
          umap_data <- obj_metadata_list()
          common_cells <- intersect(rownames(umap_data), colnames(matrix_qq))
          umap_data <- umap_data[common_cells, ]
          umap_data$gene <- as.vector(matrix_qq[input$gene_expression, common_cells, drop = FALSE])
          umap_data <- umap_data[order(umap_data$gene, decreasing = FALSE), ]
     #     umap_data$tooltip = paste0("Cell name: ", rownames(umap_data), "\nCell type: ", umap_data$celltype , "\nCell barcode: ", umap_data$barcodes , "\nUMAP coordinates: ", umap_data$UMAP_1, ", ", umap_data$UMAP_2,"\nGene expression: ",umap_data$gene)
          
          if (nrow(umap_data)>10000) { cell_size=0.15 }else{ cell_size=0.2 }
          
          colors <- c(
            rgb(225/255, 225/255, 225/255), # 浅灰色
            rgb(185/255, 138/255, 133/255), # 中间色
            rgb(185/255, 138/255, 133/255)  # 深色
          )
          plot_obj <- ggplot() +
            geom_point(data = umap_data,  aes(x = UMAP_1, y = UMAP_2, color = gene ), size = cell_size) +
            theme_void() +
            scale_color_gradientn(colors = colors) +
            labs(x = "UMAP_1", y = "UMAP_2", color = paste("Expression of", input$gene_expression))
          
          #ggsave(filename = '/data/yexing/scLT/www/gg.pdf', plot = plot_obj, device = "pdf", width = 14, height = 12, units = "in", dpi = 100, limitsize = FALSE)
          
          pp <- girafe(ggobj = plot_obj,options = list( opts_selection(type = "none"),opts_tooltip(offx = 0, offy = 0) ),width_svg = 8,height_svg = 6)

          output$plot_gene <- renderGirafe({ pp })
          output$dynamic_plot_gene <- renderUI({ girafeOutput("plot_gene",width = "100%",height= "100%") })
          
        }else{ output$dynamic_plot_gene <- renderUI({ img(src = "Note_gene_expression.png", alt = "Error Image", width = "100%", height = "100%")}) }
      }else{
        output$dynamic_plot_gene <- renderUI({
          img(src = "Caution_gene_expression.png", alt = "Error Image", width = "100%", height = "100%")  }) } },ignoreInit = TRUE)
 #----   
    
 # 更新多个选项卡----
    cleaned_data <- reactive({
      selected_id <- getSelectedDataset()
      pbmc <- obj_metadata_list()
      pbmc$celltype <- as.character(pbmc$celltype)
      pbmc$barcodes <- as.character(pbmc$barcodes)
      pbmc <- pbmc[!is.na(pbmc$celltype) & !grepl("^\\s*$", pbmc$celltype),]
      list(pbmc = pbmc, selected_id = selected_id)
    })
    
    observeEvent(cleaned_data(), {
      data <- cleaned_data()
      pbmc_1 <- data$pbmc
      pbmc_2 <- pbmc_1[!pbmc_1$barcodes %in% c('Negative', 'None') & !is.na(pbmc_1$barcodes) & !grepl("^\\s*$", pbmc_1$barcodes),]
      data$pbmc <- pbmc_2
      clone_fate_bias = clone_fate_bias_list()
      
      num_pages <- ceiling(length(unique(pbmc_1$celltype)) / 12)
      choices_page <- setNames(1:num_pages, paste0("page:", 1:num_pages))
      
      columns_to_check <- c("batch", "time", "condition", "mouse/patient")
      existing_columns <- colnames(pbmc_1)
      found_columns <- columns_to_check[columns_to_check %in% existing_columns]
      updatePickerInput(session, "change_svg", choices = c('Cell type (state)','Pseudotime',found_columns), selected = 'Cell type (state)')
      
      updateSelectInput(session, "selectPage", choices = choices_page, selected = 1)
      updatePickerInput(session, "select_A_barcode", choices = clone_fate_bias )
      updatePickerInput(session, "select_A_clone_size", choices = clone_fate_bias, selected = clone_fate_bias[[1]] )
      updatePickerInput(session, "select_A_fate_bias", choices = names(clone_fate_bias ))
      updatePickerInput(session, "select_A_fate_bias_size", choices = names(clone_fate_bias ))
      updatePickerInput(session, "select_A_celltype_fates", choices = unique(data$pbmc$celltype), selected = unique(data$pbmc$celltype)[1]) 
      updatePickerInput(session, "select_A_celltype_gene_violin", choices = unique(data$pbmc$celltype), selected = unique(data$pbmc$celltype)[1])
      })
 #----
  
   # 设置celltype(state) number barplot----  
    output$barplot_celltype_number <- renderGirafe({
      metadata =obj_metadata_list()
      metadata <- metadata %>%
        dplyr::filter(!is.na(celltype) & !grepl("^\\s*$", celltype))
      if(input$bar_or_pie == 'Bar plot'){
      plot_celltype_barplot( metadata )}else{
        plot_celltype_piechart(metadata )
      }
        })
   #----   
  
 # 设置Clone embedding----
    # 绘制clone分布
    observeEvent(c(input$select_A_barcode,getSelectedDataset()),{
      output$girafe_output_barcode <- renderGirafe({
      barcode_data <- input$select_A_barcode
      metadata =obj_metadata_list()
      plot_clone_embedding2(barcode_data,metadata ) }) })
    
    # 绘制fate bias分布
    observeEvent(input$select_A_fate_bias,{
      fate_data <- input$select_A_fate_bias
      clone_fate_bias = clone_fate_bias_list()
      metadata =obj_metadata_list()
      output$girafe_output_barcode <- renderGirafe({
        plot_clone_embedding3( metadata, fate_data, clone_fate_bias )}) })
 #----  

    
# 设置Heatmap module
   
  # 设置Fate outcome----
    # 设置用户可以自行排序
    output$celltype_clone_applysort <- renderUI({
      metadata = obj_metadata_list()
      heatmap_clone = clone_heatmap(metadata)
      div( style = "overflow-x: auto; white-space: nowrap; width: 500px; height: 30.5vw;", 
      rank_list(
        text = "",
        labels = colnames( heatmap_clone ),
        input_id = "celltype_clone_sorted_cells")) })
    
    # 设置用户可以自行筛选celltype
    output$lineage_celltype_sort <- renderUI({
      metadata = obj_metadata_list()
      lineage_relationship_data = clone_heatmap(metadata)
      
      if ('HSPC' %in% colnames(lineage_relationship_data)) {
        selected_1 <- setdiff(colnames(lineage_relationship_data), 'HSPC')
      } else {
        selected_1 <- head(colnames(lineage_relationship_data))
      }
      
      pickerInput("lg_ct_sort", 
                  label = tags$span(class = "picker-input-label", "Select cell type (state):"),
                  choices = colnames(lineage_relationship_data),
                  selected = selected_1,
                  multiple = TRUE,
                  options = pickerOptions(
                    `size` = 10,
                    actionsBox = TRUE,
                    liveSearch = TRUE,
                    selectedTextFormat = "count > 3" ), width = '100%') })
    
    # 响应用户“应用排序”
    observeEvent( c(input$celltype_clone_sorted_cells,input$change_normalize_method), {
      metadata = obj_metadata_list()
      
      # metadata <- metadata %>%
      #   filter(!celltype %in% c('Eos', 'pDC', 'Ccr7_DC', 'Mast'))
      
        sorted <-  input$celltype_clone_sorted_cells
        change_normalize_method_1 <- input$change_normalize_method
        
        celltype_clone_data <-  clone_heatmap(metadata, normalize_method= change_normalize_method_1)
        
        # celltype_clone_data <- as.matrix(celltype_clone_data)
        # pdf("/data/yexing/scLT/www/gg.pdf")
        # heatmap(celltype_clone_data, 
        #         Rowv = NA,         # 不对行进行聚类
        #         Colv = NA,         # 不对列进行聚类
        #         col = col,         # 颜色方案
        #         scale = "none",    # 不对数据进行缩放
        #         #margins = margins, # 图形边距
        #         labRow = TRUE,     # 显示行标签
        #         labCol = TRUE)
        # dev.off()
        
        new_ordered_data <- celltype_clone_data[,sorted]
        output$celltype_clone_heatmap <- renderPlotly({            #,plot_method = "plotly"
        heatmaply((new_ordered_data), color = col,show_dendrogram = c(F,T),showticklabels =c(T,F),Rowv= FALSE ,Colv =FALSE )%>%
            layout(xaxis = list(tickfont = list(size = 16)))
          })  
        },ignoreInit = TRUE)
    
    output$lineage_relationship_heatmap <- renderPlotly({
      metadata = obj_metadata_list()
      ct_sorted = input$lg_ct_sort
      if (!is.null(ct_sorted) && length(ct_sorted) > 1) {
      lineage_relationship_data = cell_type_fate_similartiy(metadata)
      new_lineage_relationship_data <- lineage_relationship_data[ct_sorted, ct_sorted]
      heatmaply((new_lineage_relationship_data), color = col,show_dendrogram = c(F,T), dendrogram_labels=F, showticklabels =c(T,F),Rowv= T ,Colv =T )%>%
        layout(xaxis = list(tickfont = list(size = 16)))} })
    
  #----  
      
# 设置Barcode statistics
    
  # 设置unique barcode number in each cell type----  
    output$barplot_barcode_1 <- renderGirafe({
      metadata =obj_metadata_list()
      metadata = clean_pbmc_data(metadata)
      plot_barplot_barcode_1(metadata)
        })
    
  # 设置number of cell with barcode in each cell type----  
    output$barplot_barcode_2 <- renderGirafe({
      metadata =obj_metadata_list()
      metadata = clean_pbmc_data(metadata)
      plot_barplot_barcode_2(metadata)
    })
    
  #设置fate bias summary----  
    output$girafe_output_celltype_fates <- renderGirafe({ 
      selected_id <- getSelectedDataset()
      metadata = obj_metadata_list()
      clone_fate_bias = clone_fate_bias_list()
      qq = input$select_A_celltype_fates
      if( is.null(qq) ){ return() }
      fate_bias_summary(selected_id, qq, clone_fate_bias, metadata, bar_pie_choice_summary = input$bar_or_pie_1 ) }) 
  #----
    
  #设置clone size statistics----
    observeEvent(c(input$select_A_clone_size, getSelectedDataset()),{
    output$girafe_output_clone_size <- renderGirafe({ 
      qq_1 = input$select_A_clone_size
      if( is.null(qq_1) ){ return() }
      pbmc = obj_metadata_list()
      pbmc = clean_pbmc_data(pbmc)
      clone_fate_bias = clone_fate_bias_list()
      create_interactive_bar_plot(pbmc , qq_1, clone_fate_bias ) }) })
   
    observeEvent(input$select_A_fate_bias_size,{
      qq_1 = input$select_A_fate_bias_size
      if( is.null(qq_1) ){ return() }
      selected_id <- getSelectedDataset()
      pbmc = obj_metadata_list()
      pbmc = clean_pbmc_data(pbmc)
      clone_fate_bias = clone_fate_bias_list()
      output$girafe_output_clone_size <- renderGirafe({ 
        create_interactive_bar_plot_2(pbmc, selected_id, qq_1, clone_fate_bias ) })  },ignoreInit = TRUE)
  #----  
    
    
# 设置Fate bias deg和Clone size deg
    
    # 设置反应判断式
    errorOccurred <- reactiveVal(FALSE)
    errorOccurred_size <- reactiveVal(FALSE)

    # 设置pickerInput("ct", "Select Celltype")
    output$select_A_deg_celltype <- renderUI({
      selected_id <- getSelectedDataset()
      dataset_deg_name <- paste0(selected_id, '_deg')
      if (dataset_deg_name %in% names(celltype__fate_use_list)) {  
        errorOccurred(FALSE)  # 没有错误，更新状态
        pickerInput("ct", "Select cell type:",
                    choices = names(celltype__fate_use_list[[dataset_deg_name]]),
                    selected = ifelse("HSPC" %in% names(celltype__fate_use_list[[dataset_deg_name]]), 
                                      "HSPC", 
                                      names(celltype__fate_use_list[[dataset_deg_name]])[1]),
                    options = list(`actions-box` = TRUE), multiple = FALSE)
      } else {
        errorOccurred(TRUE)  # 发生错误，更新状态
        pickerInput("ct", "Select cell type:", choices = character(0)) } })
    
    # 设置pickerInput("fate_choose", "Select Fate")
    output$select_A_deg_fate <- renderUI({
      if (!errorOccurred()) {  # 仅当没有错误时渲染
        selected_id <- getSelectedDataset()
        dataset_deg_name <- paste0(selected_id, '_deg')
        if (length(input$ct) && !is.null(celltype__fate_use_list[[dataset_deg_name]][[input$ct]]$fate_use) ) {
        pickerInput("fate_choose", "Select fate bias:",
                    choices = celltype__fate_use_list[[dataset_deg_name]][[input$ct]]$fate_use,
                    selected = ifelse("Lymphoid bias VS Erythroid bias" %in% celltype__fate_use_list[[dataset_deg_name]][[input$ct]]$fate_use, 
                                      "Lymphoid bias VS Erythroid bias", 
                                      celltype__fate_use_list[[dataset_deg_name]][[input$ct]]$fate_use[1] ),
                      #celltype__fate_use_list[[dataset_deg_name]][[input$ct]]$fate_use[1],
                    options = list(`actions-box` = TRUE), multiple = FALSE)}
      } else { pickerInput("fate_choose", "Select fate bias:", choices = character(0)) } })
    
    # 重置反应判断式
    observeEvent(input$coretable123_rows_selected, {
      errorOccurred(FALSE)
      errorOccurred_size <- reactiveVal(FALSE) })
    
    # 设置deg热图展示
    output$image_display_deg <- renderImage({
      selected_id <- getSelectedDataset()
      dataset_deg_name <- paste0(selected_id, '_deg')
      if (length(input$ct) && !is.null(celltype__fate_use_list[[dataset_deg_name]][[input$ct]])) {
        q_dataframe <- celltype__fate_use_list[[dataset_deg_name]][[input$ct]]
      } else { q_dataframe <- data.frame() }
      if (nrow(q_dataframe) > 0 && length(input$fate_choose) == 1  ){
      picture_value <- dplyr::filter(q_dataframe, celltype == input$ct, fate_use == input$fate_choose) %>%
        dplyr::pull(picture)
      image_path <- paste0("clone_fate_bias_deg/", dataset_deg_name, '/', picture_value)
      list( src = image_path, contentType = "image/png", width = "100%" ) }else{
        list(src = "Warning_DEG.png", alt = "Error Image",width = "100%", height = "100%" ) } }, deleteFile = FALSE)
    
    # 设置deg热图展示和warning展示
    output$imageOutputOrPreset <- renderUI({
      if (errorOccurred()) {
        tagList( img(src = "Warning_DEG.png", alt = "Error Image",width = "100%", height = "100%" )) } else {
        list( fluidRow( column(6,uiOutput("select_A_deg_celltype")), column(6,uiOutput("select_A_deg_fate")) ), imageOutput('image_display_deg' ) ) } })
    
    # 画deg交互式火山图
    output$volcanoPlot <- renderPlotly({
      selected_id <- getSelectedDataset()
      dataset_deg_name <- paste0(selected_id, '_deg')
      deg_rds <- deg_rds_list()
      if (length(input$ct) && length(input$fate_choose) && !is.null( deg_rds[[input$ct]] ) ) {
      deg_data <- deg_rds[[input$ct]]     
      q_index <- paste0(input$fate_choose,' - ',input$ct)
      deg_data <- deg_data[deg_data$index %in% q_index, ]
      
     # generate_volcano_plot(deg_data, input$pvalThreshold, input$log2foldchange)
      
      deg_data <- deg_data %>%
        arrange(p_val) %>%
        slice_head(n = 5000)
      
      deg_data$avg_log2FC <- as.numeric(deg_data$avg_log2FC)
      
      deg_data$highlight <- ifelse(deg_data$p_val < input$pvalThreshold & abs(deg_data$avg_log2FC) > input$log2foldchange, "significant", "Insignificant genes")
      deg_data$highlight <- ifelse( deg_data$highlight == "significant", paste0(deg_data$cluster, " up-regulated genes"), deg_data$highlight )
      deg_data$highlight <- str_wrap(deg_data$highlight, width = 20)
      
      unique_up_regulated_highlights <- unique(deg_data$highlight[grepl("up-regulated", deg_data$highlight)])
      color_map <- setNames(rep("grey", length(unique(deg_data$highlight))), unique(deg_data$highlight))
      color_map["Insignificant genes"] <- "grey"
      if (length(unique_up_regulated_highlights) >= 1) {
        color_map[unique_up_regulated_highlights[1]] <- rgb(51/255,153/255,255/255) }
      if (length(unique_up_regulated_highlights) >= 2) {
        color_map[unique_up_regulated_highlights[2]] <- rgb(217/255,83/255,25/255)   }
      
      p <- ggplot(deg_data, aes(x = avg_log2FC, y = -log10(p_val), color = highlight, size = -log10(p_val),customdata = gene,
                                text = paste('Cluster:', cluster , '<br>Gene:', gene, '<br>Log2FC:', avg_log2FC, '<br>P-value:', p_val))) +
        geom_point(alpha = 0.8) +
        scale_size_continuous(range = c(1, 2),name = "") +
        scale_color_manual(values = color_map, name = "") + 
        theme_minimal() +
        theme(
          panel.grid.major = element_blank(),  # 移除主要网格线
          panel.grid.minor = element_blank(),  # 移除次要网格线
          axis.line = element_line(color = "black"),  # 保留轴线
          plot.title = element_text(size = 12,hjust = 0.5),         # Sets the size of the plot title
              axis.title = element_text(size = 12),         # Sets the size of the axis titles (both x and y)
              axis.text = element_text(size = 12), 
              legend.text = element_text(size = 10),        # Sets the size of the text in the legend
            #  legend.title = element_text(size = 16),
              legend.key.height=unit(2, "cm"))+
        labs(x = "Log2 Fold Change", y = "-Log10 p-value", title = unique(deg_data$index))
      
      #ggsave(filename = '/data/yexing/scLT/www/111aaa_vol.pdf', plot = p, device = "pdf", width = 14, height = 12, units = "in", dpi = 100, limitsize = FALSE)
      
      
      plotly_p <- ggplotly(p, tooltip = "text") %>%
        layout(hovermode = 'closest')
      plotly_p    } })
    
    # 设置点击搜索基因
    observeEvent(event_data("plotly_click"), {
      click_data <- event_data("plotly_click")
      gene_name <- click_data$customdata
      if (!is.null(gene_name)) {
        url <- paste0('https://www.ncbi.nlm.nih.gov/gene/?term=', gene_name)
        browseURL(url) } })
    
    # 设置deg火山图展示和warning展示
    output$imageOutputOrPreset_2 <- renderUI({
      if (errorOccurred()) {
        tagList( img(src = "Warning_DEG.png", alt = "Error Image",width = "100%", height = "100%" )) } else {
        list( fluidRow( column(6, sliderInput("pvalThreshold", "P-value Threshold", min = 0, max = 0.05, value = 0.05) ),
                        column(6, sliderInput("log2foldchange", "Log2 Fold Change", min = 0.25, max = 1, value = 0.25) )), 
              div(style = " height: 65vh;" ,
                plotlyOutput("volcanoPlot", height = "30vw") ) ) } })
    
    # 设置第一个GO分析
    output$image_display_deg_GO_1 <- renderUI({
      selected_id <- getSelectedDataset()
      dataset_deg_name <- paste0(selected_id, '_deg')
      if (length(input$ct) && length(input$fate_choose) ) {
        q_index <- paste0(input$fate_choose,' - ',input$ct)
        deg_rds <- deg_rds_list()
        deg_data <- deg_rds[[input$ct]]
        deg_data <- deg_data[deg_data$index %in% q_index, ]
        picture_value <- paste0(input$fate_choose,' - ', input$ct ,' --',unique(deg_data$cluster)[1],' bias.pdf')
        
        display_name <- paste0("Go terms for DEGs of ", input$ct, " with fate bias toward ",unique(deg_data$cluster)[1])
        #display_name <- gsub("\\.pdf$", "", picture_value)
        
        image_path <- paste0("clone_fate_bias_deg_GO/", dataset_deg_name)
        image_path_1 <- paste0("clone_fate_bias_deg_GO/", dataset_deg_name,"/",picture_value)
        if (file.exists(image_path_1)) {
          addResourcePath("GO", image_path)
          tagList(
            h3(display_name, style = "font-size: 18px; text-align: center;"), 
            tags$iframe(style="height:27.5vw; width:100%", src=paste0("GO/", picture_value)) ) 
          }else { 
              tagList(
                # 添加文件名作为标题
                h3(display_name, style = "font-size: 18px; text-align: center;"),
                tags$iframe(style="height:27.5vw; width:100%", src="GO_Warning.pdf")
              ) }
      } else {
        tags$iframe(style="height:27.5vw; width:100%", src="GO_Warning.pdf") } })
    
    # 设置第二个GO分析
    output$image_display_deg_GO_2 <- renderUI({
      selected_id <- getSelectedDataset()
      dataset_deg_name <- paste0(selected_id, '_deg')
      if (length(input$ct) && length(input$fate_choose) ) {
        q_index <- paste0(input$fate_choose,' - ',input$ct)
        deg_rds <- deg_rds_list()
        deg_data <- deg_rds[[input$ct]]
        deg_data <- deg_data[deg_data$index %in% q_index, ]

        picture_value <- paste0(input$fate_choose,' - ', input$ct ,' --',unique(deg_data$cluster)[2],' bias.pdf')
        
        display_name <- paste0("Go terms for DEGs of ", input$ct, " with fate bias toward ",unique(deg_data$cluster)[2])
        #display_name <- gsub("\\.pdf$", "", picture_value)
        
        image_path <- paste0("clone_fate_bias_deg_GO/", dataset_deg_name)
        image_path_1 <- paste0("clone_fate_bias_deg_GO/", dataset_deg_name,"/",picture_value)
        if (file.exists(image_path_1)) {
          addResourcePath("GO", image_path)
          tagList( h3(display_name, style = "font-size: 18px; text-align: center;"), tags$iframe(style="height:27.5vw; width:100%;", src=paste0("GO/", picture_value)) ) }else { 
            tagList(
              # 添加文件名作为标题
              h3(display_name, style = "font-size: 18px; text-align: center;"),
              tags$iframe(style="height:27.5vw; width:100%", src="GO_Warning.pdf")
            ) }
      } else { tags$iframe(style="height:27.5vw; width:100%", src="GO_Warning.pdf") } })
    
    # 设置第一个GO分析和warning展示
    output$imageOutputOrPreset_3 <- renderUI({
      if (errorOccurred()) {
        tagList( img(src = "Warning_DEG.png", alt = "Error Image",width = "100%", height = "100%" ) ) } else {
        list( uiOutput('image_display_deg_GO_1')) } })
    
    # 设置第二个GO分析和warning展示
    output$imageOutputOrPreset_4 <- renderUI({
      if (errorOccurred()) {
        tagList( img(src = "Warning_DEG.png", alt = "Error Image",width = "100%", height = "100%" ) ) } else {
          list( uiOutput('image_display_deg_GO_2')) } })
    
    
    #添加motif模块的plotly展示
    output$motif_plot_1 <- renderPlotly({
      
      selected_id <- getSelectedDataset()
      if (length(input$ct) && length(input$fate_choose) ) {
        
        split_result <- strsplit(input$fate_choose, " vs ")
        var1 <- split_result[[1]][1]  
        load_1 <-paste0("motif/",selected_id,"/motif_",input$ct," ",var1,".csv")

        if (file.exists(load_1)) {
          final_df <-read.csv(load_1,row.names = 1)
          
          title_name = paste0("motif enrichment ",input$ct," with ",var1)
          
          plot_ly(final_df, 
                  x = ~rank, 
                  y = ~score, 
                  type = 'scatter', 
                  mode = 'markers', 
                  marker = list(size = 6, color = ~score, colorscale = 'Viridis', showscale = TRUE), 
                  text = ~paste("TF:", TF, "<br>Score:", score),
                  hoverinfo = 'text') %>%
            layout(
              title = title_name,
              xaxis = list(title = "Rank Sorted TFs Enriched"),
              yaxis = list(title = "Motif Enrichment Score"),
              showlegend = FALSE
            ) %>%
            colorbar(title = "Score")
        }else{
          plot_ly(type = 'scatter', mode = 'none') %>% 
            layout(
              title = list(text = ""),
              xaxis = list(visible = FALSE),
              yaxis = list(visible = FALSE),
              paper_bgcolor = 'rgba(0,0,0,0)',  # 设置背景透明
              plot_bgcolor = 'rgba(0,0,0,0)',   # 设置图表区域背景透明
              annotations = list(
                text = "This dataset does not contain chromatin accessibility modality",
                x = 0.5, y = 0.5, showarrow = FALSE,
                font = list(size = 18),  # 在这里设置字号
                xref = 'paper', yref = 'paper'
              )
            )
        }

        }})
    
    output$motif_plot_2 <- renderPlotly({
      
      selected_id <- getSelectedDataset()
      if (length(input$ct) && length(input$fate_choose) ) {
        
        split_result <- strsplit(input$fate_choose, " vs ")
        var2 <- split_result[[1]][2]  
        load_2 <-paste0("motif/",selected_id,"/motif_",input$ct," ",var2,".csv")
        
        if (file.exists(load_2)) {
        final_df <-read.csv(load_2,row.names = 1)
        
        title_name = paste0("motif enrichment ",input$ct," with ",var2)
        
        plot_ly(final_df, 
                x = ~rank, 
                y = ~score, 
                type = 'scatter', 
                mode = 'markers', 
                marker = list(size = 6, color = ~score, colorscale = 'Viridis', showscale = TRUE), 
                text = ~paste("TF:", TF, "<br>Score:", score),
                hoverinfo = 'text') %>%
          layout(
            title = title_name,
            xaxis = list(title = "Rank Sorted TFs Enriched"),
            yaxis = list(title = "Motif Enrichment Score"),
            showlegend = FALSE
          ) %>%
          colorbar(title = "Score")
        }else{
          plot_ly(type = 'scatter', mode = 'none') %>% 
            layout(
              title = list(text = ""),
              xaxis = list(visible = FALSE),
              yaxis = list(visible = FALSE),
              paper_bgcolor = 'rgba(0,0,0,0)',  # 设置背景透明
              plot_bgcolor = 'rgba(0,0,0,0)',   # 设置图表区域背景透明
              annotations = list(
                text = "This dataset does not contain chromatin accessibility modality",
                x = 0.5, y = 0.5, showarrow = FALSE,
                font = list(size = 18),  # 在这里设置字号
                xref = 'paper', yref = 'paper'
              )
            )
        }
      }})
    
    output$imageOutputOrPreset_motif_1 <- renderUI({
      if (errorOccurred()) {
        tagList( img(src = "Warning_motif.png", alt = "Error Image",width = "100%", height = "100%" )) } else {
          list(
                div(style = " height: 65vh;" ,
                    plotlyOutput("motif_plot_1", height = "30vw") ) ) } })
    
    output$imageOutputOrPreset_motif_2 <- renderUI({
      if (errorOccurred()) {
        tagList( img(src = "Warning_motif.png", alt = "Error Image",width = "100%", height = "100%" )) } else {
          list(
            div(style = " height: 65vh;" ,
                plotlyOutput("motif_plot_2", height = "30vw") ) ) } })
    
    
    
    #跳跃
    clone_szie_info <- reactive({
      pbmc = obj_metadata_list()
      pbmc = clean_pbmc_data(pbmc)
      pbmc = pbmc[pbmc$celltype == input$ct_size,]
      barcode_counts <- pbmc %>%
        group_by(celltype, barcodes) %>%
        summarise(count = n()) %>%
        arrange(celltype, desc(count))
      
      result_df <- data.frame(
        `Cell type` = character(),
        `Clone size top 1` = character(),
        `Clone size top 2` = character(),
        `Clone size top 3` = character(),
        `Clone size top 4` = character(),
        `Clone size top 5` = character(),
        stringsAsFactors = F,
        check.names = FALSE
      )
      
      unique_celltypes <- unique(pbmc$celltype)
      
      for (ct in unique_celltypes) {
        celltype_data <- filter(barcode_counts, celltype == ct)
        top_barcodes <- head(celltype_data$barcodes, 5)
        
        temp_df <- data.frame(
          `Cell type` = ct,
          `Clone size top 1` = ifelse(length(top_barcodes) >= 1, paste0('Clone ID: ',top_barcodes[1]), NA),
          `Clone size top 2` = ifelse(length(top_barcodes) >= 2, paste0('Clone ID: ',top_barcodes[2]), NA),
          `Clone size top 3` = ifelse(length(top_barcodes) >= 3, paste0('Clone ID: ',top_barcodes[3]), NA),
          `Clone size top 4` = ifelse(length(top_barcodes) >= 4, paste0('Clone ID: ',top_barcodes[4]), NA),
          `Clone size top 5` = ifelse(length(top_barcodes) >= 5, paste0('Clone ID: ',top_barcodes[5]), NA),
          stringsAsFactors = F,
          check.names = FALSE
        )
        result_df <- rbind(result_df, temp_df)
      }
      return(result_df)
        })
    
    # 输出表格
    output$clone_szie_info_out <- renderTable({
      clone_szie_info()
    }, colnames = TRUE,striped = FALSE)
    
    
    # 设置pickerInput("ct_size", "Select Celltype")
    output$select_A_deg_celltype_size <- renderUI({
      selected_id <- getSelectedDataset()
      dataset_deg_name <- paste0(selected_id, '_deg')

      if (dataset_deg_name %in% names(celltype__fate_use_size_list)) {
        errorOccurred_size(FALSE)  # 没有错误，更新状态
        
        pickerInput("ct_size", "Select cell type:",
                    choices = names(celltype__fate_use_size_list[[dataset_deg_name]]),
                    selected = ifelse("HSPC" %in% names(celltype__fate_use_size_list[[dataset_deg_name]]), 
                                      "HSPC", 
                                      names(celltype__fate_use_size_list[[dataset_deg_name]])[1]),
                    options = list(`actions-box` = TRUE), multiple = FALSE)
      } else {
        errorOccurred_size(TRUE)  # 发生错误，更新状态
        pickerInput("ct_size", "Select cell type:", choices = character(0)) } })
    
    # 设置pickerInput("fate_choose_size", "Select Fate")
    output$select_A_deg_fate_size <- renderUI({
      if (!errorOccurred_size()) {  # 仅当没有错误时渲染
        selected_id <- getSelectedDataset()
        dataset_deg_name <- paste0(selected_id, '_deg')
        if (length(input$ct_size) && !is.null(celltype__fate_use_size_list[[dataset_deg_name]][[input$ct_size]]$fate_use) ) {
          pickerInput("fate_choose_size", "Select large-sized clones:",
                      choices = celltype__fate_use_size_list[[dataset_deg_name]][[input$ct_size]]$fate_use,
                      selected = celltype__fate_use_size_list[[dataset_deg_name]][[input$ct_size]]$fate_use[1],
                      options = list(`actions-box` = TRUE), multiple = FALSE)}
      } else { pickerInput("fate_choose_size", "Select fate bias:", choices = character(0)) } })
    
    # 设置deg_size热图展示
    output$image_display_deg_size <- renderImage({
      selected_id <- getSelectedDataset()
      dataset_deg_name <- paste0(selected_id, '_deg')
      if (length(input$ct_size) && !is.null(celltype__fate_use_size_list[[dataset_deg_name]][[input$ct_size]])) {
        q_dataframe <- celltype__fate_use_size_list[[dataset_deg_name]][[input$ct_size]]
      } else { q_dataframe <- data.frame() }
      if (nrow(q_dataframe) > 0 && length(input$fate_choose_size)==1 ) {
        picture_value <- dplyr::filter(q_dataframe, celltype == input$ct_size, fate_use == input$fate_choose_size) %>%
          dplyr::pull(picture)
       
        image_path <- paste0("celltype_clone_deg/", dataset_deg_name, '/', picture_value)
        list( src = image_path, contentType = "image/png",width = "100%")}else{
          list(src = "Warning_DEG.png", alt = "Error Image",width = "100%", height = "100%" ) } }, deleteFile = FALSE)
    
    # 设置deg_size热图展示和warning展示
    output$imageOutputOrPreset_size <- renderUI({
      if (errorOccurred_size()) { tagList( img(src = "Warning_DEG.png", alt = "Error Image",width = "100%", height = "100%" ) )
      } else {
        list( fluidRow( column(6,uiOutput("select_A_deg_celltype_size")), column(6,uiOutput("select_A_deg_fate_size")) ),imageOutput('image_display_deg_size') ) } })
    
    # 画deg_size交互式火山图
    output$volcanoPlot_size <- renderPlotly({
      selected_id <- getSelectedDataset()
      dataset_deg_name <- paste0(selected_id, '_deg')
      deg_size_rds = deg_size_rds_list()
      if (length(input$ct_size) && length(input$fate_choose_size) && !is.null(deg_size_rds[[input$ct_size]]) ) {
        deg_data <- deg_size_rds[[input$ct_size]]     
        
        q_index <- paste0(input$fate_choose_size,' - ',input$ct_size)
        deg_data <- deg_data[deg_data$index %in% q_index, ]
        
        deg_data <- deg_data %>%
          arrange(p_val) %>%
          slice_head(n = 5000)
     
        deg_data$highlight <- ifelse(deg_data$p_val < input$pvalThreshold_size & abs(deg_data$avg_log2FC) > input$log2foldchange_size, "significant", "Insignificant genes")
        deg_data$highlight <- ifelse( deg_data$highlight == "significant", paste0(deg_data$cluster, " up-regulated genes"), deg_data$highlight )
        deg_data$highlight <- str_wrap(deg_data$highlight, width = 20)
        
        unique_up_regulated_highlights <- unique(deg_data$highlight[grepl("up-regulated", deg_data$highlight)])
        color_map <- setNames(rep("grey", length(unique(deg_data$highlight))), unique(deg_data$highlight))
        color_map["Insignificant genes"] <- "grey"
        if (length(unique_up_regulated_highlights) >= 1) {
          color_map[unique_up_regulated_highlights[1]] <- rgb(51/255,153/255,255/255) }
        if (length(unique_up_regulated_highlights) >= 2) {
          color_map[unique_up_regulated_highlights[2]] <- rgb(217/255,83/255,25/255)    }
       
        p <- ggplot(deg_data, aes(x = avg_log2FC, y = -log10(p_val), color = highlight, size = -log10(p_val),customdata = gene,
                                  text = paste('Cluster:', cluster , '<br>Gene:', gene, '<br>Log2FC:', avg_log2FC, '<br>P-value:', p_val))) +
          geom_point(alpha = 0.8) +
          scale_size_continuous(range = c(1, 2),name = "") +
          scale_color_manual(values = color_map, name = "") +
          theme_minimal() +
          theme(
            panel.grid.major = element_blank(),  # 移除主要网格线
            panel.grid.minor = element_blank(),  # 移除次要网格线
            axis.line = element_line(color = "black"),  # 保留轴线
            plot.title = element_text(size = 12,hjust = 0.5),         # Sets the size of the plot title
                axis.title = element_text(size = 12),         # Sets the size of the axis titles (both x and y)
                axis.text = element_text(size = 12), 
                legend.text = element_text(size = 10),        # Sets the size of the text in the legend
                #  legend.title = element_text(size = 16),
                legend.key.height=unit(2, "cm"))+
          labs(x = "Log2 Fold Change", y = "-Log10 p-value", title = unique(deg_data$index))
        
        # 转换为 plotly 对象，并添加点击事件
        plotly_p_size <- ggplotly(p, tooltip = "text") %>%
          layout(hovermode = 'closest')
       # event_register(p, 'plotly_click_size')
        plotly_p_size    } })
    
    # 设置deg_size火山图展示和warning展示
    output$imageOutputOrPreset_size_2 <- renderUI({
      if (errorOccurred_size()) {
        tagList( img(src = "Warning_DEG.png", alt = "Error Image",width = "100%", height = "100%" ) ) } else {
        list(
          fluidRow(column(6, sliderInput("pvalThreshold_size", "P-value Threshold", min = 0, max = 0.05, value = 0.05 )), 
                   column(6, sliderInput("log2foldchange_size", "Log2 Fold Change", min = 0.25, max = 1, value = 0.25))), 
          div(style = " height: 65vh;", 
            plotlyOutput('volcanoPlot_size',height = "30vw") )) } })   
    
    # 设置第一个GO_size分析
    output$image_display_deg__size_GO_1 <- renderUI({
      
      selected_id <- getSelectedDataset()
      dataset_deg_name <- paste0(selected_id, '_deg')
      if (length(input$ct_size) && length(input$fate_choose_size) ) {
        
        q_index <- paste0(input$fate_choose_size,' - ',input$ct_size)
        deg_size_rds = deg_size_rds_list()
        deg_data <- deg_size_rds[[input$ct_size]]
        deg_data <- deg_data[deg_data$index %in% q_index, ]
        
        picture_value <- paste0(input$fate_choose_size,' - ', input$ct_size ,' --',unique(deg_data$cluster)[1],'.pdf')
        display_name <- gsub("\\.pdf$", "", picture_value)
                             
        image_path <- paste0("celltype_clone_deg_GO/", dataset_deg_name)
        image_path_1 <- paste0("celltype_clone_deg_GO/", dataset_deg_name,"/",picture_value)
        if (file.exists(image_path_1)) {
          addResourcePath("GO", image_path)
          tagList(
            # 添加文件名作为标题
            h3(display_name, style = "font-size: 18px; text-align: center;"),
            tags$iframe(style="height:27.5vw; width:100%", src=paste0("GO/", picture_value))
          )}else { 
            tagList(
              # 添加文件名作为标题
              h3(display_name, style = "font-size: 18px; text-align: center;"),
              tags$iframe(style="height:27.5vw; width:100%", src="GO_Warning.pdf")
            ) } } else { tags$iframe(style="height:27.5vw; width:100%", src="GO_Warning.pdf") } })
    
    # 设置第二个GO_size分析
    output$image_display_deg__size_GO_2 <- renderUI({
      
      selected_id <- getSelectedDataset()
      dataset_deg_name <- paste0(selected_id, '_deg')
      if (length(input$ct_size) && length(input$fate_choose_size) ) {
        q_index <- paste0(input$fate_choose_size,' - ',input$ct_size)
        deg_size_rds = deg_size_rds_list()
        deg_data <- deg_size_rds[[input$ct_size]]
        deg_data <- deg_data[deg_data$index %in% q_index, ]
        
        picture_value <- paste0(input$fate_choose_size,' - ', input$ct_size ,' --',unique(deg_data$cluster)[2],'.pdf')
        display_name <- gsub("\\.pdf$", "", picture_value)
        
        image_path <- paste0("celltype_clone_deg_GO/", dataset_deg_name)
        image_path_1 <- paste0("celltype_clone_deg_GO/", dataset_deg_name,"/",picture_value)
        if (file.exists(image_path_1)) {
          addResourcePath("GO", image_path)
          tagList(
            h3(display_name, style = "font-size: 18px; text-align: center;"),
            tags$iframe(style="height:27.5vw; width:100%", src=paste0("GO/", picture_value)) )  }else { 
              tagList(
                # 添加文件名作为标题
                h3(display_name, style = "font-size: 18px; text-align: center;"),
                tags$iframe(style="height:27.5vw; width:100%", src="GO_Warning.pdf")
              ) }
      } else { tags$iframe(style="height:27.5vw; width:100%", src="GO_Warning.pdf") } })
    
    # 设置第一个GO_size分析和warning展示
    output$imageOutputOrPreset_size_3 <- renderUI({
      if (errorOccurred_size()) {
        tagList( img(src = "Warning_DEG.png", alt = "Error Image",width = "100%", height = "100%" ) )
      } else { list( uiOutput('image_display_deg__size_GO_1')) } })
    
    # 设置第二个GO_size分析和warning展示
    output$imageOutputOrPreset_size_4 <- renderUI({
      if (errorOccurred_size()) {
        tagList( img(src = "Warning_DEG.png", alt = "Error Image",width = "100%", height = "100%" ) )
      } else { list( uiOutput('image_display_deg__size_GO_2')) } })
    
# 设置Gene violin
    # 生成小提琴图的反应式表达式----
    observeEvent(cleaned_data(),{
      data <- cleaned_data()
      gene_data <- matrix_q()
      if ( nrow(gene_data) > 0 ) {
          selected_id <- getSelectedDataset()
          qq = unique(data$pbmc$celltype)[1]
          if( is.null(qq) ){ return() }
          metadata = obj_metadata_list()
          common_cells <- intersect(rownames(metadata), colnames(gene_data))
          metadata <- metadata[common_cells, ]
          metadata <- metadata %>%
            dplyr::filter(!is.na(celltype) & !grepl("^\\s*$", celltype))
          clone_fate_bias = clone_fate_bias_list()
          pbmc_data <- fate_bias_summary_2(qq, clone_fate_bias, metadata )
          if( length(pbmc_data)==0 ){ return() }
          
          common_cells <- intersect(rownames(pbmc_data), colnames(gene_data))
          pbmc_data <- pbmc_data[common_cells, ]
          gene_data <- gene_data[, common_cells]
          
          gene_expression <- gene_data[ , rownames(pbmc_data)]
          average_expression <- rowMeans(gene_expression)
          highest_expression_gene <- names(which.max(average_expression))
          gene_expression <- gene_expression[highest_expression_gene, ]
          
          #updateTextInput(session, "gene_violin", value = highest_expression_gene )
          
          updateSelectizeInput(session, "gene_violin", choices = rownames(gene_data), selected = highest_expression_gene, server = TRUE)
          
          gene_expression_vector <- as.vector(gene_expression)
          plot_data <- data.frame( Expression = gene_expression_vector, Lineage = pbmc_data$lineage )
          plot_data=as.data.frame(plot_data[plot_data$Expression!=0,])
          
          
          if(selected_id == 'Weinreb_2020_Science'){
            new_cols <- c(
              "Baso --bias" = "#D7BD90",       
              "Ccr7_DC --bias" = "#114B9B",    
              "Eos --bias" = "#193E51",        
              "Erythroid --bias" = "#F296C0",  
              "Lymphoid --bias" = "#FBBD90",   
              "Mast --bias" = "#C82129",       
              "Meg --bias" = "#647950",        
              "Monocyte --bias" = "#8DB5CE",   
              "Neutrophil --bias" = "#542E8B", 
              "pDC --bias" = "#BE93BF",        
              "HSPC --bias" = "#BDBDBD",     
              "No bias" = "black"
            ) }else{
            # 生成颜色映射
            unique_lineages <- unique(plot_data$Lineage)
            new_cols <- setNames(getPalette(length(unique_lineages)), unique_lineages)
          }
          
          plot_data$Lineage <- str_wrap(plot_data$Lineage, width = 20)
          
          pp<- ggplot(plot_data, aes(x = Lineage, y = Expression, fill = Lineage)) +
            geom_violin(trim = FALSE, drop = FALSE) +
            labs(title = paste("Violin plot of", highest_expression_gene),
                 x = "Fate bias",
                 y = "Expression Level") +
            theme_minimal()+ 
            theme(plot.title = element_text(size = 20,hjust = 0.5),         # Sets the size of the plot title
                  axis.title = element_text(size = 20),         # Sets the size of the axis titles (both x and y)
                  axis.text.x = element_text(size = 20,angle=45, hjust = 1), 
                  axis.text.y = element_text(size = 12),
                  legend.text = element_text(size = 16),        # Sets the size of the text in the legend
                  legend.title = element_text(size = 20),
                  legend.position = "none")+
            scale_fill_manual(values = new_cols, breaks = unique(plot_data$Lineage))
          
          output$violinPlot <- renderPlot({ pp } , width = function() { session$clientData$output_violinPlot_width },
            height = function() { 600 })  
          output$dynamic_gene_violin <- renderUI({ plotOutput("violinPlot",width = "100%",height= "100%") })
        }else{ output$dynamic_gene_violin <- renderUI({
        img(src = "Caution_gene_expression.png", alt = "Error Image", width = "100%", height = "100%")  }) } })
  
    observeEvent(input$submit_violin, {
      gene_data <- matrix_q()
      if ( nrow( gene_data ) > 0 ) {
        if ( input$gene_violin %in% rownames(gene_data) ) {
          selected_id <- getSelectedDataset()
          qq = input$select_A_celltype_gene_violin
          if( is.null(qq) ){ return() }
          metadata = obj_metadata_list()
          common_cells <- intersect(rownames(metadata), colnames(gene_data))
          metadata <- metadata[common_cells, ]
          metadata <- metadata %>%
            dplyr::filter(!is.na(celltype) & !grepl("^\\s*$", celltype))
          
          clone_fate_bias = clone_fate_bias_list()
          pbmc_data <- fate_bias_summary_2(qq, clone_fate_bias, metadata)
          
          common_cells <- intersect(rownames(pbmc_data), colnames(gene_data))
          pbmc_data <- pbmc_data[common_cells, ]
          gene_data <- gene_data[, common_cells]
          
          gene_expression <- gene_data[input$gene_violin, rownames(pbmc_data), drop = FALSE]
          gene_expression_vector <- as.vector(gene_expression)
          plot_data <- data.frame( Expression = gene_expression_vector, Lineage = pbmc_data$lineage )
          
          lineages_all_zero <- plot_data %>%
            group_by(Lineage) %>%
            summarize(all_zero = all(Expression == 0)) %>%
            filter(all_zero) %>%
            pull(Lineage)
          
          # 根据 Lineage 进行区分，若某个 Lineage 中所有行的 Expression 均为 0，则不移除
          plot_data <- plot_data %>%
            filter(!(Expression == 0 & !(Lineage %in% lineages_all_zero)))
          
          if(nrow(plot_data) >0 ){
            
            if(selected_id == 'Weinreb_2020_Science'){
              new_cols <- c(
                "Baso --bias" = "#D7BD90",       
                "Ccr7_DC --bias" = "#114B9B",    
                "Eos --bias" = "#193E51",        
                "Erythroid --bias" = "#F296C0",  
                "Lymphoid --bias" = "#FBBD90",   
                "Mast --bias" = "#C82129",       
                "Meg --bias" = "#647950",        
                "Monocyte --bias" = "#8DB5CE",   
                "Neutrophil --bias" = "#542E8B", 
                "pDC --bias" = "#BE93BF",        
                "HSPC --bias" = "#BDBDBD",     
                "No bias" = "black"
              ) 
              desired_order <- c('HSPC --bias', 'Monocyte --bias', 'Neutrophil --bias', 'Baso --bias', 'Mast --bias', 
                                 'Erythroid --bias', 'Meg --bias', 'Ccr7_DC --bias', 'Lymphoid --bias', 'Eos --bias', 'pDC --bias', 'No bias')
              desired_order <- str_wrap(desired_order, width = 20)
              }else{
                # 生成颜色映射
                unique_lineages <- unique(plot_data$Lineage)
                new_cols <- setNames(getPalette(length(unique_lineages)), unique_lineages)
                desired_order <- unique(plot_data$Lineage)
                desired_order <- str_wrap(desired_order, width = 20)
              } 
            
            plot_data$Lineage <- str_wrap(plot_data$Lineage, width = 20)
            plot_data$Lineage <- factor(plot_data$Lineage, levels = desired_order)
            
          pp <- ggplot(plot_data, aes(x = Lineage, y = Expression, fill = Lineage)) +
            geom_violin(trim = FALSE, drop = FALSE) +
            labs(title = paste("Violin plot of", input$gene_violin),
                 x = "Fate bias",
                 y = "Expression Level") +
            theme_minimal()+ 
            theme(plot.title = element_text(size = 20,hjust = 0.5),         # Sets the size of the plot title
                  axis.title = element_text(size = 20),         # Sets the size of the axis titles (both x and y)
                  axis.text.x = element_text(size = 20,angle=45,hjust = 1), 
                  axis.text.y = element_text(size = 12),
                  legend.text = element_text(size = 16),        # Sets the size of the text in the legend
                  legend.title = element_text(size = 20),
                  legend.position = "none")+
            scale_fill_manual(values = new_cols, breaks = desired_order)
        
          #ggsave(filename = '/data/yexing/scLT/www/violin.pdf', plot = pp, device = "pdf", width = 12, height = 6, units = "in", dpi = 100, limitsize = FALSE)
          
            
    output$violinPlot <- renderPlot({ pp } , width = function() { session$clientData$output_violinPlot_width },
      height = function() { 600 })  
    output$dynamic_gene_violin <- renderUI({ plotOutput("violinPlot",width = "100%",height= "100%") }) 
    }else{ output$dynamic_gene_violin <- renderUI({ img(src = "gene_violin.png", alt = "Error Image", width = "100%", height = "100%")}) }
    }else{ output$dynamic_gene_violin <- renderUI({ img(src = "Note_gene_expression.png", alt = "Error Image", width = "100%", height = "100%")}) }
    }else{ output$dynamic_gene_violin <- renderUI({ img(src = "Caution_gene_expression.png", alt = "Error Image", width = "100%", height = "100%")  }) } },ignoreInit = TRUE) 
    
    #----

    # download界面相关函数设置 ----   
    
    filteredData_D <- reactive({
      req(coretable1)  
      data <- coretable1
      if (length(input$species_download) > 0){ data <- data[data$Species %in% input$species_download, ]}
      if (length(input$tissue_download) > 0){ data <- data[sapply(data$`Tissue source`, function(x) any(sapply(input$tissue_download, function(y) grepl(y, x)))), ]}
      if (length(input$technology_download) > 0){ data <- data[data$Technology %in% input$technology_download, ]}
      if (length(input$barcodetype_download) > 0) {data <- data[data$`Barcode type` %in% input$barcodetype_download, ]}
      
      if (input$searchBtn_download > 0) {
        isolate({ 
          if (input$search_download != "") {
            search_term <- tolower(input$search_download)
            data <- data[apply(data, 1, function(row) any(grepl(search_term, tolower(row)))), ] } }) }
      return(data) })
    
    reactiveSearch_D <- eventReactive(input$searchBtn_download,{ input$search_download }, ignoreNULL = FALSE)
    
    reactiveData_D <- reactive({
      data <- filteredData_D()  
      search <- reactiveSearch_D() 
      if (nchar(search) > 0 ) {
        #使用 gsub 高亮匹配的文本
        data <- data %>% 
          mutate(across(everything(), ~ gsub(paste0('(', search, ')'), 
                                             '<span style="background-color: yellow;">\\1</span>', 
                                             ., ignore.case = TRUE)))}
      return(data) })
    
    output$coretable123_D <- DT::renderDT({
      data <- reactiveData_D()
      
      data <- data %>%
        mutate(Species = paste0("<i>", Species, "</i>"))
      
      datatable(
        data,
        extensions = 'Buttons',
        class = 'cell-border stripe hover',
        selection = 'single',
        rownames = FALSE,
        escape = FALSE,
        options = list(
          dom = 'lBrtip',
          scrollX = TRUE,
          pageLength = 10,
          searchHighlight = TRUE,
          lengthChange = TRUE,
          lengthMenu = list(c(10, 25, 50, 100, 150), c('10', '25', '50', '100', '150')),
          buttons = list('copy', 'csv', 'excel', 'pdf'),
          responsive = TRUE
          # columnDefs = list(
          #   list(
          #     targets = which(names(data) == 'Species') - 1,
          #     render = JS("function(data, type, row) {
          #     return type === 'display' && data != null ? '<i>' + data + '</i>' : data;
          #   }") ))
          ))})
    #----    
    
    
    
   
    
    
    
           
    # online tool界面相关函数设置
    
    # 设置online tool文件上传
    resetFlag <- reactiveVal(FALSE)
    observeEvent(input$resetButton, { resetFlag(!resetFlag())  })
    
    # 设置上传文件按钮
    output$fileInputRender <- renderUI({
      if (resetFlag()) {
        fileInput("upload_metadata", NULL,buttonLabel = tags$span("Upload...", style = "font-size: 16px; font-family: 'Helvetica'"), width = 250, multiple = FALSE,accept = c(".csv") )
      } else {
        fileInput("upload_metadata", NULL,buttonLabel = tags$span("Upload...", style = "font-size: 16px; font-family: 'Helvetica'"), width = 250, multiple = FALSE,accept = c(".csv") ) } })
    
    
    
    #fate bias degs/dar 文件上传
    resetFlag_1 <- reactiveVal(FALSE)
    observeEvent(input$resetButton_1, { resetFlag_1(!resetFlag_1())  })
    
    output$fileInputRender_1 <- renderUI({
      if (resetFlag_1()) {
        fileInput("upload_fate_matrix", NULL,buttonLabel = tags$span("Upload matrix", style = "font-size: 16px; font-family: 'Helvetica'"), width = 250, multiple = FALSE,accept = c(".csv") )
      } else {
        fileInput("upload_fate_matrix", NULL,buttonLabel = tags$span("Upload matrix", style = "font-size: 16px; font-family: 'Helvetica'"), width = 250, multiple = FALSE,accept = c(".csv") ) } })
    
    resetFlag_1_2 <- reactiveVal(FALSE)
    observeEvent(input$resetButton_1_2, { resetFlag_1_2(!resetFlag_1_2())  })
    
    output$fileInputRender_1_2 <- renderUI({
      if (resetFlag_1_2()) {
        fileInput("upload_fate_metadata", NULL,buttonLabel = tags$span("Upload metadata", style = "font-size: 16px; font-family: 'Helvetica'"), width = 250, multiple = FALSE,accept = c(".csv") )
      } else {
        fileInput("upload_fate_metadata", NULL,buttonLabel = tags$span("Upload metadata", style = "font-size: 16px; font-family: 'Helvetica'"), width = 250, multiple = FALSE,accept = c(".csv") ) } })
    
    # 上传文件接收处理
    upload_metadata <- reactive({
      req(input$upload_metadata)  # 确保文件已上传
      ext <- tools::file_ext(input$upload_metadata$name)
      max_file_size <- 100 * 1024^2  # 100MB
      
      if (tolower(ext) == "csv" && input$upload_metadata$size <= max_file_size) {
        dataset <- readDataset(input$upload_metadata$datapath)
      } else {return(NULL)  }})
    
    # 弹窗报错处理
    observe({
      req(input$upload_metadata)  # 确保文件已上传
      
      # 获取文件扩展名
      ext <- tools::file_ext(input$upload_metadata$name)
      
      # 获取文件大小限制 (100MB)
      max_file_size <- 100 * 1024^2  # 100MB
      
      # 检查文件扩展名是否为 CSV
      if (tolower(ext) != "csv") {
        showModal(modalDialog(
          title = "Invalid file type",
          "Please upload a CSV file.",
          easyClose = TRUE,
          footer = NULL
        ))
      } 
      
      # 检查文件大小是否超过 100MB
      else if (input$upload_metadata$size > max_file_size) {
        showModal(modalDialog(
          title = "File too large",
          "The file is too large to process. Please upload a file smaller than 100MB.",
          easyClose = TRUE,
          footer = NULL
        ))
      }
    })
    
    
    #deg上传文件接收处理
    upload_fate_matrix <- reactive({
      req(input$upload_fate_matrix)  # 确保文件已上传
      ext <- tools::file_ext(input$upload_fate_matrix$name)
      max_file_size <- 100 * 1024^2  # 100MB
      
      if (tolower(ext) == "csv" && input$upload_fate_matrix$size <= max_file_size) {
        dataset <- read.csv(input$upload_fate_matrix$datapath, row.names = 1,check.names = FALSE)
      } else {return(NULL)  }})
    
    observe({
      req(input$upload_fate_matrix)  # 确保文件已上传
      
      # 获取文件扩展名
      ext <- tools::file_ext(input$upload_fate_matrix$name)
      
      # 获取文件大小限制 (100MB)
      max_file_size <- 100 * 1024^2  # 100MB
      
      # 检查文件扩展名是否为 CSV
      if (tolower(ext) != "csv") {
        showModal(modalDialog(
          title = "Invalid file type",
          "Please upload a CSV file.",
          easyClose = TRUE,
          footer = NULL
        ))
      } 
      
      # 检查文件大小是否超过 100MB
      else if (input$upload_fate_matrix$size > max_file_size) {
        showModal(modalDialog(
          title = "File too large",
          "The file is too large to process. Please upload a file smaller than 100MB.",
          easyClose = TRUE,
          footer = NULL
        ))
      }
    })
    
    
     #deg_metadata上传处理
    upload_fate_metadata <- reactive({
      req(input$upload_fate_metadata)  # 确保文件已上传
      ext <- tools::file_ext(input$upload_fate_metadata$name)
      max_file_size <- 100 * 1024^2  # 100MB
      
      if (tolower(ext) == "csv" && input$upload_fate_metadata$size <= max_file_size) {
        dataset <- read.csv(input$upload_fate_metadata$datapath, row.names = 1)
      } else {return(NULL)  }})
    
    observe({
      req(input$upload_fate_metadata)  # 确保文件已上传
      
      # 获取文件扩展名
      ext <- tools::file_ext(input$upload_fate_metadata$name)
      
      # 获取文件大小限制 (100MB)
      max_file_size <- 100 * 1024^2  # 100MB
      
      # 检查文件扩展名是否为 CSV
      if (tolower(ext) != "csv") {
        showModal(modalDialog(
          title = "Invalid file type",
          "Please upload a CSV file.",
          easyClose = TRUE,
          footer = NULL
        ))
      } 
      
      # 检查文件大小是否超过 100MB
      else if (input$upload_fate_metadata$size > max_file_size) {
        showModal(modalDialog(
          title = "File too large",
          "The file is too large to process. Please upload a file smaller than 100MB.",
          easyClose = TRUE,
          footer = NULL
        ))
      }
    })
    
    
    #deg_online处理函数
    create_seurat_object <- reactive({
      req(input$upload_fate_metadata)  # 确保文件已上传
      req(input$upload_fate_matrix)  # 确保文件已上传
    
      metadata_tool = upload_fate_metadata()
      matrix_tool = upload_fate_matrix()
      
      if (!all(colnames(matrix_tool) %in% rownames(metadata_tool))) {
        showModal(modalDialog(
          title = "Error",
          "The column names in the gene expression matrix do not match the row names in the metadata.",
          easyClose = TRUE,
          footer = NULL
        ))
        return(NULL)
      }
   
      metadata_tool <- metadata_tool[colnames(matrix_tool), ]
      obj <- CreateSeuratObject(counts = matrix_tool, meta.data = metadata_tool)
      return(obj)
    })
      
    observe({
      obj = create_seurat_object()
      celltype_num = unique(obj@meta.data$celltype)
      updatePickerInput(session, "celltype_online_tool", choices = celltype_num ) })
    
    create_fate_bias_list <- reactive({
      obj = create_seurat_object()
      cell_fate=obj@meta.data
      cell_fate = cell_fate[,c('barcodes','celltype')]
      cell_fate = cell_fate[cell_fate$barcodes!='',]
      cell_fate = cell_fate[!is.na(cell_fate$barcodes),]
      cell_fate <- cell_fate[!grepl("^\\s*$", cell_fate$barcodes), ] 
      
      bias_list = list()
      for (j in unique(cell_fate[,2])) {
        ct_fate_bias = clone_fate_bias(cell_fate,j)
        ct_fate_bias_fdr = ct_fate_bias[ct_fate_bias$fdr<0.05,]
        bias_list[[j]] = ct_fate_bias_fdr
      }
      
      return(bias_list)
      })
    
    observe({
      bias_list <- create_fate_bias_list()
      
      non_empty_dfs <- sum(sapply(bias_list, function(df) nrow(df) > 0))
      if (non_empty_dfs > 1) {
      
      sig_fate = c()
      
      for (j in 1:length(bias_list)) {
        fate = bias_list[[j]]
        if ("fdr" %in% names(fate)) {
          fate = fate[fate$fdr < 0.05,]
        }
        if ("fate_ratio" %in% names(fate)) {
          fate = fate[fate$fate_ratio>0.1,]
        }
        if (nrow(fate)>0) {
          bias_list[[names(bias_list)[j]]] = fate
          sig_fate = c(sig_fate,names(bias_list)[j])
        }
      }
      
      ### all cell fate combinations
      combinations <- combn(sig_fate, 2)
      combination_names <- apply(combinations, 2, function(col) {
        paste(col[1], "VS", col[2])
      })
      
      # 使用 setNames 函数创建 combinations_dict 字典
      combinations_dict <- setNames(1:ncol(combinations), combination_names)
      
      updatePickerInput(session, "celltype_online_tool_fate", choices = combinations_dict )
        
      }else{
        showModal(modalDialog(
          title = "Error",
          "This dataset does not have sufficient data to perform DEG analysis.",
          easyClose = TRUE,
          footer = NULL
        ))
      }
      })
  

    create_deg_list <- reactiveValues()
    seurat_data <- reactiveValues()
    marker_use_part <- reactiveValues()
    
    observe({
      req(input$celltype_online_tool)
      req(input$celltype_online_tool_fate)
   
      obj = create_seurat_object()
      bias_list <- create_fate_bias_list()
      choose_cell = input$celltype_online_tool
      choose_fate_bias_num = input$celltype_online_tool_fate
      
      data = obj@meta.data[,c('barcodes','celltype')]
      data <-clean_pbmc_data(data)
      com<- intersect(rownames(obj@meta.data), rownames(data))
      obj <- subset(obj, cells = com)
      obj@meta.data$barcodes = as.character(obj@meta.data$barcodes)
      obj@meta.data$celltype = as.character(obj@meta.data$celltype)
      obj <- ScaleData(obj, assay = "RNA", features = rownames(obj))
      
      metadata = obj@meta.data
      sig_fate = c()
      all_ct = unique(metadata$celltype)
      ### retina sig
      for (j in 1:length(bias_list)) {
        fate = bias_list[[j]]
        if ("fdr" %in% names(fate)) {
          fate = fate[fate$fdr < 0.05,]
        }
        if ("fate_ratio" %in% names(fate)) {
          fate = fate[fate$fate_ratio>0.1,]
        }
        if (nrow(fate)>0) {
          bias_list[[names(bias_list)[j]]] = fate
          sig_fate = c(sig_fate,names(bias_list)[j])
        }
      }
      
      ### all cell fate combinations
      marker_list = list()
      combinations <- combn(sig_fate, 2)
      
      j = choose_fate_bias_num
      j <- as.numeric(j)
      
        fate1 = bias_list[[ combinations[1,j] ]]$clone_name
        fate2 = bias_list[[combinations[2,j]]]$clone_name
        fate1_specific = fate1[!fate1 %in% fate2]
        fate2_specific = fate2[!fate2 %in% fate1]
       
        k = choose_cell
          ct_meta = metadata[metadata$celltype==k,]
          fate1_cell = rownames(ct_meta[ct_meta$barcodes %in% fate1_specific,])
          fate2_cell = rownames(ct_meta[ct_meta$barcodes %in% fate2_specific,])
          if (length(fate2_cell)>2 & length(fate1_cell)>2) {
            data = subset(obj,cells = c(fate1_cell,fate2_cell))
            data$lineage = ifelse(colnames(data) %in% fate1_cell,combinations[1,j],
                                  combinations[2,j])
            data@active.ident = as.factor(data$lineage)
            lineages <- unique(data@meta.data$lineage)
            suppressWarnings(suppressMessages({
              marker <- FindMarkers(data, ident.1 = lineages[1], ident.2 = lineages[2],only.pos = FALSE, test.use = 'wilcox',
                                    p_val_cutoff = 1,min.pct = 0.05, logfc.threshold = 0)
            }))
            
            marker <- marker %>% 
              mutate(cluster = ifelse(avg_log2FC > 0, lineages[1], lineages[2]))
            marker$cluster=as.factor(marker$cluster)
            marker$gene=rownames(marker)
            if (nrow(marker)>0){
              marker_use = c()
              marker = marker[order(marker$p_val_adj),]
              index = paste(combinations[2,j],'bias','VS',combinations[1,j],
                            'bias','-',k)
              marker$index = index
              marker_list[[index]] = marker
              
              for (i in unique(marker$cluster)) {
                marker_cluster = marker[marker$cluster==i,]
                marker_cluster = marker_cluster[order(marker_cluster$p_val_adj),]
                if (nrow(marker_cluster)>15){
                  marker_use = c(marker_use,marker_cluster$gene[1:15])
                }else{
                  marker_use = c(marker_use,marker_cluster$gene)
                }
              }
              seurat_data$my_var <- data
              marker_use_part$my_var <- marker_use
              
              output$online_tool_heatmap_plot <- renderPlot({
                data <- seurat_data$my_var
                marker_use<- marker_use_part$my_var
                
                heatmap <- DoHeatmap(data, features = marker_use,draw.lines = T,label = F, group.colors = c("#D95319", "#3399FF"))+
                      scale_fill_gradientn(colours=viridis::viridis(100))
                print(heatmap)
              })
              
              marker_use = c()
              
              create_deg_list$my_var <- marker
              }else{
                output$online_tool_heatmap_plot <- renderPlot({
                  ggplot() +
                    geom_text(aes(x = 0.5, y = 0.5), label = "Oops!\nNo significant DEGs in this dataset", 
                              size = 10, color = "black", family = "Helvetica", fontface = "bold", hjust = 0.5, vjust = 0.5) +
                    theme_void() +  # 移除所有图形元素，只显示文字
                    theme(
                      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
                      plot.margin = margin(0, 0, 0, 0, "cm")
                    ) +
                    ggtitle("")  # 移除默认标题
                })
                
                create_deg_list$my_var <- NULL
            }
          }else{ 
            output$online_tool_heatmap_plot <- renderPlot({
              ggplot() +
                geom_text(aes(x = 0.5, y = 0.5), label = "Oops!\nNo significant DEGs in this dataset", 
                          size = 10, color = "black", family = "Helvetica", fontface = "bold", hjust = 0.5, vjust = 0.5) +
                theme_void() +  # 移除所有图形元素，只显示文字
                theme(
                  plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
                  plot.margin = margin(0, 0, 0, 0, "cm")
                ) +
                ggtitle("")  # 移除默认标题
            })
            
            create_deg_list$my_var <- NULL }
    })

   
    errorOccurred_fate_bias_online_tool <- reactiveVal(FALSE)
    
    output$online_tool_volcanoPlot <- renderPlotly({
      deg_rds <- create_deg_list$my_var
      
      if (is.null(deg_rds)) {
        errorOccurred_fate_bias_online_tool(TRUE)
        return(NULL)  
      } else {
        errorOccurred_fate_bias_online_tool(FALSE)
    
        deg_data <- deg_rds     
        
        deg_data <- deg_data %>%
          arrange(p_val) %>%
          slice_head(n = 5000)
        
        deg_data$avg_log2FC <- as.numeric(deg_data$avg_log2FC)
        
        deg_data$highlight <- ifelse(deg_data$p_val < input$online_tool_pvalThreshold & abs(deg_data$avg_log2FC) > input$online_tool_log2foldchange, "significant", "Insignificant genes")
        deg_data$highlight <- ifelse( deg_data$highlight == "significant", paste0(deg_data$cluster, " up-regulated genes"), deg_data$highlight )
        deg_data$highlight <- str_wrap(deg_data$highlight, width = 20)
        
        unique_up_regulated_highlights <- unique(deg_data$highlight[grepl("up-regulated", deg_data$highlight)])
        color_map <- setNames(rep("grey", length(unique(deg_data$highlight))), unique(deg_data$highlight))
        color_map["Insignificant genes"] <- "grey"
        if (length(unique_up_regulated_highlights) >= 1) {
          color_map[unique_up_regulated_highlights[1]] <- rgb(51/255,153/255,255/255) }
        if (length(unique_up_regulated_highlights) >= 2) {
          color_map[unique_up_regulated_highlights[2]] <- rgb(217/255,83/255,25/255)   }
        
        p <- ggplot(deg_data, aes(x = avg_log2FC, y = -log10(p_val), color = highlight, size = -log10(p_val),customdata = gene,
                                  text = paste('Cluster:', cluster , '<br>Gene:', gene, '<br>Log2FC:', avg_log2FC, '<br>P-value:', p_val))) +
          geom_point(alpha = 0.8) +
          scale_size_continuous(range = c(1, 2),name = "") +
          scale_color_manual(values = color_map, name = "") + 
          theme_minimal() +
          theme(
            panel.grid.major = element_blank(),  # 移除主要网格线
            panel.grid.minor = element_blank(),  # 移除次要网格线
            axis.line = element_line(color = "black"),  # 保留轴线
            plot.title = element_text(size = 12,hjust = 0.5),         # Sets the size of the plot title
            axis.title = element_text(size = 12),         # Sets the size of the axis titles (both x and y)
            axis.text = element_text(size = 12), 
            legend.text = element_text(size = 10),        # Sets the size of the text in the legend
            #  legend.title = element_text(size = 16),
            legend.key.height=unit(2, "cm"))+
          labs(x = "Log2 Fold Change", y = "-Log10 p-value", title = unique(deg_data$index))
        
        
        #ggsave(filename = '/data/yexing/scLT/www/111aaa_vol.pdf', plot = p, device = "pdf", width = 14, height = 12, units = "in", dpi = 100, limitsize = FALSE)
       
        
        plotly_p <- ggplotly(p, tooltip = "text") %>%
          layout(hovermode = 'closest')
        plotly_p   } })     
    
    
    output$imageOutputOrPreset_online_tool_fate_bias <- renderUI({
      if (errorOccurred_fate_bias_online_tool()) {
        tagList( img(src = "Warning_DEG.png", alt = "Error Image",width = "100%", height = "100%" )) 
        } else {
          list( fluidRow( column(6, sliderInput("online_tool_pvalThreshold", "P-value Threshold", min = 0, max = 0.05, value = 0.05) ),
                          column(6, sliderInput("online_tool_log2foldchange", "Log2 Fold Change", min = 0.25, max = 1, value = 0.25) )), 
                div(style = " height: 65vh;" ,
                    plotlyOutput("online_tool_volcanoPlot", height = "30vw") ) ) } })
    
    observeEvent(c(input$celltype_online_tool,input$celltype_online_tool_fate), {
      errorOccurred_fate_bias_online_tool(FALSE) })
    
    
    
    # 设置clone统计函数
   
      output$clone_size_tools <- renderGirafe({
        pbmc = upload_metadata()
        
        if (!("celltype" %in% colnames(pbmc) && "barcodes" %in% colnames(pbmc))) {
          # 如果缺少这两列，则将行名作为barcodes列，列名作为celltype列
          barcodes <- rownames(pbmc)
          celltypes <- colnames(pbmc)
          
          # 将所有元素等价转换为一对一形式
          pbmc_converted <- data.frame(
            barcodes = rep(barcodes, each = length(celltypes)),
            celltype = rep(celltypes, times = length(barcodes))
          )
        } else {
          pbmc_converted <- pbmc
        }
        
        pbmc_converted = clean_pbmc_data(pbmc_converted)
        qq_1 = unique(as.character(pbmc_converted$barcodes))
        clone_fate_bias = NULL
        create_interactive_bar_plot_tool(pbmc_converted , qq_1, clone_fate_bias ) }) 
     
    
    # 展示克隆热图
    output$cloneprofile_tools <- renderPlotly({
      cell_frame <- upload_metadata()
      change_normalize_method_1 =input$change_normalize_method
      celltype_clone_data <- clone_heatmap(cell_frame, input_type_clone = file_type )
     # sorted <-  input$tool_celltype_clone_sorted_cells
     # new_ordered_data <- celltype_clone_data[,sorted]
      heatmaply((celltype_clone_data), color = col,show_dendrogram = c(F,T),showticklabels =c(T,F),Rowv= FALSE ,Colv =FALSE) })
    
    # 设置谱系关系热图排序函数
    # output$tool_celltype_clone_applysort_2 <- renderUI({
    #   cell_frame <- upload_metadata()
    #   celltype_clone_data <- cell_type_fate_similartiy(cell_frame, input_type = file_type )
    #   rank_list(
    #     text = "Drag the cell name to where you want it: ",
    #     labels = rownames( celltype_clone_data ) ,
    #     input_id = "tool_celltype_clone_sorted_cells_2") })
    
    # 设置谱系关系热图展示
    output$cell_type_fate_similartiy_tools <- renderPlotly({
      cell_frame <- upload_metadata()
      celltype_clone_data <- cell_type_fate_similartiy(cell_frame, input_type = file_type )
    #  sorted <-  input$tool_celltype_clone_sorted_cells_2
     # new_ordered_data <- celltype_clone_data[sorted,sorted]
      heatmaply((celltype_clone_data), color = col,show_dendrogram = c(F,T),showticklabels =c(T,F),Rowv= FALSE ,Colv =FALSE) })
    
    
    #画clone_fate_bias列表
    output$fate_bias_summary_tools <- renderUI({
      cell_fate=upload_metadata()
      if("barcodes" %in% names(cell_fate) && "celltype" %in% names(cell_fate)) {
        cell_fate <- cell_fate %>%
          dplyr::select(barcodes, celltype)
        unique_values <- unique(cell_fate[, "celltype"])
      } else {
        unique_values <- names(cell_fate)
      }
      bias_list <- setNames(lapply(unique_values, function(j) {
        ct_fate_bias <- clone_fate_bias(cell_fate, fate_use = j, data_type = file_type)
        ct_fate_bias_fdr <- subset(ct_fate_bias, fdr < 0.05)
        return(ct_fate_bias_fdr)
      }), unique_values)
      combined_df <- bind_rows(bias_list)
      #combined_df$fate_ratio <- round(as.numeric(combined_df$fate_ratio),4)
      combined_df$pvalue <- `if`("pvalue" %in% names(combined_df), round(as.numeric(combined_df$pvalue), 4), 'None')
      combined_df$fdr <- `if`("fdr" %in% names(combined_df), round(as.numeric(combined_df$fdr), 4), 'None')
      combined_df$clone_size_rank <- `if`("clone_size_rank" %in% names(combined_df), combined_df$clone_size_rank, 'None')
      data <- combined_df[,-c(4, 5, 7)]
      
      if (length(rownames(combined_df))==0) {
        tagList(
          img(src = "Warning.png", alt = "Error Image",width = "100%", height = "100%")
        )} else {
          renderDT(
            data,
            extensions = c('Select', 'SearchPanes', "Buttons"),
            colnames = c("clone_name", "fate_use", "clone_size", "fdr"),
            selection = 'none',
            server = FALSE,
            options = list(
              dom = 'Bftip',
              columnDefs = list(
                list(searchPanes = list(show = FALSE), targets = c(1, 3, 4)),
                list(targets = c(1:4), className = 'dt-center')
              ),
              buttons = list('searchPanes'),
              language = list(searchPanes = list(collapse = 'Select fate bias')),
              pageLength = 10,
              searchHighlight = TRUE,
              lengthChange = FALSE
            )
          )
        }
    })
    
    #clone_fate_bias列表下载----
    output$downloadData <- downloadHandler(
      filename = function() {
        paste("data-", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        cell_fate <- upload_metadata()
        if("barcodes" %in% names(cell_fate) && "celltype" %in% names(cell_fate)) {
          unique_values <- unique(cell_fate[, "celltype"])
        } else {
          unique_values <- names(cell_fate)
        }
        
        bias_list <- lapply(unique_values, function(j) {
          ct_fate_bias <- clone_fate_bias(as.data.frame(cell_fate),fate_use = j,data_type = file_type )
          ct_fate_bias_fdr <- subset(ct_fate_bias, fdr < 0.05)
          return(ct_fate_bias_fdr)
        })
        
        combined_df <- bind_rows(bias_list)
        
        combined_df <- combined_df %>% select(-fate_ratio)
        
        write.csv(combined_df, file, row.names = FALSE)
      }
    )
    #----
    #kyh functions
    data <- reactive({
      req(input$file1)  
      readRDS(input$file1$datapath)  
    })
    
    output$plot <- renderPlot({
      req(data())  
      
      
      plot_tree(data(), input$param1, input$param2)
    })
    }
