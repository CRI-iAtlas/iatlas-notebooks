
### Notebook functions ###

                            
                                
list_iatlas_features <- function(){
    iatlas_fmx <- feather::read_feather('data/fmx_df.feather')
    table(colnames(iatlas_fmx))
}

                                
list_iatlas_feature_sets <- function(){
  feature_df <- feather::read_feather('data/feature_df.feather')
  table(feature_df$`Variable Class`)
}
                                


se <- function(x){
  mean(x) / sqrt(length(x))
}


assert_df_has_rows <- function(df){
  if(nrow(df) == 0){
    stop("result df is empty")
  }
}


assert_df_has_columns <- function(df, columns){
  missing_columns <- columns[!columns %in% colnames(df)]
  if(length(missing_columns) != 0){
    stop("df has missing columns: ",
         stringr::str_c(missing_columns, collapse = ", "))
  }
}


summarise_df_at_column <- function(df, column, grouping_columns, function_names){
  assert_df_has_columns(df, c(column, grouping_columns))
  result_df <- df %>% 
    dplyr::group_by_at(vars(dplyr::one_of(grouping_columns))) %>%
    dplyr::summarise_at(column, .funs = function_names) %>%
    dplyr::ungroup() 
  if(length(function_names) == 1){
    result_df <- dplyr::rename(result_df, !!function_names := column)
  }
  assert_df_has_columns(result_df, c(grouping_columns, function_names))
  assert_df_has_rows(result_df)
  return(result_df)
}



create_label <- function(
  df,
  value_columns,
  title = "ParticipantBarcode",
  name_column = "name",
  group_column = "group") {
  
  result_df <- wrapr::let(
    alias = c(
      namevar = name_column,
      groupvar = group_column),
    df %>%
      dplyr::mutate(
        label = stringr::str_glue(
          "<b>{title}:</b> {name} ({group})",
          title = title,
          name = namevar,
          group = groupvar
        )) %>% 
      tidyr::gather(value_name, value, dplyr::one_of(value_columns)) %>%
      dplyr::mutate(
        value_label = stringr::str_glue(
          "{name}: {value}",
          name = stringr::str_to_upper(value_name),
          value = sprintf("%0.3f", value)
        )
      ) %>%
      dplyr::group_by(label) %>%
      dplyr::mutate(value_label = stringr::str_c(value_label, collapse = "</br>")) %>%
      ungroup() %>%
      tidyr::spread(value_name, value) %>%
      tidyr::unite(label, label, value_label, sep = "</br></br>")
  )
  assert_df_has_columns(result_df, c("label", name_column, group_column, value_columns))
  assert_df_has_rows(result_df)
  return(result_df)
  
}


notebook_corr <- function(response_df, var_df, method = 'spearman') {
  
  # need to have GROUP in response_df
  shared_ids <- unique(response_df$ParticipantBarcode, var_df$ParticipantBarcode)
  res2_df <- response_df[match(response_df$ParticipantBarcode, table = shared_ids), ]
  var2_df <- var_df[match(var_df$ParticipantBarcode, table = shared_ids), ]
  
  
  res_list <- list()
  # for each group in response_df
  for (gi in unique(res2_df$GROUP)) {
    yi <- res2_df %>% dplyr::filter(GROUP == gi) %>% dplyr::select(value)
    zi <- var2_df %>% dplyr::filter(GROUP == gi) %>% dplyr::select(-GROUP, -ParticipantBarcode)
    res_list[[gi]] <- cor(yi, zi, use = 'pairwise.complete.obs', method = method)  
  }
  
  res_mat <- as.matrix(do.call('rbind', res_list))
  rownames(res_mat) <- names(res_list)
  
  return(res_mat)
}


build_cellcontent_barplot_df2 <- function(df, x_column, y_column, sort_by_var_choice, reorder_func_choice) {
  
  #assert_df_has_columns(df, c("GROUP", "fraction_type", "fraction"))
  
  #
  # Here we reorder the bars. Max = mean+error, Min = mean-error
  #
  
  reorder_function2 <- function(result_df, sort_by_var_choice, reorder_func_choice) {
    if (reorder_func_choice == 'Mean') {
      x_levels <- result_df %>% 
        dplyr::filter(color == sort_by_var_choice) %>% 
        dplyr::arrange(y) %>% 
        dplyr::pull(x)
    } else if (reorder_func_choice == 'Max') {
      x_levels <- result_df %>% 
        dplyr::filter(color == sort_by_var_choice) %>% 
        dplyr::arrange(y+error) %>% 
        dplyr::pull(x)
    } else if (reorder_func_choice == 'Min') {
      x_levels <- result_df %>% 
        dplyr::filter(color == sort_by_var_choice) %>% 
        dplyr::arrange(y-error) %>% 
        dplyr::pull(x)
    }
    result_df$x <- factor(result_df$x, levels=x_levels)
    result_df
  }
  
  # sort_by_var_choice is labeled 'color' in the result_df
  result_df <- df %>%
    summarise_df_at_column(
      column = "fraction",
      grouping_columns = c("GROUP", "fraction_type"),
      function_names = c("mean", "se")) %>% 
    create_label(
      title = stringr::str_to_title(y_column),
      name_column = x_column,
      group_column = "GROUP",
      value_columns = c("mean", "se")) %>% 
    dplyr::select(
      x = "GROUP",
      y = "mean",
      color = "fraction_type",
      error = "se",
      label) 
  
  if (sort_by_var_choice != 'Group' & reorder_func_choice != 'None') {
    # then we want to sort by something other than the group labels
    result_df <- reorder_function2(result_df, sort_by_var_choice, reorder_func_choice)
  }
  assert_df_has_columns(result_df, c("x", "y", "label", "color", "error"))
  assert_df_has_rows(result_df)
  return(result_df)
}



build_distribution_plot_df2 <- function(
  df, 
  ysel,
  scale_func_choice = "None",
  reorder_choice = "None"
){
  
  scale_function <- switch(
    scale_func_choice,
    "None" = identity, 
    "Log2" = log2,
    "Log2 + 1" = function(x) log2(x + 1),
    "Log10" = log10,
    "Log10 + 1" = function(x) log10(x + 1),
  )
  
  reorder_function <- function(reorder_choice, x,y) {
    x <- factor(x)
    switch(
      reorder_choice,
      "None" = x,
      "Median" = forcats::fct_reorder(x, y, median, na.rm=T),
      "Mean" = forcats::fct_reorder(x, y, mean, na.rm=T),
      "Max" = forcats::fct_reorder(x, y, max, na.rm=T),
      "Min" = forcats::fct_reorder(x, y, min, na.rm=T)
    )
  }
  
  colnames(df) <- c('x', 'label', 'y')
  
  filter_df <- df[df$label == ysel,]
  
  df2 <- filter_df %>% 
    dplyr::select(x, y, label) %>% 
    tidyr::drop_na() %>% 
    dplyr::mutate(y = scale_function(y)) %>% 
    dplyr::mutate(x = reorder_function(reorder_choice,x,y)) %>%
    tidyr::drop_na() %>% 
    dplyr::filter(!is.infinite(y))
  
  return(df2)
  
}



get_iatlas_feature <- function(
  group = 'Subtype_Immune_Model_Based',  ## Subtype_Immune_Model_Based or Study
  group_filter='None',                   ## List of group IDs we want like KICH 
  group_mod = 'iAtlas_',
  feature_name = 'leukocyte_fraction',
  return_barcodes = F,
  fraction_format = F
  ){
  
  # read in the "feature matrix" (fmx)
  iatlas_fmx <- feather::read_feather('data/fmx_df.feather')
  
  if (return_barcodes) {
    group_fmx <- iatlas_fmx %>% select(!!group, !!feature_name, ParticipantBarcode)
  } else {
    # let's just get the columns we're interested in
    group_fmx <- iatlas_fmx %>% select(!!group, !!feature_name)
  }
  # let's filter out rows we don't want.
  group_fmx2 <- group_fmx[ group_fmx[[group]] %in% group_filter, ]
  
  # then we'll annotate the iatlas labels.
  group_fmx2[[group]] <- sapply(group_fmx2[[group]], function(x) paste0(group_mod,x))
  
  # rename the columns
  if (return_barcodes) {
    group_fmx3 <- data.frame(GROUP=group_fmx2[[group]],
                             ParticipantBarcode = group_fmx2$ParticipantBarcode,
                             feature_name=feature_name,
                             value=group_fmx2[[feature_name]]
                             )
  } else {
    group_fmx3 <- data.frame(GROUP=group_fmx2[[group]],
                             feature_name=feature_name,
                             value=group_fmx2[[feature_name]])
  }
  
  if (fraction_format) {
    colnames(group_fmx3) <- c('GROUP', 'fraction_type', 'fraction')
  }
  
  return(group_fmx3)
}


                                
# return a dataset with group, barcode, and columns of vars
get_iatlas_feature_set <- function(group='Study', 
                                    group_filter=c('KICH', 'KIRC'),
                                    group_mod = 'iAtlas_',
                                    feature_set = 'T Helper Cell Score',
                                    return_barcodes = T,
                                    format = 'wide') {
  
  # read in the "feature matrix" (fmx)
  iatlas_fmx <- feather::read_feather('data/fmx_df.feather')
  
  # read in the feature sets
  feature_df <- feather::read_feather('data/feature_df.feather')

  
  feature_names <- feature_df %>% 
    dplyr::filter(`Variable Class` == !!feature_set) %>%
    dplyr::select(FeatureMatrixLabelTSV) %>% 
    pull()
  
  if (return_barcodes) {
    # first get the columns we need
    group_fmx <- iatlas_fmx %>% select(!!group, ParticipantBarcode, !!feature_names)
  } else {
    # let's just get the columns we're interested in
    group_fmx <- iatlas_fmx %>% select(!!group, ParticipantBarcode, !!feature_name, !!feature_names)
  }
  
  # let's filter out rows we don't want.
  group_fmx2 <- group_fmx[ group_fmx[[group]] %in% group_filter, ]
  # then we'll annotate the iatlas labels.
  group_fmx2[[group]] <- sapply(group_fmx2[[group]], function(x) paste0(group_mod,x))
                                
  colnames(group_fmx2)[1] <- 'GROUP'
  return(group_fmx2)
}



get_iatlas_survial_feature <- function(
  group = 'Study',  ## Subtype_Immune_Model_Based or Study
  group_filter='STAD',                   ## List of group IDs we want like KICH 
  group_mod = 'iAtlas_',
  feature_name = 'leukocyte_fraction',
  survival_feature = 'OS',  # 'OS', 'OS_time', 'PFI_1', 'PFI_time_1'
  na_rm = TRUE,
  return_barcodes = F
  ){
  
  # read in the "feature matrix" (fmx)
  iatlas_fmx <- feather::read_feather('data/fmx_df.feather')
    
  # let's filter out rows we don't want.
  group_fmx <- iatlas_fmx[ iatlas_fmx[[group]] %in% group_filter, ]

  # let's just get the columns we're interested in
  if (survival_feature == 'OS' & return_barcodes == FALSE) {
      group_fmx2 <- group_fmx %>% select(!!feature_name, 'OS_time', 'OS')
  } else if (survival_feature == 'PFI' & return_barcodes == FALSE) {
      group_fmx2 <- group_fmx %>% select(!!feature_name, 'PFI_time_1', 'PFI_1')
  } else if (survival_feature == 'OS' & return_barcodes == TRUE) {
      group_fmx2 <- group_fmx %>% select(ParticipantBarcode, !!feature_name, 'OS_time', 'OS')
  } else if (survival_feature == 'PFI' & return_barcodes == TRUE) {
      group_fmx2 <- group_fmx %>% select(ParticipantBarcode, !!feature_name, 'PFI_time_1', 'PFI_1')
  }
  
  # rename the columns
  colnames(group_fmx2) <- c('Variable', 'Time', 'Status')
    
  if (na_rm) {
      group_fmx2 <- na.omit(group_fmx2)
  }
    
  return(group_fmx2)
}

                                
get_iatlas_survial_feature_set <- function(
  group = 'Study',  ## Subtype_Immune_Model_Based or Study
  group_filter='STAD',                   ## List of group IDs we want like KICH 
  group_mod = 'iAtlas_',
  feature_name = 'Subtype_Immune_Model_Based',
  feature_set = 'T Helper Cell Score',
  survival_feature = 'OS',  # 'OS', 'OS_time', 'PFI_1', 'PFI_time_1'
  na_rm = TRUE,
  return_barcodes = F
  ){
  
  # read in the "feature matrix" (fmx)
  iatlas_fmx <- feather::read_feather('data/fmx_df.feather')
      
  # read in the feature sets
  feature_df <- feather::read_feather('data/feature_df.feather')
    
  feature_names <- feature_df %>% 
    dplyr::filter(`Variable Class` == !!feature_set) %>%
    dplyr::select(FeatureMatrixLabelTSV) %>% 
    pull()
      
  # let's filter out rows we don't want.
  group_fmx <- iatlas_fmx[ iatlas_fmx[[group]] %in% group_filter, ]
    
  # let's just get the columns we're interested in
  if (survival_feature == 'OS' & return_barcodes == FALSE) {
      group_fmx2 <- group_fmx %>% select(!!feature_name, 'OS_time', 'OS', !!feature_names)
        # rename the columns
      colnames(group_fmx2)[1:3] <- c('Variable', 'Time', 'Status')
  } else if (survival_feature == 'PFI' & return_barcodes == FALSE) {
      group_fmx2 <- group_fmx %>% select(!!feature_name, 'PFI_time_1', 'PFI_1',  !!feature_names)
      # rename the columns
      colnames(group_fmx2)[1:3] <- c('Variable', 'Time', 'Status')
  } else if (survival_feature == 'OS' & return_barcodes == TRUE) {
      group_fmx2 <- group_fmx %>% select(ParticipantBarcode, !!feature_name, 'OS_time', 'OS', !!feature_names)
      # rename the columns
      colnames(group_fmx2)[1:4] <- c('ParticipantBarcode', 'Variable', 'Time', 'Status')
  } else if (survival_feature == 'PFI' & return_barcodes == TRUE) {
      group_fmx2 <- group_fmx %>% select(ParticipantBarcode, !!feature_name, 'PFI_time_1', 'PFI_1', !!feature_names)
      # rename the columns
      colnames(group_fmx2)[1:4] <- c('ParticipantBarcode', 'Variable', 'Time', 'Status')
  }
    

    
  if (na_rm) {
      group_fmx2 <- na.omit(group_fmx2)
  }
    
  return(group_fmx2)
}
                                

                                
get_concordance <- function(
  df, value_column, time_column, status_column
) {
  wrapr::let(
    alias = c(valuevar = value_column,
              timevar = time_column,
              statusvar = status_column),
    mat <- df %>% 
      dplyr::select(valuevar, timevar, statusvar) %>% 
      .[complete.cases(.),] %>% 
      as.data.frame() %>% 
      as.matrix()
  )
  
  concordanceIndex::concordanceIndex(mat[,1], mat[,-1])
}


                                
get_concordance_by_group <- function(
  df, value_columns, time_column, status_column
) {
  value_columns %>% 
    purrr::map(function(f) get_concordance(df, f, time_column, status_column)) %>% 
    magrittr::set_names(value_columns)
}


               
build_ci_mat <- function(
  df, group_column, value_columns, time_column, status_column
) {
  
  # read in the feature sets
  feature_df <- feather::read_feather('data/feature_df.feather')
    
  #value_names <- purrr::map(value_columns, get_variable_display_name)
  #value_names <- feature_df %>% 
  #  dplyr::filter(`Variable Class` == !!feature_set) %>%
  #  dplyr::select(FriendlyLabel) %>% 
  #  pull()
    
  group_v <- magrittr::extract2(df, group_column) 
  groups <- group_v %>% 
    unique() %>% 
    purrr::discard(is.na(.)) %>% 
    sort()
  
  df %>% 
    split(group_v) %>% 
    purrr::map(get_concordance_by_group, value_columns, time_column, status_column) %>% 
    unlist() %>% 
    unname() %>% 
    matrix(ncol = length(groups)) %>%
    magrittr::set_rownames(value_columns) %>% ## change value_columns to value_names
    magrittr::set_colnames(groups)
}
               

               
               

build_survival_df <- function(df, var_column, time_column, status_column=NULL, div_method=NULL, k, 
                              group_choice = NULL, group_subset = NULL) {
  
  # subset to a smaller group of samples #
  if(!is.null(group_choice)){
    if (group_choice == 'Study' & group_subset != 'All') {      
      df <- df  %>% dplyr::filter(Study == UQ(group_subset))
    } 
    else if (group_choice == 'Subtype_Immune_Model_Based' & group_subset != 'All') {  
      df <- df %>% dplyr::filter(Subtype_Immune_Model_Based == UQ(group_subset))
    } 
    else if (group_choice == 'Subtype_Curated_Malta_Noushmehr_et_al' & group_subset != 'All') {  
      df <- df %>% dplyr::filter(Subtype_Curated_Malta_Noushmehr_et_al == UQ(group_subset))
    }
  }
  
  get_groups <- function(df, div_method, var_column, k) {
    if (is.null(div_method) | div_method == 'group' | div_method =='Group') {
      as.character(df[[var_column]])
    }    
    else  if (div_method == 'median') {      
        as.character( ifelse (df[[var_column]] < median(df[[var_column]], na.rm=T), 
                                yes='lower half', no='upper half') )
    }
    else if (div_method == 'cut') {
          as.character(cut(df[[var_column]], k, ordered_result = T))
    }
  }

  # get the vectors associated with each term
  # if facet_column is already a catagory, just use that.
  # otherwise it needs to be divided into k catagories.
  groups <- get_groups(df, div_method, var_column, k)
  
  data.frame(
    status = purrr::pluck(df, status_column), 
    time = purrr::pluck(df, time_column),
    group = groups,
    variable = groups, 
    measure = purrr::pluck(df, var_column)
  ) %>% 
    na.omit()
}



               


############## PLOT FUNCTIONS ###################

notebook_barplot <- function(
  df,
  sort_by_var_choice,
  reorder_func_choice,
  show_error_bars,
  ylab, 
  xlab,
  title,
  width=600,
  height=400
) {

  # fix it if written like normal human
  if (colnames(df)[1] == 'Group') {
    colnames(df)[1] <- 'GROUP'
  }
   
  # need columns to have specific names. 
  if (! all(colnames(df) %in% c('GROUP', 'fraction_type', 'fraction')) ){
    print('error: please use GROUP, fraction_type, fraction as column names for df')
    return()
  }
  
  # check parameters
  if (show_error_bars == TRUE) {
    show_error_bars = 'error'
  } else if (show_error_bars == FALSE) {
    show_error_bars <- NA
  } else if (show_error_bars == 'error') {
    show_error_bars <- 'error'  # same
  } else {
    print('error: show_error_bars must take values TRUE, FALSE')
    return()
  }
  
  
  if (sort_by_var_choice != 'Group' & (!sort_by_var_choice %in% df$fraction_type)) { 
    print('error: sort_by_var_choice must be Group or a selection in fraction_type')  
    return()
  }
  

  barplot_df <- build_cellcontent_barplot_df2( # could use better name
    df,
    x_column = "fraction_type",
    y_column = "fraction",
    sort_by_var_choice = sort_by_var_choice, 
    reorder_func_choice = reorder_func_choice
  )
  
  embed_notebook(
    iatlas.modules::plotly_bar(
      plot_data = barplot_df, 
      x_col = 'x', 
      y_col = 'y', 
      error_col = show_error_bars,   # 'rror' or NA
      color_col = 'color', 
      ylab = ylab, 
      xlab = xlab,
      title = title),
      width,
      height
  )  
}


notebook_violinplot <- function(
  data,
  y_col,
  scale_func_choice = 'None',
  reorder_func_choice = 'None',
  fill_colors = NULL,
  points = NULL,
  showlegend = T,
  ylab = '', 
  xlab = '',
  title = '',
  width=600,
  height=400
) {

  # fix it if written like normal human
  if (colnames(data)[1] == 'Group') {
    colnames(data)[1] <- 'GROUP'
  }
  
  # need columns to have specific names. 
  if (! all(colnames(data) %in% c('GROUP', 'fraction_type', 'fraction')) ){
    print('error: please use GROUP, fraction_type, fraction as column names for df')
    return()
  }
  
  #  ## check for correct function names #
  # if (sort_by_var_choice != 'Group' & (!sort_by_var_choice %in% df$fraction_type)) { 
  #  print('error: sort_by_var_choice must be Group or a selection in fraction_type')  
  #  return()
  #}
  
  violin_df <- build_distribution_plot_df2(data,
                                          ysel=y_col,
                                          scale_func_choice = scale_func_choice,
                                          reorder_choice = reorder_func_choice)

  embed_notebook(
      iatlas.modules::plotly_violin(
        plot_data = violin_df, 
        x_col = 'x', 
        y_col = 'y',
        fill_colors=fill_colors,
        points=points,
        showlegend=showlegend,
        ylab = ylab, 
        xlab = xlab,
        title = title
        ),
        width,
        height
    )  
}



notebook_boxplot <- function(
  data,
  y_col,
  scale_func_choice = 'None',
  reorder_func_choice = 'None',
  fill_colors = NULL,
  ylab = '', 
  xlab = '',
  title = '',
  width=600,
  height=400
  ) {
  
  # fix it if written like normal human
  if (colnames(data)[1] == 'Group' | colnames(data)[1] == 'group') {
    colnames(data)[1] <- 'GROUP'
  }
  
  # need columns to have specific names. 
  if (! all(colnames(data) %in% c('GROUP', 'fraction_type', 'fraction')) ){
    print('error: please use GROUP, fraction_type, fraction as column names for df')
    return()
  }
  
  # check for function names and reorder choices
  
  box_df <- build_distribution_plot_df2(data,
                                        ysel=y_col,
                                        scale_func_choice = scale_func_choice,
                                        reorder_choice = reorder_func_choice)
  embed_notebook(
      iatlas.modules::plotly_box(
        plot_data = box_df, 
        x_col = 'x', 
        y_col = 'y',
        fill_colors=fill_colors,
        ylab = ylab, 
        xlab = xlab,
        title = title
      ),
      width,
      height
  )
}



notebook_heatmap <- function(corr_mat, 
                           title = '', 
                           scale_colors = F,  
                           legend_title = NULL,
                           source_name = '',
                           width=500,
                           height=500){
  zmin <- NULL
  zmax <- NULL
  if(scale_colors){
    extreme <- max(abs(min(corr_mat)),
                   abs(max(corr_mat)))
    zmax <- extreme
    zmin <- -extreme
  }

  p <-
    plotly::plot_ly(
      z = corr_mat,
      x = colnames(corr_mat),
      y = rownames(corr_mat),
      type = "heatmap",
      source = source_name,
      colors = rev(RColorBrewer::brewer.pal(8, "RdBu")),
      colorbar = list(title = legend_title),
      zmin = zmin,
      zmax = zmax
    ) %>%
    plotly::layout(
      title = title,
      xaxis = list(tickangle = 90)
    )
  embed_notebook(
    p,
    width,
    height
  )
}


               
notebook_kmplot <- function(df, 
                            div_method,
                            k=2,
                            confint =FALSE, 
                            risktable =FALSE, 
                            title='', 
                            subtitle = NULL, 
                            group_colors, 
                            facet = FALSE,
                            width=6,
                            height=5) {
  

  if(!is.null(subtitle)){
    long_title <- paste0(title, '\n', subtitle)
  }else{
    long_title <- title
  }
  
  # build the survival dataframe
  survival_df <- build_survival_df(df=df, var_column = 'Variable', time_column = 'Time', 
                                   status_column='Status', div_method = div_method, k=k) 
    
  # then we'll fit the survival model
  fit <- survival::survfit(survival::Surv(time, status) ~ variable, data = survival_df)
 
  if(facet == FALSE){
    survminer::ggsurvplot(
      fit,
      data = survival_df,
      conf.int = confint,
      risk.table = risktable,
      title = long_title,
      palette = group_colors
    )
    
  }else{
    survminer::ggsurvplot_list(
      fit,
      data = survival_df,
      pval = TRUE,
      pval.method = TRUE,
      conf.int = confint,
      risk.table = risktable,
      title = long_title,
      palette = group_colors
    )
  }
  
}

               
notebook_concordance <- function(
                     df,
                     value_columns,
                     title = '', 
                     scale_colors = F,  
                     legend_title = NULL,
                     width=500,
                     height=500,
                     group_column='Variable',
                     time_column ='Time', 
                     status_column ='Status') {
    
    
    df <- df %>% dplyr::select('Variable', 'Time', 'Status', !!value_columns)
    
    ci_mat <- build_ci_mat(    
                  df=df, 
                  group_column ='Variable', 
                  value_columns = value_columns, ## get friendly term function wanted here
                  time_column ='Time', 
                  status_column ='Status') 
    
    notebook_heatmap(ci_mat,
                     title = title, 
                     scale_colors = F,  
                     legend_title = legend_title,
                     width=500,
                     height=500)
}
