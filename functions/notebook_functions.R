
### Notebook functions ###

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


list_iatlas_feature_sets <- function(){
  feature_df <- feather::read_feather('data/feature_df.feather')
  table(feature_df$`Variable Class`)
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



notebook_barplot <- function(
  df,
  sort_by_var_choice,
  reorder_func_choice,
  show_error_bars,
  ylab, 
  xlab,
  title
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
  
  iatlas.modules::plotly_bar(
    plot_data = barplot_df, 
    x_col = 'x', 
    y_col = 'y', 
    error_col = show_error_bars,   # 'rror' or NA
    color_col = 'color', 
    ylab = ylab, 
    xlab = xlab,
    title = title)
  
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
  title = ''
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
  title = ''
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
  
  iatlas.modules::plotly_box(
    plot_data = box_df, 
    x_col = 'x', 
    y_col = 'y',
    fill_colors=fill_colors,
    ylab = ylab, 
    xlab = xlab,
    title = title
  )
  
}


############
# ############
# 
# build_immunefeatures_df <- function(
#   df,
#   group_column,
#   value1_column,
#   value2_columns,
#   group_options,
#   id_column = "ParticipantBarcode"){
#   
#   assert_df_has_columns(
#     df, c(group_column, value1_column, value2_columns, id_column))
#   
#   result_df <- df %>%
#     dplyr::select(
#       ID = id_column,
#       GROUP = group_column,
#       VALUE1 = value1_column,
#       value2_columns) %>%
#     dplyr::filter(GROUP %in% group_options) %>%
#     dplyr::filter(!is.na(VALUE1))
#   return(result_df)
# }
# 
# 
# build_immunefeatures_df(
#   subset_df(),
#   group_column = group_internal_choice(),
#   value1_column = input$heatmap_values,
#   value2_columns = hm_variables(),
#   group_options = sample_groups,
#   id_column = "ParticipantBarcode"
# )
# 
# 
create_heatmap <- function(corr_mat, 
                           title = '', 
                           scale_colors = F,  
                           legend_title = NULL,
                           source_name = ''){
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
  p
}


# 
# data_df <- {
#   dplyr::select(
#     subset_df(),
#     x = group_internal_choice(),
#     label = "ParticipantBarcode",
#     dplyr::everything())
# }
# 
# relationship_df <- {
#   panimmune_data$feature_df %>%
#     dplyr::filter(VariableType == "Numeric") %>%
#     dplyr::select(
#       INTERNAL = FeatureMatrixLabelTSV,
#       DISPLAY = FriendlyLabel,
#       `Variable Class`)
# }
# 
# sample_groups <- get_unique_column_values(
#   group_internal_choice(), 
#   subset_df())
# 
# 
# 
# 
# build_immunefeatures_correlation_matrix <- function(df, method = "spearman") {
#   long_df  <- df %>%
#     dplyr::select(-ID) %>%
#     tidyr::gather(
#       key = "VARIABLE",
#       value = "VALUE2",
#       -c(GROUP, VALUE1)) %>%
#     dplyr::group_by(GROUP, VARIABLE) %>%
#     tidyr::drop_na()
#   
#   if(nrow(long_df) == 0) return(long_df)
#   
#   result_matrix <- long_df %>%
#     dplyr::summarise(COR = cor(
#       VALUE1,
#       VALUE2,
#       method = method,
#       use = "pairwise.complete.obs")) %>%
#     tidyr::spread(key = "GROUP", value = "COR", fill = NA) %>%
#     dplyr::left_join(
#       dplyr::select(
#         panimmune_data$feature_df, 
#         FeatureMatrixLabelTSV, 
#         `Variable Class Order`, 
#         FriendlyLabel),
#       by = c("VARIABLE" = "FeatureMatrixLabelTSV")) %>% 
#     dplyr::arrange(dplyr::desc(`Variable Class Order`)) %>% 
#     dplyr::select(- c(`Variable Class Order`, VARIABLE)) %>% 
#     as.data.frame() %>%
#     tibble::column_to_rownames("FriendlyLabel") %>%
#     as.matrix()
# }
# 
# 
# validate(
#   need(nrow(immunefeatures_df()) > 0, 
#        "Current selected group and selected variable have no overlap")
# )
# 
# immunefeatures_correlation_matrix <- build_immunefeatures_correlation_matrix(
#   immunefeatures_df(), 
#   input$correlation_method)
# 
# 
# create_heatmap(immunefeatures_correlation_matrix, "heatplot", scale_colors = T)
# 
# #########
# #########
# 
# build_immunefeatures_scatter_plot_df <- function(df, x_col, group_filter_value){
#   assert_df_has_columns(df, c(x_col, "VALUE1", "ID", "GROUP"))
#   df %>%
#     select(ID, GROUP, y = "VALUE1", x = x_col) %>%
#     filter(GROUP == group_filter_value) %>%
#     create_label(
#       name_column = "ID",
#       group_column = "GROUP",
#       value_columns = c("x", "y")) %>%
#     select("x", "y", "label") %>%
#     get_complete_df_by_columns(c("x", "y", "label"))
# }

