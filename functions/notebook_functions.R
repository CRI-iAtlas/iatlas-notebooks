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
