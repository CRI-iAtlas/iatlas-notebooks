## This function is the iatlas-app code, in
## iatlas-app/R/ici_clinical_outcomes_functions.R
build_survival_df <- function(df, group_column, time_column, filter_df = TRUE,  extra_group_df , extra_group = "None") {

  if(extra_group != "None"){
# In iatlas-app; cohort_obj (a cohort object) is input to this function, obtained using shiny reactive and the code is as follows
#    extra_group_df <- cohort_obj$get_feature_values(features = extra_group) %>%
#      dplyr::select(
#        "sample_name",
#        "value" = "feature_value")
# Here, the derived dataframe extra_group_df is instead the input this
    extra_group_df <- extra_group_df %>%
        dplyr::select(
          "sample_name",
          "value" = "feature_value")

    df <- df %>%
      dplyr::inner_join(extra_group_df, by = "sample_name") %>%
      dplyr::mutate(
        "group" = paste(
          group_name,
          ifelse(value > median(value), "upper half", "lower half"),
          sep = " - "
        )
      ) %>%
      dplyr::select(sample_name, group, dataset_name, feature_name, feature_display, feature_value) %>%
      dplyr::rename(group_name = group)
  }

  if (time_column == "OS_time") {
    time_status <-  "OS"
    if(filter_df == TRUE){
      df <- df %>%
        dplyr::distinct() %>%
        dplyr::filter(feature_name %in% c("OS", "OS_time")) %>%
        dplyr::select(sample_name, group_column, dataset_name, feature_name, feature_value) %>%
        tidyr::pivot_wider(names_from = feature_name, values_from = feature_value)
    }
  } else {
    time_status <-  "PFI_1"
    if(filter_df == TRUE){
      df <- df %>%
        dplyr::distinct() %>%
        dplyr::filter(feature_name %in% c("PFI_1", "PFI_time_1")) %>%
        dplyr::select(sample_name, group_column, dataset_name, feature_name, feature_value) %>%
        tidyr::pivot_wider(names_from = feature_name, values_from = feature_value)
    }
  }

  if(nrow(df) == 0) return(NULL)

  data.frame(
    status = purrr::pluck(df, time_status),
    time = purrr::pluck(df, time_column),
    measure = purrr::pluck(df, group_column)
  ) %>%
    na.omit()
}

## This function is the iatlas-app code, in
## iatlas-app/R/kmplot.R

create_kmplot <- function(fit, df, confint, risktable, title, group_colors, show_pval = FALSE, show_pval_method = FALSE, facet = FALSE) {

  if(facet ==FALSE){
    survminer::ggsurvplot(
      fit,
      data = df,
      conf.int = confint,
      risk.table = risktable,
      title = title,
      palette = group_colors,
      pval = show_pval,
      pval.method = show_pval_method,
    )
  }else{
    survminer::ggsurvplot_list(
      fit,
      data = df,
      pval = show_pval,
      pval.method = show_pval_method,
      conf.int = confint,
      risk.table = risktable,
      title = title,
      palette = group_colors
    )
  }

}



