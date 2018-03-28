
day_to_week = function(data, dt_col, group_cols, agg_cols, wt_col = NA, agg_to = "weekly"){
  # Install Any Necessary Packages Not on Machine
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load("dplyr", "magrittr", "lubridate")
  
  # Temporary Column Naming
  group_names = paste0("group_col", 1:length(group_cols))
  agg_names = paste0("agg_col", 1:length(agg_cols))
  new_colnames = c("dt", group_names, agg_names, "wt")
  
  # Create New Dataframe 'df'
  if(is.na(wt_col)){
    df = data[,c(dt_col, group_cols, agg_cols)] %>%
      mutate(wt_col = 1) %>%
      magrittr::set_colnames(c(new_colnames))
  } else{
    df = data[,c(dt_col, group_cols, agg_cols, wt_col)] %>%
      magrittr::set_colnames(c(new_colnames))
  }
  
  # Create Lookup Table for Dates
  uniq_dt = df$dt %>% unique() %>% sort()
  n_dt = uniq_dt %>% length()
  n_whole_wks = floor(n_dt / 7)
  remain_dates = tail(uniq_dt, n_whole_wks * 7)
  date_df = data.frame(dt = remain_dates,
                       wknum = rep(1:n_whole_wks, 7) %>% sort()) %>%
    group_by(wknum) %>%
    mutate(week_start = min(dt),
              week_end = max(dt)) %>%
    data.frame() %>%
    ungroup()
  
  # Join Lookup Table to Data
  df = df %>%
    filter(dt %in% remain_dates) %>%
    left_join(date_df, by = "dt") %>%
    dplyr::select(-dt)
  
  # Find Weight Denominator
  wt_denom_df = df[,c("wknum", group_names, "wt")] %>%
    group_by_at(c("wknum", group_names)) %>%
    summarise_all(funs(sum)) %>%
    data.frame() %>%
    ungroup() %>%
    dplyr::rename(wt_total = wt)
  
  # Join Weight Denominator on Table
  if(is.na(wt_col)){
    df = df %>%
      left_join(wt_denom_df, by = c("wknum", group_names)) %>%
      mutate(wt_perc = 1)
  } else{
    df = df %>%
      left_join(wt_denom_df, by = c("wknum", group_names)) %>%
      mutate(wt_perc = wt / wt_total)
  }
  
  # Aggregate
  for(a in 1:length(agg_names)){
    df[,c(agg_names[a])] = df[,c(agg_names[a])] * df[,"wt_perc"]
  }
  
  if(!is.na(wt_col)){
    return_cols = c("week_start", "week_end", group_names, agg_names, "wt")
    return_colnames = c("week_start", "week_end", group_cols, agg_cols, wt_col)
  } else{
    return_cols = c("week_start", "week_end", group_names, agg_names)
    return_colnames = c("week_start", "week_end", group_cols, agg_cols)
  }
  
  agg = df %>%
    dplyr::select_at(return_cols) %>%
    group_by_at(c("week_start", "week_end", group_names)) %>%
    summarise_all(funs(sum)) %>%
    data.frame() %>%
    ungroup() %>%
    magrittr::set_colnames(c(return_colnames)) %>%
    arrange_at(c(group_cols, "week_start", "week_end"))
  
  return(agg)
}
  
  