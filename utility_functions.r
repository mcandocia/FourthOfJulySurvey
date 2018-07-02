library(qdapTools)
library(tidyr)

split_columns <- function(data, column, sep=';'){
  new_columns = mtabulate(lapply( strsplit(data[,column], sep),
                                  function(x) paste0(column,'_',x))
  )
  return(cbind(data, new_columns))
}

relevel_var <- function(data, var, lvls){
  data[,var]=factor(as.character(data[,var]),levels=lvls)
  return(data)
}

fix_factors <- function(data){
  data %>% relevel_var('region', 
                       c('Midwest', 'Northeast', 'Southeast', 
                         'Southwest', 'West', 
                         'I do not live in the United States')) %>%
    relevel_var('celebrates_christmas', c('Yes','No'))  %>%
    mutate(region=mapvalues(region,from='I do not live in the United States', to='Non-US')) %>%
    relevel_var('age_group', c('Under 18','18-24','25-34','35-44','45-54','55+')) %>% 
    relevel_var('parent_gifts', c('Yes, and they claimed they were from Santa','Yes, but they didn\'t say they were from Santa','No'))
  
}

#replaces each space in a factor's levels with a newline
split_factor_levels_with_newlines <- function(data, variable){
  levels = levels(data[,variable])
  target_levels = gsub(' ', '\n', levels)
  data[,variable] = mapvalues(data[,variable], from=levels, to=target_levels)
  return(data)
}

#uses SE on middle two columns and NSE on the pattern
smart_gather <- function(data, keycol, valcol, pattern, factor_key){
  command = sprintf('gather(data, %s, %s, pattern, factor_key=factor_key)', keycol, valcol)
  eval(parse(text=command))
}

#regroup columns into molten frame
gather_category <- function(data, prefix, sort_by_count=TRUE, deperiodize=TRUE) {
  new_column = gsub('_','', prefix)
  val_column =  paste0(prefix, 'value')
  #in case it hasn't been removed
  data[,new_column] = NULL
  newcol_e = new_column
  newval_e = val_column
  new_data = smart_gather(data, newcol_e, newval_e, starts_with(prefix), 
                          factor_key=TRUE)
  if (sort_by_count){
    counts = table(new_data[new_data[,val_column]==1, new_column])
    count_names = names(counts)
    new_data[,new_column] = factor(new_data[,new_column], levels = count_names[order(counts)])
  }
  else{
    new_data[,new_column] = factor(new_data[,new_column])
  }
  #remove periods for values.that.look.like.this
  deperiodize_ <- function(x) gsub('[.]+',' ',x)
  if (deperiodize)
    trans=deperiodize_
  else
    trans=identity
  new_data[,new_column] = mapvalues(new_data[,new_column], 
                                    from = levels(new_data[,new_column,]),
                                    to = trans(gsub(prefix,'', levels(new_data[,new_column]))))
  return(new_data)
}