detect.outliers <- function(df=raw, col.n=NULL, method="sd-linear", n.sd=2, n.iqr=3){
  if (sum(grepl( "uuid", colnames(raw)))==1) df <- df %>% rename_with(~"uuid", matches("uuid"))
  if (!is.null(col.n)) df <- df %>% select(uuid, all_of(col.n))
  res <- data.frame()
  for (col in colnames(df)[colnames(df)!="uuid"]){
    df.temp <- data.frame(uuid=df$uuid, value=as.numeric(df[[col]])) %>% filter(!is.na(value) & value>0)
    if (method=="sd-linear"){
      df.temp <- df.temp %>%
        mutate(is.outlier.high=ifelse(value > mean(value, na.rm=T) + n.sd*sd(value, na.rm=T), T, F),
               is.outlier.low =ifelse(value < mean(value, na.rm=T) - n.sd*sd(value, na.rm=T), T, F))
    } else if (method=="iqr-linear") {
      df.temp <- df.temp %>%
        mutate(is.outlier.high=ifelse(value > quantile(value, 0.75) + n.iqr*IQR(value), T, F),
               is.outlier.low =ifelse(value < quantile(value, 0.25) - n.iqr*IQR(value), T, F))
    } else if (method=="sd-log"){
      df.temp <- df.temp %>%
        mutate(col.log=log(value),
               is.outlier.high=ifelse(col.log > mean(col.log, na.rm=T) + n.sd*sd(col.log, na.rm=T), T, F), 
               is.outlier.low =ifelse(col.log < mean(col.log, na.rm=T) - n.sd*sd(col.log, na.rm=T), T, F))
    } else if (method=="iqr-log") {
      df.temp <- df.temp %>%
        mutate(col.log=log(value),
               is.outlier.high=ifelse(col.log > quantile(col.log, 0.75) + n.iqr*IQR(col.log), T, F),
               is.outlier.low =ifelse(col.log < quantile(col.log, 0.25) - n.iqr*IQR(col.log), T, F))
    } else stop("Method unknown")
    df.temp <- filter(df.temp, is.outlier.high | is.outlier.low) %>% 
      mutate(variable=col,
             issue = ifelse(is.outlier.high, paste0("High numerical outlier, using ", method, " method"),
                            ifelse(is.outlier.low, paste0("Low numerical outlier, using ", method, " method"), ""))) %>%
      select(uuid, variable, value, issue)
    res <- rbind(res, df.temp)
  }
  return(res)
}

combine_tool <- function(survey=tool, responses=choices){
  survey <- survey %>% 
  	select(-matches("^label$", ignore.case = TRUE)) %>% 
  	rename_with(~ "label", .cols = matches("^label::english$", ignore.case = TRUE)) %>% 
  select( name,type, name, label, any_of(c("label_ar"="label::Arabic", "label_fr"="label::Francais"))) %>%
  mutate(q.type = lapply(str_split(type, " "), function(x) x[[1]]) %>% unlist, 
           list_name = lapply(str_split(type, " "), function(x) ifelse(length(x)>1, x[[2]], NA)) %>% unlist)
  survey %>% right_join(responses %>% distinct %>% 
  												rename_with(~ "label.choice", .cols = matches("^label::english$", ignore.case = TRUE)) %>% 
  												select(list_name, name.choice=name, label.choice, any_of(c("label.choice_ar"="label::Arabic", "label.choice_fr"="label::Francais"))) %>% 
                 filter(!if_any(everything(), is.na)), multiple = "all") %>%
    filter(!is.na(name)) %>% bind_rows(survey %>% filter(type=="integer")) %>% distinct()
}

check_unique <- function(df=raw, ...){
  tool.combined <- combine_tool(...)
  list.unique <- lapply(df, function(x) str_split(x, " ") %>% unlist %>% unique) %>% keep(names(.) %in% unique(tool.combined$name))
  non.matching <- list()
  for (i in names(list.unique)){
    unique.val <- list.unique[[i]]
    tool.val <- tool.combined %>% filter(name==i) %>% pull(name.choice)
    non.matching[[i]] <- unique.val[!unique.val %in% tool.val]
  }
  return(list(data.unique=list.unique, non.matching=non.matching))
}

check_na_select <- function(df, col.select=c()){
  lapply(c(col.sm, col.so), function(col){
    col.b <- str_subset(names(raw), paste0(col, "\\."))
    raw %>% mutate(check_id = paste0("na_binary_", col), add_col = paste(c(col, col.b), collapse = " "),
                   flag = case_when(rowSums(across(all_of(col.b), ~ is.na(.)), na.rm=T) > 0 & rowSums(across(all_of(col.b), ~ !is.na(.)), na.rm=T) > 0 ~ T, T ~ F),
                   issue = paste0("there is at least one NA entry in the binary columns alongside at least one recorded choice. Recode all or drop.")) %>% filter(flag) %>% 
      mutate_all(as.character) %>% select(uuid, check_id, issue, add_col, all_of(c(col,col.b))) %>% pivot_longer(all_of(c(col, col.b)), names_to = "variable") %>%
      mutate(value_new=case_when(is.na(value) ~ "0", T ~ value), change="TRUE", send_field_teams="FALSE", comment = "Old version of the kobo tool in the tablet, assumption that the NA choice not available would not have been picked to enable keeping the record.")}) %>%
    setNames(col.sm) %>% bind_rows()
}

add_to_log <- function(check_df, col_uuid="uuid", var, col_flag="flag", issue_message="", col_new_val=NULL, add_col=NULL, check.id=NA){
  check_df %>% filter(!!sym(col_flag)) %>%
    mutate(issue=issue_message, check_id=check.id) %>% select(uuid=all_of(col_uuid), check_id, any_of(add_col), issue, all_of(var), any_of("change")) %>% mutate_all(as.character) %>%
    pivot_longer(cols=all_of(var), names_to="variable") %>% mutate(value=as.character(value))
# check_df <- check_df %>% filter(flag) %>% 
#   mutate(issue=issue_message) %>% select(uuid=all_of(col_uuid), issue, all_of(var)) %>% 
#   pivot_longer(cols=all_of(var), names_to="variable") %>% mutate(value=as.character(value))
}

add_to_deletion <- function(check_deletion, col_uuid="uuid", var=c(NULL), col_flag="flag", issue_message=""){
  check_deletion %>% filter(!!sym(col_flag)) %>% mutate(issue=issue_message, variable=var) %>% select(uuid=all_of(col_uuid), issue, any_of("variable"))
}

data.validation.list <- function(...){
  tool.c <- combine_tool(survey = tool, responses = choices)
  nrow_validation <- tool.c %>% group_by(name) %>% summarise(n_max=n()) %>% pull(n_max) %>% max
  data.val <- data.frame(matrix(NA, nrow = nrow_validation, ncol = 0))
  lapply(unique(tool.c$name), function(c){
    unique.c <- tool.c %>% filter(name==c) %>% pull(name.choice)
    data.val <<- data.val %>% mutate(!!sym(c) := c(unique.c, rep(NA, nrow_validation-length(unique.c))))
    }) %>% invisible
  return(data.val)
  rm(data.val)
}

get.col.range <- function(variable){
  column.number <- which(colnames(data.val)==variable)
  all <- expand.grid(LETTERS, LETTERS)
  all <- all[order(all$Var1,all$Var2),]
  alphabet <- c(LETTERS, do.call('paste0',all))
  col.excel <- alphabet[column.number]
  nrow <- nrow(data.val %>% filter(!is.na(!!sym(variable))))
  range.vect <- c("$", col.excel, "$2:$", col.excel, "$", (nrow + 1))             ## if nrow + 2 => will keep an additionnal field in drop down list to be updated if needed by partner
  range <- paste(range.vect, sep="", collapse="")
  value.sheet <- paste("'Choices validation'!")
  value <- paste(value.sheet, range, sep="", collapse="")
  return(value)
}

save.follow.up.requests <- function(cl=cleaning_log,  filename.out="output/test.xlsx", col_var_format="variable", col_format="value_new", ...){       # save follow-up requests
  wb <- createWorkbook()
  addWorksheet(wb, "Follow-up")
  addWorksheet(wb, "Choices validation")
  writeData(wb = wb, x = cl, sheet = "Follow-up", startRow = 1)
  
  data.val <<- data.validation.list(tool, choices)
  writeData(wb = wb, x = data.val, sheet = "Choices validation", startRow = 1)
  
  col.style <- createStyle(textDecoration="bold", fgFill="#CECECE", halign="center", border="TopBottomLeftRight", borderColour="#000000")
  
  setColWidths(wb, "Follow-up", cols=1, widths=12)
  setColWidths(wb, "Follow-up", cols=2, widths=40)
  setColWidths(wb, "Follow-up", cols=3, widths=20)
  setColWidths(wb, "Follow-up", cols=4, widths=15)
  setColWidths(wb, "Follow-up", cols=5, widths=15)
  setColWidths(wb, "Follow-up", cols=6, widths=20)
  setColWidths(wb, "Follow-up", cols=7, widths=15)
  setColWidths(wb, "Follow-up", cols=8, widths=16)
  setColWidths(wb, "Follow-up", cols=9, widths=30)

  addStyle(wb, "Follow-up", style = createStyle(wrapText=F), rows = 1:(ncol(cl)+1), cols=6)
  addStyle(wb, "Follow-up", style = createStyle(wrapText=F), rows = 1:(ncol(cl)+1), cols=7)
  addStyle(wb, "Follow-up", style = col.style, rows = 1, cols=1:dim(cl)[2])
  
  # col.id <- which(colnames(cl) %in% c("variable", "issue", "old_value"))
  # random.color <- ""
  # if (nrow(cl)>1) {for (r in 2:nrow(cl)){
  #   if(as.character(cl[r, "uuid"])==as.character(cl[r-1, "uuid"]) & 
  #      as.character(cl[r, "check_id"])==as.character(cl[r-1, "check_id"])){
  #     if (random.color == "") random.color <- randomColor(1, luminosity = "light")
  #     addStyle(wb, "Follow-up", style = createStyle(fgFill=random.color, wrapText=F), 
  #              rows = r:(r+1), cols=col.id, gridExpand = T)
  #   } else random.color=""
  # }}
   
  for (r in 1:nrow(cl)){
    if (cl[r,col_var_format] %in% colnames(data.val)){
      dataValidation(wb, "Follow-up", cols = which(colnames(cl)==col_format),
                     rows = r+1, type = "list",
                     value = get.col.range(as.character(cl[r,col_var_format])))
    }
  }
  saveWorkbook(wb, filename.out, overwrite = TRUE)
} 

expand.select.one <- function(df, var, val.parent.na=NA){
  unique <- df %>% pull(!!sym(var)) %>% unique %>% na.omit %>% as.character
  lapply(unique,
         function(val) {
           bin.col <- paste0(var, ".", val)
           df <<- df %>% 
             mutate(!!sym(bin.col) := case_when(!!sym(var) %in% val.parent.na ~ NA_real_, 
                                                !!sym(var) == val ~ 1,
                                                TRUE ~ 0), .after=!!sym(var))
         })
  return(df)
}

expand.select.one.vec <- function(df, x=c()){
  lapply(x, function(var) {df <<- df %>% expand.select.one(var)})
  return(df)
}


collapse.select.multiple <- function(df, cols, name=unique(gsub("(\\.|__).*","",cols))){
  if (sum(!cols %in% colnames(df))>0) print("some binary columns are not present in the dataset")
  if (length(name)>1) stop("Cannot find unique name for the parent column from the binary columns.")
  if (sum(unlist(list(df[,cols]>1)), na.rm = T)>1) stop("Not binary variable.")
  
  cols <- sort(cols)
  var.parent <- name
  df <- df %>% mutate(!!sym(var.parent) := NA_character_)
  lapply(cols, function(c){
    df <<- df %>% 
      mutate(!!sym(var.parent) := case_when(
        !!sym(c) > 0 & !!sym(c) <= 1 ~ paste.remove.na(!!sym(var.parent), gsub(paste0("^", var.parent, "(\\.|__)"), "", c)),
        TRUE ~ !!sym(var.parent)
      )
      )
  })
  df <- df %>% mutate(!!sym(var.parent) := reorder.select.multiple(!!sym(var.parent)))
  return(df)
}


expand.select.multiple <- function(df, var, val.parent.na=NA){
  unique <- df %>% pull(!!sym(var)) %>% str_split(" ") %>% unlist %>% unique %>% na.omit %>% as.character
  unique <- unique[!unique %in% ""]
  lapply(unique,
         function(val) {
           bin.col <- paste0(var, ".", val)
           df <<- df %>% 
             dplyr::mutate(!!sym(bin.col) := case_when(!!sym(var) %in% val.parent.na ~ NA_real_, 
                                                       str_detect(!!sym(var), paste0("(^| )", 
                                                                                     str_replace_all(val, c("\\("="\\\\\\(", "\\)"="\\\\\\)", "\\'"="\\\\\\'", "\\/"="\\\\\\/")), 
                                                                                     "($| )")) ~ 1,
                                                       TRUE ~ 0), .after=!!sym(var))
         })
  return(df)
}


expand.select.multiple.vec <- function(df, x=c(),...){
  lapply(x, function(var) {df <<- df %>% expand.select.multiple(var,...)})
  return(df)
}

type_convert_silent <- function(x) suppressMessages(readr::type_convert(x))

## For composite indicators

hhs_calc = function(data, 
                    cols.foodhome = c(no_food_house, no_food_house_freq),
                    cols.sleephungry = c(sleep_hungry, sleep_hungry_freq),
                    cols.daynofood = c(whole_day_no_food, whole_day_no_food_freq),
                    unique.yn = c(yes, no), 
                    unique.freq = c(often, sometimes, rarely)){
  
  data %>%
    mutate(hhs_lack_food = case_when(!!sym(cols.foodhome[1]) == unique.yn[1] & !!sym(cols.foodhome[2]) %in% unique.freq[2:3] ~ 1,
                                     !!sym(cols.foodhome[1]) == unique.yn[1] & !!sym(cols.foodhome[2]) == unique.freq[1] ~ 2,
                                     !!sym(cols.foodhome[1]) == unique.yn[2] ~ 0,
                                     TRUE ~ NA_real_),
           hhs_sleep_hungry = case_when(!!sym(cols.sleephungry[1]) == unique.yn[1] & !!sym(cols.sleephungry[2]) %in% unique.freq[2:3] ~ 1,
                                        !!sym(cols.sleephungry[1]) == unique.yn[1] & !!sym(cols.sleephungry[2]) == unique.freq[1] ~ 2,
                                        !!sym(cols.sleephungry[1]) == unique.yn[2] ~ 0,
                                        TRUE ~ NA_real_),
           hhs_not_eating = case_when(!!sym(cols.daynofood[1]) == unique.yn[1] & !!sym(cols.daynofood[2]) %in% unique.freq[2:3] ~ 1,
                                      !!sym(cols.daynofood[1]) == unique.yn[1] & !!sym(cols.daynofood[2]) == unique.freq[1] ~ 2,
                                      !!sym(cols.daynofood[1]) == unique.yn[2] ~ 0,
                                      TRUE ~ NA_real_)) %>%
    
    mutate(hhs_score = rowSums(select(., hhs_lack_food, hhs_sleep_hungry, hhs_not_eating), na.rm = F)) %>%
    mutate(hhs_score_cat = case_when(hhs_score < 2 ~ 'little',
                                     hhs_score >= 2 & hhs_score <= 3 ~ 'moderate',
                                     hhs_score > 3 ~ 'severe',
                                     TRUE ~ NA_character_))
}
