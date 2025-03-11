latin_to_utf8<-function(x, from="latin1", to="UTF-8"){Encoding(x) <- from;iconv(x, from, to,sub='')}

remove_blank_headings<-function(data){data[,names(data)!=""]}
remove_vars<-function(data,vars){data[,names(data) %!in%vars]}

`%!in%` = Negate(`%in%`)

humanTime <- function() {
  format(Sys.time(), "%Y%m%d-%H%M%OS")
}

rec_missing<-function(x,missings=c(NULL,'NULL','N/A','n/a',999,998,888,' ','(vide)','d/m','','NA','na',""," ")) {
  x[x %in% missings] <- NA
  return(x)
}

rec_missing_all<-function(data){lapply(data,rec_missing) %>% bind_cols}

cleanheaders<-function(data,slashtodot){
  if(slashtodot){
    names(data)<-gsub("^X_","",names(data))
    names(data)<-gsub("^_","",names(data))
    names(data)<-gsub("\\/",".",names(data)) 
  } else {
    names(data)<-gsub("^X_","",names(data))
    names(data)<-gsub("^_","",names(data))
  }
  return(data)}

prepdata<-function(data,slashtodot){data %>% cleanheaders(.,slashtodot) %>% rec_missing_all %>% remove_blank_headings %>% type_convert}

ch<-as.character
chr<-as.character

label_clog<- function(clog,survey,choices,survey_label,choices_label){
  
  # names(choices)<-gsub(":.*","",names(choices))
  # names(survey)<-gsub(":.*","",names(survey))
  choices_label <- choices[[choices_label]]
  survey_label <- survey[[survey_label]]
  question.name_label <- match(clog[["question.name"]], survey[["name"]])
  old.value_label <- match(clog[["old.value"]], choices[["name"]])
  parent.other.question_label <- match(clog[["parent.other.question"]], survey[["name"]])
  parent.other.answer_label<-match(clog[["parent.other.answer"]], choices[["name"]])
  
  old.value_label<-str_split(clog[["old.value"]]," ")
  old.value_label<-lapply(old.value_label, function(x)match(x, choices[["name"]])) %>% 
    lapply(.,function(x){ifelse(is.na(x),x,choices_label[x])}) %>% lapply(., function(x)paste(x,collapse = " ")) %>% unlist
  
  parent.other.answer_label<-str_split(clog[["parent.other.answer"]]," ")
  parent.other.answer_label<-lapply(parent.other.answer_label, function(x)match(x, choices[["name"]])) %>% 
    lapply(.,function(x){ifelse(is.na(x),x,choices_label[x])}) %>% lapply(., function(x)paste(x,collapse = " ")) %>% unlist
  
  labeled_clog <- clog %>%
    mutate(question.name_label = ifelse(is.na(question.name_label),question.name,survey_label[question.name_label]),
           old.value_label =ifelse(is.na(old.value_label)|old.value_label=="NA",old.value,old.value_label),
           parent.other.question_label = ifelse(is.na(parent.other.question_label),parent.other.question,survey_label[parent.other.question_label]),
           parent.other.answer_label =ifelse(is.na(parent.other.answer_label)|parent.other.answer_label=="NA",parent.other.answer,parent.other.answer_label)
    )
  
  # labeled_clog <- clog %>%
  #   mutate(question.name_label = ifelse(is.na(question.name_label),question.name,survey_label[question.name_label]),
  #          old.value_label = ifelse(is.na(old.value_label),old.value,choices_label[old.value_label]),
  #          parent.other.question_label = ifelse(is.na(parent.other.question_label),parent.other.question,survey_label[parent.other.question_label])
  #          )
  
  vars<-c("today","base","enumerator","uuid","question.name","question.name_label","old.value","old.value_label","new.value","parent.other.question","parent.other.question_label","parent.other.answer","parent.other.answer_label")
  labeled_clog<-labeled_clog %>% select(all_of(vars),everything())
  
  return(labeled_clog)
}

load_file <- function(name, path) {
  ext <- tools::file_ext(name)
  switch(ext,
         csv=read.csv(path,stringsAsFactors = F),
         # xlsx=readxl::read_excel(path,1,col_types = "text"),
         # xls=readxl::read_excel(path,1,col_types = "text"),
         xlsx=readxl::read_excel(path,1),
         xls=readxl::read_excel(path,1),
         validate("Invalid file; Please upload a .csv .xlsx or .xls file")
  )
}

pulluuid<-function(data,logiquetest){data$uuid[which(logiquetest)]}


# produce plot chart for categorical variables
# df <- plot_data
# var <- selected_question
# n_min=3
plot.select <- function(df=result_long %>% filter(is.na(disag_var_1), is.na(disag_var_2)), var="safety_concern_increase", n_min=threshold){
  df_filtered <- df %>% filter(label==var, !is.na(label.choice)) 
  # note <- df_filtered %>% pull(indicator_note) %>% unique %>% na.omit
  if (nrow(df_filtered)>0){
    plot <- df_filtered %>% ggplot(aes(x=reorder(label.choice,mean), y=mean)) +
      geom_bar(position='dodge', stat='identity', fill="#EE5859")+
      scale_x_discrete(labels=~str_wrap(., width = 60))+
      scale_y_continuous(labels=scales::percent_format(), limits=c(0,1))+ # to display count only
      labs(x="", y="% of respondents", 
           # title=str_wrap(paste0(unique(df_filtered$label)), width = 65),
           subtitle=paste0("\n", str_wrap(paste0(unique(df_filtered$label) %>% na.omit), width = 65), "\n\n"),
           caption = paste0(unique(df_filtered$resp), " respondents answered the question.")) +
      theme_minimal() + coord_flip() + theme(plot.title = element_text(), plot.subtitle = element_text(size=9))
    if (sum(df_filtered$count, na.rm=T)>=n_min) {
      plot <- plot + 
      scale_y_continuous(labels=scales::percent_format(), limits=c(0,1), breaks=NULL) + # if you want to display %
      labs(y="% of respondents") + geom_text(aes(y=mean+0.05, label=paste0(round(100*mean, 1), "%")), size=2.5)
      } else {
        plot <- plot + geom_text(aes(y=mean+0.05, label=paste0("n=", count)), size=3)
      }
    # if (perc) {plot <- plot + 
    #     scale_y_continuous(labels=scales::percent_format(), limits=c(0,1)) + # if you want to display %
    #     labs(y="% of respondents") + geom_text(aes(y=mean+0.05, label=paste0(round(100*mean, 1), "%")), size=2.5)} else {
    #       plot <- plot + geom_text(aes(y=mean+0.05, label=paste0("n=", count)), size=3)
    #     }    
    return(plot)
  }
}
# df <- data
# var <- selected_question
# n_min=3
plot.ind_comparison <- function(df=result_long %>% filter(is.na(disag_var_1), is.na(disag_var_2)), var="safety_concern_increase", n_min=threshold){
  # df_filtered <- df %>% filter(label==var, !is.na(label.choice))

  # note <- df_filtered %>% pull(indicator_note) %>% unique %>% na.omit
  if (nrow(df_filtered)>0){
    plot <- df_filtered %>% ggplot(aes(x=reorder(disag_val_1,percent), y=percent))+
      geom_bar(position='dodge', stat='identity', fill="#EE5859")+
      scale_x_discrete(labels=~str_wrap(., width = 60)) +
      # scale_y_continuous(labels=scales::percent_format(), limits=c(0,1), breaks = NULL)+ # to display count only
      labs(x="", y="% of responses", 
           title=str_wrap(paste0(unique(df_filtered$label)), width = 65),
           # subtitle=paste0("\n", str_wrap(paste0(unique(df_filtered$label) %>% na.omit), width = 65), "\n\n"), 
           caption = paste0(unique(df_filtered$resp), " respondents answered the question.")) +
      theme_minimal() + coord_flip() + theme(plot.title = element_text(), plot.subtitle = element_text(size=9))
    if (sum(df_filtered$count, na.rm=T)>=n_min) {plot <- plot + 
      scale_y_continuous(labels=scales::percent_format(), limits=c(0,1)) + # if you want to display %
      labs(y="% of respondents") + geom_text(aes(y=mean+0.05, label=paste0(round(100*mean, 1), "%")), size=2.5)} else {
        plot <- plot + geom_text(aes(y=mean+0.05, label=paste0("n=", count)), size=3)
      }
    # if (perc) {plot <- plot + 
    #     scale_y_continuous(labels=scales::percent_format(), limits=c(0,1)) + # if you want to display %
    #     labs(y="% of respondents") + geom_text(aes(y=mean+0.05, label=paste0(round(100*mean, 1), "%")), size=2.5)} else {
    #       plot <- plot + geom_text(aes(y=mean+0.05, label=paste0("n=", count)), size=3)
    #     }    
    return(plot)
  }
}


# produce histogram for numerical variables
options(scipen = 200000)
plot.int <- function(df=clean, df_res=result_long %>% filter(is.na(disag_var_1), is.na(disag_var_2)), var="transportation_cost"){
  df_filtered <- df %>% select(any_of(c(var))) %>% pivot_longer(any_of(var)) %>% mutate(value=as.numeric(value))
  label <- df_res %>% filter(question==var)
  # note <- df_res %>% filter(question==var) %>% pull(indicator_note) %>% unique %>% na.omit
  plot <- df_filtered %>% ggplot(aes(x=value, fill="#EE5859")) +
    geom_histogram(aes(y =stat(count/sum(count))), show.legend = FALSE) +
    scale_x_continuous() + scale_y_continuous(labels=scales::percent_format())+
    labs(x="", y="% of KIs",
         title=paste0(unique(label$label)), 
         # subtitle=paste0("\n", str_wrap(paste0(unique(label$label)), width = 80), "\n\n", paste0(note, collapse = "\n")),
         caption = paste0(unique(label$resp), " out of ", unique(label$n), " respondents answered the question.")) +
    theme_minimal() + theme(plot.title = element_text(), plot.subtitle = element_text(size=9))
  if ("median" %in% names(df_res)) {plot <- plot +
    geom_vline(aes(xintercept=label$median), col="#0067A9", size=1) +
    geom_text(aes(label=paste0("median: ", round(label$median, 1)), y=0, x=label$median), vjust=-1, hjust=-0.1, col="#0067A9", size=3)
  } else {
    plot <- plot + geom_vline(aes(xintercept=label$mean), col="#0067A9", size=1) +
      geom_text(aes(label=paste0("average: ", round(label$mean, 1)), y=0, x=label$mean), vjust=-1, hjust=-0.1, col="#0067A9", size=3)}
  return(plot)
}

# clean <- function(x) {
#   x <- tolower(x)
#   x <- gsub("  "," ",x)
#   x <- gsub("  "," ",x)
#   #supprime l'espace en debut
#   Nettoyage <- x[substr(x, 0, 1)==" "]
#   x <- replace(x,substr(x, 0, 1)==" ",substr(Nettoyage, 2, nchar(Nettoyage)))
#   #supprime l'espace en fin
#   Nettoyage <- x[substr(x, nchar(x), nchar(x)+1)==" "]
#   x <- replace(x,substr(x, nchar(x), nchar(x)+1)==" ", substr(Nettoyage, 1, nchar(Nettoyage)-1))
#   # x <- gsub("?|?|?","a",x)
#   # x <- gsub("?|?|?|?","e",x)
#   # x <- gsub("?|?","i",x)
#   # x <- gsub("?|?","o",x)
#   # x <- gsub("?|?|?","u",x)
#   # x <- gsub(" |-|'","_",x)
#   return (x)
# }