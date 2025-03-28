
reshape_long <- function(data, admin_bounds){
	data %>%
		mutate(across(everything(), as.character)) %>%  # Convert all columns to character
		pivot_longer(
			cols = -c(all_of(admin_bounds)),  # Specify columns to pivot (exclude those to keep)
			names_to = "question",    # Name of the new column for the question names
			values_to = "choice"      # Name of the new column for the values
		)
}


max_aggregate_sm <- function(data, severity_dictionary, admin_bounds = admin_boundaries) {
	# Identify select multiple prefixes
	prefixes <- severity_dictionary %>%
		filter(type %in% c("select_multiple", "select multiple")) %>%
		pull(question) %>%
		sub("\\..*|/.*", "", .) %>%
		unique()
	
	# Process data: adjust questions and calculate max severity_value
	data %>%
		mutate(
			question = ifelse(
				sub("\\..*|/.*", "", question) %in% prefixes,
				paste0(sub("\\..*|/.*", "", question)),
				# paste0(sub("\\..*|/.*", "", question), "_final"),
				question
			)
		) %>%
		group_by(across(admin_bounds), sector, type, question) %>%
		summarize(
			severity_value = ifelse(all(is.na(severity_value)), NA, max(severity_value, na.rm = TRUE)),.groups = "drop") %>% ungroup()
}


add_flag3 <- function(data){
	data %>% 
		mutate(flag3  = case_when(
			severity_value >= "3" ~ 1,
			severity_value < "3" ~ 0,
			TRUE ~ NA_real_  ))
}

add_flag4 <- function(data){
	data %>% 
		mutate(flag4  = case_when(
			severity_value >= "4" ~ 1,
			severity_value < "4" ~ 0,
			TRUE ~ NA_real_ ))
}

add_flag4_plus <- function(data){
	data %>% 
		mutate(flag4_plus  = case_when(
			severity_value > "4" ~ 1,
			severity_value <= "4" ~ 0,
			TRUE ~ NA_real_ ))
}

add_flag3_per_settlement <- function(data, admin_bounds){
	data %>% 
		group_by(across(admin_bounds)) %>% 
		mutate(flag3_settlement = sum (flag3, na.rm = TRUE )
					 )%>% 
		ungroup()
}
add_flag4_per_settlement <- function(data, admin_bounds){
  data %>% 
    group_by(across(admin_bounds)) %>% 
    mutate(flag4_settlement = sum (flag4, na.rm = TRUE )
    )%>% 
    ungroup()
}
add_flag4_plus_per_settlement <- function(data, admin_bounds){
  data %>% 
    group_by(across(admin_bounds)) %>% 
    mutate(flag4_plus_settlement = sum (flag4_plus, na.rm = TRUE )
    )%>% 
    ungroup()
}

add_flag3_per_sector <- function(data, admin_bounds){
	data %>% 
		group_by(across(admin_bounds), sector) %>% 
		mutate(flag3_sector = sum (flag3, na.rm = TRUE )
					 )%>% 
		ungroup()
}
add_flag4_per_sector <- function(data, admin_bounds){
  data %>% 
    group_by(across(admin_bounds), sector) %>% 
    mutate(flag4_sector = sum (flag4, na.rm = TRUE )
    )%>% 
    ungroup()
}
add_flag4_plus_per_sector <- function(data, admin_bounds){
  data %>% 
    group_by(across(admin_bounds), sector) %>% 
    mutate(flag4_plus_sector = sum (flag4_plus, na.rm = TRUE )
    )%>% 
    ungroup()
}

add_proportion3_per_sector <- function(data, admin_bounds){
	data %>% 
    add_flag3() %>%
    add_flag3_per_sector(admin_bounds) %>% 
		group_by(across(admin_bounds), sector) %>% 
		mutate(num_ind_sector = sum(is.na(severity_value)==F)) %>% 
		group_by(across(admin_bounds)) %>%
		mutate(proportion3_sector = round(flag3_sector / num_ind_sector* 100, 2)
		) %>% select(-num_ind_sector)%>% 
		ungroup()
}
add_proportion4_per_sector <- function(data, admin_bounds){
  data %>% 
    add_flag4() %>%
    add_flag4_per_sector(admin_bounds) %>%     
    group_by(across(admin_bounds), sector) %>% 
    mutate(num_ind_sector = sum(is.na(severity_value)==F)) %>% 
    group_by(across(admin_bounds)) %>%
    mutate(proportion4_sector = round(flag4_sector/ num_ind_sector*100,2)
    ) %>% select(-num_ind_sector)%>% 
    ungroup()
}

add_proportion4_plus_per_sector <- function(data, admin_bounds){
  data %>% 
    add_flag4_plus() %>%
    add_flag4_plus_per_sector(admin_bounds) %>% 
    group_by(across(admin_bounds), sector) %>% 
    mutate(num_ind_sector = sum(is.na(severity_value)==F)) %>% 
    group_by(across(admin_bounds)) %>%
    mutate(proportion4_plus_sector = round(flag4_plus_sector / num_ind_sector*100,2)
    ) %>% select(-num_ind_sector)%>% 
    ungroup()
}


add_proportion3_per_settlement <- function(data, admin_bounds, len_all_indicators) {
	data %>% 
    add_flag3_per_settlement(admin_bounds) %>% 
		filter(is.na(severity_value)==F) %>% 
		group_by(across(admin_bounds)) %>% 
		mutate(num_ind = sum(!is.na(severity_value))) %>% 
		mutate(
			flag_below_threshold = num_ind < (0.5 * len_all_indicators),
			error_message = ifelse(flag_below_threshold, 
														 "Number of indicators is below 50% of the total number of indicators in the severity table. No proportion should be calculated; use the absolute flag index instead.", 
														 NA_character_)
		) %>% 
		mutate(
			proportion3_settlement = ifelse(flag_below_threshold, NA, round(flag3_settlement / num_ind * 100, 2))
		) %>% 
		ungroup()
}

add_proportion4_per_settlement <- function(data, admin_bounds, len_all_indicators) {
  data %>% 
    add_flag4_per_settlement(admin_bounds) %>% 
    filter(is.na(severity_value)==F) %>% 
    group_by(across(admin_bounds)) %>% 
    mutate(num_ind = sum(!is.na(severity_value))) %>% 
    mutate(
      flag_below_threshold = num_ind < (0.5 * len_all_indicators),
      error_message = ifelse(flag_below_threshold, 
                             "Number of indicators is below 50% of the total number of indicators in the severity table. No proportion should be calculated; use the absolute flag index instead.", 
                             NA_character_)
    ) %>% 
    mutate(proportion4_settlement = ifelse(flag_below_threshold, NA, round(flag4_settlement / num_ind * 100, 2))
    ) %>% 
    ungroup()
}
add_proportion4_plus_per_settlement <- function(data, admin_bounds, len_all_indicators) {
  data %>% 
    add_flag4_plus_per_settlement(admin_bounds) %>% 
    filter(is.na(severity_value)==F) %>% 
    group_by(across(admin_bounds)) %>% 
    mutate(num_ind = sum(!is.na(severity_value))) %>% 
    mutate(
      flag_below_threshold = num_ind < (0.5 * len_all_indicators),
      error_message = ifelse(flag_below_threshold, 
                             "Number of indicators is below 50% of the total number of indicators in the severity table. No proportion should be calculated; use the absolute flag index instead.", 
                             NA_character_)
    ) %>% 
    mutate(
      proportion4_plus_settlement = ifelse(flag_below_threshold, NA, round(flag4_plus_settlement / num_ind * 100, 2))
    ) %>% 
    ungroup()
}

add_mean_index_per_settlement <- function(data, admin_bounds){
	data %>% 
		filter(is.na(severity_value)==F) %>% 
		group_by(across(admin_bounds)) %>% 
		mutate(mean_index_settlement = mean(severity_value, na.rm=T)) %>% 
		ungroup()
}

add_mean_anf_index_per_settlement <- function(data, admin_bounds) {
	mean_values <- data %>%
		filter(!grepl('edu|cm', sector, ignore.case = TRUE)) %>%  # Temporarily exclude rows
		group_by(across(admin_bounds)) %>%
		summarise(mean_anf_index_settlement = mean(severity_value, na.rm = TRUE), .groups = "drop")  # Calculate mean
	
	data %>%
		left_join(mean_values, by = admin_bounds)  # Join the calculated means back to the original data
}


add_settlement_proportion_score <- function(data_index, official_admin_boundaries) {
	settlement_proportions <- data_index %>% 
		filter(is.na(severity_value)==F) %>% 
		group_by(across(all_of(official_admin_boundaries)), severity_value ) %>% 
		summarise(n = n(), .groups = "drop") %>% 
		group_by(across(all_of(official_admin_boundaries))) %>% 
		mutate(N = sum(n)) %>% 
		mutate(prop = round(n/N*100, 2)) %>% 
		select(-n) %>% 
		arrange(across(all_of(official_admin_boundaries)), desc(severity_value)) %>%
		group_by(across(all_of(official_admin_boundaries))) %>%
		mutate(cumulative_prop = cumsum(prop)) %>%
		mutate(
			score_index_settlement = ifelse(length(na.omit(cumulative_prop)) > 1,
																			approx(cumulative_prop, severity_value, xout = 25, rule = 2)$y,
																			severity_value),
			score_index_settlement_rounded = round(score_index_settlement, 0)
		) %>% 
		ungroup() %>% 
		select(all_of(official_admin_boundaries), score_index_settlement, score_index_settlement_rounded) %>% 
		unique()
	
	data_index %>% 
		left_join(settlement_proportions, by = c(all_of(official_admin_boundaries)))
}

add_sector_proportion_score <- function(data_index, official_admin_boundaries) {
	sector_proportions <- data_index %>% 
		filter(is.na(severity_value)==F) %>% 
		group_by(across(all_of(official_admin_boundaries)), sector, severity_value) %>% 
		summarise(n = n(), .groups = "drop") %>% 
		group_by(across(all_of(official_admin_boundaries)),sector) %>% 
		mutate(N = sum(n)) %>% 
		mutate(prop = round(n/N*100, 2)) %>% 
		select(-n) %>% 
		arrange(across(all_of(official_admin_boundaries)), sector, desc(severity_value)) %>%
		group_by(across(all_of(official_admin_boundaries)), sector)%>%
		mutate(cumulative_prop = cumsum(prop))%>%
		mutate(
			score_index_sector = ifelse(length(na.omit(cumulative_prop)) > 1,
																	 approx(cumulative_prop, severity_value, xout = 25, rule = 2)$y,
																	 severity_value),
			score_index_sector_rounded = round(score_index_sector, 0)
		) %>% 
		ungroup() %>% 
		select(all_of(official_admin_boundaries), sector, score_index_sector, score_index_sector_rounded) %>% 
		unique()
	
	data_index %>% 
		left_join(sector_proportions, by = c(all_of(official_admin_boundaries), "sector"))
}



add_mean_flag3_area <- function(data, admin_bounds) {
  # area_bounds <- admin_bounds
  # area_bounds <- admin_bounds[1:2]
  area_bounds <- admin_bounds
  
  
  out <- data %>% 
    filter(is.na(severity_value)==F) %>% 
    select(-c(sector, type, question, severity_value, flag3, contains('sector'))) %>% 
    unique() %>% 
    group_by(across(all_of(area_bounds))) %>% 
    summarise(
      mean_flag3_area = mean(flag3_settlement, na.rm = TRUE)
      )
      
  
  data %>% 
    left_join(out, by = c(all_of(area_bounds)))
  
}
add_mean_flag4_area <- function(data, admin_bounds) {
  area_bounds <- admin_bounds
  
  out <- data %>% 
    filter(is.na(severity_value)==F) %>% 
    select(-c(sector, type, question, severity_value, flag4, contains('sector'))) %>% 
    unique() %>% 
    group_by(across(all_of(area_bounds))) %>% 
    summarise(
      mean_flag4_area = mean(flag4_settlement, na.rm = TRUE)
    )
  
  
  data %>% 
    left_join(out, by = c(all_of(area_bounds)))
  
}
add_mean_flag4_plus_area <- function(data, admin_bounds) {
  area_bounds <- admin_bounds

  out <- data %>% 
    filter(is.na(severity_value)==F) %>% 
    select(-c(sector, type, question, severity_value, flag4_plus, contains('sector'))) %>% 
    unique() %>% 
    group_by(across(all_of(area_bounds))) %>% 
    summarise(
      mean_flag4_plus_area = mean(flag4_plus_settlement, na.rm = TRUE)
    )
  
  data %>% 
    left_join(out, by = c(all_of(area_bounds)))
  
}


add_mean_proportion3_area <- function(data, admin_bounds) {
	area_bounds <- admin_bounds

	out <- data %>% 
		filter(is.na(severity_value)==F) %>% 
		select(-c(sector, type, question, severity_value, flag3,  contains('sector'))) %>% 
		unique() %>% 
		group_by(across(all_of(area_bounds))) %>% 
		summarise(
			flag3_area = sum(flag3_settlement, na.rm = TRUE),
			n = n(), 
			num_ind = sum(num_ind, na.rm=T)
			) %>% 
		mutate(
			mean_proportion3_area = ifelse(num_ind == 0, NA, round(flag3_area / num_ind * 100, 2))
		) %>% 
		select(contains('admin'), contains('proportion')) 
	
	data %>% 
		left_join(out, by = c(all_of(area_bounds)))
	
}

add_mean_proportion4_area <- function(data, admin_bounds) {
  area_bounds <- admin_bounds

  out <- data %>% 
    filter(is.na(severity_value)==F) %>% 
    select(-c(sector, type, question, severity_value, flag4,contains('sector'))) %>% 
    unique() %>% 
    group_by(across(all_of(area_bounds))) %>% 
    summarise(
      flag4_area = sum(flag4_settlement, na.rm = TRUE),
      n = n(), 
      num_ind = sum(num_ind, na.rm=T)
    ) %>% 
    mutate(
      mean_proportion4_area = ifelse(num_ind == 0, NA, round(flag4_area / num_ind * 100, 2))
    ) %>% 
    select(contains('admin'), contains('proportion')) 
  
  data %>% 
    left_join(out, by = c(all_of(area_bounds)))
  
}
add_mean_proportion4_plus_area <- function(data, admin_bounds) {
  area_bounds <- admin_bounds

  out <- data %>% 
    filter(is.na(severity_value)==F) %>% 
    select(-c(sector, type, question, severity_value,flag4_plus, contains('sector'))) %>% 
    unique() %>% 
    group_by(across(all_of(area_bounds))) %>% 
    summarise(
      flag4_plus_area = sum(flag4_plus_settlement, na.rm = TRUE),
      n = n(), 
      num_ind = sum(num_ind, na.rm=T)
    ) %>% 
    mutate(
      mean_proportion4_plus_area = ifelse(num_ind == 0, NA, round(flag4_plus_area / num_ind * 100, 2))
    ) %>% 
    select(contains('admin'), contains('proportion')) 
  
  data %>% 
    left_join(out, by = c(all_of(area_bounds)))
  
}


# add_area_proportion_25 <- function()
add_area_score_index_proportion_25 <- function(data, admin_bounds) {
	area_bounds <- admin_bounds

	
	area_proportions <- data %>%
		group_by(across(all_of(area_bounds)), severity_value) %>%
		summarise(n = n(), .groups = "drop") %>%
		group_by(across(all_of(area_bounds))) %>%
		mutate(N = sum(n)) %>%
		mutate(prop = round(n / N * 100, 4)) %>%
		group_by(across(all_of(area_bounds))) %>%
		arrange(across(all_of(area_bounds)), desc(severity_value)) %>%
		mutate(
			cumulative_prop = cumsum(prop),
			score_index_area = ifelse(length(na.omit(cumulative_prop)) > 1,
															approx(cumulative_prop, severity_value, xout = 25, rule = 2)$y,
															severity_value),
			# top10_severity = ifelse(length(na.omit(cumulative_prop)) > 1,
															# approx(cumulative_prop, severity_value, xout = 25, rule = 2)$y,
															# severity_value),
			score_index_area_rounded = round(score_index_area, 0)
			# ,
			# index10_severity = round(top10_severity, 0)
		) %>%
		ungroup()
	
	area_proportions_short <- area_proportions %>%
		select(all_of(area_bounds), score_index_area, score_index_area_rounded) %>%
		unique()
	
	data %>% 
		left_join(area_proportions_short, by = c(all_of(area_bounds)))

}



combine_tool <- function(survey=tool, responses=choices){
	survey <- survey %>% 
		select(-matches("^label$", ignore.case = TRUE)) %>% 
		rename_with(~ "label", .cols = matches("^label::english", ignore.case = TRUE))%>% 
		select( name,type, name, label, any_of(c("label_ar"="label::Arabic", "label_fr"="label::Francais")))%>%
		mutate(q.type = lapply(str_split(type, " "), function(x) x[[1]]) %>% unlist, 
					 list_name = sapply(str_split(type, "\\s+"), function(x) x[2]))
	
	survey %>% right_join(responses %>% distinct %>% 
													rename_with(~ "label.choice", .cols = matches("^label::english", ignore.case = TRUE)) %>% 
													select(list_name, name.choice=name, label.choice, any_of(c("label.choice_ar"="label::Arabic", "label.choice_fr"="label::Francais"))) 
												%>% 
													filter(!if_any(everything(), is.na)), multiple = "all") %>%
		filter(!is.na(name)) %>% bind_rows(survey %>% filter(type=="integer")) %>% distinct()
}

