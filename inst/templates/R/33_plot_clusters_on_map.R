
get_coord_norway <- function(data, col_municipality, col_address) {
	prep_data <-
		data %>%
		mutate(visit_number = as.integer(gsub("[^0-9/]*", "", col_address)),        # Gives warnings, but they are ok.
			   visit_street = str_replace(string = col_address, 
			   						   pattern = "([[:alpha:][:space:]]*) [0-9/]*$", # Extract anything before a final number
			   						   replacement = "\\1"), # \\1 refers to whatever is inside the parentheses above
			   visit_street = str_replace(string = visit_street, 
			   						   pattern = "[0-9\\-]*", # Remove any leftover numbers or dashes
			   						   replacement = ""),                    
			   visit_street = str_trim(visit_street),                                   # Remove whitespace from beginning and end of strings
			   URL =
			   	glue(.na = "",
			   		 "https://ws.geonorge.no/adresser/v1/sok?kommunenummer={col_municipality}&adressenavn={visit_street}&nummer={visit_number}&filtrer=adresser.representasjonspunkt",
			   	),
			   URL = str_replace(string = URL, pattern = "nummer=&", replacement = ""),
			   URL = as.character(URL),
			   URL = str_replace(string = URL, pattern = " ", replacement = "%20"))
	
	coord_data <-
		prep_data %>%
		dplyr::filter(!is.na(col_address)) %>%                                             # Remove empty addresses
		distinct(URL) %>%                                                             # Remove duplicate URLs
		pull(URL) %>%                                                                 # Take out URL column from dataset and make it a vector
		set_names() %>%                                                               # The vector will get names, being the same as itself
		map_dfr(., .id = "URL",
				.f =  function(.x) {                                  # For each of these URLs, do the following, and return a tibble (dataframe)
					try(silent = TRUE, expr = {                                                 # If it fails to lookup, continue without complaining
						print(.x)                                                                 # Print the URL to us (just so we can see what goes on)
						x <-
							read_html(x = .x) %>%                                                   # Visit the URL and read the HTML into R
							xml_text()                                                             # Take out the text from the HTML code
						lon <-
							str_extract(string = x, pattern = 'lon\":[0-9\\.]*') %>%                # Extract longitude from the text (+ some junk)
							str_remove(string = ., pattern = '.*:') %>%                             # Remove junk
							as.numeric()                                                            # Convert from character to numeric
						lat <-
							str_extract(string = x, pattern = 'lat\":[0-9\\.]*') %>%
							str_remove(string = ., pattern = '.*:') %>%
							as.numeric()
						
						tibble(long=lon, lat=lat)                                                 # Make a tibble (dataframe) with column names long and lat
						
					})})                                                                        # Return the tibble and continue the loop
	
	prep_data %>%
		select(!one_of("long", "lat")) %>% # If columns long and lat are already in dat_survey, removes them. In case one runs the same command twice.
		left_join(x = ., y = coord_data, by = "URL")
}


dat_survey <-
	dat_survey_raw %>%
	left_join(x = ., y = dat_gsi, by = c("skole" = "school")) %>%                 # Merges what can be found in dat_gsi to dat_survey_raw, keeping all rows in dat_survey_raw
	left_join(x = ., y = dat_sporringene, by = "gsiid") %>%
	mutate(visit_address =
		   	case_when(visit_address == "Bruhagen" ~ "Djupmyrveien 21",           # Fix some errors I discovered during the process
		   			  visit_address == "Sveberg" ~ "Svebergvegen 4",
		   			  TRUE ~ visit_address)) %>%
	get_coord_norway(col_municipality = municipality_number, col_address = visit_address)
