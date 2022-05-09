patent_inventors <- patent_inventors %>% 
  filter(country_code == "CH") %>% # Filter to Swiss Patents
  mutate(Year = year(grant_date)) %>% 
  filter(Year >= 1997) %>% # Filter to patents granted after 1997
  unite("inv_name", last_name:first_name, sep = " ") # Combine inventor name columns 

patent_cite_counts <- patent_cite_counts %>% filter(grant_year >= 1997) %>% 
#Since patent_cite does not record country it is filtered based on patent_id from inventor data
  filter(patent_id %in% patent_inventors$patent_id) 

# Total citations count
total_citations_95pct <- patent_cite_counts %>% 
  group_by(patent_id) %>% 
  summarise(num_years = n(), all_citations = sum(citations)) %>% 
  mutate(citations_per_year = all_citations/num_years) %>% 
  # Patents above 95 percentile in citations per year
  filter(citations_per_year >= quantile(citations_per_year, 0.95))

# Now the most valuable patents are known, however, the names of the inventors (the star 
# inventors) who filed these patents are unknown. This requires a slightly circular approach
# to overcome the data structure:
# First subset patent_inventors to top 95 percentile based on the most valuable patent IDs
patent_inventors_top95 <- patent_inventors %>% 
  filter(patent_id %in% total_citations_95pct$patent_id)

# Second subset patent_inventors based on the inventor names from the valuable patents
patent_inventors <- patent_inventors %>% 
  filter(inv_name %in% patent_inventors_top95$inv_name)

# Subsetting patent class
patent_class <- patent_class %>%
  # These patent IDs are not necessarily most valuable but are all filed by the star inventors
  filter(patent_id %in% patent_inventors$patent_id) %>%
  mutate(intl_class = substr(intl_class, 1, 1)) %>% # Noting only the 8 major subclasses
  # Sometimes the same patent ID and class has two different inv positions this accounts for 
  # the duplicated data that does not provide additional information:
  distinct(patent_id, intl_class) 

# Merging patent class into patent inventors
patent_inventors <- merge(patent_inventors, patent_class, by = "patent_id")
