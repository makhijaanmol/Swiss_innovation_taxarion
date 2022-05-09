# Now that the cities/munipalities/streets are assigned to Cantons the dominant patent type in
# each canton every year can be analyzed:
dominant_patent_class <- patent_inventors %>% 
  group_by(Year, Canton) %>% 
  count(intl_class, sort = TRUE) %>% # Frequency of patent class per year and canton sorted
  arrange(Canton, .by_group = TRUE) %>% # Dataset arranged in ascending order of Canton
  mutate(most_dominant_class = intl_class[1], # Most Frequent patent type
         second_dominant_class = intl_class[2]) %>% # Second most frequent patent type
  subset(select = -c(intl_class, n))

dominant_patent_class <- unique(dominant_patent_class [ , 1:4]) # One record per year/Canton

patent_inventors <- merge(patent_inventors, dominant_patent_class, by = c("Year", "Canton")) %>% 
  subset(select = -c(middle_name, suffix, street, city, state, postal_code, 
                     country, bea_code))

# Creating count of number of inventors and patents at the Canton-Year level
patent_count_table <- patent_inventors %>% 
  group_by(Year, Canton) %>%
  mutate(num_inventors = n_distinct(inv_name)) %>%
  mutate(num_patents = n_distinct(patent_id)) %>%
  ungroup() %>% 
  group_by(Year) %>% 
  mutate(total_national_inventors = n_distinct(inv_name)) %>%
  mutate(total_national_patents = n_distinct(patent_id))

patent_count_table <- unique(patent_count_table [ , c(1:2, 12:17)])

# Merging tax and patent data
canton_year_taxes <- canton_year_taxes[,-c(3:12, 14:16, 18)] 
canton_year_taxes <- canton_year_taxes %>% 
  mutate(tax_progress = log(1-(inc_500k/100)) - log(1 - (inc_100k/100)))

patent_count_table <- merge(patent_count_table, canton_year_taxes, by = c("Year", "Canton"))

# Count Mobility of Inventors
mobile_inventors <- unique(patent_inventors[ ,c("inv_name", "Canton")])
mobile_inventors <- mobile_inventors[mobile_inventors$inv_name %in% 
                                       mobile_inventors$inv_name[duplicated(mobile_inventors$inv_name)],]
n_distinct(mobile_inventors$inv_name)

mobile_inventors <- patent_inventors %>%
  filter(inv_name %in% mobile_inventors$inv_name)

