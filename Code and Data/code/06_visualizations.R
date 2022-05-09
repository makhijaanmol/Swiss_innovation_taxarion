# Patent Count Visualization
patent_time_trend <- ggplot(data = patent_count_table, 
                  mapping = aes(x = Year, y = total_national_patents)) +
  geom_point() +
  # geom_line() +
  geom_smooth(method = "lm", se = F) +
  theme_light() +
  labs(x = "Year", y = "Number of Patents", 
       title = "Patents Granted Per Year (1997-2010)") +
  scale_x_continuous(limits = c(1997,2010), breaks = seq(1997, 2010, 1)) +
  theme_clean() +
  theme(text = element_text(size = 10))

patent_time_trend

ggsave("Patents Filed Time Trend.pdf", patent_time_trend, 
       path = (str_interp("${output_dir}/Graphs")), width = 6.5, height = 4.2)
while (!is.null(dev.list())) dev.off()

# Tax Trend Visualization
tax_time_trend <- ggplot(data = patent_count_table, 
                         mapping = aes(x = Year, y = inc_500k)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Canton) +
  labs(x = "Year", y = "Income Tax Rate Above CHF 500,000", 
       title = "Cantonal Tax Trend (1997-2010)", 
       caption = "Note: Tax rates displayed for only those years in which patents were filed in the respective canton") +
  theme(text = element_text(size = 10))

tax_time_trend
ggsave("Cantonal Tax Trend.pdf", tax_time_trend, 
       path = (str_interp("${output_dir}/Graphs")), width = 6.5, height = 4.2)
while (!is.null(dev.list())) dev.off()

# Inventor Supply/ Tax Progression Visualization
inv_tax <- ggplot(data = patent_count_table, 
                  mapping = aes(x = tax_progress, y = num_inventors, color = Canton)) +
  geom_point() +
  facet_wrap(~Year) +
  theme_light() +
  geom_point(size = 0.01) + 
  theme(text = element_text(size = 8)) +
  labs(x = "Tax Progressitivity", y = "Number of Inventors", 
       title = "Inventor Supply and Tax Progressitivity")
inv_tax

ggsave("Inventor Tax Progressitivity.pdf", inv_tax, 
       path = (str_interp("${output_dir}/Graphs")), width = 6.7, height = 4.9)
while (!is.null(dev.list())) dev.off()

