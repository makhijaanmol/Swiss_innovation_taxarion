# Mobile Inventor Panel
canton_list <- data.frame(c("Zurich", "Bern", "Luzern", "Uri", "Schwyz", "Obwalden", "Nidwalden", "Glarus", "Zug",
                 "Fribourg", "Solothurn", "Basel-Stadt", "Basel-Land", "Schaffhausen", "Appenzell A.Rh.",
                 "Appenzell I.Rh.", "St.Gallen", "Graubünden", "Aargau", "Thurgau", "Ticino", "Vaud",
                 "Valais", "Neuchätel", "Genève", "Jura"))
names(canton_list)[1] <- "Alternatives"

 # Balancing the panel forward
inventor_choice_panel <-  patent_inventors %>% 
  # Balance panel with inv/year index
  complete(nesting(inv_name), Year = full_seq(Year, period = 1)) %>%
  subset(select = -c(5:10)) %>% 
  arrange(inv_name, Year) %>% 
  group_by(inv_name) %>% 
  fill(Canton, intl_class, most_dominant_class, second_dominant_class) %>% # Fill forward
  drop_na(Canton, intl_class, most_dominant_class, second_dominant_class) %>% #drop backward NAs
  ungroup() %>% 
  # is the patent filed in the same region its classification is dominant
  mutate(patent_in_dominant = case_when(intl_class == most_dominant_class ~ 1,
                                        intl_class == second_dominant_class ~ 1,
                                        TRUE ~ 0)) %>% 
  group_by(inv_name, Year) %>% 
  slice(n()) # Drop repeated inv_name/year panels if inv files multiple pantents in a year or 
# a patent with multiply classifications

inventor_choice_panel <- merge(inventor_choice_panel, canton_list) %>%
  mutate(choice = case_when(Canton == Alternatives ~ 1, # Set up choice for mlogit model
                             TRUE ~ 0)) %>% 
  merge(canton_year_taxes, by.x = c("Year", "Alternatives"), by.y = c("Year", "Canton")) %>% 
  mutate(ID = str_c(inv_name, Year)) %>% #Inventor-Year case ID - 8537 distinct cases 
  subset(select = c("ID", "Alternatives", "choice", "patent_in_dominant",
                    "inc_100k", "inc_500k", "tax_progress"))
  n_distinct(inventor_choice_panel$ID)

# # Getting data ready to be analyzed in a multinominal logit model
# mlogit_inventor <- mlogit.data(inventor_choice_panel,
#                           choice = 'choice',
#                           shape = 'long',
#                           alt.var = 'Alternatives',
#                           id.var = 'ID')

model <- mlogit(choice ~ tax_progress | 0, 
                data = inventor_choice_panel)

model_1 <- mlogit(choice ~ inc_500k | 0, 
                data = inventor_choice_panel)

model_2 <- mlogit(choice ~ inc_100k | 0, 
                  data = inventor_choice_panel)

summary(model)
model_rrr <- exp(coef(model))

# Not sure if mean patent in dominant makes sense - followed from youtube video but they had
# a numerical case specific variable mine is a binary case specific variable
z <- with(inventor_choice_panel, data.frame(tax_progress = tapply(tax_progress, 
                                                                   index(model)$Alternatives, 
                                                                  mean),
                                            patent_in_dominant = mean(patent_in_dominant)))
# Conditional logit model tax progression marginal effects
tax_progress_effects_matrix <- effects(model, covariate = "tax_progress", data = z)
marginal_effects_model <- diag(tax_progress_effects_matrix) # Shows marginal effect of 1% increase in taxes (if interpret correct)
# Read as: 1% increase in taxes decreases the probability of location in given Canton by -1.32%

marginal_effects_model
stargazer(marginal_effects_model,
          type="html", digits = 2,
          out = str_interp("${output_dir}/marginal_effects_model.htm"))

tax_progress_national_marginal_effect <- mean(diag(tax_progress_effects_matrix))
tax_progress_national_marginal_effect

# # Conditional logit model dominant patent location marginal effects
# Gives Error: Error in if (rhs %in% c(1, 3)) { : argument is of length zero
# Error possibly since it is a binary variable
# dominant_patent_effects_matrix <- effects(model, covariate = "patent_in_dominant", data = z)
# diag(dominant_patent_effects_matrix) # Shows marginal effect of 1% increase in taxes (if interpret correct)
# # Read as: 1% increase in taxes decreases the probability of location in given Canton by -1.32%
# 
# tax_progress_national_marginal_effect <- mean(diag(tax_progress_effects_matrix))
# tax_progress_national_marginal_effect

# Hauseman-McFadden test of independence of irrelevant alternatives 
model_3 <- mlogit(choice ~ tax_progress | 0, data = inventor_choice_panel, 
                  alt.subset = c("Zurich", "Bern", "Luzern", "Uri", "Schwyz", "Obwalden", 
                                 "Nidwalden", "Glarus", "Zug","Fribourg", "Solothurn", 
                                 "Basel-Stadt", "Basel-Land", "Schaffhausen", "Appenzell A.Rh.",
                                 "Appenzell I.Rh.", "St.Gallen", "Graubünden", "Aargau", 
                                 "Thurgau", "Ticino", "Vaud","Valais", "Neuchätel", "Genève"))

hmftest(model, model_2) # IIA rejected however several papers may lead to an invalid 
# statistical inference even when the test value is positive (https://ftp.iza.org/dp5826.pdf)

# Final output of all regression tables
stargazer(model, model_1, model_2,model_3,
          type="html", p.auto=FALSE, out = str_interp("${output_dir}/model.htm"))

