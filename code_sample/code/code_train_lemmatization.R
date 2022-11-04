#### code_train_lemmatization.R                                                ####
#### Ingests raw training data, lemmatizes using spacyr, exports lemmatized df ####

## Import State of State/Executive Speeches
data_raw = rio::import(here("data", "train_prestm.rds")) |> 
  filter(year_pooled >= 2000 & year_pooled <= 2018) |> 
  mutate(level_year = paste0(level, "_", year_pooled)) |> 
  rename(fed_level = level)

## Import State of the State Responses
data_raw_extra = rio::import(here("data","sos_response.rds")) |> 
  bind_rows(rio::import(here("data", "platforms10.rds"))) |> 
  mutate(level_year = paste0(fed_level, "_", year_pooled))

## Bind and Prepare for Lemmatization
data_raw_all = bind_rows(data_raw, data_raw_extra) |> 
  filter(text != "") |> 
  mutate(level_year = paste0(fed_level, "_", year_pooled),
         fed_level = as.factor(fed_level),
         year_pooled = as.factor(year_pooled)) |> 
  rownames_to_column(var = "doc_id") |> 
  mutate(text = str_remove_all(text, "â"),
         text = str_remove_all(text, "œ"))

## Lemmatize
data_spacy = data_raw_all |> 
  corpus(text_field = "text") |> 
  spacy_parse() |> 
  filter(pos != "PUNCT", pos != "SYM") 

data_lemmatized = data_spacy |> 
  mutate(lemma = tolower(lemma)) |> 
  filter(nchar(lemma) > 3) |> 
  group_by(doc_id) |> 
  summarise(text = str_c(lemma, collapse = " ")) |> 
  ungroup() |> 
  left_join(data_raw_all |> select(-text))

rio::export(data_lemmatized, here("data","data_lemmatized.rds"))
rm(data_raw, data_raw_all, data_spacy)