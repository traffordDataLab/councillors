# Trafford councillors by ward #

# Source: Trafford Council
# URL: https://democratic.trafford.gov.uk/mgMemberIndex.aspx
# Licence: OGL v3.0

library(tidyverse) ; library(rvest)

webpage <- read_html("https://democratic.trafford.gov.uk/mgMemberIndex.aspx?FN=WARD&VW=TABLE&PIC=1")

# retrieve basic information
info <- webpage %>% 
  html_node("#mgTable1") %>%
  html_table(header = NA) %>% 
  as_tibble() %>% 
  mutate(Name = str_remove_all(Councillor, "\\\r\n.*"),
         Ward = str_remove_all(Ward, " Ward"),
         Telephone = str_extract_all(Councillor, "(?<=: )(.*)(?= \t)"),
         Email = str_extract_all(Councillor, paste(c("(?<=\tWork: )(.*)","(?<=\t\r\n\t\t\t\t\tHome: )(.*)"), collapse="|"))) %>%
  unnest_wider(Telephone, names_sep = "#") %>% 
  unite(Telephone, c(`Telephone#1`,`Telephone#2`,`Telephone#3`), sep = ", ", remove = TRUE) %>%
  mutate(Telephone = str_replace_all(Telephone,  ", NA", replacement = "NA")) %>% 
  mutate(Telephone = str_remove_all(Telephone,  "NA")) %>% 
  unnest(Email, keep_empty = TRUE) %>% 
  select(Name, Party = `Political party`, Ward, Telephone, Email)

# retrieve personal pages
url <- webpage %>%
  html_nodes("p:nth-child(1) a") %>%
  html_attr("href")
text <- webpage %>%
  html_nodes("p:nth-child(1) a") %>%
  html_text()
page <- tibble(Name = text, Page = url) %>% 
  mutate(Page = paste0("https://democratic.trafford.gov.uk/", Page))

# retrieve images
url <- webpage %>%
  html_nodes(".mgCouncillorImages") %>%
  html_attr("src")
text <- webpage %>%
  html_nodes(".mgCouncillorImages") %>%
  html_attr("alt")
image <- tibble(Name = text, Image = url) %>% 
  mutate(Name = str_remove(Name, "photo of "),
         Image = paste0("https://democratic.trafford.gov.uk/", Image))

# join datasets and update ward names
councillors <- left_join(info, page, by = "Name") %>% 
  left_join(image, by = "Name") %>% 
  mutate(Ward = case_when(
    Ward == "Ashton Upon Mersey" ~ "Ashton upon Mersey",
    Ward == "Bucklow St. Martins" ~ "Bucklow-St Martins",
    TRUE ~ Ward))

# write results
write_csv(councillors, "councillors.csv")
