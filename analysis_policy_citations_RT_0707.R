library(tidyverse)
library(RPostgres)

#dbDisconnect(db)
db = dbConnect(Postgres(),
               user = read.csv("admin/db_connection.csv")$user,
               password = read.csv("admin/db_connection.csv")$pw,
               host = read.csv("admin/db_connection.csv")$host,
               port = 5432,
               dbname = read.csv("admin/db_connection.csv")$dbname,
               options=paste("-c search_path=",read.csv("admin/db_connection.csv")$schema,sep = ""))


a<-dbReadTable(db, "report_citing_policy_document")
b<-dbReadTable(db, "citing_policy_document")

data<-a %>%
  rename_at("Table3.Citing_report_id", ~'Citing_report_id') %>%
  inner_join(b, by="Citing_report_id")

# citations by year
colnames(data)
data %>%
  filter(is.na(If_GESAMP_id)) %>%
  mutate(pub_year = as.integer(str_sub(Published,1,4))) %>% 
  group_by(pub_year) %>% 
  summarize(n = n()) %>% 
  arrange(pub_year) %>% 
  filter(pub_year > 1969) %>%
  ggplot() +
  geom_col(aes(y=n, x=pub_year)) +
  ylab("Number of citations") +
  xlab("Year") +
  theme_minimal()

ggsave("tables and figures/n_policy_citations_by_year_updated.tiff", width = 7, height = 4, device='tiff', dpi=600)

# citations by source

data %>%
  filter(is.na(If_GESAMP_id)) %>%
  mutate(pub_year = as.integer(str_sub(Published,1,4))) %>% 
  filter(pub_year > 1969) %>%
  rename(source = Source.Name) %>% 
  group_by(source) %>% 
  summarize(n = n()) %>% 
  arrange(desc(n)) %>% 
  filter(n>6) %>% 
  ggplot() +
  geom_col(aes(x=n, y=reorder(source, n))) +
  #  geom_text(aes(x=n, y=reorder(source, n), label = n), hjust = -0.5) +
  xlab("Number of references") +
  ylab("Citing source") +
  theme_minimal()


ggsave("tables and figures/n_policy_citations_by_source_updated.tiff", width = 10, height = 10, device='tiff', dpi=600)







