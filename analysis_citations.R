library(tidyverse)
library(RPostgres)

dbDisconnect(db)
db = dbConnect(Postgres(),
               user = read.csv("")$user,
               password = read.csv("")$pw,
               host = read.csv("")$host,
               port = ,
               dbname = read.csv("v")$dbname,
               options=paste("-c search_path=",read.csv("")$schema,sep = ""))



# citations by year

dbReadTable(db, "citing_articles") %>% 
  group_by(pub_year) %>% 
  summarize(n = n()) %>% 
  arrange(pub_year) %>% 
  filter(pub_year > 1969) %>% 
  ggplot() +
  geom_col(aes(y=n, x=pub_year)) +
  ylab("Number of citations") +
  xlab("Year") +
  theme_minimal()

ggsave("tables and figures/n_citations_by_year.tiff", width = 7, height = 4, device='tiff', dpi=600)

# citations by source

dbReadTable(db, "citing_articles") %>% 
  filter(pub_year > 1969) %>% 
  group_by(source) %>% 
  summarize(n = n()) %>% 
  arrange(desc(n)) %>% 
  filter(n>1) %>% 
  top_n(25) %>% 
  ggplot() +
  geom_col(aes(x=n, y=reorder(source, n))) +
  #  geom_text(aes(x=n, y=reorder(source, n), label = n), hjust = -0.5) +
  scale_x_continuous(breaks = seq(0, 600, 100)) +
  xlab("Number of references") +
  ylab("Citing journal") +
  theme_minimal()


ggsave("tables and figures/n_citations_by_journal.tiff", width = 7, height = 4, device='tiff', dpi=600)







