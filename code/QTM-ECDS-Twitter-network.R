###########################################################################
# Josh Fjelstul, Ph.D.
# QTM-ECDS Workshops, Coding in R Series
# Social Media
###########################################################################

# libraries
library(stringr)
library(dplyr)
library(tidyr)
library(rtweet)
library(ggraph)
library(igraph)
library(ggplot2)
library(visNetwork)

###########################################################################
# New York Times data
###########################################################################

# get accounts that follow the NYT
nyt_friends <- get_friends("nytimes")

# lookup users that follow the NYT
nyt_friends_metadata <- lookup_users(nyt_friends$user_id)

# select accounts of verified users who work for the NYT in New York
nyt_friends_metadata <- nyt_friends_metadata %>% 
  select(user_id, screen_name, name, location, description, verified, followers_count, friends_count) %>%
  filter(verified == TRUE & str_detect(description, "NYT|New York Times|@nytimes") & str_detect(location, "NY|New York"))

# get timelines of those users (last 100 tweets)
nyt_timelines <- get_timelines(nyt_friends_metadata$screen_name, n = 100)

# get variables
nyt_timelines <- nyt_timelines %>% 
  select(user_id, screen_name, text, reply_to_screen_name, mentions_screen_name, retweet_screen_name)

# clean mentions
nyt_timelines$mentions_screen_name <- nyt_timelines$mentions_screen_name %>% 
  as.character() %>% 
  str_replace("^c", "") %>%
  str_replace_all("[\"()]+", "")

# drop row names
rownames(nyt_timelines) <- NULL

# clean
nyt_timelines <- select(nyt_timelines, user_id, screen_name, text, reply_to_screen_name, mentions_screen_name, retweet_screen_name)

###########################################################################
# convert API data to network data
###########################################################################

# set your working directory to the root folder for this workshop

# read in NYT data
edges <- read.csv("data/nyt-timelines.csv", stringsAsFactors = FALSE)

# make an edges dataset
edges <- select(nyt_timelines, screen_name, reply_to_screen_name, mentions_screen_name, retweet_screen_name) %>%
  rename(user = screen_name, reply = reply_to_screen_name, mention = mentions_screen_name, retweet = retweet_screen_name)

# covert NA into "NA"
edges$reply <- str_replace_na(edges$reply)
edges$mention <- str_replace_na(edges$mention)
edges$retweet <- str_replace_na(edges$retweet)

# gather data into one column
edges <- gather(edges, key = "type", value = "to", reply, mention, retweet) %>% 
  rename(from = user) %>% 
  select(from, to, type)

# separate lists into their own rows
edges <- separate_rows(edges, to, sep = ", ")

# clean edges data
edges <- edges %>%
  filter(to != "NA" & from != to & to %in% edges$from) %>%
  group_by(from, to, type) %>%
  summarize(count = n()) %>%
  ungroup()

# data frame of reply edges
edges <- filter(edges, type == "reply")

# data frame of nodes
nodes <- data.frame(name = unique(c(edges$from, edges$to)))

# make a graph object (igraph)
graph <- graph_from_data_frame(edges, directed = TRUE, vertices = nodes)

###########################################################################
# network plots (ggraph/ggplot)
###########################################################################

# make a network graph
plot <- ggraph(graph, layout = "fr") +
  geom_edge_link(aes(colour = factor(count)), 
                 arrow = arrow(length = unit(2, "mm"), type = "open"), 
                 start_cap = circle(2, "mm"), 
                 end_cap = circle(2, "mm"), 
                 edge_width = 0.5) +
  geom_node_point(color = "black", size = 1) +
  geom_node_text(aes(label = nodes$name), 
                 size = 2.5, 
                 hjust = 0, vjust = 1, 
                 nudge_x = 0.1, nudge_y = -0.1, 
                 colour = "black") +
  scale_edge_color_discrete(name = "Number of Replies") +
  theme_graph()
plot

# curved arrows
plot <- ggraph(graph, layout = "fr") +
  geom_edge_fan(aes(colour = factor(count)),
                arrow = arrow(length = unit(2, "mm"), type = "open"),
                start_cap = circle(2, "mm"),
                end_cap = circle(2, "mm"),
                edge_width = 0.5) +
  geom_node_point(color = "black", size = 1) +
  geom_node_text(aes(label = nodes$name), 
                 size = 2.5, 
                 hjust = 0, vjust = 1, 
                 nudge_x = 0.1, nudge_y = -0.1, 
                 colour = "black") +
  scale_edge_color_discrete(name = "Number of Replies") +
  theme_graph()
plot

# add density
plot <- ggraph(graph, layout = "fr") +
  geom_edge_density(fill = "gray75") +
  geom_edge_fan(aes(colour = factor(count)), 
                arrow = arrow(length = unit(2, "mm"), type = "open"), 
                start_cap = circle(2, "mm"), 
                end_cap = circle(2, "mm"), 
                edge_width = 0.5) +
  geom_node_point(color = "black", size = 1) +
  geom_node_text(aes(label = nodes$name), 
                 size = 2.5, 
                 hjust = 0, vjust = 1, 
                 nudge_x = 0.1, nudge_y = -0.1, 
                 colour = "black") +
  scale_edge_color_discrete(name = "Number of Replies") +
  theme_graph()
plot

# prepare data for a vizNetwork plot
nodes <- nodes %>% rename(id = name)

###########################################################################
# interactive network plots (visNetwork)
###########################################################################

# interactive plot
plot <- visNetwork(nodes, edges, background = "white") %>%
  visNodes(color = list(background = "#56C1FF", border = "#515151", highlight = list(background = "#FF644E", border = "#515151")),
           borderWidth = 1,
           labelHighlightBold = FALSE,
           shapeProperties = list(borderRadius = 5),
           shape = "box",
           size = 10) %>%
  visEdges(physics = TRUE,
           smooth = list(enabled = FALSE),
           color = list(color = "#D6D6D6", highlight = "#515151"),
           width = 1,
           arrows = list(middle = list(enabled = TRUE, scaleFactor = 1))) %>%
  visOptions(highlightNearest = list(enabled = TRUE, hideColor = "#D6D6D6", degree = 1, labelOnly = FALSE))
plot

# curved lines
plot <- visNetwork(nodes, edges, background = "white") %>%
  visNodes(color = list(background = "#56C1FF", border = "#515151", highlight = list(background = "#FF644E", border = "#515151")),
           borderWidth = 1,
           labelHighlightBold = FALSE,
           shapeProperties = list(borderRadius = 5),
           shape = "box",
           size = 10) %>%
  visEdges(physics = TRUE,
           smooth = list(enabled = TRUE),
           color = list(color = "#D6D6D6", highlight = "#515151"),
           width = 1,
           arrows = list(middle = list(enabled = TRUE, scaleFactor = 1))) %>%
  visOptions(highlightNearest = list(enabled = TRUE, hideColor = "#D6D6D6", degree = 1, labelOnly = FALSE))
plot

###########################################################################
# end R script
###########################################################################