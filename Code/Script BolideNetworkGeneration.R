library(tidyverse); library(igraph); library(ggraph); library(tidygraph)
library(fs); library(magrittr)

## read in host-virus pairs -----

Edges <- read.csv("Data/DataNetwork.csv") %>% 
  group_by(Virus) %>% 
  mutate(nVirus=sum(n)) %>% 
  ungroup() %>% 
  group_by(Mosquito) %>% 
  mutate(nMosquito=sum(n)) %>% 
  ungroup()

## theme ----- 
pal <- MetBrewer::met.brewer('Kandinsky')

## unipartite network ---- 

NodesV <- Edges %>% select(Virus, nVirus) %>% 
  rename(n=nVirus, name=Virus) %>% 
  mutate(NodeCategory="Virus") %>% 
  distinct # keep one row per virus / count - 35 unique 

NodesM <- Edges %>% select(Mosquito, nMosquito) %>% 
  rename(n=nMosquito, name=Mosquito) %>% 
  mutate(NodeCategory="Mosquito") %>% 
  distinct # keep one row per vector / count - 122 unique 

Nodes <- bind_rows(NodesM, NodesV) %>% 
  as.data.frame() %>% 
  mutate(TextSize=sqrt(n)*5) %>% as.data.frame()

Edges %>% 
  select(Mosquito:n) %>% 
  rename(from=Mosquito, to=Virus, weights=n) %>% 
  graph_from_data_frame(., directed = F, Nodes) -> G 

layout = create_layout(G, layout = "dh")
  
set.seed(333)

G %>% as_tbl_graph %>% 
  ggraph(layout) + 
  geom_edge_link(aes(width=weights), alpha=0.5, edge_colour="grey") +
  geom_node_point(aes(size=n, 
                      #shape=NodeCategory, 
                      #alpha=n, 
                      fill=NodeCategory),
                  shape=21, 
                  colour="white", 
                  alpha=0.8) +
  geom_node_text(aes(label = name, size=TextSize, 
                     colour=NodeCategory), 
                 repel=TRUE, show.legend = F) + 
  theme_graph() + 
  scale_colour_manual(values=pal[2:1]) + 
  scale_fill_manual(values=pal[2:1]) + 
  scale_edge_width(range = c(0.3, 6)) +  # control edge width 
  theme(legend.position = "none") -> Net


G %>% as_tbl_graph %>% 
  ggraph(layout) + 
  geom_edge_link(aes(width=weights), alpha=0.6, edge_colour="grey") +
  geom_node_point(aes(size=n, 
                      #shape=NodeCategory, 
                      #alpha=n, 
                      fill=NodeCategory),
                  shape=21, 
                  colour= "black", 
                  alpha=0.8) +
  geom_node_text(aes(label = name, size=TextSize, 
                     colour=NodeCategory), 
                 repel=TRUE,   
                 show.legend = F) + 
  theme_graph() + 
  scale_colour_manual(values=pal[2:1]) + 
  scale_fill_manual(values=pal[2:1]) + 
  scale_edge_width(range = c(0.3, 6)) -> NetLegend 

## save to pdfs ---- 

# Network only 
Net + ggsave("Figures/Figure X Network.pdf", 
             width = 10, height = 8, units="in", dpi=600, device="pdf")


# Network with legend  - this is not working come back to 
NetLegend + ggsave("Figures/Figure X NetworkLegend.png", 
             width = 11, height = 8, units="in", dpi=600) # will fix to be pdf 


