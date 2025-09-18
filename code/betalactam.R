library(networkD3)
library(tidyverse)
library(htmlwidgets)
library(manipulateWidget)
library(webshot2)

links <- read_csv("data/betalactams.csv")

nodes <- data.frame(
        name = unique(c(as.character(links$source), as.character(links$target)))
)


nodes$group <- case_when(
        nodes$name %in% c("Penicillinases", "blaTEM-1", "blaSHV-1") ~ "Penicillinases",
        nodes$name %in% c("IRT", "blaTEM-30") ~ "IRT",
        nodes$name %in% c("Oxcillinases", "blaOXA-1") ~ "Oxcillinases",
        nodes$name %in% c("ESBL", "blaTEM-10", "blaSHV-12", "blaCTX-M-15", "blaROB-1", "blaGES-3") ~ "ESBL",
        nodes$name %in% c("AmpC", "ampC*", "blaCMY-2", "blaDHA-1", "blaFOX-1", "blaACC-4") ~ "AmpC",
        nodes$name %in% c("Carbapenemases", "blaKPC-18", "blaNDM-1", "blaVIM-4") ~ "Carbapenemases",
        nodes$name %in% c("Reduced permeability", "ompC*", "ompF*") ~ "Reduced permeability",
        nodes$name %in% c("Efflux pump", "acrAB-tolC*") ~ "Efflux pump",
        TRUE ~ "Other"
)

links$IDsource <- match(links$source, nodes$name) - 1
links$IDtarget <- match(links$target, nodes$name) - 1

# Generate vector containing the group names for all the antibiotic classes
# subclass <- c("NS penicillins", "ES penicillins", "1G cephalosporins", "2G cephalosporins", "3G cephalosporins", "4G cephalosporins", "Carbapenems", "Monobactams")
# Generate vector containing the group names for all the mechanisms
mechanisms <- c("Penicillinases", "IRT", "Oxcillinases", "ESBL", "AmpC", "Carbapenemases", "Reduced permeability", "Efflux pump")
genes <- c("ompC*", "ompF*", "acrAB-tolC*","blaTEM-1", "blaSHV-1", "blaTEM-30", "blaOXA-1", "blaTEM-10", "blaSHV-12", "blaCTX-M-15",
           "blaROB-1", "ampC*", "blaCMY-2", "blaDHA-1", "blaFOX-1", "blaACC-4", "blaKPC-18", "blaNDM-1",
           "blaVIM-4", "blaGES-3")

# Assign groups
links$group <- case_when(
        links$source %in% mechanisms ~ nodes$group[match(links$source, nodes$name)],
        links$source %in% genes ~ nodes$group[match(links$source, nodes$name)],
        TRUE ~ "Other"
)
my_color <- 'd3.scaleOrdinal()
  .domain(["Reduced permeability", "Efflux pump", "Penicillinases", "IRT", "Oxcillinases", "ESBL", "AmpC", "Carbapenemases", "Other"])
  .range(["#A6E1F4FF", "#008ECEFF", "#FFECB3FF", "#FFD54FFF", "#FFC107FF", "#FFA000FF", "#FF6F00FF", "#DA291CFF",  "#E0E0E0"]);'

"#A6E1F4FF", "#008ECEFF", "#FFECB3FF", "#FFD54FFF", "#FFC107FF", "#FFA000FF", "#FF6F00FF", "#5D2A2CFF", "#DA291CFF",  "#E0E0E0"


"#FFB300FF", "#FFA000FF","#FF8F00FF", "#FF6F00FF"


q <- sankeyNetwork(
        Links = links, Nodes = nodes,
        Source = "IDsource", Target = "IDtarget",
        Value = "value", NodeID = "name",
        NodeGroup = "group", LinkGroup = "group",
        colourScale = my_color,
        width = 900, height = 900,
        nodeWidth = 60
)

q

paletteer_d("unikn::pal_seeblau")
paletteer_d("calecopal::eschscholzia")
paletteer_d("nbapalettes::jazz_city")
paletteer_d("ggsci::amber_material")

#remotes::install_github("davidsjoberg/ggsankey")
library(ggsankey)

#Tutorial for the ggplot version of sankey https://r-graph-gallery.com/package/ggsankey.html

df <- read_csv("data/betalactams_gsankey.csv")
#Add a new column called x, where x == 1 if in the vector genes otherwise 2
#gsankey <- links %>%
 #       mutate(x = ifelse(source %in% genes, 1, 2)) %>%
  #      mutate(next_x = ifelse(target %in% mechanisms, 2, 3))


df_long <- df %>%
        make_long(gene, mechanism, subclass)

#Space alters space between rows/nodes in the y-direction
ggplot(df_long, 
       aes(x = x, 
           next_x = next_x, 
           node = node, 
           next_node = next_node,
           fill = factor(node))) +
        geom_sankey(show.legend = FALSE, space = 3) +
        geom_sankey_label(data = df_long, aes(label = node)) +
        theme_void()

cg <- read_csv("data/class_group.csv")

cg_long <- cg %>%
        pivot_longer(cols = c("Amber class", "Functional group"),
                     names_to = "Category", values_to = "Value")

ggplot(data = cg_long, aes(x = Category, y = Enzyme, fill = Value)) +
        geom_tile(color = "white", size = 0.8) +
        scale_fill_viridis_d(name = "Class/Group") +
        scale_x_discrete(position = "top") +
        theme_minimal() +
        theme(
                axis.title = element_blank(),
                axis.text.x = element_text(hjust = 0),
                axis.text.y = element_text(size = 8),
                panel.grid = element_blank(),
                legend.position = "right"
        ) +
        labs(title = "Annotations")
