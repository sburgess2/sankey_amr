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
        nodes$name %in% c("Penicilinases", "blaTEM-1", "blaSHV-1") ~ "Penicillinases",
        nodes$name %in% c("IRT", "blaTEM-1") ~ "IRT",
        nodes$name %in% c("Oxcillinases", "blaOXA-1") ~ "Oxcillinases",
        nodes$name %in% c("ESBL", "blaTEM-10", "blaSHV-12", "blaCTX-M-15") ~ "ESBL",
        nodes$name %in% c("AmpC", "ampC*", "blaCMY-2", "blaDHA-1", "blaFOX-1", "blaACC-4") ~ "AmpC",
        nodes$name %in% c("Carbapenemases", "blaKPC-18", "blaNDM-1", "blaVIM-4", "blaGES-3") ~ "Carbapenemases",
        TRUE ~ "Other"
)

links$IDsource <- match(links$source, nodes$name) - 1
links$IDtarget <- match(links$target, nodes$name) - 1

# Generate vector containing the group names for all the antibiotic classes
subclass <- c("NS penicillins", "ES penicillins", "1G cephalosporins", "2G cephalosporins", "3G cephalosporins", "4G cephalosporins", "Carbapenems", "Monobactams")
# Generate vector containing the group names for all the mechanisms
enzymes <- c("Penicillinases", "IRT", "Oxcillinases", "ESBL", "AmpC", "Carbapenemases")

# Assign groups
links$group <- case_when(
        links$source %in% subclass ~ nodes$group[match(links$source, nodes$name)],
        links$source %in% enzymes ~ nodes$group[match(links$target, nodes$name)],
        TRUE ~ "Other"
)

my_color <- 'd3.scaleOrdinal()
  .domain(["Penicillinases", "IRT", "Oxcillinases", "ESBL", "AmpC", "Carbapenemases", "Other"])
  .range(["#90CAF9", "#A5D6A7", "#D1C4E9", "#ffffb3", "#fb8072", "#fdb462", "#E0E0E0"]);'

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
