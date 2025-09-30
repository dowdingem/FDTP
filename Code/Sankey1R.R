install.packages("networkD3")
install.packages("dplyr")
install.packages("circlize")



library(networkD3)
library(dplyr)
library(circlize)
library(viridis)  #

#Change this to whatever
setwd("C:/Users/xo11zube/Desktop/FDTP")

# Load dataset
data <- read.csv("Sankey.csv", header=TRUE)


#For clarity, not efficiency
df <- data.frame(
  Proxy = data$Proxy, #This is the broad category from the webform
  Tag1  = data$Tag1, #This is a label I gave the data to reduce complexity. To be condensed further
  OOp = data$Oop #OOp is the number of times the team said the same thing according to the tags I gave)
)

# Create node list (unique values from both columns, you will note there are a lot of double ups)
nodes <- data.frame(name = unique(c(df$Proxy, df$Tag1)))

# Map Proxy/Tag1 values to node indices (0-based for networkD3)
links <- df %>%
  mutate(
    source = match(Proxy, nodes$name) - 1,
    target = match(Tag1, nodes$name) - 1,
    value  = OOp 
  ) %>%
  select(source, target, value)

# Build sankey diagram
sankeyNetwork(
  Links = links, Nodes = nodes,
  Source = "source", Target = "target",
  Value = "value", NodeID = "name",
  fontSize = 15, nodeWidth = 30
)



########################################################################################
#Chord Diagram..  This bit is a mess. But there are lots of choices

library(circlize) # :: 

#Matrix
mat <- xtabs(OOp ~ Proxy + Tag1, data = df)

# Plot basic default chord diagram. Colours change each time.
chordDiagram(mat, transparency = 0.8) #Looks better more transparent. Adjust at will
circos.clear()



# If we want non random colours... Viridis or rainbow. Define colours for all categories (both Proxy + Tag1 levels)
#all_groups <- c(levels(df$Proxy), levels(df$Tag1))
#grid_col <- setNames(
#  rainbow(length(all_groups)),  # or RColorBrewer::brewer.pal
#  all_groups
#)

# All sector names (row + column)
#all_sectors <- union(rownames(mat), colnames(mat))

# Assign colors as a named vector
#grid_col <- setNames(rainbow(length(all_sectors)), all_sectors)
#grid_col <- setNames(viridis(length(all_sectors)), all_sectors)

#Spaces between segments?
#circos.par(gap.after = c(rep(5, nrow(mat)-1), 15, rep(5, ncol(mat)-1), 15))

# Pre-allocate space for labels
chordDiagram(
  mat,
  #grid.col = grid_col, #remove # if you want to see the selected colour palettes 
  transparency = 0.6,
  annotationTrack = "grid",       # show sector grid
  preAllocateTracks = list(track.height = 0.3)  # space for labels
)

# Customize labels
circos.trackPlotRegion(
  track.index = 1,
  panel.fun = function(x, y) {
    sector_name <- get.cell.meta.data("sector.index")
    circos.text(
      CELL_META$xcenter,
      CELL_META$ylim[1] - 0.1,  # push text outside
      sector_name,
      facing = "clockwise",      # rotate along circle
      niceFacing = TRUE,
      adj = c(0, 0.5),
      cex = 0.9,       # smaller text
      font=2
    )
  },
  bg.border = NA
)

circos.clear()


