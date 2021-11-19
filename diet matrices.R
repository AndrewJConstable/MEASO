# Ecopath models

# 
Path_ecopath<-"/Users/andreworca/Desktop/_w/_p/MEASO/02_research/Ecosystem network synthesis/"
###############################################
# Dahood 2019 

# Nodes

nodes_Dahood<-read.csv(paste(Path_ecopath,"Nodes-Dahood2019.csv",sep=""))
nodes_Dahood[8,"B"]<-148 # needed correction to 0.1x as per Roshni.

# <1% was changed to 0.1%

diet_Dahood<-list(
    Detritus   = NULL
  , IceAlgae   = NULL
  , LargePhyto = NULL
  , SmallPhyto = NULL
  , Macrozoo   = matrix(c(
                   1  , 10 # large krill
                 , 2  ,  9 # small krill
                 , 1  ,  8 # other euphausiids
                 ,50  ,  6 # mesozooplankton
                 ,10  ,  4 #Small	phytoplankton
                 ,21  ,  3 # large phytoplankton	
                  ),ncol=2,byrow=TRUE) # end matrix
  , Mesozoo    = matrix(c( 
                   3  ,  7 # microzooplankton
                 ,24  ,  4 # small phytoplankton
                 ,66  ,  3 # large phytoplankton
                 , 7  ,	 1 # detritus	
                 ),ncol=2,byrow=TRUE) # end matrix
  , Microzoo   = matrix(c( 
                  60  ,  4 # small phytoplankton
                 ,25  ,  3 # large phytoplankton
                 ,15  ,  1 # detritus	
                 ),ncol=2,byrow=TRUE) # end matrix
  , KrillOther = matrix(c(
                  20  ,  6 # mesozooplankton
                 ,60  ,  3 # large phytoplankton
                 ,20  ,  1 # detritus	
                 ),ncol=2,byrow=TRUE) # end matrix
  , KrillSmall = matrix(c( 
                  10  ,  7 # microzooplankton
                 ,27.5,  4 # small phytoplankton
                 ,27.5,  3 # large phytoplankton
                 ,25  ,  2 # ice algae
                 ,10  ,  1 # detritus	
                 ),ncol=2,byrow=TRUE) # end matrix
  ,KrillLarge  = matrix(c( 
                  10  ,  6 # mesozooplankton
                 ,50  ,  3 # large phytoplankton
                 ,10  ,  2 # ice algae
                 ,30  ,  1 # detritus	
                 ),ncol=2,byrow=TRUE) # end matrix
  ,BenthInvert = matrix(c(
                 100  ,  1 # detritus	
                 ),ncol=2,byrow=TRUE) # end matrix
  ,Salps       = matrix(c( 
                   0.1,  9 # small krill
                 ,10.4,  7 # microzooplankton
                 , 3  ,  6 # mesozooplankton
                 ,41.5,  4 # Small	phytoplankton
                 ,45  ,  3 # large phytoplankton
                 ),ncol=2,byrow=TRUE) # end matrix
  ,G_gibber    = matrix(c( 
                   1  , 18 # cephalopods
                 , 2  , 17 # myctophids
                 ,17  , 12 # salps
                 ,59  , 11 # benthic invertebrates
                 , 9  , 10 # large krill
                 , 2  ,  5 # macrozooplankton
                 ,10  ,  2 # ice algae	
                 ),ncol=2,byrow=TRUE) # end matrix
  ,C_gunnari   = matrix(c(
                   1  , 17 # myctophids
                 ,90  , 10 # large krill
                 , 8  ,  8 # other euphausiids
                 , 1  ,  5 # macrozooplankton	
                 ),ncol=2,byrow=TRUE) # end matrix
  ,N_rossii    = matrix(c( 
                  10  , 17 # myctophids
                 , 2  , 12 # salps
                 , 2  , 11 # benthic invertebrates
                 ,60  , 10 # large krill
                 ,20  ,  8 # Other	Euphausiids
                 , 6  ,  2 # ice algae	
                 ),ncol=2,byrow=TRUE) # end matrix
  ,FishOnShelf = matrix(c( 
                   5.5, 18 # cephalopods
                 , 2  , 17 # myctophids
                 , 1.5, 14 # c. gunnari
                 , 1  , 12 # salps
                 ,20  , 11 # Benthic	Invertebrates
                 ,25  , 10 # large krill
                 ,13.5,  8 # other euphausiids
                 , 8.5,  6 # mesozooplankton
                 ,23  ,  5 # macrozooplankton	
                 ),ncol=2,byrow=TRUE) # end matrix
  ,Myctophids  = matrix(c( 
                  25  , 10 # large krill
                 ,35  ,  8 # other euphausiids
                 , 5  ,  6 # mesozooplankton
                 ,35  ,  5 # macrozooplankton	
                 ),ncol=2,byrow=TRUE) # end matrix
  ,Squid       = matrix(c( 
                   2  , 17 # myctophids
                 , 2  , 16 # on-shelf fish
                 ,21  , 11 # benthic invertebrates
                 ,40  , 10 # large krill
                 ,15  , 8 # other euphausiids
                 ,20  ,  5 # macrozooplankton	
                 ),ncol=2,byrow=TRUE) # end matrix
  ,BirdsFly    = matrix(c( 
                  46  , 18 # cephalopods
                 , 4.3, 17 # myctophids
                 , 8.7, 16 # on-shelf fish
                 ,30  , 10 # large krill
                 , 0.1,  6 # mesozooplankton
                 ,10.5,  5 # macrozooplankton	
                 ),ncol=2,byrow=TRUE) # end matrix
  ,MacaroniP   = matrix(c( 
                   1  , 18 # cephalopods
                 ,10  , 17 # myctophids
                 ,12  , 16 # on-shelf fish
                 ,34  , 10 # large krill
                 ,35  ,  8 # Other Euphausiids
                 , 8  ,  6 # mesozooplankton	
                 ),ncol=2,byrow=TRUE) # end matrix
  ,AdelieP     = matrix(c( 
                   1.25,17 # myctophids
                 , 0.1 ,14 # c. gunnari
                 , 1.25,13 # g. gibberifrons
                 ,96.2 ,10 # large krill
                 , 1.25, 5 # macrozooplankton	
                 ),ncol=2,byrow=TRUE) # end matrix
  ,ChinstrapP  = matrix(c( 
                   2.25,17 # myctophids
                 , 2.25,16 # on-shelf fish
                 ,95   ,10 # large krill
                 , 0.1 , 5 # macrozooplankton	
                 ),ncol=2,byrow=TRUE) # end matrix
  ,GentooP     = matrix(c( 
                  10  , 17 # myctophids
                 ,10  , 16 # On-shelf-fish
                 ,80  , 10 # large krill	
                 ),ncol=2,byrow=TRUE) # end matrix
  ,EmperorP    = matrix(c( 
                  10  , 18 # cephalopods
                 ,38  , 16 # on-shelf fish
                 ,52  , 10 # large krill	
                 ),ncol=2,byrow=TRUE) # end matrix
  ,HumpbackW   = matrix(c( 
                   6  , 18 # cephalopods
                 , 4  , 17 # myctophids
                 , 4  , 16 # on-shelf fish
                 ,76  , 10 # large krill
                 , 1.5,  6 # mesozooplankton
                 , 8.5,  5 # macrozooplankton	
                 ),ncol=2,byrow=TRUE) # end matrix
  ,MinkeW      = matrix(c( 
                   1  , 17 # myctophids
                 , 1  , 16 # on-shelf fish
                 ,76  , 10 # large krill
                 ,11  ,  8 # other euphausiids
                 ,11  ,  5 # macrozooplankton	
                 ),ncol=2,byrow=TRUE) # end matrix
  ,FinW        = matrix(c( 
                   1.5, 17 # myctophids
                 , 1.5, 16 # on-shelf fish
                 ,71  , 10 # large krill
                 ,12  ,  8 # other euphausiids
                 , 1  ,	 6 # mesozooplankton
                 ,13  ,  5 # macrozooplankton	
                 ),ncol=2,byrow=TRUE) # end matrix
  ,BlueW       = matrix(c( 
                  61  , 10 # large krill
                 ,20  ,  8 # other euphausiids
                 ,19  ,  5 # macrozooplankton	
                 ),ncol=2,byrow=TRUE) # end matrix
  ,SpermW      = matrix(c( 
                  85  , 18 # cephalopods
                 , 0.1, 17 # myctophids
                 , 4.5, 16 # on-shelf fish
                 ,10  , 11 # benthic invertebrates	
                 ),ncol=2,byrow=TRUE) # end matrix
  ,ElephantS   = matrix(c( 
                  60  , 18 # cephalopods
                 ,10  , 17 # myctophids
                 ,14  , 16 # on-shelf fish
                 ,10  , 15 # N. rosii
                 , 6  , 13 # g. gibberifrons	
                 ),ncol=2,byrow=TRUE) # end matrix
  ,AntFurS = matrix(c( 
                   1  , 23 # gentoo penguin
                 , 3  , 22 # chinstrap penguin
                 , 0.1, 21 # Adélie penguin
                 , 0.1, 20 # Macaroni penguin
                 , 5.4, 18 # cephalopods
                 ,20  , 17 # myctophids
                 ,20  , 16 # on-shelf fish
                 ,50  , 10 # large krill	
                 ),ncol=2,byrow=TRUE) # end matrix
  ,CrabeaterS  = matrix(c( 
                   7.5, 18 # cephalopods
                 , 7.5, 17 # myctophids
                 , 7  , 16 # on-shelf fish
                 ,78  , 10 # large krill	
                 ),ncol=2,byrow=TRUE) # end matrix
  ,WeddellS    = matrix(c( 
                   8  , 18 # cephalopods
                 , 5  , 17 # myctophids
                 ,60  , 16 # on-shelf fish
                 ,22  , 13 # g. gibberifrons
                 , 5  , 11 # Benthic Invertebrates	
                 ),ncol=2,byrow=TRUE) # end matrix
  ,LeopardS    = matrix(c(   
                   0.1, 31 # antarctic fur seal
                 , 0.1, 23 # gentoo penguin
                 , 3  , 22 # chinstrap penguin
                 , 7.8, 18 # cephalopods
                 , 4  , 17 # myctophids
                 ,15  , 13 # g. gibberifrons
                 ,70  , 10 # large krill	
                 ),ncol=2,byrow=TRUE) # end matrix
  ,KillerW = matrix(c( 
                   3  , 34 # leopard seal
                 ,46.5, 33 # Weddell seal
                 ,36.5, 32 # Crabeaters seal
                 , 1  , 30 # Elephant seal
                 , 1  , 28 # blue whale
                 , 1  , 27 # fin whale
                 , 1  , 26 # minke whale
                 , 1  , 25 # humpback whale
                 , 0.1, 24 # emperor penguin
                 , 0.1, 23 # gentoo penguin
                 , 2  , 22 # chinstrap penguin
                 , 0.1, 21 # Adélie penguin
                 , 3  , 17 # Myctophid fish
                 , 2  , 16 # on-shelf fish
                 , 0.1, 15 # n. rossii
                 , 1  , 13 # g. gibberifrons
                 ),ncol=2,byrow=TRUE) # end matrix
) # end list

#####################################################################
# Generate diet matrix where each column is a consumer (Cons) and each row is prey (Prey)
# indexes for column and row are according to Node numbers.

Nodes<-nodes_Dahood
Diet<-diet_Dahood
DM<-sapply(seq(1,nrow(Nodes),1),function(n,N,D){
          diet<-rep(0,N)
          d<-D[[n]][,1]
          # change <1 percent to remainder from 100-sum(rest)
          if(sum(d<1)>0) d[d<1]<- (100-sum(d[d>0.1]))/sum(d<1)
                
          diet[D[[n]][,2]]<-d/100 # change to proportion of diet
          diet
          },nrow(Nodes),Diet)

# generate edges with Col 1 (from) Prey, Col 2 (to) Cons, Col 3 Prop (diet of consumer)

Edges<-do.call(rbind,lapply(seq(1,nrow(DM),1),function(n,dm){
            edges<-cbind(seq(1,nrow(dm),1),rep(n,nrow(dm)),dm[,n])
            return(edges[edges[,3]>0,])
            },DM))
dimnames(Edges)[[2]]<-c("Prey","Cons","Prop")

###################################
# plot using ggraph (https://ggraph.data-imaginist.com/index.html)
# https://github.com/thomasp85/ggraph
# devtools::install_github('thomasp85/ggraph')

# based on code from Roshni Subramanium (30 August 2021)
library(igraph)
library(ggplot2)
library(ggraph)

# Create diagram (from igraph)
net<-graph_from_data_frame(d=Edges,vertices=Nodes,directed = T)

# Set line width according to diet fraction
net_width <- E(net)$Prop*10


# Set colour palette (I like to make my own but you can use ones from RColorBrewer or Viridis). Number of colours depends on whether you want to colour by functional group/species or something else like trophic level
coul2 <- c("#1A1334","#26294A","#01545A","#017351","#03C383","#AAD962","#FBBF45","#EF6A32","#ED0345","#A12A5E", "#710162","#022C7D")

#Expand colour palette - here I'm expanding to equal number of functional groups
coul2 = colorRampPalette(coul2)(nrow(Nodes))

# establishing layout for scaling plot for including labels and images

la <- create_layout(net, layout = 'linear', circular = TRUE)  # same inputs as ggraph

# ratio of plot dimension to diameter of circle

PlotRatio<-3.2

aMin<-c(min(la$x),min(la$y))
aMax<-c(max(la$x),max(la$y))
Diam<-aMax-aMin
Centre<-Diam/2+aMin

pMin<-Centre-Diam*PlotRatio/2
pMax<-pMin+Diam*PlotRatio

## Apply labels manually
#Specify x and y coordinates of labels, adjust outward as desired
Label_radius<-1.1
L_x = la$x*Label_radius
L_y = la$y*Label_radius

#create vector of angles for text based on number of nodes (flipping the orientation of the words half way around so none appear upside down)
L_angle = ifelse(atan(-(la[,1]/la[,2]))*(180/pi) < 0,  90 + atan(-(la[,1]/la[,2]))*(180/pi), 270 + atan(-la[,1]/la[,2])*(180/pi))

# Plot food web - I use ggraph as it's very customisable. I've coloured the interactions from predator to prey using geom_edge. I've included two options, straight line or curved line.
pNet<- ggraph(graph = net, layout = 'linear', circular = TRUE) + 
      lims(x= c(pMin[1],pMax[1]), y=c(pMin[2],pMax[2])) +

  #geom_edge_fan(alpha = 0.8, aes(color = as.factor(from), width = net_width), show.legend = F) + # straight lines, alpha specifies transparency of lines.
#  geom_edge_arc(aes(colour = as.factor(from), width = net_width), alpha = 0.9, show.legend = F) + # curved lines
  geom_edge_arc(aes(colour = as.factor(from), width = net_width), alpha = 0.9, show.legend = F) + # curved lines
#  scale_edge_width_manual(net_width) +
  scale_edge_color_manual(values = coul2) +
  geom_node_point(size = 2, shape = 21, fill = coul2, color = "black", stroke = 1) +
#  geom_node_text(aes(label = Nodes$Order), size=4, color="white") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), axis.text.x = element_blank(), axis.ticks = element_blank(), axis.text.y = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank())+
#  geom_text(aes(x=1.4, y=1.4, label="Here",angle=20), vjust = "outward", hjust = "outward") +
  geom_text(aes(x=L_x, y=L_y, label=V(net)$Node,angle=L_angle),hjust="outward") + #, vjust = "outward", hjust = "outward")
  geom_image(aes(x = L_x, y = L_y, image = img_url))

print(pNet)

library(imager)
library(ggimage)

img_biota_Path<-"/Users/andreworca/Desktop/_w/_r/MEASO/MEASO_Biota_Silhouettes_copy/"
img_biota<-read.csv(paste("Biota_images.csv"))
img_url <- paste0(img_biota_Path, V(net)$Image)

imgs <- map_il(img_url,load.image)
img<-load.image(img_url[1])
img_url[1]
# placing images on the plot
geom_image(aes(x = L_x, y = L_y, image = headshot_href))



 
 
 
