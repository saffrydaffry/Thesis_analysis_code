##Get color from colorPalette
getColorByPoints <- function(val) {
  minVal <- 0
  maxVal <- 0.8
  numCols <- 6
  pal <- colorRampPalette(c("#acd3ee", "#04395c"))  # Yellow to red
  cols <- pal(numCols) #pal is a function that takes the number of colors and outputs an index of colors
  # Get index to pick color.
  colIndex <- round(numCols * (val - minVal) / (maxVal - minVal))
  colIndex <- max(1, colIndex)
  return(cols[colIndex])
}
## default ggplot colors
gg_color_hue <- function(n) {
  hues = seq(15, 375, length=n+1)
  hcl(h=hues, l=65, c=100)[1:n]
}

## Print linear regression on ggplot
lm_eqn = function(lm_data){
  fit = lm(R0_new~  factor(mut_rate), lm_data);
  fitw = glm(abs(fit$residuals)~ + factor(mut_rate), data = lm_data)
  m = lm(R0_new~ factor(mut_rate), weights = 1/fitw$fitted.values^2, data = lm_data)
  
  l <- list(a = format(coef(m)[1], digits = 2),
            b = format(abs(coef(m)[2]), digits = 2),
            r2 = format(summary(m)$r.squared, digits = 3));
  if (coef(m)[2] >= 0)  {
    eq <- substitute(italic(y) == a + b %.% italic(x)*"," ~~italic(r)^2~"="~r2,l)
  } else {
    eq <- substitute(italic(y) == a - b %.% italic(x)*"," ~~italic(r)^2~"="~r2,l)    
  }
  
  as.character(as.expression(eq));                 
}

## Stream Graph with ggplot
areaGraph_gg = function(thedata, type=2, smooth = TRUE){
  nColors <- nrow(thedata)
  # pal <- colorRampPalette(c("#0f7fb4", "#e2e2e2"))
  # pal <- colorRampPalette(c("#6364a9", "#e2e2e2"))  # Purple
  pal <- colorRampPalette(c("#48611d", "#f0f0f0"))    # Green
  colors <- pal(nColors)
if (type == 0) {					# Stacked area
  
  # Greatest to least weights
  sortedData <- thedata[order(rowSums(thedata), decreasing=TRUE),]
  layerNames <- rownames(sortedData)
  
} else if (type == 1 || type == 2) {				# Themeriver or streamgraph
  
  # Initialize sorted data frame
  sortedData <- thedata[1,]
  weights <- rowSums(thedata)
  topWeight <- weights[1]
  bottomWeight <- weights[1]
  layerNames <- c(rownames(thedata)[1])
  
  if (length(thedata[,1]) > 1) {
    
    # Commence sorting. Apparently not most efficient way, but whatever.
    for (i in 2:length(thedata[,1])) {
      
      if (topWeight > bottomWeight) {
        sortedData <- rbind(sortedData, thedata[i,])
        layerNames <- c(layerNames, rownames(thedata)[i])
      } else {
        sortedData <- rbind(thedata[i,], sortedData)
        bottomWeight <- bottomWeight + weights[i]
        layerNames <- c(rownames(thedata)[i], layerNames)
      }
    }
  }
  
}

# Smooth the data
if (smooth) {
  
  nPoints <- length(thedata[1,]) * 3
  
  # Initialize smoothed data. Note: Probably a better way to do this, but it works. [NY]
  firstRow <- spline(1:length(sortedData[1,]), sortedData[1,], nPoints)$y
  firstRow <- sapply(firstRow, zeroNegatives)
  #spline time points also
  time = spline(1:ncol(sortedData), as.numeric(colnames(sortedData)),nPoints)$y
  
  smoothData <- data.frame( rbind(firstRow, rep(0, length(firstRow))) )
  smoothData <- smoothData[1,]
  
  # Smooth the rest of the data using spline().
  if (length(sortedData[,1]) > 1) {
    
    for (i in 2:length(sortedData[,1])) {	
      newRow <- spline(1:length(sortedData[i,]), sortedData[i,], nPoints)$y
      newRow <- sapply(newRow, zeroNegatives)
      smoothData <- rbind(smoothData, newRow)
    }
  }
  
  colnames(smoothData) = time
  finalData <- smoothData
  rownames(finalData) = rownames(sortedData)
  
} else {
  
  finalData <- sortedData
  
}  


totals <- colSums(finalData)

# Determine baseline offset
if (type == 0) {
  
  yOffset <- rep(0, length(totals))
  
} else if (type == 1) {
  
  yOffset <- -totals / 2
  
} else if (type == 2) {
  n <- length(finalData[,1])
  i <- 1:length(finalData[,1])
  parts <- (n - i + 1) * finalData
  theSums <- colSums(parts)
  yOffset <- -theSums / (n + 1)	
}

# Axis upper and lower bounds
yLower <- min(yOffset)  
yUpper <- max(yOffset + totals)

# Max, min, and span of weights for each layer
maxRow <- max(rowSums(finalData))
minRow <- min(rowSums(finalData))
rowSpan <- if ( (maxRow - minRow) > 0 ) { maxRow - minRow } else { 1 }

# Time series x axis
newdata = data.frame()
xmax = 0
xtext = rep(0, length(finalData[,1]))
ytext =  rep(0, length(finalData[,1]))
textSizes =  rep(0, length(finalData[,1]))
colIndex = c()
numparam = length(finalData[,1])
## try with ggplot
col_names = colnames(finalData)

## Remove duplicate names by adding digits
add=1
while(any(duplicated(col_names))){
  dup.ind = which(duplicated(col_names))
  for(i in dup.ind){
    col_names[i] <- paste0(col_names[i], as.character(add))  
  }
  add = add+1
}

#replace column names
colnames(finalData)<-col_names

newdata = melt(t(finalData), id = rownames(finalData))

#prepare indices, variables
yOffset_rev = yOffset
oneLength = length(yOffset)  #number of values in each group
totLength = numparam*oneLength #total length of all data
yOffset.frag = rep(0, oneLength)
for(i in 1:(numparam)){
  #colIndex[i] <- floor( (nColors-2) * ( (maxRow - sum(finalData[i,])) / rowSpan ) ) + 1
  yOffset.frag <- yOffset.frag + finalData[i,]
  yOffset = append(yOffset, yOffset.frag)
  xmax <- newdata$Var1[which.max(finalData[i,])]
  xtext[i] <-  xmax-i*0.0005
  ytext[i] <-  yOffset.frag[which.max(finalData[i,])]* 0.9
  #newdata$Var2[(oneLength*(i-1)+1):(oneLength*i)]<-rep(layerNames[i], oneLength)
  #ytext[i] <-  finalData[i,which.max(finalData[i,])]/2 + yOffset[which.max(finalData[i,])]
  #textSizes[i] <-  1.7 * sqrt( (nColors-colIndex[i])/nColors )
}
yOffset = unlist(yOffset)
yOffset <- yOffset[1:totLength]
newdata$value = newdata$value+yOffset
newdata_down = data.frame(Var1 = newdata$Var1, Var2 = newdata$Var2, value = yOffset)
newdata_down = ddply(newdata_down, .(Var2), transform,
                     Var1 = rev(Var1),
                     value = rev(value))
newdata2 = rbind(newdata, newdata_down)

textData = data.frame(xpos=xtext,
                      ypos=unlist(ytext),
                      #sizes = textSizes
                      labels = layerNames)

newdata$Var2 <-factor(newdata$Var2, levels=unique(newdata$Var2), ordered=TRUE)
ggplot(data = newdata, aes(x =Var1, y = value)) +
  #geom_area(aes(fill = factor(Var2)), size = 0.2, color = "white") +
  stat_smooth(method = "gam", formula= y~s(x, k=300), 
              aes(fill=factor(Var2)), position="stack", geom = "area",alpha = 0.5, size = 0.2, color = "white" )+
  ylim(yLower,yUpper) +
  scale_fill_brewer(type = "qual", palette =3) +
  geom_text(data= textData, family="Gill Sans MT", aes(x = xpos, y = ypos, label = labels), size = 4)+
  xlab("Years") + ylab('Population Numbers') +
  theme_tufte(base_family='Gill Sans MT', base_size =12) +
  theme(legend.title = element_blank())


}

#### Stream Graph from Flowing Data #####
# Type 0: stacked area, 1: themeriver, 2: streamgraph
areaGraph <- function(thedata, type=2, smooth=TRUE) {
  
  # Color palette
  nColors <- 15
  # pal <- colorRampPalette(c("#0f7fb4", "#e2e2e2"))
  # pal <- colorRampPalette(c("#6364a9", "#e2e2e2"))  # Purple
  pal <- colorRampPalette(c("#48611d", "#f0f0f0"))
  colors <- pal(nColors)
  
  # Sort the data
  if (type == 0) {					# Stacked area
    
    # Greatest to least weights
    sortedData <- thedata[order(rowSums(thedata), decreasing=TRUE),]
    layerNames <- rownames(sortedData)
    
  } else if (type == 1 || type == 2) {				# Themeriver or streamgraph
    
    # Initialize sorted data frame
    sortedData <- thedata[1,]
    weights <- rowSums(thedata)
    topWeight <- weights[1]
    bottomWeight <- weights[1]
    layerNames <- c(rownames(thedata)[1])
    
    if (length(thedata[,1]) > 1) {
      
      # Commence sorting. Apparently not most efficient way, but whatever.
      for (i in 2:length(thedata[,1])) {
        
        if (topWeight > bottomWeight) {
          sortedData <- rbind(sortedData, thedata[i,])
          layerNames <- c(layerNames, rownames(thedata)[i])
        } else {
          sortedData <- rbind(thedata[i,], sortedData)
          bottomWeight <- bottomWeight + weights[i]
          layerNames <- c(rownames(thedata)[i], layerNames)
        }
      }
    }
    
  }
  
  # Smooth the data
  if (smooth) {
    
    nPoints <- length(thedata[1,]) * 3
    
    # Initialize smoothed data. Note: Probably a better way to do this, but it works. [NY]
    firstRow <- spline(1:length(sortedData[1,]), sortedData[1,], nPoints)$y
    firstRow <- sapply(firstRow, zeroNegatives)
    
    smoothData <- data.frame( rbind(firstRow, rep(0, length(firstRow))) )
    smoothData <- smoothData[1,]
    
    # Smooth the rest of the data using spline().
    if (length(sortedData[,1]) > 1) {
      
      for (i in 2:length(sortedData[,1])) {	
        newRow <- spline(1:length(sortedData[i,]), sortedData[i,], nPoints)$y
        newRow <- sapply(newRow, zeroNegatives)
        smoothData <- rbind(smoothData, newRow)
      }
    }
    
    finalData <- smoothData
    
  } else {
    
    finalData <- sortedData
    
  }
  
  
  # Totals for each vertical slice
  totals <- colSums(finalData)
  
  # Determine baseline offset
  if (type == 0) {
    
    yOffset <- rep(0, length(totals))
    
  } else if (type == 1) {
    
    yOffset <- -totals / 2
    
  } else if (type == 2) {
    n <- length(finalData[,1])
    i <- 1:length(finalData[,1])
    parts <- (n - i + 1) * finalData
    theSums <- colSums(parts)
    yOffset <- -theSums / (n + 1)	
  }
  
  
  # Axis upper and lower bounds
  yLower <- min(yOffset)	
  yUpper <- max(yOffset + totals)
  
  # Max, min, and span of weights for each layer
  maxRow <- max(rowSums(finalData))
  minRow <- min(rowSums(finalData))
  rowSpan <- if ( (maxRow - minRow) > 0 ) { maxRow - minRow } else { 1 }
  
  # Make the graph.
  par(las=1, cex=0.6, bty="n")
  xtext <- c(); ytext <- c(); textSizes <- c()
  
  plot(0, 0, type="n", xlim=c(1, length(finalData[1,])), ylim=c(yLower, yUpper), xlab=NA, ylab=NA)
  for (i in 1:length(finalData[,1])) {
    
    colIndex <- floor( (nColors-2) * ( (maxRow - sum(finalData[i,])) / rowSpan ) ) + 1
    polygon(c(1:length(finalData[i,]), length(finalData[i,]):1), c(finalData[i,] + yOffset, rev(yOffset)), col=colors[colIndex], border="#ffffff", lwd=0.2)
    
    
    # Label locations
    xmax <- which.max(finalData[i,])
    xtext <- c(xtext, xmax)
    ytext <- c(ytext, finalData[i,xmax] / 2 + yOffset[xmax])
    textSizes <- c(textSizes, 1.7 * sqrt( (nColors-colIndex) / nColors ))
    
    # Move up to next layer.
    yOffset <- yOffset + finalData[i,]
  }
  
  
  # Add labels last.
  if (length(layerNames) > 0) {
    text(xtext, ytext, layerNames, cex=textSizes)
  }
  
}

#### Zero Negatives FUnction
# Helper function to convert negative values to zero
zeroNegatives <- function(x) {
  if (x < 0) { return(0) }
  else { return(x) }
}

### Multiple plot function for ggplot2
### Allows the creation of multiple ggplots on one page
###  Credit to Cookbook for R:
###  http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/

# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
