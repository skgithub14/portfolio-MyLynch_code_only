source("./global-utils.R")
source("./data-utils.R")

#### Size and Spacing ####
## graph dimensions
# bar/line graphs
plotDims <- function(mobileStatus){
  if(mobileStatus){
    height <- 450
    l.width <- 400
    b.width <- 500
  } else {
    height <- 750
    l.width <- b.width <- 850
  }
  return(list(height = height, l.width = l.width, b.width = b.width))
}
height <- 750
width <- 850

# waffle
w.display.height <- 500
w.display.height.rr <- 750
mult <- 1.3

# reports
report.width  <- 11
report.height <- 8.5
report.width.waffle <- 950
report.height.waffle <- 735
graph.height <- 7
graph.width <- 7.5
settings.width <- 2.5
settings.height <- 3
interventions.height <- 3
logo.height <- 1

### plot settings
## line/bar graphs
# settings for plots based on number of cancers selected (.r suffix is for the PDF reports)
legend.h.positions <- c(0.1, 0.25, 0.35)
legend.v.positions <- c(0.95, 0.9, 0.8)
legend.v.positions.i <- c(0.93, 0.87, 0.78)
legend.h.positions.line.r <- c(0.15, 0.25, 0.4)
legend.v.positions.line.r <- c(0.92, 0.87, 0.79)
legend.h.positions.bar.r <- c(0.15, 0.25, 0.32)
legend.v.positions.bar.r <- c(0.94, 0.88, 0.81)
legend.heights.line.r <- c(5,3,2)
legend.heights.bar.r <- c(3.5,2.5,0.6)
legend.widths.r <- c(4,3,2)

# number of rows and columns for faceted plots
num.grid.rows <- function(selected.cancers){
  number.of.plots <- length(selected.cancers)
  full.rows <- number.of.plots %/% 3
  partial.rows <- ceiling((number.of.plots - (full.rows * 3)) / 2)
  total.rows <- full.rows + partial.rows
  total.rows
}

num.grid.cols <- function(selected.cancers){
  number.of.plots <- length(selected.cancers)
  if(number.of.plots == 1){
    total.cols <- 1
  } else if(number.of.plots == 2 | number.of.plots == 4){
    total.cols <- 2
  } else {
    total.cols <- 3
  }
  total.cols
}


#### Text ####
## text size
#ggplot/plotly graphs
title.size <- 26
axis.title.size <- 16
default.axis.text.size <- 12
legend.text.sizes.r <- c(10,8,7)
axis.text.sizes.r <- c(14,10,7)
bar.label.text.sizes.r <- c(4,3,2)

# waffle
w.title.font <- 26
w.sub.title.font <- 8
w.legend.font <- 6
w.titles.x.pos <- 7.5

## Plot Titles
line.plot.title <- "Cancer Risk by Age"
bar.plot.title <- "Short Term and Long Term Cancer Risks"
report.title <- "Lynch Syndrome Cancer Risk Tool"
grid.report.title <- textGrob(report.title, gp = gpar(fontsize = title.size))
grid.title.line <- textGrob(line.plot.title, gp = gpar(fontsize = title.size))
grid.title.bar <- textGrob(bar.plot.title, gp = gpar(fontsize = title.size))

## X axis titles
line.x.title <- "Age"
line.x.title.bar <- "Short Term or Long Term"
grid.x.axis.title.line <- textGrob(line.x.title, gp = gpar(fontsize = axis.title.size))
grid.x.axis.title.bar <- textGrob(line.x.title.bar, gp = gpar(fontsize = axis.title.size))

## Y axis titles
y.title <- "Probability of Cancer"
grid.y.axis.title <- textGrob(y.title, rot = 90, gp = gpar(fontsize = axis.title.size))


#### Colors ####
gen.pop.color <- "grey"

# set consistent colors by cancer type
color.scheme <- grDevices::rainbow(n = length(cancer.choices), s = 1, v = .85)
color.scheme[c(3,4,6)] <- c("#FFD700", "#228B22", "#1E90FF")
color.scheme <- color.scheme[c(2,6,9,4,3,8,7,5,1)]
cancer.colors <- stats::setNames(color.scheme, cancer.choices)


#### Icons ####
# load all user icons into global env
user.images <- list.files(path = "./www", pattern = "^user.*png$")
for(i in user.images){
  user.icon <- readPNG(paste0("./www/",i))
  user.icon <- rasterGrob(user.icon, interpolate = F)
  icon.name <- sub(pattern = ".png", replacement = "", i)
  icon.name <- gsub(pattern = "-", replacement = ".", icon.name)
  assign(icon.name, user.icon, envir = globalenv())
}


#### Line Plot Utilities ####
format.plotly <- function(base.plotly, selected.cancer, age, isMobile){
  
  # legend position change for mobile devices
  if(isMobile){
    leg.list <- list(orientation = "h", 
                     traceorder = "reversed")
  } else {
    leg.list <- list(x = 0.02,
                     y = 0.98,
                     traceorder = "reversed")
  }
  
  # x-axis ticks for line graphs
  x.tickvals <- seq(age, max.age, 10)
  if(max.age - as.numeric(age) <= 5){
    x.tickvals <- seq(age, max.age, 1)
  } else if(max.age - as.numeric(age) < 15){
    x.tickvals <- seq(age, max.age, 2)
  } else if(max.age - as.numeric(age) < 30){
    x.tickvals <- seq(age, max.age, 5)
  }
  x.ticktext <- c(" ",as.character(x.tickvals)," ")
  x.tickvals <- c(as.numeric(age)-2, x.tickvals, 87)
  
  # apply format settings
  base.plotly <- 
    base.plotly %>% 
    layout(
      title = list(
        text = paste0(line.plot.title,"<br><sup>",selected.cancer,"</sup>"),
        font = list(
          size = title.size,
          color = 'black'
        ),
        xref = "paper"
      ),
      xaxis = list(
        title = line.x.title,
        font = list(
          size = axis.text.sizes.r[1],
          color = 'black'
        ),
        zeroline = F,
        range = c(as.numeric(age)-2,87),
        fixedrange = TRUE,
        mirror = TRUE,
        ticks = 'outside',
        showline = TRUE,
        tickvals = x.tickvals,
        ticktext = x.ticktext,
        ticklen = 2
      ),
      yaxis = list(
        title = y.title,
        font = list(
          size = axis.text.sizes.r[1],
          color = 'black'
        ),
        zeroline = F,
        range = c(-0.02,1.02),
        fixedrange = TRUE,
        tickformat = "%",
        mirror = TRUE,
        ticks = 'outside',
        showline = TRUE,
        tickvals = c(-0.02, seq(0,1,0.2), 1.02),
        ticktext = c(" ",paste0(seq(0,100,20),"%")," "),
        ticklen = 2
      ),
      legend = leg.list,
      margin = list(
        l = 1,
        r = 1,
        t = 75,
        b = 1
      ),
      hovermode = "x"
    )
  
  # remove mode bar buttons for simplicity
  base.plotly <- 
    base.plotly %>% 
    plotly::config(modeBarButtonsToRemove = c("lasso2d","select2d","pan2d",
                                              "autoScale2d",
                                              "zoom2d","zoomIn2d","zoomOut2d",
                                              "hoverClosestCartesian",
                                              "hoverCompareCartesian",
                                              "toggleSpikelines"),
                   displaylogo = FALSE)
  base.plotly
}

make.ggline <- function(data, 
                        cancer,
                        report = FALSE, 
                        useRR, 
                        colonoscopies.only = FALSE,
                        age, 
                        grows,
                        gcols){
  
  # line colors
  user.color <- cancer.colors[grep(pattern = cancer, names(cancer.colors))]
  if(!useRR){
    these.colors <- c(user.color, gen.pop.color)
    names(these.colors) <- c(paste0(cancer,".Someone Like Me"),
                             paste0(cancer,".Average Person"))
  } else {
    these.colors <- c(user.color, user.color, gen.pop.color)
    names(these.colors) <- c(paste0(cancer,".Someone Like Me"),
                             paste0(cancer,".Reduced Risk"),
                             paste0(cancer,".Average Person"))
  }
  
  # point type
  ptType <- 19
  
  # x-axis ticks for line graphs
  x.ticks <- seq(age, max.age, 10)
  if(max.age - as.numeric(age) <= 5){
    x.ticks <- seq(age, max.age, 1)
  } else if(max.age - as.numeric(age) < 15){
    x.ticks <- seq(age, max.age, 2)
  } else if(max.age - as.numeric(age) < 30){
    x.ticks <- seq(age, max.age, 5)
  }
  
  # axis text size
  if(!report){
    axis.text.size <- default.axis.text.size
    legend.height <- 5
  } else {
    axis.text.size <- axis.text.sizes.r[gcols]
    legend.height <- legend.heights.line.r[grows]
  }
  
  # legend formatting
  if(!report){
    
    # position
    legend.h.pos <- legend.h.positions[gcols]
    if(!useRR){
      legend.v.pos <- legend.v.positions[grows]
    } else {
      legend.v.pos <- legend.v.positions.i[grows]
      legend.h.pos <- legend.h.pos * 1.2
    }
    
    # dimensions
    legend.height <- 5
    
    # text
    if(grows == 3){
      legend.text.size <- 10
    } else {
      legend.text.size <- 12
    }
  } else {
    
    # position
    legend.h.pos <- legend.h.positions.line.r[gcols]
    if(grows == 1){ legend.h.pos <- legend.h.pos * 1.2 }
    if(useRR){ legend.h.pos <- legend.h.pos * 1.2 }
    legend.v.pos <- legend.v.positions.line.r[grows]
    
    # dimensions
    legend.height <- legend.heights.line.r[grows]
    
    # text
    legend.text.size <- legend.text.sizes.r[grows]
  }
  
  # set-up the plot
  p <- 
    ggplot(data = data, aes(x = Age,
                            y = Percent,
                            color = interaction(Cancer, Who),
                            shape = Who,
                            linetype = Who)) +
    geom_point(data = subset(data, Age %% 3 == 1), size = 3) + # point only every other year
    geom_line(size = 1.5) +
    scale_x_continuous(breaks = x.ticks) +
    scale_y_continuous(labels = function(x) paste0(x,"%"),
                       limits = c(0,100)) +
    scale_color_manual(values = these.colors)
  
  # conditionally assign line types
  if(!useRR){
    p <- 
      p +
      scale_linetype_manual(values = c("solid","solid"),
                            labels = c("Someone Like Me","Average Person")) +
      scale_shape_manual(values = c(ptType,NA),
                         labels = c("Someone Like Me","Average Person"))
    
    # show effect of colonoscopies only
  } else if(colonoscopies.only){
    p <-
      p +
      scale_linetype_manual(values = c("solid","dashed","solid"),
                            labels = c("Without colonoscopies","With colonoscopies","Average Person")) +
      scale_shape_manual(values = c(ptType,NA,NA),
                         labels = c("Without colonoscopies","With colonoscopies","Average Person"))
    
    # show any effects
  } else {
    p <-
      p +
      scale_linetype_manual(values = c("solid","dashed","solid"),
                            labels = c("Without risk reduction","With risk reduction","Average Person")) +
      scale_shape_manual(values = c(ptType,NA,NA),
                         labels = c("Without risk reduction","With risk reduction","Average Person"))
  }
  
  # uniformly set labels and bw theme
  p <- 
    p +
    labs(title = element_blank(),x = element_blank(),y = element_blank()) +
    facet_wrap(~ Cancer, scales = "free") +
    theme_bw()
  
  # conditionally set the theme
  if(!report){
    p <-
      p +
      theme(axis.text = element_text(size = axis.title.size),
            legend.position = c(legend.h.pos,legend.v.pos),
            legend.text = element_text(size = legend.text.size),
            legend.title = element_blank(),
            strip.text = element_text(size = axis.text.size))
  } else {
    p <-
      p +
      theme(axis.text = element_text(size = axis.text.size),
            legend.position = c(legend.h.pos,legend.v.pos),
            legend.text = element_text(size = legend.text.size),
            legend.title = element_blank(),
            legend.key.height = unit(legend.height,"mm"),
            strip.text = element_text(size = axis.text.size))
  }
  
  # conditionally create legend
  rr.linetype <- "dashed"
  if(report){ rr.linetype <- "dotted" }
  
  if(!useRR){
    p <- p + guides(linetype = guide_legend(override.aes = list(color = these.colors,
                                                                linetype = c("solid","solid"),
                                                                shape = c(NA,ptType),
                                                                size = c(1,1))),
                    shape = guide_legend(override.aes = list(color = these.colors,
                                                             linetype = c("solid","solid"),
                                                             shape = c(NA,ptType),
                                                             size = c(10,10,10))),
                    color = "none")
  } else {
    p <- p + guides(linetype = guide_legend(override.aes = list(color = these.colors,
                                                                linetype = c("solid",rr.linetype,"solid"),
                                                                shape = c(NA,NA,ptType),
                                                                size = c(1,1,1))),
                    shape = guide_legend(override.aes = list(color = these.colors,
                                                             linetype = c("solid",rr.linetype,"solid"),
                                                             shape = c(NA,NA,ptType),
                                                             size = c(10,10,10))),
                    color = "none")
  }
  p
}


#### Bar Plot Utilities ####
make.ggbar <- function(data, 
                       cancer,
                       cancerView,
                       report = FALSE, 
                       useRR, 
                       colonoscopies.only = FALSE,
                       age,
                       grows,
                       gcols,
                       isMobile){
  
  # bar colors
  user.color <- cancer.colors[grep(pattern = cancer, names(cancer.colors))]
  if(!useRR){
    these.colors <- c(user.color, gen.pop.color)
    names(these.colors) <- c(paste0(cancer,".Someone Like Me"),
                             paste0(cancer,".Average Person"))
  } else {
    these.colors <- c(user.color, user.color, gen.pop.color)
    names(these.colors) <- c(paste0(cancer,".Someone Like Me"),
                             paste0(cancer,".Reduced Risk"),
                             paste0(cancer,".Average Person"))
  }
  
  # legend formatting
  if(!report){
    
    # position
    legend.h.pos <- legend.h.positions[gcols]
    if(colonoscopies.only & length(cancerView) == 1){ legend.h.pos <- legend.h.pos * 2 }
    if(!useRR){
      legend.v.pos <- legend.v.positions[grows]
    } else {
      legend.v.pos <- legend.v.positions.i[grows]
      legend.h.pos <- legend.h.pos * 1.2
    }
    
    # dimensions
    legend.height <- 5
    legend.width <- 4
    
    # text
    if(grows == 3){
      legend.text.size <- 10
    } else {
      legend.text.size <- 12
    }
  } else {
    
    # position
    legend.h.pos <- legend.h.positions.bar.r[gcols]
    if(grows == 1){ legend.h.pos <- legend.h.pos * 1.2 }
    if(useRR){ legend.h.pos <- legend.h.pos * 1.2 }
    legend.v.pos <- legend.v.positions.bar.r[grows]
    
    # dimensions
    legend.height <- legend.heights.bar.r[grows]
    legend.width <- legend.widths.r[gcols]
    
    # special case dimensions
    if(gcols == 3 & grows == 1){ legend.width <- 4}
    if(gcols == 3 & grows == 2){ legend.width <- 2.7}
    if(gcols == 2 & grows == 1){ legend.width <- 3.3}
    
    # text
    legend.text.size <- legend.text.sizes.r[grows]
  }
  
  # axis text size 
  if(!report){
    axis.text.size <- default.axis.text.size
  } else {
    axis.text.size <- axis.text.sizes.r[gcols]
  }
  
  # bar label text size
  if(!report){
    bar.label.text.size <- 4
  } else {
    bar.label.text.size <- bar.label.text.sizes.r[gcols]
  }
  
  # uniformly set the bars, bar labels, x and y scales
  p <-
    ggplot(data, aes(x = Age, 
                     y = Percent, 
                     pattern = Who.Pattern,
                     fill = interaction(Cancer, Who)))  +
    geom_bar_pattern(stat = "identity",
                     position = "dodge",
                     color = "black",
                     # width = col.width,
                     pattern_angle = 45,
                     pattern_density = 0.5,
                     pattern_key_scale_factor = 0.41,
                     pattern_fill = "white",
                     pattern_color = "white") +
    geom_text(aes(label = paste0(ifelse(Percent < 0.5, "<0.5", round(Percent,1)),"%")),
              # position = position_dodge(width = col.width),
              position = position_dodge(width = 0.9),
              size = bar.label.text.size,
              vjust = -0.2) +
    scale_x_discrete(labels = c("In Next 5 Years","Lifetime")) +
    scale_y_continuous(labels = function(x) paste0(x,"%"),
                       limits = c(0,100)) + # round max y + 5 up to nearest 2%
    scale_fill_manual(values = these.colors)
  
  # conditionally assign bar colors
  if(!useRR){
    p <- 
      p +
      scale_pattern_manual(values = c("SomeoneLikeMe" = "none",
                                      "AveragePerson" = "circle"),
                           labels = c("Someone Like Me","Average Person"))
  } else if(colonoscopies.only){
    if(length(cancerView) > 1){
      p <-
        p +
        scale_pattern_manual(values = c("SomeoneLikeMe" = "none",
                                        "ReducedRisk" = "stripe",
                                        "AveragePerson" = "circle"),
                             labels = c("Without colonoscopies","With colonoscopies","Average Person"))
      
      # use longer wording if only one cancer displayed
    } else {
      p <-
        p +
        scale_pattern_manual(values = c("SomeoneLikeMe" = "none",
                                        "ReducedRisk" = "stripe",
                                        "AveragePerson" = "circle"),
                             labels = c("Someone Like Me, without colonoscopies",
                                        "Someone Like Me, with colonoscopies",
                                        "Average Person"))
    }
  } else {
    p <-
      p +
      scale_pattern_manual(values = c("SomeoneLikeMe" = "none",
                                      "ReducedRisk" = "stripe",
                                      "AveragePerson" = "circle"),
                           labels = c("Without risk reduction","With risk reduction","Average Person"))
  }
  
  # uniformly set labels and bw theme
  p <- 
    p +
    labs(title = element_blank(), x = element_blank(), y = element_blank()) +
    facet_wrap(~ Cancer, scales = "free") +
    theme_bw()
  
  # conditionally set the theme
  if(!report){
    
    # change legend position based on mobile device status
    if(isMobile){
      p <-
        p +
        theme(axis.text = element_text(size = axis.title.size),
              legend.position = "bottom",
              legend.text = element_text(size = legend.text.size),
              legend.title = element_blank(),
              strip.text = element_text(size = axis.text.size))
    } else {
      p <-
        p +
        theme(axis.text = element_text(size = axis.title.size),
              legend.position = c(legend.h.pos,legend.v.pos),
              legend.text = element_text(size = legend.text.size),
              legend.title = element_blank(),
              strip.text = element_text(size = axis.text.size))
    }
    
  } else {
    p <-
      p +
      theme(axis.text = element_text(size = axis.text.size),
            legend.position = c(legend.h.pos,legend.v.pos),
            legend.text = element_text(size = legend.text.size),
            legend.title = element_blank(),
            legend.key.height = unit(legend.height,"mm"),
            legend.key.width = unit(legend.width,"mm"),
            strip.text = element_text(size = axis.text.size))
  }
  
  # set custom legend for ggpattern
  if(!useRR){
    p <-
      p +
      guides(pattern = guide_legend(override.aes = list(fill = these.colors,
                                                        color = "black",
                                                        pattern = c("none","circle"))),
             fill = "none")
  } else {
    p <-
      p +
      guides(pattern = guide_legend(override.aes = list(fill = these.colors,
                                                        color = "black",
                                                        pattern = c("none","stripe","circle"))),
             fill = "none")
  }
  p
}


#### Waffle Plot Utilities ####
create.waffle <- function(cancerView, 
                          useRR, 
                          colonoscopies.only = FALSE,
                          cancersRR,
                          report = FALSE, 
                          waffleTime, 
                          data = window.risks){
  
  # with/without cancer out of 20
  carriers.cancer <- data$Percent[which(data$Who == "Someone Like Me")]
  carriers.interv.cancer <- data$Percent[which(data$Who == "Reduced Risk")]
  non.carriers.cancer <- data$Percent[which(data$Who == "Average Person")]
  
  # Titles
  ## main title
  waffle.grid.title <- textGrob(paste0(cancerView, " Risk ", waffleTime), gp=gpar(fontsize=w.title.font))
  
  ## carrier titles
  if(colonoscopies.only){
    c.title <- "Someone Like Me, without colonoscopies"
  } else {
    c.title <- "Someone Like Me, without risk reduction"
  }
  
  ## non-carrier title
  nc.title <- "Average Person"
  
  ## carrier w/ intervention title
  if(useRR & cancerView %in% cancersRR){
    # change plot title conditionally
    if(colonoscopies.only){
      r.title <- "Someone Like Me, with colonoscopies"
    } else {
      r.title <- "Someone Like Me, with risk reduction"
    }
  }
  
  # legend wording
  ## legend for carriers w/o interventions
  lc <- paste0(carriers.cancer, "/20 People Like Me Develop Cancer")
  if(carriers.cancer == 19){
    lc <- paste0("More Than ", carriers.cancer, "/20 People Like Me Develop Cancer")
  } else if(carriers.cancer == 1){
    lc <- paste0("Less Than ", carriers.cancer, "/20 People Like Me Develop Cancer")
  }
  
  ## legend for non-carriers
  lnc <- paste0(non.carriers.cancer, "/20 People without Lynch Develop Cancer")
  if(non.carriers.cancer == 19){
    lnc <- paste0("More Than ", non.carriers.cancer, "/20 People without Lynch Develop Cancer")
  } else if(non.carriers.cancer == 1){
    lnc <- paste0("Less Than ", non.carriers.cancer, "/20 People without Lynch Develop Cancer")
  }
  
  ## legend for carriers with interventions
  if(useRR & cancerView %in% cancersRR){
    lr <- paste0(carriers.interv.cancer, "/20 People Like Me, with the Options I Selected, Develop Cancer")
    if(carriers.interv.cancer == 19){
      lr <- paste0("More Than ", carriers.interv.cancer, "/20 People Like Me, with the Options I Selected, Develop Cancer")
    } else if(carriers.interv.cancer == 1){
      lr <- paste0("Less Than ", carriers.interv.cancer, "/20 People Like Me, with the Options I Selected, Develop Cancer")
    }
  }
  
  # format titles and grob positions
  if(!report){
    w.titlefont <- w.title.font
    hts2 <- unit(c(2,16),"cm")
    hts3 <- unit(c(2,24),"cm")
    wdths <- unit(25,"cm")
  } else {
    w.titlefont <- w.title.font-10
    hts2 <- unit(mult * c(0.5,3.5),"in")
    hts3 <- unit(mult * c(0.5,4.5),"in")
    wdths <- unit(mult * 6,"in")
  }
  
  # create the waffles
  plot.types <- c("c")
  if(useRR & cancerView %in% cancersRR){ plot.types <- c(plot.types,"r") }
  plot.types <- c(plot.types,"nc")
  
  ## dummy plotting space
  xs <- seq(1,14,length.out=12)
  div.ys <- 12 / (length(plot.types) * 2)
  ys <- c(rep(0.25,div.ys),rep(0.75,div.ys),rep(1.25,div.ys),rep(1.75,div.ys))
  if(useRR & cancerView %in% cancersRR){ ys <- c(ys, rep(2.25,div.ys), rep(2.75,div.ys))}
  ys <- as.factor(ys)
  plot <- qplot(xs, ys, geom="blank")
  
  ## create mini plots on the same ggplot graph
  for(pl in plot.types){
    
    # select the icon to use
    if(pl=="nc"){
      user.icon <- user.non.carrier
    } else {
      simple.cancer <- tolower(cancerView)
      simple.cancer <- sub(pattern = " cancer", replacement = "", simple.cancer)
      simple.cancer <- gsub(pattern = " ", replacement = ".", simple.cancer)
      user.icon <- get(paste0("user.",simple.cancer))
    }
    
    # how many should be grey
    with.cancer <- ifelse(pl=="c", carriers.cancer, ifelse(pl=="r", carriers.interv.cancer, non.carriers.cancer))
    without.cancer <- 20-with.cancer
    
    # set plot y position for each different plot
    if(pl=="c" & length(plot.types)==2){
      base <- 1.9
    } else if(pl=="c" & length(plot.types)==3){
      base <- 3.9
    } else if(pl=="r"){
      base <- 2
    } else if(pl=="nc"){
      base <- 0
    }
    
    # plot each icon iteratively
    for(i in 1:20){
      
      # x position
      if(i==1 | i==11){
        x.min.pos <- 1
      } else {
        x.min.pos <- x.min.pos + 1.25
      }
      x.max.pos <- x.min.pos + ifelse(!report, 1, 0.75)
      
      # y position
      if(i<=10){
        y.min.pos <- base + 0.25
      } else {
        y.min.pos <- base + 0.75
      }
      y.max.pos <- y.min.pos + ifelse(!report, 2, 0.75)
      
      # select the icon to plot
      tmp.icon <- user.icon
      if(i<=without.cancer){ tmp.icon <- user.no.cancer } 
      
      # add icon to the plot
      plot <- plot + annotation_custom(tmp.icon, 
                                       xmin=x.min.pos, xmax=x.max.pos, 
                                       ymin=y.min.pos, ymax=y.max.pos)
      
    }
    
    # add title and legend and remove background
    tmp.title <- ifelse(pl=="c", c.title, ifelse(pl=="r", r.title, nc.title))
    tmp.legend <- ifelse(pl=="c", lc, ifelse(pl=="r", lr, lnc))
    
    plot <-
      plot +
      annotate(geom="text",
               label = tmp.title,
               size = ifelse(!report, w.sub.title.font, w.sub.title.font),
               x = w.titles.x.pos, 
               y = base + ifelse(!report, 2.4, 1.8)) +
      annotate(geom="text",
               label = tmp.legend,
               size = ifelse(!report, w.legend.font, w.legend.font),
               x = w.titles.x.pos, 
               y = base + ifelse(!report, 2.2, 1.5)) + 
      theme_void()
  }
  
  w.grob <- as.grob(plot)
  
  # create grob of graphs and title
  if((!useRR | (useRR & !(cancerView %in% cancersRR))) & !report){
    w <- grid.arrange(grobs = list(waffle.grid.title, w.grob), 
                      nrow = 2, 
                      widths = wdths,
                      heights = hts2)
  } else if(useRR & cancerView %in% cancersRR & !report){
    w <- grid.arrange(grobs = list(waffle.grid.title, w.grob), 
                      nrow = 2, 
                      widths = wdths,
                      heights = hts3)
  } else if((!useRR | (useRR & !(cancerView %in% cancersRR))) & report){
    w <- arrangeGrob(grobs = list(waffle.grid.title, w.grob), 
                      nrow = 2, 
                      widths = wdths,
                      heights = hts2)
  } else {
    w <- arrangeGrob(grobs = list(waffle.grid.title, w.grob), 
                      nrow = 2, 
                      widths = wdths,
                      heights = hts3)
  }
  w
}


#### Reports ####
make.settings.table <- function(ht_FT, 
                                ht_IN, 
                                weight, 
                                gender, 
                                age, 
                                gene, 
                                ooph.status, 
                                hyst.status,
                                base.size,
                                hadEndo){
  
  # report settings table left column
  setting.opts <- c("Sex: ","Age: ","Lynch Gene: ")
  settings <- c(gender, as.character(age), gene)
  if(gene == 'MLH1' | 
     (gender == 'Female' & gene != 'EPCAM' & hyst.status == "No" & !hadEndo)){
    setting.opts <- c(setting.opts, "Height: ", "Weight: ")
    ht <- paste0(ht_FT,"ft ",ht_IN,"in")
    wt <- paste0(weight,"lbs")
    settings <- c(settings, ht, wt)
  }
  
  # add prior surgeries if relevant
  if(gender == "Female"){
    setting.opts <- c(setting.opts,"Prior Oophorectomy: ","Prior Hysterectomy: ")
    settings <- c(settings, ooph.status, hyst.status)
  }
  
  # create settings table grob
  settings.df <- data.frame('Setting'  = setting.opts,
                            'Selected' = settings    )
  
  # grob theme
  tt1 <- ttheme_default(
    base_size = base.size,
    core    = list(fg_params=list(hjust=0, x=0.03)),
    colhead = list(fg_params=list(hjust=0, x=0.03)),
    padding = unit(c(3,3),"mm")
  )
  t0 <- tableGrob(settings.df, theme = tt1, rows = NULL)
  title <- textGrob("Me", gp=gpar(fontsize = 14))
  padding <- unit(5,"mm")
  table <- gtable_add_rows(t0,heights = grobHeight(title) + padding, pos = 0)
  table <- gtable_add_grob(table, title, 1, 1, 1, ncol(table))
  table
}

make.interventions.table <- function(gene, 
                                     gender, 
                                     aspirin.status,
                                     colonoscopy.status,
                                     ht_FT,
                                     ht_IN,
                                     weight,
                                     lowerBMI.selected,
                                     ooph.input,
                                     hyst.input,
                                     rr.ooph,
                                     rr.hyst,
                                     base.size,
                                     hadColo,
                                     hadEndo,
                                     hadOvar,
                                     Hweight,
                                     any.int.avail){
  
  setting.opts <- as.character()
  settings <- as.character()
  
  # add intervention options as required
  if(any.int.avail){
    
    # aspirin
    if(!hadColo){
      setting.opts <- c(setting.opts,"Aspirin Regimen: ")
      settings <- c(settings,aspirin.status)
    }
    
    # colonoscopies
    if(!hadColo){
      setting.opts <- c(setting.opts,"Colonoscopies: ")
      settings <- c(settings,colonoscopy.status)
    }
    
    # weight loss
    
    # check how many BMI points the user could lose to get into the normal range
    temp.bmi <- get.BMI(ht_FT = ht_FT, ht_IN = ht_IN, weight = weight)
    bmi.delta <- ceiling(temp.bmi) - 25
    
    if((gene == "MLH1" & bmi.delta > 0) | 
       (gender == "Female" & gene != "EPCAM" & !hyst.input & !hadEndo & Hweight)){
      setting.opts <- c(setting.opts,"Weight Loss: ")
      setting.lowerBMI.selected <- lowerBMI.selected
      if(lowerBMI.selected == 0){ setting.lowerBMI.selected <- "Maintain current weight"}
      settings <- c(settings,setting.lowerBMI.selected)
    }
    
    # surgeries
    if(gender == "Female"){
      
      # oophorecotomy
      if(!ooph.input){
        setting.opts <- c(setting.opts,"New Oophorectomy: ")
        settings <- c(settings,rr.ooph)
      }
      
      # hysterectomy
      if(!hyst.input){
        setting.opts <- c(setting.opts,"New Hysterectomy: ")
        settings <- c(settings,rr.hyst)
      }
    }
  }
  
  # create settings table grob
  settings.df <- data.frame('Setting'  = setting.opts,
                            'Selected' = settings    )
  
  # grob theme
  tt1 <- ttheme_default(
    base_size = base.size,
    core    = list(fg_params=list(hjust=0, x=0.03)),
    colhead = list(fg_params=list(hjust=0, x=0.03)),
    padding = unit(c(3,3),"mm")
  )
  t0 <- tableGrob(settings.df, theme = tt1, rows = NULL)
  title <- textGrob("What I Selected", gp=gpar(fontsize = 14))
  padding <- unit(5,"mm")
  table <- gtable_add_rows(t0,heights = grobHeight(title) + padding, pos = 0)
  table <- gtable_add_grob(table, title, 1, 1, 1, ncol(table))
  table
}