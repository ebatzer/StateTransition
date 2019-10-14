plot_stm <- function(edges){
  
  starts <- data.frame(source = as.character(c("A", "B", "C", "D")),
             xsource = c(-1, 1, -1, 1),
             ysource = c(1, 1, -1, -1))
  
  ends <- data.frame(target = as.character(c("A", "B", "C", "D")),
                     xtarget = c(-1, 1, -1, 1),
                     ytarget = c(1, 1, -1, -1))
  
  graphdat <- edges %>% 
    mutate(source = as.character(source), target = as.character(target)) %>%
    full_join(starts, by = "source") %>% 
    full_join(ends, by = "target") %>% 
    mutate(offsetx = xsource - xtarget,
           offsety = ysource - ytarget) %>%
    mutate(offsetx = case_when(offsetx != 0 & offsety != 0 ~ offsetx * cos(pi/4),
                               TRUE ~ as.numeric(offsetx)),
           offsety = case_when(offsetx != 0 & offsety != 0 ~ offsety * cos(pi/4),
                                      TRUE ~ as.numeric(offsety)))
  
  edgelabels <- graphdat %>% 
    select(source, target, weight) %>% 
    filter(source != target) %>%
    bind_cols(data.frame(
      xpos = c(0, -.75, .5,
               0, -.15, 1.25,
               -1.25, .15, 0,
               -.5, .75, 0),
      ypos = c(1.2, 0, -.15,
               .8, -.5, 0,
               0, .5, -.8,
               .15, 0, -1.2)
      ))
  
  namevec <- c(1,2,3,4)
  names(namevec) <- c("A", "B", "C", "D")
  
  # Creating figure
  
  corr_fac <- .2
  arrow_val <- arrow(length=unit(0.75,"cm"), angle = 15, ends="last", type = "closed")
  
  ggplot() +
    theme_void() +
    geom_curve(aes(x = xsource - offsetx * corr_fac,
                     y = ysource - offsety * corr_fac,
                     xend = xtarget + offsetx * corr_fac,
                     yend = ytarget + offsety * corr_fac),
               curvature = .15,
               arrow = arrow_val,
               size = 2.5,
               data = graphdat %>% filter(source != target)) +
    geom_curve(aes(x = xsource - offsetx * corr_fac,
                   y = ysource - offsety * corr_fac,
                   xend = xtarget + offsetx * corr_fac,
                   yend = ytarget + offsety * corr_fac),
               curvature = .15,
               arrow = arrow_val,
               size = 2,
               color = "white",
               data = graphdat %>% filter(source != target)) +
    geom_curve(aes(x = xsource - offsetx * corr_fac,
                   y = ysource - offsety * corr_fac,
                   xend = xtarget + offsetx * corr_fac,
                   yend = ytarget + offsety * corr_fac,
                   color = target,
                   alpha = weight),
               curvature = .15,
               arrow = arrow_val,
               size = 2,
               data = graphdat %>% filter(source != target)) +
    geom_point(aes(x = xsource,
                   y = ysource,
                   size = 1),
               pch = 21,
               color = "black",
               fill = "white",
               data = graphdat %>% filter(source == target)) +
    geom_point(aes(x = xsource,
                   y = ysource,
                   size = weight,
                   fill = source),
               pch = 21,
               color = "black",
               data = graphdat %>% filter(source == target)) +
    scale_radius(range = c(10, 40), limits = c(0, 1)) + 
    scale_alpha(range = c(0, 1)) + 
    coord_cartesian(xlim = c(-2, 2),
                    ylim = c(-2, 2)) + 
    guides(fill = FALSE, size = FALSE, color = FALSE, alpha = FALSE) +
    geom_text(aes(label = round(weight, 2),
              x = xsource,
              y = ysource),
              data = graphdat %>% filter(source == target)) +
    geom_text(aes(label = paste("State", namevec[source]),
                x = xsource,
                y = ysource + .5 * sign(ysource)),
            data = graphdat %>% filter(source == target),
            fontface = "bold") +
    geom_text(aes(label = round(weight, 2),
                  x = xpos,
                  y = ypos),
              data = edgelabels)+
    theme(plot.margin = unit(c(0,0,0,0), "cm"))
  
  # nodes = data.frame(node_id = c("A", "B", "C", "D"),
  #                  cluster = c("I", "haven't", "assigned", "yet"),
  #                  stability = edges$weight[edges$source == edges$target])
  # 
  # net = graph_from_data_frame(d = edges, vertices = nodes, directed = TRUE)
  # net = simplify(net, remove.loops = T)
  # 
  # colors = rainbow(4, v = .75, s = .3)
  # V(net)$color = colors
  # E(net)$width <- E(net)$weight * 40
  # V(net)$size <- (V(net)$stability * 100)
  # V(net)$label <- round(V(net)$stability, 2)
  # V(net)$label.font=2
  # E(net)$label.font=2
  # 
  # graph_attr(net, "layout") <- layout_with_lgl(net)
  # 
  # plot(net, 
  #      edge.arrow.size = 1.5,
  #      edge.curved = .5,
  #      vertex.label.family = "sans",
  #      edge.label.family = "sans",
  #      edge.label.color = "black",
  #      vertex.label.color = "black",
  #      edge.label = round(E(net)$weight, 2),
  #      margin = c(.5,.1,0,.1),
  #      edge.color = "grey70",
  #      main = "State Resilience and Transition Probabilities",
  #      cex = 1.25,
  #      layout = matrix(c(1,1,-1,1,1,-1,-1,-1),
  #                      nrow = 4,
  #                      byrow = TRUE),
  #      margin = c(-1,-1,-1,-1))
  # 
  # legend(x=-1.7, y=-1.5, c("State 1: E. glaucus - S.pulchra", 
  #                          "State 2: F. perennis - B. hordeaceous", 
  #                          "State 3: E. caput-medusae - A. triuncialis", 
  #                          "State 4: A. fatua - B. diandrus"), pch=21,
  #        col="#777777", 
  #        pt.bg=colors, pt.cex=2, cex=.8, bty="n", ncol=1)
}
