library(ggplot2)
library(gridExtra)
library(grid)
library(png)
library(cowplot)
library(colorspace)
library(extrafont)


brand_plot <- function(orig_plot, save_name, asp = 1, base_size = 5, data_home = '', fade_borders = '', axis_rot = F) {
  logo_size <- 0.06
  
  ## is image taller than wider? if so, make sure the width is at least the base_size
  if (asp < 1) {
    base_size_rat_wid <- (5/base_size)
    logo_size <- (5/base_size) * logo_size * asp
    base_size <- base_size / asp
  } else {
    base_size_rat_wid <- (5/base_size) / asp
    logo_size <- (5/base_size) * logo_size
  }
  
  ## local logo to read in
  logo_file <- readPNG('C:/Users/rei1740/Desktop/Anthony/statbutler.png')

  author_txt <- textGrob('By Anthony Reinhard', x=unit(0.08 * (base_size_rat_wid), 'npc'), gp=gpar(col='darkblue', fontfamily='Bahnschrift', fontsize=6), hjust=0)
  data_txt <- textGrob(data_home, x=unit(1 - (.01 * (base_size_rat_wid)), 'npc'), gp=gpar(col='grey95', fontfamily='Bahnschrift', fontsize=6), hjust=1)
  footer_bg <- grid.rect(x = unit(seq(0.5,1.5,length=1000), 'npc'), gp=gpar(col = 'transparent', fill = colorRampPalette(c('grey95', 'darkblue'), space = 'rgb')(1000)), draw = F)
  footer <- grobTree(footer_bg, author_txt, data_txt)

  if (axis_rot) {axis_adj <- 90} else {axis_adj <- 0}

  if (fade_borders!='') {
    ## set up bounds for fade plot
    x_lim <- axis_limits_x(orig_plot)
    y_lim <- axis_limits_y(orig_plot)
    
    ## figure out which sides to fade
    border_layers <- c()
    if (grepl('t',fade_borders)) {
      border_layers <- c(border_layers, annotation_custom(make_gradient(deg = 270 + axis_adj), xmin=-Inf, xmax=Inf, ymin=y_lim[2], ymax=Inf))
    }
    if (grepl('b',fade_borders)) {
      border_layers <- c(border_layers, annotation_custom(make_gradient(deg = 90 + axis_adj), xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=y_lim[1]))
    }
    if (grepl('r',fade_borders)) {
      border_layers <- c(border_layers, annotation_custom(make_gradient(deg = 0 - axis_adj), xmin=x_lim[2], xmax=Inf, ymin=-Inf, ymax=Inf))
    }
    if (grepl('l',fade_borders)) {
      border_layers <- c(border_layers, annotation_custom(make_gradient(deg = 180 - axis_adj), xmin=-Inf, xmax=x_lim[1], ymin=-Inf, ymax=Inf))
    }
    orig_plot$layers <- c(orig_plot$layers, border_layers)
    
    ## add axis (or not) to unfaded
    orig_plot <- orig_plot +
      theme(
        axis.line.y.left = element_line(color = ifelse(grepl('l',fade_borders), 'grey95', 'darkblue')),
        axis.line.y.right = element_line(color = ifelse(grepl('r',fade_borders), 'grey95', 'darkblue')),
        axis.line.x.top = element_line(color = ifelse(grepl('t',fade_borders), 'grey95', 'darkblue')),
        axis.line.x.bottom = element_line(color = ifelse(grepl('b',fade_borders), 'grey95', 'darkblue')),
	panel.border = element_rect(color = 'grey95', size = 0.1),
      )
  }
  
  plt.final <- grid.arrange(orig_plot, footer, heights=unit(c(1, 12), c('null','pt')))
  plt <- ggdraw(plt.final) + draw_image(logo_file, x = 0.002 * (base_size_rat_wid), y = 0, hjust = 0, vjust = 0, height = logo_size, width = 0.08 * (base_size_rat_wid))
  ggsave(save_name, plt, dpi = 700, height = base_size, width = base_size * (asp))
}

theme_SB <-  theme(
    line = element_line(lineend='round', color='darkblue'),
    text = element_text(family='Bahnschrift', color='darkblue'),
    plot.background = element_rect(fill = 'grey95', color = 'transparent'),
    panel.border = element_rect(color = 'darkblue', fill = NA),
    panel.background = element_rect(fill = 'white', color = 'transparent'),
    axis.ticks = element_line(color = 'darkblue', size = 0.3),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 8, color = 'darkblue'),
    plot.title = element_text(size = 14),
    plot.subtitle = element_text(size = 8),
    plot.caption = element_text(size = 5),
    legend.background = element_rect(fill = 'grey90', color = 'darkblue'),
    legend.key = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color='grey70', size = 0.3)
  ) 


properLims <- function(vec) {
  labs <- labeling::extended(min(vec, na.rm = T), max(vec, na.rm = T), m = 5)
  gap <- diff(labs[1:2])
  plot_max <- ifelse(rev(labs)[1] < max(vec, na.rm = T), rev(labs)[1] + gap, rev(labs)[1])
  plot_min <- ifelse(labs[1] > min(vec, na.rm = T), labs[1] - gap, labs[1])
  return(c(plot_min,plot_max))
} 

properLimsRev <- function(vec) {
  labs <- labeling::extended(min(vec, na.rm = T), max(vec, na.rm = T), m = 5)
  gap <- diff(labs[1:2])
  plot_max <- ifelse(rev(labs)[1] < max(vec, na.rm = T), rev(labs)[1] + gap, rev(labs)[1])
  plot_min <- ifelse(labs[1] > min(vec, na.rm = T), labs[1] - gap, labs[1])
  return(c(plot_max,plot_min))
} 


make_gradient <- function(deg, n = 500, col = 'grey95', corner = F) {
  rad <- deg / (180 / pi)
  mat <- matrix(
    data = rep(seq(0, 1, length.out = n) * cos(rad), n),
    byrow = TRUE,
    ncol = n
  ) +
    matrix(
      data = rep(seq(0, 1, length.out = n) * sin(rad), n),
      byrow = FALSE,
      ncol = n
    )
  mat <- mat - min(mat)
  mat <- mat / max(mat)
  mat <- 1 + mat * n

  mat <- matrix(data = alpha(col, mat/(n+1)), ncol = n)
  grid::rasterGrob(
    image = mat,
    width = unit(1, "npc"),
    height = unit(1, "npc"), 
    interpolate = TRUE
  )
}

axis_limits_x <- function(p) {
  if (!is.null(ggplot_build(p)$layout$coord$limits$x)) {
    return(ggplot_build(p)$layout$coord$limits$x)
  } else if (!is.null(ggplot_build(p)$layout$panel_scales_x[[1]]$range_c$range)) {
    return(ggplot_build(p)$layout$panel_scales_x[[1]]$range_c$range)
  } else {
    return(ggplot_build(p)$layout$panel_scales_x[[1]]$range$range)
  }
}

axis_limits_y <- function(p) {
  if (!is.null(ggplot_build(p)$layout$coord$limits$y)) {
    return(ggplot_build(p)$layout$coord$limits$y)
  } else if (!is.null(ggplot_build(p)$layout$panel_scales_y[[1]]$range_c$range)) {
    return(ggplot_build(p)$layout$panel_scales_y[[1]]$range_c$range)
  } else {
    return(ggplot_build(p)$layout$panel_scales_y[[1]]$range$range)
  }
}

color_SB <- c("#ff7f00", "#9932cc", "#8cff72", "#00008b", "#51dbd8", "#674b00", "#ff66cf", "#8f8f8f", "#ff0000", "#e1ed00", "#0b5209", "#636363")

NFL_pri <- c('ARI'='#97233f',
'ATL'='#a71930',
'BAL'='#241773',
'BUF'='#00338d',
'CAR'='#0085ca',
'CHI'='#0b162a',
'CIN'='#000000',
'CLE'='#fb4f14',
'DAL'='#002244',
'DEN'='#002244',
'DET'='#005a8b',
'GB'='#203731',
'HOU'='#03202f',
'IND'='#002c5f',
'JAX'='#000000',
'KC'='#e31837',
'LAC'='#002244',
'LA'='#003693',
'MIA'='#008e97',
'MIN'='#4f2683',
'NE'='#002244',
'NO'='#9f8958',
'NYG'='#0b2265',
'NYJ'='#125740',
'OAK'='#a5acaf',
'PHI'='#004953',
'PIT'='#000000',
'SF'='#aa0000',
'SEA'='#002244',
'TB'='#d50a0a',
'TEN'='#002244',
'WAS'='#773141')


NFL_sec <- c('ARI'='#000000',
'ATL'='#000000',
'BAL'='#000000',
'BUF'='#c60c30',
'CAR'='#000000',
'CHI'='#c83803',
'CIN'='#fb4f14',
'CLE'='#22150c',
'DAL'='#b0b7bc',
'DEN'='#fb4f14',
'DET'='#b0b7bc',
'GB'='#ffb612',
'HOU'='#a71930',
'IND'='#a5acaf',
'JAX'='#006778',
'KC'='#ffb612',
'LAC'='#0073cf',
'LA'='#ffd000',
'MIA'='#f58220',
'MIN'='#ffc62f',
'NE'='#c60c30',
'NO'='#000000',
'NYG'='#a71930',
'NYJ'='#000000',
'OAK'='#000000',
'PHI'='#a5acaf',
'PIT'='#ffb612',
'SF'='#b3995d',
'SEA'='#69be28',
'TB'='#34302b',
'TEN'='#4b92db',
'WAS'='#ffb612')

brand_plot_no_save <- function(orig_plot, save_name, asp = 1, base_size = 5, data_home = '', fade_borders = '') {
  logo_size <- 0.06
  
  ## is image taller than wider? if so, make sure the width is at least the base_size
  if (asp < 1) {
    base_size_rat_wid <- (5/base_size)
    logo_size <- (5/base_size) * logo_size * asp
    base_size <- base_size / asp
  } else {
    base_size_rat_wid <- (5/base_size) / asp
    logo_size <- (5/base_size) * logo_size
  }
  
  ## local logo to read in
  logo_file <- readPNG('C:/Users/rei1740/Desktop/Anthony/statbutler.png')

  author_txt <- textGrob('By Anthony Reinhard', x=unit(0.08 * (base_size_rat_wid), 'npc'), gp=gpar(col='darkblue', family='Bahnschrift', fontsize=6), hjust=0)
  data_txt <- textGrob(data_home, x=unit(1 - (.01 * (base_size_rat_wid)), 'npc'), gp=gpar(col='grey95', family='Bahnschrift', fontsize=6), hjust=1)
  footer_bg <- grid.rect(x = unit(seq(0.5,1.5,length=1000), 'npc'), gp=gpar(col = 'transparent', fill = colorRampPalette(c('grey95', 'darkblue'), space = 'rgb')(1000)))
  footer <- grobTree(footer_bg, author_txt, data_txt)

  if (fade_borders!='') {
    ## set up bounds for fade plot
    x_lim <- axis_limits_x(orig_plot)
    y_lim <- axis_limits_y(orig_plot)
    
    ## figure out which sides to fade
    border_layers <- c()
    if (grepl('t',fade_borders)) {
      border_layers <- c(border_layers, annotation_custom(make_gradient(deg = 270), xmin=-Inf, xmax=Inf, ymin=y_lim[2], ymax=Inf))
    }
    if (grepl('b',fade_borders)) {
      border_layers <- c(border_layers, annotation_custom(make_gradient(deg = 90), xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=y_lim[1]))
    }
    if (grepl('r',fade_borders)) {
      border_layers <- c(border_layers, annotation_custom(make_gradient(deg = 0), xmin=x_lim[2], xmax=Inf, ymin=-Inf, ymax=Inf))
    }
    if (grepl('l',fade_borders)) {
      border_layers <- c(border_layers, annotation_custom(make_gradient(deg = 180), xmin=-Inf, xmax=x_lim[1], ymin=-Inf, ymax=Inf))
    }
    orig_plot$layers <- c(orig_plot$layers, border_layers)
    
    ## add axis (or not) to unfaded
    orig_plot <- orig_plot +
      theme(
        axis.line.y.left = element_line(color = ifelse(grepl('l',fade_borders), 'grey95', 'darkblue')),
        axis.line.y.right = element_line(color = ifelse(grepl('r',fade_borders), 'grey95', 'darkblue')),
        axis.line.x.top = element_line(color = ifelse(grepl('t',fade_borders), 'grey95', 'darkblue')),
        axis.line.x.bottom = element_line(color = ifelse(grepl('b',fade_borders), 'grey95', 'darkblue')),
        panel.border = element_rect(fill = 'transparent', color = 'grey95')
      )
  }
  
  plt.final <- grid.arrange(orig_plot, footer, heights=unit(c(1, 12), c('null','pt')))
  plt <- ggdraw(plt.final) + draw_image(logo_file, x = 0.002 * (base_size_rat_wid), y = 0, hjust = 0, vjust = 0, height = logo_size, width = 0.08 * (base_size_rat_wid))
  #ggsave(save_name, plt, dpi = 700, height = base_size, width = base_size * (asp))
  return(plt)
}


