library(ggplot2)
library(gridExtra)
library(grid)
library(png)
library(cowplot)
library(colorspace)
library(extrafont)
library(RCurl)
library(ggpmisc)
library(magick)
library(ggimage)
library(shadowtext)
library(ggrepel)
library(scales)
library(magrittr)
library(tidyverse)
library(gganimate)
library(gt)

# decide what font I should use based on what is available on computer
font_SB <- ifelse(length(grep('HP Simplified',fonts()))>0,'HP Simplified','Bahnschrift')

# functions to retrieve images
wordmark_url = function(x) ifelse(is.na(x),NA,paste0('https://raw.githubusercontent.com/ajreinhard/data-viz/master/wordmark/',x,'.png'))
helmet_url = function(x) ifelse(is.na(x),NA,paste0('https://raw.githubusercontent.com/ajreinhard/data-viz/master/helmet_left/',x,'.png'))
ESPN_logo_url = function(x) ifelse(is.na(x),NA,ifelse(x=='KC',paste0('https://raw.githubusercontent.com/ajreinhard/data-viz/master/alt-logo/',x,'.png'),paste0('https://a.espncdn.com/i/teamlogos/nfl/500/',x,'.png')))

# my prefered team order for facets
.tm_div_order <- c('BUF', 'MIA', 'NE', 'NYJ', 'BAL', 'CIN', 'CLE', 'PIT', 'HOU', 'IND', 'JAX', 'TEN', 'DEN', 'KC', 'LAC', 'LV', 'DAL', 'NYG', 'PHI', 'WAS', 'CHI', 'DET', 'GB', 'MIN', 'ATL', 'CAR', 'NO', 'TB', 'ARI', 'LA', 'SEA', 'SF')
.tm_div_order_alt <- c('BUF', 'MIA', 'NE', 'NYJ', 'DAL', 'NYG', 'PHI', 'WAS', 'BAL', 'CIN', 'CLE', 'PIT', 'CHI', 'DET', 'GB', 'MIN', 'HOU', 'IND', 'JAX', 'TEN', 'ATL', 'CAR', 'NO', 'TB', 'DEN', 'KC', 'LAC', 'LV', 'ARI', 'LA', 'SEA', 'SF')

# main function to save my branded plots
brand_plot <- function(orig_plot, save_name, asp = 1, base_size = 5, data_home = '', fade_borders = '', fade_prop = 0.5, axis_rot = F, tm_wordmarks = F) {
	
  ## start by adding team wordmarks
  if (tm_wordmarks) {
    orig_plot_bld <- ggplot_gtable(ggplot_build(orig_plot))
    grob_strip_index <- which(sapply(orig_plot_bld$grob, function(x) x$name)=='strip')
    facet_id <- sapply(grob_strip_index, function(grb) {
      orig_plot_bld$grobs[[grb]]$grobs[[1]]$children[[2]]$children[[1]]$label
    })
    
    orig_plot_bld$layout$z[grob_strip_index] <- 0
    
    for (i in 1:length(facet_id)) {
      team_wd <- rasterGrob(image = image_read(wordmark_url(facet_id[i])), vp = viewport(height = .8, width = .6))
      tot_tree <- grobTree(team_wd)
      
      orig_plot_bld$grobs[[grob_strip_index[i]]] <- tot_tree
    }
    orig_plot <- ggdraw(orig_plot_bld)
  }

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
  logo_file <- readPNG(getURLContent('https://raw.githubusercontent.com/ajreinhard/data-viz/master/ggplot/statbutler.png'))
  
  author_txt <- textGrob('By Anthony Reinhard', x=unit(0.08 * (base_size_rat_wid), 'npc'), gp=gpar(col='darkblue', fontfamily=font_SB, fontsize=6), hjust=0)
  data_txt <- textGrob(data_home, x=unit(1 - (.01 * (base_size_rat_wid)), 'npc'), gp=gpar(col='grey95', fontfamily=font_SB, fontsize=6), hjust=1)
  footer_bg <- grid.rect(x = unit(seq(0.5,1.5,length=1000), 'npc'), gp=gpar(col = 'transparent', fill = colorRampPalette(c('grey95', 'darkblue'), space = 'rgb')(1000)), draw = F)
  footer <- grobTree(footer_bg, author_txt, data_txt)

  if (axis_rot) {axis_adj <- 90} else {axis_adj <- 0}

  if (fade_borders!='') {
    ## set up bounds for fade plot
    x_lim <- axis_limits_x(orig_plot) * fade_prop + ggplot_build(orig_plot)$layout$panel_params[[1]]$x.range * (1-fade_prop)
    y_lim <- axis_limits_y(orig_plot) * fade_prop + ggplot_build(orig_plot)$layout$panel_params[[1]]$y.range * (1-fade_prop)
    
    if (axis_rot) {
      x_lim <- axis_limits_x(orig_plot) * fade_prop + ggplot_build(orig_plot)$layout$panel_params[[1]]$y.range * (1-fade_prop)
      y_lim <- axis_limits_y(orig_plot) * fade_prop + ggplot_build(orig_plot)$layout$panel_params[[1]]$x.range * (1-fade_prop)
    }
    
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
        axis.line.y.left = element_line(color = ifelse(grepl('l',fade_borders), 'transparent', 'darkblue')),
        axis.line.y.right = element_line(color = ifelse(grepl('r',fade_borders), 'grey95', 'darkblue')),
        axis.line.x.top = element_line(color = ifelse(grepl('t',fade_borders), 'grey95', 'darkblue')),
        axis.line.x.bottom = element_line(color = ifelse(grepl('b',fade_borders), 'transparent', 'darkblue')),
	panel.border = element_rect(color = 'grey95', size = 0.1),
      )
  }
  
  plt.final <- grid.arrange(orig_plot, footer, heights=unit(c(1, 12), c('null','pt')))
  plt <- ggdraw(plt.final) + draw_image(logo_file, x = 0.002 * (base_size_rat_wid), y = 0, hjust = 0, vjust = 0, height = logo_size, width = 0.08 * (base_size_rat_wid))
  ggsave(save_name, plt, dpi = 700, height = base_size, width = base_size * (asp))
}

# main StatButler theme
theme_SB <-  theme(
  line = element_line(lineend = 'round', color='darkblue'),
  text = element_text(family = font_SB, color='darkblue'),
  plot.background = element_rect(fill = 'grey95', color = 'transparent'),
  panel.border = element_rect(color = 'darkblue', fill = NA),
  panel.background = element_rect(fill = 'white', color = 'transparent'),
  axis.ticks = element_line(color = 'darkblue', size = 0.5),
  axis.ticks.length = unit(2.75, 'pt'),
  axis.title = element_text(size = 8),
  axis.text = element_text(size = 7, color = 'darkblue'),
  plot.title = element_text(size = 14),
  plot.subtitle = element_text(size = 8),
  plot.caption = element_text(size = 5),
  legend.background = element_rect(fill = 'grey90', color = 'darkblue'),
  legend.key = element_blank(),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_line(color='grey85', size = 0.3),
  axis.title.y = element_text(angle = 0, vjust = 0.5),
  strip.background = element_blank(),
  strip.text = element_text(size = 6, color = 'darkblue', family = font_SB),
  legend.position = 'bottom',
  panel.spacing.y = unit(0, 'lines'),
  panel.spacing.x = unit(0.1, 'lines')
) 

# StatButler theme for animations
vid_theme_SB <-  theme(
  line = element_line(lineend = 'round', color='darkblue'),
  text = element_text(family = font_SB, color='darkblue'),
  plot.background = element_rect(fill = 'grey95', color = 'transparent'),
  panel.border = element_rect(color = 'darkblue', fill = NA),
  panel.background = element_rect(fill = 'white', color = 'transparent'),
  axis.ticks = element_line(color = 'darkblue', size = 1.5),
  axis.ticks.length = unit(8.25, 'pt'),
  axis.title = element_text(size = 24),
  axis.text = element_text(size = 21, color = 'darkblue'),
  plot.title = element_text(size = 42),
  plot.subtitle = element_text(size = 24),
  plot.caption = element_text(size = 15),
  legend.background = element_rect(fill = 'grey90', color = 'darkblue'),
  legend.key = element_blank(),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_line(color='grey70', size = 0.9),
  axis.title.y = element_text(angle = 0, vjust = 0.5),
  strip.background = element_blank(),
  strip.text = element_text(size = 18, color = 'darkblue', family = font_SB)
)
				     
table_theme_SB <- function (data) {
  data %>%
  tab_options(
  table.font.color = 'darkblue',
  data_row.padding = '2px',
  row_group.padding = '3px',
  table.width = '800px',
  column_labels.border.bottom.color = 'darkblue',
  column_labels.border.bottom.width = 1.4,
  table_body.border.top.color = 'darkblue',
  row_group.border.top.width = 1.5,
  row_group.border.top.color = '#999999',
  table_body.border.bottom.width = 0.7,
  table_body.border.bottom.color = '#999999',
  row_group.border.bottom.width = 1,
  row_group.border.bottom.color = 'darkblue',
  table.border.top.color = 'transparent',
  table.background.color = '#F2F2F2',
  table.border.bottom.color = 'transparent',
  row.striping.background_color = '#FFFFFF',
  row.striping.include_table_body = TRUE
  ) %>% return
}
				     
				     
# function to set rounded plot limits
properLims <- function(vec) {
  labs <- labeling::extended(min(vec, na.rm = T), max(vec, na.rm = T), m = 5)
  gap <- diff(labs[1:2])
  plot_max <- ifelse(rev(labs)[1] < max(vec, na.rm = T), rev(labs)[1] + gap, rev(labs)[1])
  plot_min <- ifelse(labs[1] > min(vec, na.rm = T), labs[1] - gap, labs[1])
  return(c(plot_min,plot_max))
} 

plus_lab = function(x, accuracy = NULL, suffix = '') paste0(ifelse(x>0,'+',''),number(x, accuracy = accuracy, suffix = suffix, scale = ifelse(suffix == '%', 100, 1)))
plus_lab_format <- function (accuracy = NULL, suffix = '') function(x) plus_lab(x, accuracy = accuracy, suffix = suffix)

full_alpha_hex = function(color, alpha) {
  old_rgb <- col2rgb(color)
  new_rgb <- (255 - (255 - old_rgb) * alpha) / 255
  return(rgb(new_rgb['red',], new_rgb['green',], new_rgb['blue',]))
}
	
# function to set rounded plot limits if scale_x_reverse is used
properLimsRev <- function(vec) {
  labs <- labeling::extended(min(vec, na.rm = T), max(vec, na.rm = T), m = 5)
  gap <- diff(labs[1:2])
  plot_max <- ifelse(rev(labs)[1] < max(vec, na.rm = T), rev(labs)[1] + gap, rev(labs)[1])
  plot_min <- ifelse(labs[1] > min(vec, na.rm = T), labs[1] - gap, labs[1])
  return(c(plot_max,plot_min))
} 

# makes a gradient if I want to fade plot borders
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

# gets the limits of the x axis from a plot
axis_limits_x <- function(p) {
  if (!is.null(ggplot_build(p)$layout$coord$limits$x)) {
    return(ggplot_build(p)$layout$coord$limits$x)
  } else if (!is.null(ggplot_build(p)$layout$panel_scales_x[[1]]$range_c$range)) {
    return(ggplot_build(p)$layout$panel_scales_x[[1]]$range_c$range)
  } else {
    return(ggplot_build(p)$layout$panel_scales_x[[1]]$range$range)
  }
}

# gets the limits of the y axis from a plot
axis_limits_y <- function(p) {
  if (!is.null(ggplot_build(p)$layout$coord$limits$y)) {
    return(ggplot_build(p)$layout$coord$limits$y)
  } else if (!is.null(ggplot_build(p)$layout$panel_scales_y[[1]]$range_c$range)) {
    return(ggplot_build(p)$layout$panel_scales_y[[1]]$range_c$range)
  } else {
    return(ggplot_build(p)$layout$panel_scales_y[[1]]$range$range)
  }
}

# reads in an image as a grob and allows for some image alterations

grob_img_adj<-function(img_url, alpha = 0, whitewash = 0) {
  return(lapply(img_url, function(x) {
    if(is.na(x)) {
      return(NULL)
    }else{     
      img <- image_read(x)[[1]]
      img[1,,] <- as.raw(255 - (255 - as.integer(img[1,,])) * (1-whitewash))
      img[2,,] <- as.raw(255 - (255 - as.integer(img[2,,])) * (1-whitewash))
      img[3,,] <- as.raw(255 - (255 - as.integer(img[3,,])) * (1-whitewash))
      img[4,,] <- as.raw(as.integer(img[4,,]) * (1-alpha))
      return(grid::rasterGrob(image = image_read(img)))
    }
  }))
}

# used to create branded videos
Scene2 <- ggproto(
  "Scene2",
  gganimate:::Scene,
  plot_frame = function(self, plot, i, newpage = is.null(vp), vp = NULL, widths = NULL, heights = NULL, ...) {
    plot <- self$get_frame(plot, i)
    plot <- ggplot_gtable(plot)
    
    # insert changes here
    logo_file <- readPNG(getURLContent('https://raw.githubusercontent.com/ajreinhard/data-viz/master/ggplot/statbutler.png'))
    
    author_txt <- textGrob('By Anthony Reinhard', x=unit(0.065, 'npc'), gp=gpar(col='darkblue', fontfamily=font_SB, fontsize=18), hjust=0)
    data_txt <- textGrob(self$data_home, x=unit(1 - (.01), 'npc'), gp=gpar(col='grey95', fontfamily=font_SB, fontsize=18), hjust=1)
    footer_bg <- grid.rect(x = unit(seq(0.5,1.5,length=1000), 'npc'), gp=gpar(col = 'transparent', fill = colorRampPalette(c('grey95', 'darkblue'), space = 'rgb')(1000)), draw = F)
    footer <- grobTree(footer_bg, author_txt, data_txt)
    
    plt.final <- grid.arrange(plot, footer, heights=unit(c(1, 36), c('null','pt')))
    plot <- ggdraw(plt.final) + draw_image(logo_file, x = 0.002, y = 0, hjust = 0, vjust = 0, height = 0.08, width = 0.1067 * (9/16))
    
    if (!is.null(widths)) plot$widths <- widths
    if (!is.null(heights)) plot$heights <- heights
    if (newpage) grid.newpage()
    grDevices::recordGraphics(
      requireNamespace("gganimate", quietly = TRUE),
      list(),
      getNamespace("gganimate")
    )
    if (is.null(vp)) {
      grid.draw(plot)
    } else {
      if (is.character(vp)) seekViewport(vp)
      else pushViewport(vp)
      grid.draw(plot)
      upViewport()
    }
    invisible(NULL)
  }
)

Scene2$data_home <- NULL

### the next four functions will simply duplicate existing nested gganimate functions and replace them with my personalized Scene2 function
# used to create branded videos
create_scene2 <- function(transition, view, shadow, ease, transmuters, nframes, data_home) {
  if (is.null(nframes)) nframes <- 100
  ggproto(NULL, Scene2, transition = transition, 
          view = view, shadow = shadow, ease = ease, 
          transmuters = transmuters, nframes = nframes,
          data_home = data_home)
}

# used to create branded videos
ggplot_build2 <- gganimate:::ggplot_build.gganim
formals(ggplot_build2) <- c(formals(ggplot_build2), alist(data_home = ))
body(ggplot_build2) <- body(ggplot_build2) %>%
  as.list() %>%
  inset2(4,
         quote(scene <- create_scene2(plot$transition, plot$view, plot$shadow, 
                                      plot$ease, plot$transmuters, plot$nframes, data_home))) %>%
  as.call()


# used to create branded videos
prerender2 <- gganimate:::prerender
formals(prerender2) <- c(formals(prerender2), alist(data_home = ))
body(prerender2) <- body(prerender2) %>%
  as.list() %>%
  inset2(3,
         quote(ggplot_build2(plot, data_home))) %>%
  as.call()


# used to create branded videos
animate_SB <- gganimate:::animate.gganim
formals(animate_SB) <- c(formals(animate_SB)[-length(formals(animate_SB))], alist(data_home = ''), formals(animate_SB)[length(formals(animate_SB))])
body(animate_SB) <- body(animate_SB) %>%
  as.list() %>%
  inset2(8,
         quote(plot <- prerender2(plot, nframes_total, data_home))) %>%
  as.call()


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
'LAC'='#002a5e',
'LA'='#003594',
'MIA'='#008e97',
'MIN'='#4f2683',
'NE'='#002244',
'NO'='#9f8958',
'NYG'='#0b2265',
'NYJ'='#125740',
'OAK'='#000000',
'LV'='#000000',
'PHI'='#004953',
'PIT'='#000000',
'SD'='#002244',	     	     
'SF'='#aa0000',
'SEA'='#002244',
'STL'='#002244',
'TB'='#d50a0a',
'TEN'='#002244',
'WAS'='#773141')


NFL_sec <- c('ARI'='#000000',
'ATL'='#000000',
'BAL'='#9e7c0c',
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
'LAC'='#0080c6',
'LA'='#ffd100',
'MIA'='#fc4c02',
'MIN'='#ffc62f',
'NE'='#c60c30',
'NO'='#000000',
'NYG'='#a71930',
'NYJ'='#000000',
'OAK'='#a5acaf',
'LV'='#a5acaf',
'PHI'='#a5acaf',
'PIT'='#ffb612',
'SD'='#0073cf',
'SF'='#b3995d',
'SEA'='#69be28',
'STL'='#b3995d',
'TB'='#34302b',
'TEN'='#4b92db',
'WAS'='#ffb612')
