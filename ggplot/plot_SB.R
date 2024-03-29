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
library(ggridges)
library(nflfastR)
library(nflreadr)

# decide what font I should use based on what is available on computer
font_SB <- ifelse(length(grep('HP Simplified',fonts()))>0,'HP Simplified','Bahnschrift')

# functions to retrieve images
wordmark_url = function(x) ifelse(is.na(x),NA,paste0('https://raw.githubusercontent.com/ajreinhard/data-viz/master/wordmark/',x,'.png'))
helmet_url = function(x) ifelse(is.na(x),NA,paste0('https://raw.githubusercontent.com/ajreinhard/data-viz/master/helmet_left/',x,'.png'))
ESPN_logo_url <- function(x) ifelse(is.na(x),NA,paste0('https://a.espncdn.com/i/teamlogos/nfl/500/',x,'.png'))
alt_logo_url <- function(x) ifelse(is.na(x),NA,paste0('https://raw.githubusercontent.com/ajreinhard/data-viz/master/alt-logo/',x,'.png'))
helm2023 <- function(team, side) paste0('https://raw.githubusercontent.com/ajreinhard/data-viz/master/2023_helm/',team,'_',side,'.png')

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
  logo_file <- magick::image_read('https://raw.githubusercontent.com/ajreinhard/data-viz/master/ggplot/statbutler.png')
  
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
    if (axis_rot) {
      border_layers <- c(border_layers, annotation_custom(make_gradient(deg = 180 - axis_adj), xmin=-Inf, xmax=x_lim[1], ymin=-Inf, ymax=Inf))
    } else {
      border_layers <- c(border_layers, annotation_custom(make_gradient(deg = 90 + axis_adj), xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=y_lim[1]))
    }
  }
  if (grepl('r',fade_borders)) {
    border_layers <- c(border_layers, annotation_custom(make_gradient(deg = 0 - axis_adj), xmin=x_lim[2], xmax=Inf, ymin=-Inf, ymax=Inf))
  }
  if (grepl('l',fade_borders)) {
    if (axis_rot) {
      border_layers <- c(border_layers, annotation_custom(make_gradient(deg = 90 + axis_adj), xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=y_lim[1]))
    } else {
      border_layers <- c(border_layers, annotation_custom(make_gradient(deg = 180 - axis_adj), xmin=-Inf, xmax=x_lim[1], ymin=-Inf, ymax=Inf))
    }
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
				     
table_theme_SB <- function (data, width = 800) {
  data %>%
  tab_options(
  table.font.color = 'darkblue',
  data_row.padding = '2px',
  row_group.padding = '3px',
  table.width = paste0(width,'px'),
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


# add logo to table and space around outside of image
#brand_table <- function(gt_table, file, data_home, base_size = base_size, t = 3, r = 6, b = 8, l = 6) {
#  gt_table %>% gtsave(file)
#  
#  img <- png::readPNG(file)
#  img <- img[11:(nrow(img)-10),11:(ncol(img)-10),]
#  
#  p <- ggplot(iris, aes(Species, Sepal.Length))+
#    annotation_custom(rasterGrob(img, width = unit(1,"npc"), height = unit(1,"npc")), -Inf, Inf, -Inf, Inf) +
#    theme_void() +
#    theme(
#      plot.margin = unit(c(t,r,b,l),units = 'points'),
#      plot.background = element_rect(fill = 'grey95', color = NA)
#    )
#  
#  brand_plot(p, asp = ncol(img)/nrow(img), save_name = file, data_home = data_home, base_size = base_size)
#}	

# better brand table function
brand_table <- function(gt_table, file, data_home, base_size = base_size) {
  gt_table %>% gtsave(file)
  
  img <- png::readPNG(file)
  img <- img[11:(nrow(img)-10),11:(ncol(img)-10),]
  
  p <- ggplot(iris, aes(Species, Sepal.Length))+
    annotation_custom(rasterGrob(img, width = unit(0.965, 'npc'), height = unit(0.965, 'npc'), y = unit(0.995, 'npc'), vjust = 1), -Inf, Inf, -Inf, Inf) +
    theme_void() +
    theme(plot.background = element_rect(fill = 'grey95', color = NA))
  
  brand_plot(p, asp = ncol(img)/nrow(img) * 0.965, save_name = file, data_home = data_home, base_size = base_size)
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

# rsq function
rsq <- function (x, y) cor(x, y) ^ 2

	
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
grob_img_adj <- function(img_url, alpha = 1, whitewash = 0, boost_color = T, bw = F) {
  return(lapply(img_url, function(x) {
    if(is.na(x)) {
      return(NULL)
    } else {     
      img <- image_read(x)[[1]]
      
      if (bw) {
        grey_scale <- as.integer(img[1,,]) * 0.3 + as.integer(img[2,,]) * 0.59 + as.integer(img[3,,]) * 0.11
        img[1,,] <- as.raw(grey_scale)
        img[2,,] <- as.raw(grey_scale)
        img[3,,] <- as.raw(grey_scale)
      }

      lowest_alpha <- function(x) (255 - as.integer(x)) / 255
      alpha_min <- sapply(1:3, function(x) lowest_alpha(img[x,,])) %>%
        apply(., 1, max) %>% 
        ifelse(. >= alpha, ., alpha)
      
      if(!boost_color) {alpha_min <- 1}
      
      img[1,,] <- as.raw(255 - (255 - as.integer(img[1,,])) * (1-whitewash)  * (1/alpha_min))
      img[2,,] <- as.raw(255 - (255 - as.integer(img[2,,])) * (1-whitewash) * (1/alpha_min))
      img[3,,] <- as.raw(255 - (255 - as.integer(img[3,,])) * (1-whitewash) * (1/alpha_min))
      img[4,,] <- as.raw(as.integer(img[4,,]) * alpha)
      return(grid::rasterGrob(image = image_read(img)))
    }
  }))
}


#functions to get pbp quickly
get_pbp <- function(seasons = 2020)  do.call(rbind, lapply(seasons, function(yr) {
  readRDS(url(paste0('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_',yr,'.rds')))
}))

#function to get nflgamedata.com games
get_games <- function() readRDS(url('http://nflgamedata.com/games.rds'))
				
#adds expected points over incomplete metric (EPOI)	     
add_epoi <- function(pbp_df) {
  no_fumble_return <- pbp_df %>% 
    filter(complete_pass == 1 & fumble_lost == 1) %>% 
    select(game_id, play_id, yards_gained, half_seconds_remaining, yardline_100, season, home_team, posteam, defteam, roof, down, ydstogo, posteam_timeouts_remaining, defteam_timeouts_remaining) %>% 
    mutate(
      down = 1,
      half_seconds_remaining = ifelse(half_seconds_remaining < 6, 0, half_seconds_remaining - 6),
      yardline_100 = 99 - (yardline_100 - yards_gained),
      posteam = defteam,
      ydstogo = 10
    ) %>% 
    calculate_expected_points %>% 
    mutate(ep = -ep) %>% 
    select(game_id, play_id, no_fumb_ret_EP = ep)
  
  
  pbp_df %>% 
    select(game_id, play_id, half_seconds_remaining, yardline_100, season, home_team, posteam, defteam, roof, down, ydstogo, posteam_timeouts_remaining, defteam_timeouts_remaining) %>% 
    mutate(
      pos_change = ifelse(down == 4, 1, 0),
      down = ifelse(pos_change == 1, 1, down + 1),
      half_seconds_remaining = ifelse(half_seconds_remaining < 5, 0, half_seconds_remaining - 5),
      yardline_100 = ifelse(pos_change == 1, 99 - yardline_100, yardline_100),
      posteam = ifelse(pos_change == 1, defteam, posteam),
      ydstogo = ifelse(pos_change == 1, 10, ydstogo)
    ) %>% 
    calculate_expected_points %>% 
    mutate(ep = ifelse(pos_change == 1, -ep, ep)) %>% 
    select(game_id, play_id, incomplete_ep = ep) %>% 
    left_join(no_fumble_return) %>% 
    right_join(pbp_df) %>% 
    mutate(
      epoi = ifelse(is.na(no_fumb_ret_EP), ep + epa, no_fumb_ret_EP) - incomplete_ep,
      epoi = ifelse(complete_pass == 1, epoi, 0)
    ) %>% 
    return
}

# used to create branded videos
Scene2 <- ggproto(
  "Scene2",
  gganimate:::Scene,
  plot_frame = function(self, plot, i, newpage = is.null(vp), vp = NULL, widths = NULL, heights = NULL, ...) {
    plot <- self$get_frame(plot, i)
    plot <- ggplot_gtable(plot)
    
    # insert changes here
    logo_file <- magick::image_read('https://raw.githubusercontent.com/ajreinhard/data-viz/master/ggplot/statbutler.png')
    
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


# functions for manipulating games files from 538 or nflgamedata.com				     
double_games <- function(df) {
  home_tms <- df %>% 
    rename_with(function(x) gsub('home_','team_', x), contains('home_')) %>% 
    rename_with(function(x) gsub('away_','opp_', x), contains('away_')) %>% 
    rename(team = team_team, opp = opp_team)
  
  away_tms <- df %>% 
    rename_with(function(x) gsub('away_','team_', x), contains('away_')) %>% 
    rename_with(function(x) gsub('home_','opp_', x), contains('home_')) %>% 
    rename(team = team_team, opp = opp_team) %>% 
    mutate(location = ifelse(location == 'Home', 'Away', location)) %>% 
    mutate_at(vars(one_of('result','spread_line')), function(x) -x) %>% 
    relocate(home_tms %>% names)
  
  rbind(away_tms, home_tms) %>% arrange(gameday) %>% return
}

# convert team cols to franchise
team2fran <- function (df) {
  df %>% 
    mutate_at(
      .vars = vars(ends_with('team')),
      .funs = function(tm) {
        case_when(
          tm %in% c('OAK','LRD') ~ 'LV',
          tm %in% c('SLC','PHX') ~ 'ARI',		
          tm %in% c('STL','LAR') ~ 'LA',
          tm == 'HSO' ~ 'TEN',
          tm == 'SD' ~ 'LAC',
          tm == 'BLC' ~ 'IND',
          tm == 'BOS' ~ 'NE',		
          TRUE ~ tm
        )
      }
    ) %>% 
    return
}

# convert franchise cols to team
fran2team <- function (df) {
  df %>% 
    mutate_at(
      .vars = vars(ends_with('team')),
      season = df %>% pull(season),
      .funs = function(tm, season) {
        case_when(
          tm == 'NE' & season < 1971 ~ 'BOS',
          tm == 'LV' & season < 1982 ~ 'OAK',
          tm == 'IND' & season < 1984 ~ 'BLC',
          tm == 'ARI' & season < 1988 ~ 'SLC',
          tm == 'ARI' & season < 1994 ~ 'PHX',
          tm == 'LV' & season < 1995 ~ 'LRD',
          tm == 'TEN' & season < 1997 ~ 'HSO',
          tm == 'LA' & season < 2016 & season >= 1995 ~ 'STL',
          tm == 'LAC' & season < 2017 ~ 'SD',
          tm == 'LV' & season < 2020 ~ 'OAK',
          TRUE ~ tm
        )
      }
    ) %>% 
    return
}

# get URL to a historical team logo 
hist_logo_url <- function (df) {
  df %>% 
    mutate(
      logo_url = case_when(
        team %in% c('ARI','PHX','SLC') & season < 2005 ~ alt_logo_url('ARI94'),
        team == 'ATL' & season < 1990 ~ alt_logo_url('ATL66'),
        team == 'ATL' & season < 2003 ~ alt_logo_url('ATL90'),
        team == 'BAL' & season < 1999 ~ alt_logo_url('BAL96'),
        team == 'BUF' & season < 1974 ~ alt_logo_url('BUF70'),
        team == 'CAR' & season < 2012 ~ alt_logo_url('CAR95'),
        team == 'CHI' & season < 1974 ~ alt_logo_url('CHI62'),
        team == 'CIN' & season < 1981 ~ alt_logo_url('CIN70'),
        team == 'CIN' & season < 1990 ~ alt_logo_url('CIN81'),
        team == 'CIN' & season < 1997 ~ alt_logo_url('CIN90'),
        team == 'CIN' & season < 2004 ~ alt_logo_url('CIN97'),
        team == 'CLE' & season < 1970 ~ alt_logo_url('CLE59'),
        team == 'CLE' & season < 1986 ~ alt_logo_url('CLE70'),
        team == 'CLE' & season < 1992 ~ alt_logo_url('CLE86'),
        team == 'CLE' & season < 2006 ~ alt_logo_url('CLE99'),
        team == 'CLE' & season < 2015 ~ alt_logo_url('CLE06'),
        team == 'DEN' & season < 1993 ~ alt_logo_url('DEN70'),
        team == 'DEN' & season < 1997 ~ alt_logo_url('DEN93'),
        team == 'DET' & season < 2003 ~ alt_logo_url('DET70'),
        team == 'DET' & season < 2009 ~ alt_logo_url('DET03'),
        team == 'DET' & season < 2017 ~ alt_logo_url('DET09'),
        team == 'GB' & season < 1980 ~ alt_logo_url('GB61'),
        team %in% c('IND','BLC') & season < 1979 ~ alt_logo_url('BLC61'),
        team %in% c('IND','BLC') & season < 2002 ~ alt_logo_url('IND84'),
        team == 'JAX' & season < 2002 ~ alt_logo_url('JAX95'),
        team == 'JAX' & season < 2013 ~ alt_logo_url('JAX02'),
        team %in% c('SD','LAC') & season < 1974 ~ alt_logo_url('SD61'),
        team %in% c('SD','LAC') & season < 1988 ~ alt_logo_url('SD74'),
        team %in% c('SD','LAC') & season < 2002 ~ alt_logo_url('SD88'),
        team %in% c('SD','LAC') & season < 2007 ~ alt_logo_url('SD02'),
        team %in% c('SD','LAC') & season < 2020 ~ alt_logo_url('LAC17'),
        team == 'LA' & season < 1983 ~ alt_logo_url('LA66'),
        team == 'LA' & season < 1989 ~ alt_logo_url('LA83'),
        team == 'LA' & season < 1995 ~ alt_logo_url('LA89'),
        team %in% c('STL','LA') & season < 2000 ~ alt_logo_url('STL95'),
        team %in% c('STL','LA') & season < 2017 ~ alt_logo_url('LA16'),
        team == 'LA' & season < 2020 ~ alt_logo_url('LA17'),
        team == 'MIA' & season < 1974 ~ alt_logo_url('MIA66'),
        team == 'MIA' & season < 1989 ~ alt_logo_url('MIA74'),
        team == 'MIA' & season < 1997 ~ alt_logo_url('MIA89'),
        team == 'MIA' & season < 2013 ~ alt_logo_url('MIA97'),
        team == 'MIA' & season < 2018 ~ alt_logo_url('MIA13'),
        team == 'MIA' ~ alt_logo_url('MIA'),
        team %in% c('NE','BOS') & season < 1993 ~ alt_logo_url('BOS60'),
        team == 'NE' & season < 2000 ~ alt_logo_url('NE93'),
        team == 'NO' & season < 2000 ~ alt_logo_url('NO67'),
        team == 'NO' & season < 2002 ~ alt_logo_url('NO00'),
        team == 'NO' & season < 2012 ~ alt_logo_url('NO02'),
        team == 'NO' & season < 2017 ~ alt_logo_url('NO12'),
        team == 'NYG' & season < 1976 ~ alt_logo_url('NYG61'),
        team == 'NYG' & season < 2000 ~ alt_logo_url('NYG76'),
        team == 'NYG' ~ alt_logo_url('NYG'),
        team == 'NYJ' & season < 1967 ~ alt_logo_url('NYJ64'),
        team == 'NYJ' & season < 1970 ~ alt_logo_url('NYJ67'),
        team == 'NYJ' & season < 1978 ~ alt_logo_url('NYJ70'),
        team == 'NYJ' & season < 1998 ~ alt_logo_url('NYJ78'),
        team == 'NYJ' & season < 2019 ~ alt_logo_url('NYJ98'),
        team == 'PHI' & season < 1969 ~ alt_logo_url('PHI48'),
        team == 'PHI' & season < 1973 ~ alt_logo_url('PHI69'),
        team == 'PHI' & season < 1987 ~ alt_logo_url('PHI73'),
        team == 'PHI' & season < 1996 ~ alt_logo_url('PHI87'),
        team == 'PIT' & season < 2002 ~ alt_logo_url('PIT69'),
        team == 'PIT' ~ alt_logo_url('PIT'),
        team == 'SF' & season < 1996 ~ alt_logo_url('SF68'),
        team == 'SEA' & season < 2002 ~ alt_logo_url('SEA76'),
        team == 'SEA' & season < 2012 ~ alt_logo_url('SEA02'),
        team == 'TB' & season < 1997 ~ alt_logo_url('TB76'),
        team == 'TB' & season < 2014 ~ alt_logo_url('TB97'),
        team == 'TB' & season < 2020 ~ alt_logo_url('TB14'),
        team %in% c('TEN','HSO') & season < 1999 ~ alt_logo_url('TEN97'),
        team == 'KC' ~ alt_logo_url('KC'),
        TRUE ~ ESPN_logo_url(team)
      )
    ) %>% 
    return
}

# map to older team color combos
hist_color_map <- function (df) {
  df %>% 
    mutate(
      color_key = case_when(
        team == 'CLE' & season < 2015 ~ 'CLE46',
        team == 'CIN' & season < 1981 ~ 'CIN68',
        team == 'DEN' & season < 1997 ~ 'DEN68',
        team == 'DET' & season < 2009 ~ 'DET70',
        team == 'DET' & season < 2017 ~ 'DET09',
        team %in% c('IND','BLC') & season < 2002 ~ 'IND84',
        team == 'IND' & season < 2020 ~ 'IND19',
        team %in% c('SD','LAC') & season < 1974 ~ 'SD61',
        team %in% c('SD','LAC') & season < 1988 ~ 'SD74',
        team %in% c('SD','LAC') & season < 2019 ~ 'SD',
        team == 'LA' & season < 1973 ~ 'LA64',
        team %in% c('STL','LA') & season < 2000 ~ 'LA73',
        team %in% c('STL','LA') & season < 2018 ~ 'STL',
        team == 'LA' & season < 2020 ~ 'LA73',
        team == 'MIA' & season < 1989 ~ 'MIA66',
        team == 'MIA' & season < 2001 ~ 'MIA89',
        team == 'MIA' & season < 2013 ~ 'MIA01',
        team == 'MIA' & season < 2018 ~ 'MIA13',
        team %in% c('NE','BOS') & season < 1993 ~ 'BOS60',
        team == 'NE' & season < 2000 ~ 'NE93',
        team == 'NO' & season < 2000 ~ 'NO67',
        team == 'NYJ' & season < 1970  ~ 'NYJ64',
        team == 'NYJ' & season < 1978 ~ 'NYJ70',
        team == 'NYJ' & season < 1990 ~ 'NYJ78',
        team == 'NYJ' & season < 1998 ~ 'NYJ90',
        team == 'NYJ' & season < 2019 ~ 'NYJ98',
        team == 'PHI' & season < 1974 ~ 'PHI36',
        team == 'PHI' & season < 1985 ~ 'PHI74',
        team == 'PHI' & season < 1996 ~ 'PHI85',
        team == 'PHI' & season < 2014 ~ 'PHI96',
        team == 'SEA' & season < 2002 ~ 'SEA76',
        team == 'SEA' & season < 2012 ~ 'SEA02',
        team == 'TB' & season < 1997 ~ 'TB76',
        team == 'TB' & season < 2014 ~ 'TB97',
        team == 'TB' & season < 2020 ~ 'TB14',
        team %in% c('TEN','HSO') & season < 1999 ~ 'TEN97',
        TRUE ~ team
      )
    ) %>% 
    return
}
      
	      

	      
#used for creating game_id below
leading_zero <- function(x, max_len) sapply(x, function(y) ifelse(nchar(y) >= max_len, y, paste0(rep('0', max_len - nchar(y)), y)))

#get 538 elo data frame
get_538elo <- function() {
  read_csv('https://projects.fivethirtyeight.com/nfl-api/nfl_elo.csv') %>% 
    rename(gameday = date, game_type = playoff, location = neutral) %>% 
    rename_with(function(x) paste0('home_', gsub('1','', x)), contains('1')) %>% 
    rename_with(function(x) paste0('away_', gsub('2','', x)), contains('2')) %>% 
    team2fran %>% 
    fran2team %>% 
    group_by(season) %>% 
    mutate(
      gameday = as.Date(gameday),
      weekday = format(gameday, '%A'), 
      week1_wed = min(gameday) - as.numeric(format(min(gameday), '%u')) + 3,
      week = as.numeric(ceiling((gameday - week1_wed)/7)),
      week = ifelse(gameday == '2012-09-05', 1, week),
      week = ifelse(season == 2001 & week > 1, week - 1, week),
      week1_wed = NULL,
      home_team = case_when(
        home_team == 'WSH' ~ 'WAS',
        TRUE ~ home_team
      ),
      away_team = case_when(
        away_team == 'WSH' ~ 'WAS',
        TRUE ~ away_team
      ),
      game_id = paste(season, leading_zero(week, 2), away_team, home_team, sep = '_'),
      game_type = case_when(
	is.na(game_type) ~ 'REG',
        game_type == 's' ~ 'SB',
        game_type == 'c' ~ 'CON',
        game_type == 'd' ~ 'DIV',
        game_type == 'w' ~ 'WC'
      ),
      location = ifelse(location == 0, 'Home', 'Neutral'),
      result = home_score - away_score,
      total = home_score + away_score
    ) %>%
    ungroup %>% 
    relocate(game_id) %>% 
    relocate(c(game_type, week, gameday, weekday), .after = season) %>% 
    relocate(c(home_score, away_score, location, result, total), .after = away_team) %>% 
    return
}

# get active roster from PFF
pff_active_rosters <- function() {

lapply(1:32, function(i) {
  Sys.sleep(2)
  
    paste0('https://www.pff.com/api/teams/',i,'/roster') |>  
      jsonlite::fromJSON() |>  
      extract2('team_players') |>  
      as_tibble() |>  
      (function(x) return(x))()
  }) %>%
  bind_rows() |>  
  type.convert(as.is = T) |> 
  mutate(team = team_abbr_mapping[paste0(team_name)])

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
             'IND'='#013369',
             'JAX'='#006778',
             'KC'='#e31837',
             'LAC'='#0080C6',
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
             'WAS'='#773141',
             'IND19'='#002c5f',
             'IND84'='#02168C',
             'LA73'='#0B215E',
             'LA64'='#022464',
             'MIA13'='#008e97',
             'MIA01'='#2496a4',
             'MIA89'='#366f88',
             'MIA66'='#1A7395',
             'BOS60'='#CC122C',
             'NE93'='#012169',
             'NYJ98'='#0e421b',
             'NYJ90'='#046A3C',
             'NYJ78'='#046A3C',
             'NYJ70'='#004B23',
             'NYJ64'='#006E35',
             'PHI96'='#004040',
             'PHI85'='#05683C',
             'PHI74'='#05683C',
             'PHI36'='#05683C',
             'SEA02'='#3c5376',
             'SEA76'='#0f358c',
             'TB14'='#d50a0a',
             'TB97'='#A6192E',
             'TB76'='#FF8200',
             'TEN97'='#418FDE',
             'CLE46'='#EC5614',
             'DEN68'='#FD4614',
             'CIN68'='#000000',
             'DET09'='#006DB0',
             'DET70'='#005DA6',
             'SD74'='#041A6F',
             'SD61'='#0472CC',
             'NO67'='#B9932C',
             'PHX'='#97233f',
             'SLC'='#97233f'
)


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
             'JAX'='#9F792C',
             'KC'='#ffb612',
             'LAC'='#FFC20E',
             'LA'='#ffd100',
             'MIA'='#fc4c02',
             'MIN'='#ffc62f',
             'NE'='#c60c30',
             'NO'='#000000',
             'NYG'='#a71930',
             'NYJ'='#000000',
             'OAK'='#a5acaf',
             'LV'='#a5acaf',
             'PHI'='#000000',
             'PIT'='#ffb612',
             'SD'='#F4B41C',
             'SF'='#b3995d',
             'SEA'='#69be28',
             'STL'='#b3995d',
             'TB'='#34302b',
             'TEN'='#4b92db',
             'WAS'='#ffb612',
             'IND19'='#a5acaf',
             'IND84'='#a5acaf',
             'LA73'='#FECE0C',
             'LA64'='#a5acaf',
             'MIA13'='#f58220',
             'MIA01'='#FF3E00',
             'MIA89'='#FF3E00',
             'MIA66'='#FF3E00',
             'BOS60'='#04328C',
             'NE93'='#c8102e',
             'NYJ98'='#1F3731',
             'NYJ90'='#000000',
             'NYJ78'='#a5acaf',
             'NYJ70'='#a5acaf',
             'NYJ64'='#a5acaf',
             'PHI96'='#000000',
             'PHI85'='#BFBFBF',
             'PHI74'='#BFBFBF',
             'PHI36'='#BFBFBF',
             'SEA02'='#002144',
             'SEA76'='#008F4F',
             'TB14'='#34302B',
             'TB97'='#696158',
             'TB76'='#C8102E',
             'TEN97'='#CC122C',
             'CLE46'='#342624',
             'DEN68'='#041386',
             'CIN68'='#F36A24',
             'DET09'='#b0b7bc',
             'DET70'='#b0b7bc',
             'SD74'='#F0A804',
             'SD61'='#FBCE04',
             'NO67'='#000000',
             'PHX'='#000000',
             'SLC'='#000000'
)
	

		
NFL_point_fill <- c(
'ARI'='#ffffff',
'ATL'='#a71930',
'BAL'='#9e7c0c',
'BUF'='#00338d',
'CAR'='#0085ca',
'CHI'='#0b162a',
'CIN'='#fb4f14',
'CLE'='#22150c',
'DAL'='#acc0c6',
'DEN'='#fb4f14',
'DET'='#b0b7bc',
'GB'='#ffb612',
'HOU'='#03202f',
'IND'='#ffffff',
'JAX'='#006778',
'KC'='#ffb612',
'LA'='#ffd100',
'LAC'='#FFC20E',
'LV'='#a5acaf',
'MIA'='#ffffff',
'MIN'='#ffc62f',
'NE'='#b0b7bc',
'NO'='#9f8958',
'NYG'='#0b2265',
'NYJ'='#203731',
'OAK'='#a5acaf',
'PHI'='#a5acaf',
'PIT'='#000000',
'SD'='#ffb612',
'SEA'='#69be28',
'SF'='#b3995d',
'STL'='#b3995d',
'TB'='#34302b',
'TEN'='#4b92db',
'WAS'='#ffb612'
)

		
NFL_point_text <- c(
'ARI'='#97233f',
'ATL'='#000000',
'BAL'='#241773',
'BUF'='#ffffff',
'CAR'='#000000',
'CHI'='#c83803',
'CIN'='#000000',
'CLE'='#fb4f14',
'DAL'='#002244',
'DEN'='#002244',
'DET'='#005a8b',
'GB'='#203731',
'HOU'='#ffffff',
'IND'='#002c5f',
'JAX'='#ffffff',
'KC'='#e31837',
'LA'='#003594',
'LAC'='#0080C6',
'LV'='#000000',
'MIA'='#008e97',
'MIN'='#4f2683',
'NE'='#002244',
'NO'='#000000',
'NYG'='#ffffff',
'NYJ'='#ffffff',
'OAK'='#000000',
'PHI'='#004953',
'PIT'='#ffb612',
'SD'='#002244',
'SEA'='#002244',
'SF'='#aa0000',
'STL'='#002244',
'TB'='#d50a0a',
'TEN'='#002244',
'WAS'='#773141'
)
