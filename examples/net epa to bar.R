library(tidyverse)
library(ggplot2)
library(ggimage)
library(nflfastR)

NFL_pri <- teams_colors_logos$team_color
NFL_sec <- teams_colors_logos$team_color2
names(NFL_pri) <- teams_colors_logos$team_abbr
names(NFL_sec) <- teams_colors_logos$team_abbr

pbp20_df <- readRDS(url(paste0('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2020.rds')))

p <- pbp20_df %>% 
  filter(fumble_lost==1 | interception == 1) %>% 
  select(posteam, defteam, epa) %>% 
  pivot_longer(c(posteam, defteam), values_to = 'team', names_to = 'team_type') %>% 
  mutate(
    epa = ifelse(team_type=='posteam', epa, -epa),
    net_turnover = ifelse(team_type=='posteam', -1, 1),
    off_turnover = ifelse(team_type=='posteam', -1, 0),
    def_turnover = ifelse(team_type=='posteam', 0, 1),
    team_type = NULL
  ) %>% 
  group_by(team) %>% 
  summarise_all(sum) %>% 
  arrange(-epa) %>% 
  mutate(
    team = factor(team, rev(team)),
    to_lab = paste0('#', row_number(), ') ', team, ' (Off: ', off_turnover, ', Def: ', def_turnover, ', Net: ',net_turnover,')'),
    img_url = paste0('https://a.espncdn.com/i/teamlogos/nfl/500/', team, '.png')
  ) %>% 
  ggplot(aes(x = team, y = epa, label = to_lab, image = img_url)) +  
  geom_bar(aes(color = team, fill = team), stat = 'identity', size = 0.7, width = 0.7, alpha = 0.8, show.legend = F) +
  geom_text(aes(y = ifelse(epa > 0, -1, 1), hjust = ifelse(epa > 0, 1, 0)), color = 'grey20', size = 2.4) +
  ggimage::geom_image(aes(y = epa + ifelse(epa>0,1,-1) * .05 * diff(range(epa))), size = 0.06, asp = 9/16) +
  coord_flip() +
  scale_y_continuous(limits = c(-45,45), expand = expansion(add = c(0,0)), breaks = seq(-45,45,15)) +
  scale_fill_manual(values = NFL_pri) +
  scale_color_manual(values = NFL_sec) +
  labs(title = 'EPA Impact from Turnovers, 2020',
       subtitle = 'Net EPA Gained/Lost on Takeaways/Giveaways through 2020 Week 4',
       x = NULL,
       y = 'Net Cummulative EPA') +
  theme_minimal() +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank()
  )

ggsave('net epa to bar.png', p, dpi = 'retina', width = 9, height = 16, scale = 0.7)
