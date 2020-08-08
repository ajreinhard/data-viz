library(rtweet)
library(tidyverse)
library(ggplot2)
library(ggimage)

## your twitter handle
handle <- 'reinhurdler'

## tibble of your followers
followers <- handle %>%
  get_followers(n = 75000, retryonratelimit = T) %>%
  pull(user_id) %>%
  lookup_users

## tibble of accounts you follow
following <- handle %>%
  get_friends %>%
  pull(user_id) %>%
  lookup_users

## accounts followed by accounts you follow
## does not include accounts you follow that follow >5k accounts
following_following <- following %>%
  filter(friends_count <= 5000 & friends_count > 0 & protected==F) %>%
  pull(user_id) %>%
  get_friends(retryonratelimit = T)

## top 30 accounts followed by accounts you follow
top_following_count <- following_following %>%
  group_by(user_id) %>%
  summarize(n = n()) %>%
  arrange(-n) %>%
  slice(1:31)

## info about the top 30 accounts
top_following_info <- top_following_count %>%
  pull(user_id) %>%
  lookup_users %>%
  filter(screen_name != handle) %>% 
  left_join(top_following_count) %>%
  left_join(following %>% select(user_id) %>% mutate(following = 1)) %>%
  left_join(followers %>% select(user_id) %>% mutate(follower = 1)) %>%
  mutate(
    acct_cat = case_when(
      is.na(follower) & is.na(following) ~ 'No Relationship',
      is.na(follower) & !is.na(following) ~ paste0('Followed By @', handle),
      !is.na(follower) & is.na(following) ~ paste0('Following @', handle),
      !is.na(follower) & !is.na(following) ~ 'Mutual Following'
    ),
    user_id = factor(user_id, rev(user_id)),
    acct_cat = factor(acct_cat, c('Mutual Following',paste0('Following @', handle), paste0('Followed By @', handle), 'No Relationship'))
  )

## basic bar graph
ggplot(data = top_following_info, aes(x = user_id, y = n, label = paste0('@',screen_name), image = profile_image_url, fill = acct_cat)) +
  geom_bar(stat = 'identity', color = 'darkblue', width = 0.8) +
  geom_text(aes(y = n - max(n) * 0.035), size = 3, color = 'white', hjust = 1, show.legend = F) +
  geom_image(aes(y = n - max(n) * 0.014), size = 0.024) +
  coord_flip() +
  labs(title = paste0('Who Are Accounts Followed by @', handle, ' Following?'),
       subtitle = paste0('Based on ',length(unique(following_following$user)), ' of ',nrow(following), ' Accounts Followed by @', handle),
       y = '# of Followers') +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank()
  )


## my own personal source code and graph
source('https://github.com/ajreinhard/data-viz/raw/master/ggplot/plot_SB.R')

p <- ggplot(data = top_following_info, aes(x = user_id, y = n, label = paste0('@',screen_name), image = gsub('_normal','_400x400',profile_image_url), fill = acct_cat)) +
  geom_bar(stat = 'identity', color = 'darkblue', width = 0.8, size = 0.6) +
  geom_text(aes(y = n - max(n) * 0.06), size = 4, family = font_SB, color = 'white', hjust = 1, show.legend = F) +
  geom_image(aes(y = n - max(n) * 0.0245), size = 0.043, asp = 9/16) +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), breaks = seq(0,1000,10)) +
  scale_fill_manual(values = c(
    'Mutual Following' = '#1DA1F2',
    'Following @reinhurdler' = 'violet',
    'Followed By @reinhurdler' = 'darkorange1',
    'No Relationship' = 'grey60'
  )) +
  labs(title = paste0('Who Are Accounts Followed by @', handle, ' Following?'),
       subtitle = paste0('Based on ',length(unique(following_following$user)), ' of ',nrow(following), ' Accounts Followed by @', handle),
       y = paste0('# of Followers From @', handle, '\'s Following List'),
       fill = NULL) +
  theme_SB +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank()
  )

brand_plot(p, asp = 9/16, save_name = 'twitter followin.png', data_home = 'Data: Twitter API', fade_borders = 'tr', axis_rot = T)


