This repository contains code and images used to create data visualizations. I'll be updating it with new materials periodically.
`
wordmark_url = function(x) ifelse(is.na(x),NA,paste0('https://raw.githubusercontent.com/ajreinhard/data-viz/master/wordmark/',x,'.png'))
helmet_url = function(x) ifelse(is.na(x),NA,paste0('https://raw.githubusercontent.com/ajreinhard/data-viz/master/helmet_left/',x,'.png'))
ESPN_logo_url = function(x) ifelse(is.na(x),NA,ifelse(x %in% c('WAS','KC'),paste0('https://raw.githubusercontent.com/ajreinhard/data-viz/master/ggplot/',x,'.png'),paste0('https://a.espncdn.com/i/teamlogos/nfl/500/',x,'.png')))
`
