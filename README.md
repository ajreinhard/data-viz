# NFL Team Images  
I've saved a few different images in this repo that can help identify NFL teams on graphs. All file names the nflfastR team abbrevations.

## Team Logos  
I typically use ESPN logos in my graphs, but I no longer wish to use the official logos for Washington and Kansas City. For Washington, I'll instead be using their present-day alternate logo which is a yellow cursive R. Kansas City has no alternate logo so I simply cropped the outlines of their primary logo and left the "KC". These logos can be viewed in the [alt-logo folder](https://github.com/ajreinhard/data-viz/tree/master/alt-logo). I have been using this function to retrieve team logos in R:  
```
ESPN_logo_url = function(x) ifelse(is.na(x),NA,ifelse(x %in% c('WAS','KC'),paste0('https://raw.githubusercontent.com/ajreinhard/data-viz/master/alt-logo/',x,'.png'),paste0('https://a.espncdn.com/i/teamlogos/nfl/500/',x,'.png')))
```  

## Helmets  
I have every NFL team's updated right-facing (left-hand side) helmet saved in the [Helmet Left folder](https://github.com/ajreinhard/data-viz/tree/master/helmet_left). Each image has a transparent background with a thin white outline. Again, I will not be using the primary helmets for Washington or Kansas City. Washington's is a modified version of the helmet they wore in the 1970 and 1971 season. Kansas CIty has never worn an alternative to their current helmet, so I will be using an all red helmet with white "KC" initials. The easiest way to retrieve the helmets in R would be using the function below:  
```
helmet_url = function(x) ifelse(is.na(x),NA,paste0('https://raw.githubusercontent.com/ajreinhard/data-viz/master/helmet_left/',x,'.png'))
```  

## Wordmarks  
I've collected current wordmarks for each team in the [wordmark folder](https://github.com/ajreinhard/data-viz/tree/master/wordmark). Each wordmark has only the team name (with the exception of Washington and Kansas City) with a transparent background. The text is typically in the team's primary color and goes best on a light background. If using these for graphing, be careful to retain the aspect ratio. I recommend geom_grob from the ggpmisc package in R for this.  
```
wordmark_url = function(x) ifelse(is.na(x),NA,paste0('https://raw.githubusercontent.com/ajreinhard/data-viz/master/wordmark/',x,'.png'))
```  
