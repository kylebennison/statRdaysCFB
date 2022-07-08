# statRdaysCFB 1.0.0
Work With The Collegefootballdata.com API, Easily

This package will allow you to easily interact with the cfbdata API without having to worry about the boring stuff.

## Getting Started

Install the package using
``` r
devtools::install_github("kylebennison/statRdaysCFB")
```

Load it using
``` r
library(statRdaysCFB)
```

The first thing you need to do is set your API key to your environment. Do this using:
``` r
set_cfb_api_key("yourkeygoeshere")
```

If you don't have an API key, you'll need to request one first from collegefootballdata.com/key. They're free!

Once you set your key, you can confirm that it's set properly by calling ```my_key()```.

## Getting Data

You can easily get play-by-play data using the function ```get_plays()```:

``` r
plays_df <- get_plays(start_week = 1, end_week = 15, start_year = 2021, end_year = 2021)
```

Add success rate stats using ```add_success()```:
``` r
plays_w_success <- add_success(plays_df)
```

You can get mascots, team nicknames, logo image urls, and more using ```get_colors()```:
``` r
colors <- get_colors()

# Join to plays for plotting teams using their team colors

library(dplyr)
plays_w_colors <- plays_w_success %>% 
  left_join(colors, by = c("offense" = "school")) %>% 
  left_join(colors, by = c("defense" = "school"),
            suffix = c("_offense", "_defense"))
            
# Now you can plot colors:
library(tidyr)

plays_w_colors <- plays %>% 
  left_join(colors, by = c("offense" = "school")) %>% 
  left_join(colors, by = c("defense" = "school"),
            suffix = c("_offense", "_defense"))

library(ggplot2)
plays_w_colors %>% 
  filter(game_id == 401331236) %>% 
  mutate(home_score = if_else(offense == home, offense_score, defense_score),
         away_score = if_else(offense == away, offense_score, defense_score),
         home_color = if_else(offense == home, color_offense, color_defense),
         away_color = if_else(offense == away, color_offense, color_defense),
         play_num = row_number(id.x)) %>%
  ggplot(aes(x = as.numeric(play_num))) + 
  geom_line(aes(y = home_score, color = home_color), size = 2) +
  geom_line(aes(y = away_score, color = away_color), size = 2) +
  scale_color_identity(guide = "legend", labels = c("Penn State", "Wisconsin")) +
  labs(x = "Play",
       y = "Score",
       color = "Team",
       caption = "Data: @cfb_data via statRdaysCFB") +
  staturdays_theme
```

![image](https://user-images.githubusercontent.com/66328277/177992942-db2409f6-c9bc-4e39-bfc2-305d20824b44.png)


## Usage

Please tag [@cfb_data](https://twitter.com/cfb_data) and [@staturdays](https://twitter.com/Staturdays) in any work created using this data/package.
