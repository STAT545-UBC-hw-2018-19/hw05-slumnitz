Homework 05: Factor and figure management
================
Stefanie Lumnitz
19 October, 2018

### Load required packages

``` r
suppressPackageStartupMessages(library(gapminder))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(knitr))
suppressPackageStartupMessages(library(plotly))
suppressPackageStartupMessages(library(gridExtra))
suppressPackageStartupMessages(library(grid))
suppressPackageStartupMessages(library(scales))
suppressPackageStartupMessages(library(gganimate))
```

# Part 1: Factor management

## Functions:

| function                                  | use                       |
| ----------------------------------------- | ------------------------- |
| `factor(x)` or `forcats::parse_factor(x)` | convert x into a factor   |
| `levels()`                                | value of levels of factor |
| `nlevels()`                               | count of levels of factor |
| `forcats::fct_count()`                    | count                     |
| `droplevels()`                            | drop levels               |

FIirst I will be checking wether there are any factors in our gapminder
dataset:

``` r
glimpse(gapminder)
#> Observations: 1,704
#> Variables: 6
#> $ country   <fct> Afghanistan, Afghanistan, Afghanistan, Afghanistan, ...
#> $ continent <fct> Asia, Asia, Asia, Asia, Asia, Asia, Asia, Asia, Asia...
#> $ year      <int> 1952, 1957, 1962, 1967, 1972, 1977, 1982, 1987, 1992...
#> $ lifeExp   <dbl> 28.801, 30.332, 31.997, 34.020, 36.088, 38.438, 39.8...
#> $ pop       <int> 8425333, 9240934, 10267083, 11537966, 13079460, 1488...
#> $ gdpPercap <dbl> 779.4453, 820.8530, 853.1007, 836.1971, 739.9811, 78...
```

Indeed, `country` and `continent` are factors, delineated by `<fct>`.

``` r
sapply(gapminder, nlevels) %>% 
  kable()
```

|           |   x |
| --------- | --: |
| country   | 142 |
| continent |   5 |
| year      |   0 |
| lifeExp   |   0 |
| pop       |   0 |
| gdpPercap |   0 |

`nlevels()` shows us, that `country` has `142` levels, whereas
`continent` has `5`. `year`, `lifeExp`, `pop` and `gdpPercap` do not
have levels since they are not factors, how we discovered earlier. Let’s
test this in more detail:

``` r
sapply(gapminder, is.factor) %>% 
  kable()
```

|           | x     |
| --------- | :---- |
| country   | TRUE  |
| continent | TRUE  |
| year      | FALSE |
| lifeExp   | FALSE |
| pop       | FALSE |
| gdpPercap | FALSE |

Let’s select all columns or variables for which `is.factor` is `TRUE`
and check which values the `levels` contain:

``` r
select_if(gapminder, is.factor) %>% 
sapply(levels)
#> $country
#>   [1] "Afghanistan"              "Albania"                 
#>   [3] "Algeria"                  "Angola"                  
#>   [5] "Argentina"                "Australia"               
#>   [7] "Austria"                  "Bahrain"                 
#>   [9] "Bangladesh"               "Belgium"                 
#>  [11] "Benin"                    "Bolivia"                 
#>  [13] "Bosnia and Herzegovina"   "Botswana"                
#>  [15] "Brazil"                   "Bulgaria"                
#>  [17] "Burkina Faso"             "Burundi"                 
#>  [19] "Cambodia"                 "Cameroon"                
#>  [21] "Canada"                   "Central African Republic"
#>  [23] "Chad"                     "Chile"                   
#>  [25] "China"                    "Colombia"                
#>  [27] "Comoros"                  "Congo, Dem. Rep."        
#>  [29] "Congo, Rep."              "Costa Rica"              
#>  [31] "Cote d'Ivoire"            "Croatia"                 
#>  [33] "Cuba"                     "Czech Republic"          
#>  [35] "Denmark"                  "Djibouti"                
#>  [37] "Dominican Republic"       "Ecuador"                 
#>  [39] "Egypt"                    "El Salvador"             
#>  [41] "Equatorial Guinea"        "Eritrea"                 
#>  [43] "Ethiopia"                 "Finland"                 
#>  [45] "France"                   "Gabon"                   
#>  [47] "Gambia"                   "Germany"                 
#>  [49] "Ghana"                    "Greece"                  
#>  [51] "Guatemala"                "Guinea"                  
#>  [53] "Guinea-Bissau"            "Haiti"                   
#>  [55] "Honduras"                 "Hong Kong, China"        
#>  [57] "Hungary"                  "Iceland"                 
#>  [59] "India"                    "Indonesia"               
#>  [61] "Iran"                     "Iraq"                    
#>  [63] "Ireland"                  "Israel"                  
#>  [65] "Italy"                    "Jamaica"                 
#>  [67] "Japan"                    "Jordan"                  
#>  [69] "Kenya"                    "Korea, Dem. Rep."        
#>  [71] "Korea, Rep."              "Kuwait"                  
#>  [73] "Lebanon"                  "Lesotho"                 
#>  [75] "Liberia"                  "Libya"                   
#>  [77] "Madagascar"               "Malawi"                  
#>  [79] "Malaysia"                 "Mali"                    
#>  [81] "Mauritania"               "Mauritius"               
#>  [83] "Mexico"                   "Mongolia"                
#>  [85] "Montenegro"               "Morocco"                 
#>  [87] "Mozambique"               "Myanmar"                 
#>  [89] "Namibia"                  "Nepal"                   
#>  [91] "Netherlands"              "New Zealand"             
#>  [93] "Nicaragua"                "Niger"                   
#>  [95] "Nigeria"                  "Norway"                  
#>  [97] "Oman"                     "Pakistan"                
#>  [99] "Panama"                   "Paraguay"                
#> [101] "Peru"                     "Philippines"             
#> [103] "Poland"                   "Portugal"                
#> [105] "Puerto Rico"              "Reunion"                 
#> [107] "Romania"                  "Rwanda"                  
#> [109] "Sao Tome and Principe"    "Saudi Arabia"            
#> [111] "Senegal"                  "Serbia"                  
#> [113] "Sierra Leone"             "Singapore"               
#> [115] "Slovak Republic"          "Slovenia"                
#> [117] "Somalia"                  "South Africa"            
#> [119] "Spain"                    "Sri Lanka"               
#> [121] "Sudan"                    "Swaziland"               
#> [123] "Sweden"                   "Switzerland"             
#> [125] "Syria"                    "Taiwan"                  
#> [127] "Tanzania"                 "Thailand"                
#> [129] "Togo"                     "Trinidad and Tobago"     
#> [131] "Tunisia"                  "Turkey"                  
#> [133] "Uganda"                   "United Kingdom"          
#> [135] "United States"            "Uruguay"                 
#> [137] "Venezuela"                "Vietnam"                 
#> [139] "West Bank and Gaza"       "Yemen, Rep."             
#> [141] "Zambia"                   "Zimbabwe"                
#> 
#> $continent
#> [1] "Africa"   "Americas" "Asia"     "Europe"   "Oceania"
```

This returns all factor levels alphabetically sorted.

## Drop Oceania and remove unused factor levels

> Filter the Gapminder data to remove observations associated with the
> continent of Oceania. Additionally, remove unused factor levels.
> Provide concrete information on the data before and after removing
> these rows and Oceania; address the number of rows and the levels of
> the affected
factors.

``` r
drop_continent <- c("Oceania") # use vector to easily change which continent to drop

rm_oceania <- gapminder %>% 
  filter(continent != drop_continent)
  
unique(rm_oceania$continent) # check if Oceania obervations are removed
#> [1] Asia     Europe   Africa   Americas
#> Levels: Africa Americas Asia Europe Oceania
```

We can see here that eventhough all observations of `Oceania` have been
removed, the factor level Oceania is still present but unused. Let’s
remove these unused factor levels and compare the gapminder dataset
before and after. For an easy comparison I provide this function that
can help visualize results in one go:

``` r
mysummary_table <- function(df) {
  tab <- data.frame(
    "rows" = nrow(df),
    "Continent_levels" = nlevels(df$continent),
    "Country_levels" = nlevels(df$country))
  return(tab)
}
```

``` r
drop_continent <- c("Oceania") # use vector to easily change which continent to drop

five_continents <- gapminder %>% 
    mysummary_table()
    
four_continents <- gapminder %>% 
  filter(continent != drop_continent) %>% 
  droplevels() %>% 
  mysummary_table() 

grid.arrange(textGrob("gapminder dataset",gp=gpar(fontsize=12)),
             tableGrob(five_continents, rows=NULL),
             textGrob("without Oceania",gp=gpar(fontsize=12)),
             tableGrob(four_continents, rows=NULL), ncol=2)
```

![](hw05-exercise_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

We can see than we removed 24 rows, one continent factor level and 2
country factor levels. Let’s find out which countries have been removed:

``` r
gapminder %>% 
  filter(continent=="Oceania") %>% 
  select(country) %>% 
  unique() %>% 
  kable()
```

| country     |
| :---------- |
| Australia   |
| New Zealand |

## Reorder the levels of country or continent using `forcats`

> Reorder the levels of country or continent. Use the forcats package to
> change the order of the factor levels, based on a principled summary
> of one of the quantitative variables. Consider experimenting with a
> summary statistic beyond the most basic choice of the median.

Remember that countries and continents were ordered in alphabetical
order as shown above, let’s try to change this order, sicne it does not
always make sense to display data alphabetically ordered.

In order to provide nice visualization on how and what changes, I will
use the `Maximum GdP` variable over the timeframe of 1952-2007 for all
countries in Asia as an example. Let’s see how `fct_reorder` and
`arrange()`:

``` r

gapminder %>%
  filter(continent == "Asia") %>% 
  group_by(country) %>% 
  mutate(max_gdpPercap = max(gdpPercap)) %>%
  ggplot(aes(x = max_gdpPercap, y = fct_reorder(country, gdpPercap, max),
             color = country)) +
  geom_point() + 
  scale_x_log10(labels=comma_format()) +
  xlab("Maximum gdp per capita") +
  ylab("country") +
  labs(title = "Maximum gdp per capita for countries in Asia") +
  theme_bw()
```

![](hw05-exercise_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

In comparison, how does our visualization look like if we use
`arrange()` for this task:

``` r

gapminder %>%
  filter(continent == "Asia") %>% 
  group_by(country) %>% 
  mutate(max_gdpPercap = max(gdpPercap)) %>%
  arrange(max_gdpPercap) %>% 
  ggplot(aes(x = max_gdpPercap, y = country,
             color = country)) +
  geom_point() + 
  scale_x_log10(labels=comma_format()) +
  xlab("Maximum gdp per capita") +
  ylab("country") +
  labs(title = "Maximum gdp per capita for countries in Asia") +
  theme_bw()
```

![](hw05-exercise_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

We can see that `arrange()` alone does not change the order of levels in
a factor.How does it change if both `arrange()` and `fct_reorder()` are
used:

``` r

gapminder %>%
  filter(continent == "Asia") %>% 
  group_by(country) %>% 
  mutate(max_gdpPercap = max(gdpPercap)) %>%
  arrange(max_gdpPercap) %>% 
  ggplot(aes(x = max_gdpPercap, y = fct_reorder(country, gdpPercap, max),
             color = country)) +
  geom_point() + 
  scale_x_log10(labels=comma_format()) +
  xlab("Maximum gdp per capita") +
  ylab("country") +
  labs(title = "Maximum gdp per capita for countries in Asia") +
  theme_bw()
```

![](hw05-exercise_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

Not surprising, `fct_reorder()` takes the lead and orders our levels.

# Part 2: File I/O and I want to do more

> Experiment with one or more of write\_csv()/read\_csv() (and/or TSV
> friends), saveRDS()/readRDS(), dput()/dget(). Create something new,
> probably by filtering or grouped-summarization of Singer or Gapminder.
> I highly recommend you fiddle with the factor levels, i.e. make them
> non-alphabetical (see previous section). Explore whether this survives
> the round trip of writing to file then reading back in

### My new dataset

``` r
beverage <- tribble( # create a tibble
                    ~country, ~beverage,
                    "Germany", "beer",
                    "Mexico", "tequila",
                    "France", "wine",
                    "Russia", "vodka",
                    "Japan", "sake",
                    "Grece", "ouzo",
                    "Italy", "ramazotti")

popularity <- tribble( # create a tibble
                    ~beverage, ~popularity,
                    "beer", 1,
                    "tequila", 7,
                    "wine", 2,
                    "vodka", 6,
                    "sake", 5,
                    "ouzo", 3,
                    "ramazotti", 4)

# convert all columns to factors:
beverage[] <- lapply(beverage, factor) # "[]" keeps the dataframe structure
 col_names <- names(beverage)

world_beverage <- right_join(beverage, popularity)
#> Joining, by = "beverage"
#> Warning: Column `beverage` joining factor and character vector, coercing
#> into character vector

# Check
glimpse(world_beverage)
#> Observations: 7
#> Variables: 3
#> $ country    <fct> Germany, Mexico, France, Russia, Japan, Grece, Italy
#> $ beverage   <chr> "beer", "tequila", "wine", "vodka", "sake", "ouzo",...
#> $ popularity <dbl> 1, 7, 2, 6, 5, 3, 4
```

This dataset will be used to test if changed factor levels will be kept
while data is saved and reloaded in multiple packages.
[Stackoverflow](https://stackoverflow.com/questions/9251326/convert-data-frame-column-format-from-character-to-factor)
helped me to create this factor-df. Interestingly, a join converts our
variable `beverage` back to a `character`

## Comparison

Orignal dataframe:

``` r
write_csv(world_beverage, "world_beverage_csv.csv")
df_csv <- read_csv("world_beverage_csv.csv")
#> Parsed with column specification:
#> cols(
#>   country = col_character(),
#>   beverage = col_character(),
#>   popularity = col_integer()
#> )

saveRDS(world_beverage, "world_beverage_rds.rds")
df_rds <- readRDS("world_beverage_rds.rds")

dput(world_beverage, "world_beverage.txt")
df_get <- dget("world_beverage.txt")

comparison <- data.frame(
    "csv" = sapply(df_csv, class),
    "rds" = sapply(df_rds, class),
    "get" = sapply(df_get, class))

kable(comparison)
```

|            | csv       | rds       | get       |
| ---------- | :-------- | :-------- | :-------- |
| country    | character | factor    | factor    |
| beverage   | character | character | character |
| popularity | integer   | numeric   | numeric   |

The comparison shows that `rds` and `txt` formats automatically parse to
factor when saved or loaded. `read_csv()` on the other hand
automatically parses variables and I get this message in the command
line, while a check reveales that the data frame does not contain
factors anymore:

    Parsed with column specification:
    cols(
      country = col_character(),
      beverage = col_character(),
      popularity = col_integer()
    )

The `read_csv()` documentation suggests:

> col\_factor() (f) allows you to load data directly into a factor if
> you know what the levels are.

what happens if I specify these while loading:

``` r
# df_csv <- read_csv("world_beverage_csv.csv",
#                   col_types = cols(country = col_factor(),
#                                    beverage = col_factor(),
#                                    popularity = col_factor()
#                                    )
# )
```

It seems like this is not as straight forward as one also has to know
the levels. And there is no option to safe these with the `.csv`
according to the `write_csv()`
\[documentation\](<https://www.rdocumentation.org/packages/readr/versions/0.1.1/topics/write_csv>.
The best way forward to handle factors is therefore to choose one of the
other two formats.

# Part 3: Visualization design

In this part I experiment with the
[graphics](https://www.youtube.com/watch?v=hVimVzgtD6w) for which
[gapminder](https://www.gapminder.org) got known: visualizing life
expectancy over time. I however will explore how the connection between
life expectancy, the population size and gdp per capita looks like for
2007 looks like.

``` r
plot <- gapminder %>% 
  filter(year == "2007") %>% 
  ggplot(aes(pop, lifeExp, size = gdpPercap, color = continent)) +
  geom_point(alpha=0.4) +
  scale_x_log10(labels=comma_format()) +
  ggtitle("Life Expectancy over population Worldwide") +
  xlab("Population") +
  ylab("Life Expectancy") +
  theme_minimal()
plot
```

![](hw05-exercise_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

In order to be able to zoome in and out and assess which data point
belongs to which country, we are going to plot this graph more
interactively with `plotly`:

``` r
plot %>% 
    ggplotly()
```

<!--html_preserve-->

<div id="htmlwidget-3847803aec0fa9dac8dd" class="plotly html-widget" style="width:672px;height:480px;">

</div>

<script type="application/json" data-for="htmlwidget-3847803aec0fa9dac8dd">{"x":{"data":[{"x":[7.52287721656107,7.09413823998002,6.90732072997002,6.21461366394489,7.15613110074939,6.92378810053229,7.24788230035865,6.64038582203128,7.0102493566886,5.85184516716021,7.81027795519092,6.57985330671495,7.25559590992049,5.69580902513884,7.90452373730654,5.74130999680612,6.69077932681486,7.88372891302894,6.16282329311148,6.22746479723151,7.35932954760618,6.99772765642009,6.16791990635052,7.55157413233565,6.30376804182622,6.50432702535642,6.78081498907337,7.2825689612243,7.12473497213005,7.08033042371904,6.51455638533942,6.09721634313392,7.52836609514419,7.29997894823402,6.31282873272415,7.11041679974562,8.13043401139359,5.90205404583329,6.94746254318299,5.30011484224232,7.08875581883863,6.78849093071106,6.95993640464649,7.64343123760215,7.62626776328567,6.05425520783425,7.58137658942442,6.75599514620266,7.01183077309534,7.46494235466252,7.06989129058869,7.09029837588409],"y":[72.301,42.731,56.728,50.728,52.295,49.58,50.43,44.741,50.651,65.152,46.462,55.322,48.328,54.791,71.338,51.579,58.04,52.947,56.735,59.448,60.022,56.007,46.388,54.11,42.592,45.678,73.952,59.443,48.303,54.467,64.164,72.801,71.164,42.082,52.906,56.867,46.859,76.442,46.242,65.528,63.062,42.568,48.159,49.339,58.556,39.613,52.517,58.42,73.923,51.542,42.384,43.487],"text":["pop:   33333216<br />lifeExp: 72.301<br />gdpPercap:  6223.3675<br />continent: Africa","pop:   12420476<br />lifeExp: 42.731<br />gdpPercap:  4797.2313<br />continent: Africa","pop:    8078314<br />lifeExp: 56.728<br />gdpPercap:  1441.2849<br />continent: Africa","pop:    1639131<br />lifeExp: 50.728<br />gdpPercap: 12569.8518<br />continent: Africa","pop:   14326203<br />lifeExp: 52.295<br />gdpPercap:  1217.0330<br />continent: Africa","pop:    8390505<br />lifeExp: 49.580<br />gdpPercap:   430.0707<br />continent: Africa","pop:   17696293<br />lifeExp: 50.430<br />gdpPercap:  2042.0952<br />continent: Africa","pop:    4369038<br />lifeExp: 44.741<br />gdpPercap:   706.0165<br />continent: Africa","pop:   10238807<br />lifeExp: 50.651<br />gdpPercap:  1704.0637<br />continent: Africa","pop:     710960<br />lifeExp: 65.152<br />gdpPercap:   986.1479<br />continent: Africa","pop:   64606759<br />lifeExp: 46.462<br />gdpPercap:   277.5519<br />continent: Africa","pop:    3800610<br />lifeExp: 55.322<br />gdpPercap:  3632.5578<br />continent: Africa","pop:   18013409<br />lifeExp: 48.328<br />gdpPercap:  1544.7501<br />continent: Africa","pop:     496374<br />lifeExp: 54.791<br />gdpPercap:  2082.4816<br />continent: Africa","pop:   80264543<br />lifeExp: 71.338<br />gdpPercap:  5581.1810<br />continent: Africa","pop:     551201<br />lifeExp: 51.579<br />gdpPercap: 12154.0897<br />continent: Africa","pop:    4906585<br />lifeExp: 58.040<br />gdpPercap:   641.3695<br />continent: Africa","pop:   76511887<br />lifeExp: 52.947<br />gdpPercap:   690.8056<br />continent: Africa","pop:    1454867<br />lifeExp: 56.735<br />gdpPercap: 13206.4845<br />continent: Africa","pop:    1688359<br />lifeExp: 59.448<br />gdpPercap:   752.7497<br />continent: Africa","pop:   22873338<br />lifeExp: 60.022<br />gdpPercap:  1327.6089<br />continent: Africa","pop:    9947814<br />lifeExp: 56.007<br />gdpPercap:   942.6542<br />continent: Africa","pop:    1472041<br />lifeExp: 46.388<br />gdpPercap:   579.2317<br />continent: Africa","pop:   35610177<br />lifeExp: 54.110<br />gdpPercap:  1463.2493<br />continent: Africa","pop:    2012649<br />lifeExp: 42.592<br />gdpPercap:  1569.3314<br />continent: Africa","pop:    3193942<br />lifeExp: 45.678<br />gdpPercap:   414.5073<br />continent: Africa","pop:    6036914<br />lifeExp: 73.952<br />gdpPercap: 12057.4993<br />continent: Africa","pop:   19167654<br />lifeExp: 59.443<br />gdpPercap:  1044.7701<br />continent: Africa","pop:   13327079<br />lifeExp: 48.303<br />gdpPercap:   759.3499<br />continent: Africa","pop:   12031795<br />lifeExp: 54.467<br />gdpPercap:  1042.5816<br />continent: Africa","pop:    3270065<br />lifeExp: 64.164<br />gdpPercap:  1803.1515<br />continent: Africa","pop:    1250882<br />lifeExp: 72.801<br />gdpPercap: 10956.9911<br />continent: Africa","pop:   33757175<br />lifeExp: 71.164<br />gdpPercap:  3820.1752<br />continent: Africa","pop:   19951656<br />lifeExp: 42.082<br />gdpPercap:   823.6856<br />continent: Africa","pop:    2055080<br />lifeExp: 52.906<br />gdpPercap:  4811.0604<br />continent: Africa","pop:   12894865<br />lifeExp: 56.867<br />gdpPercap:   619.6769<br />continent: Africa","pop:  135031164<br />lifeExp: 46.859<br />gdpPercap:  2013.9773<br />continent: Africa","pop:     798094<br />lifeExp: 76.442<br />gdpPercap:  7670.1226<br />continent: Africa","pop:    8860588<br />lifeExp: 46.242<br />gdpPercap:   863.0885<br />continent: Africa","pop:     199579<br />lifeExp: 65.528<br />gdpPercap:  1598.4351<br />continent: Africa","pop:   12267493<br />lifeExp: 63.062<br />gdpPercap:  1712.4721<br />continent: Africa","pop:    6144562<br />lifeExp: 42.568<br />gdpPercap:   862.5408<br />continent: Africa","pop:    9118773<br />lifeExp: 48.159<br />gdpPercap:   926.1411<br />continent: Africa","pop:   43997828<br />lifeExp: 49.339<br />gdpPercap:  9269.6578<br />continent: Africa","pop:   42292929<br />lifeExp: 58.556<br />gdpPercap:  2602.3950<br />continent: Africa","pop:    1133066<br />lifeExp: 39.613<br />gdpPercap:  4513.4806<br />continent: Africa","pop:   38139640<br />lifeExp: 52.517<br />gdpPercap:  1107.4822<br />continent: Africa","pop:    5701579<br />lifeExp: 58.420<br />gdpPercap:   882.9699<br />continent: Africa","pop:   10276158<br />lifeExp: 73.923<br />gdpPercap:  7092.9230<br />continent: Africa","pop:   29170398<br />lifeExp: 51.542<br />gdpPercap:  1056.3801<br />continent: Africa","pop:   11746035<br />lifeExp: 42.384<br />gdpPercap:  1271.2116<br />continent: Africa","pop:   12311143<br />lifeExp: 43.487<br />gdpPercap:   469.7093<br />continent: Africa"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(248,118,109,1)","opacity":0.4,"size":[10.357053389456,9.51422697295377,6.68946412288872,13.2369637075835,6.39410118464323,4.83298884609202,7.36274376756045,5.54521714964519,7.00129759573581,6.05020893795825,3.77952755905512,8.72039828019232,6.81606840068373,7.40351755901193,9.99169878969551,13.0756486739973,5.40656945108892,5.51359198295643,13.4787781669956,5.63901858015298,6.54368828309134,5.97941816874924,5.26112432171756,6.71679696657494,6.84537861341279,4.77779419321425,13.0377693761645,6.14226961334891,5.65188759796793,6.13889722427516,7.11131381521573,12.594705210158,8.85666985770899,5.77298219286561,9.52299369158242,5.35731781840495,7.33407990923002,11.1137586070261,5.84364246072499,6.87972288540899,7.01077882334711,5.84267685386218,5.95193711031252,11.8683918952782,7.89247984713003,9.33129376504527,6.23693778046021,5.87839272128437,10.8216180304975,6.16007968473248,6.4684339891143,4.9619848008935],"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(248,118,109,1)"}},"hoveron":"points","name":"Africa","legendgroup":"Africa","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[7.60532581203339,6.95995445468329,8.27877793676229,7.52361825299845,7.21178085573692,7.64569288214711,6.61635828582133,7.05755150654359,6.96939829790727,7.13848206422393,6.84133994553781,7.09943642853432,6.92956267894046,6.87412002556475,6.44406541660536,8.03623310392804,6.75399310885835,6.51083618476938,6.82394003073454,7.45749974622435,6.59577071058565,6.02391389457986,8.47876836963079,6.53750377095262,7.41638521359104],"y":[75.32,65.554,72.39,80.653,78.553,72.889,78.782,78.273,72.235,74.994,71.878,70.259,60.916,70.198,72.567,76.195,72.899,75.537,71.752,71.421,78.746,69.819,78.242,76.384,73.747],"text":["pop:   40301927<br />lifeExp: 75.320<br />gdpPercap: 12779.3796<br />continent: Americas","pop:    9119152<br />lifeExp: 65.554<br />gdpPercap:  3822.1371<br />continent: Americas","pop:  190010647<br />lifeExp: 72.390<br />gdpPercap:  9065.8008<br />continent: Americas","pop:   33390141<br />lifeExp: 80.653<br />gdpPercap: 36319.2350<br />continent: Americas","pop:   16284741<br />lifeExp: 78.553<br />gdpPercap: 13171.6388<br />continent: Americas","pop:   44227550<br />lifeExp: 72.889<br />gdpPercap:  7006.5804<br />continent: Americas","pop:    4133884<br />lifeExp: 78.782<br />gdpPercap:  9645.0614<br />continent: Americas","pop:   11416987<br />lifeExp: 78.273<br />gdpPercap:  8948.1029<br />continent: Americas","pop:    9319622<br />lifeExp: 72.235<br />gdpPercap:  6025.3748<br />continent: Americas","pop:   13755680<br />lifeExp: 74.994<br />gdpPercap:  6873.2623<br />continent: Americas","pop:    6939688<br />lifeExp: 71.878<br />gdpPercap:  5728.3535<br />continent: Americas","pop:   12572928<br />lifeExp: 70.259<br />gdpPercap:  5186.0500<br />continent: Americas","pop:    8502814<br />lifeExp: 60.916<br />gdpPercap:  1201.6372<br />continent: Americas","pop:    7483763<br />lifeExp: 70.198<br />gdpPercap:  3548.3308<br />continent: Americas","pop:    2780132<br />lifeExp: 72.567<br />gdpPercap:  7320.8803<br />continent: Americas","pop:  108700891<br />lifeExp: 76.195<br />gdpPercap: 11977.5750<br />continent: Americas","pop:    5675356<br />lifeExp: 72.899<br />gdpPercap:  2749.3210<br />continent: Americas","pop:    3242173<br />lifeExp: 75.537<br />gdpPercap:  9809.1856<br />continent: Americas","pop:    6667147<br />lifeExp: 71.752<br />gdpPercap:  4172.8385<br />continent: Americas","pop:   28674757<br />lifeExp: 71.421<br />gdpPercap:  7408.9056<br />continent: Americas","pop:    3942491<br />lifeExp: 78.746<br />gdpPercap: 19328.7090<br />continent: Americas","pop:    1056608<br />lifeExp: 69.819<br />gdpPercap: 18008.5092<br />continent: Americas","pop:  301139947<br />lifeExp: 78.242<br />gdpPercap: 42951.6531<br />continent: Americas","pop:    3447496<br />lifeExp: 76.384<br />gdpPercap: 10611.4630<br />continent: Americas","pop:   26084662<br />lifeExp: 73.747<br />gdpPercap: 11415.8057<br />continent: Americas"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(163,165,0,1)","opacity":0.4,"size":[13.3172262854835,8.85807548741529,11.776176302497,19.9737229954412,13.4656987841698,10.7768684134063,12.0355129633066,11.7224476635069,10.2466120194044,10.7072046363222,10.0773010068316,9.75581062419984,6.37258939704268,8.65798413360415,10.9384196003016,13.0063083668402,8.02045452957394,12.1075237483332,9.10338665523062,10.983015485085,15.5533511309663,15.1380786787413,21.4008718046641,12.4509272610721,12.7820746008495],"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(163,165,0,1)"}},"hoveron":"points","name":"Americas","legendgroup":"Americas","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[7.50365347061595,5.85038459962796,8.17738739734618,7.15019926489879,9.12014043906824,6.84388105644064,9.0454780181062,8.34936884601265,7.84169457392464,7.439326976898,6.8079866082953,8.10540107629152,6.78198452117817,7.36738807253893,7.69059287931173,6.39890463380879,6.5934276328093,7.39482427870322,6.45850595453606,7.67908232241884,7.46092474113194,6.50581407658481,8.22858157723326,7.95941008542051,7.44092541500708,6.65829850878534,7.30916665142572,7.28588902378172,7.36500651232788,7.81336845249196,7.9307573290578,6.60404581588944,7.34658263984431],"y":[43.828,75.635,64.062,59.723,72.961,82.208,64.698,70.65,70.964,59.545,80.745,82.603,72.535,67.297,78.623,77.588,71.993,74.241,66.803,62.069,63.785,75.64,65.483,71.688,72.777,79.972,72.396,74.143,78.4,70.616,74.249,73.422,62.698],"text":["pop:   31889923<br />lifeExp: 43.828<br />gdpPercap:   974.5803<br />continent: Asia","pop:     708573<br />lifeExp: 75.635<br />gdpPercap: 29796.0483<br />continent: Asia","pop:  150448339<br />lifeExp: 64.062<br />gdpPercap:  1391.2538<br />continent: Asia","pop:   14131858<br />lifeExp: 59.723<br />gdpPercap:  1713.7787<br />continent: Asia","pop: 1318683096<br />lifeExp: 72.961<br />gdpPercap:  4959.1149<br />continent: Asia","pop:    6980412<br />lifeExp: 82.208<br />gdpPercap: 39724.9787<br />continent: Asia","pop: 1110396331<br />lifeExp: 64.698<br />gdpPercap:  2452.2104<br />continent: Asia","pop:  223547000<br />lifeExp: 70.650<br />gdpPercap:  3540.6516<br />continent: Asia","pop:   69453570<br />lifeExp: 70.964<br />gdpPercap: 11605.7145<br />continent: Asia","pop:   27499638<br />lifeExp: 59.545<br />gdpPercap:  4471.0619<br />continent: Asia","pop:    6426679<br />lifeExp: 80.745<br />gdpPercap: 25523.2771<br />continent: Asia","pop:  127467972<br />lifeExp: 82.603<br />gdpPercap: 31656.0681<br />continent: Asia","pop:    6053193<br />lifeExp: 72.535<br />gdpPercap:  4519.4612<br />continent: Asia","pop:   23301725<br />lifeExp: 67.297<br />gdpPercap:  1593.0655<br />continent: Asia","pop:   49044790<br />lifeExp: 78.623<br />gdpPercap: 23348.1397<br />continent: Asia","pop:    2505559<br />lifeExp: 77.588<br />gdpPercap: 47306.9898<br />continent: Asia","pop:    3921278<br />lifeExp: 71.993<br />gdpPercap: 10461.0587<br />continent: Asia","pop:   24821286<br />lifeExp: 74.241<br />gdpPercap: 12451.6558<br />continent: Asia","pop:    2874127<br />lifeExp: 66.803<br />gdpPercap:  3095.7723<br />continent: Asia","pop:   47761980<br />lifeExp: 62.069<br />gdpPercap:   944.0000<br />continent: Asia","pop:   28901790<br />lifeExp: 63.785<br />gdpPercap:  1091.3598<br />continent: Asia","pop:    3204897<br />lifeExp: 75.640<br />gdpPercap: 22316.1929<br />continent: Asia","pop:  169270617<br />lifeExp: 65.483<br />gdpPercap:  2605.9476<br />continent: Asia","pop:   91077287<br />lifeExp: 71.688<br />gdpPercap:  3190.4810<br />continent: Asia","pop:   27601038<br />lifeExp: 72.777<br />gdpPercap: 21654.8319<br />continent: Asia","pop:    4553009<br />lifeExp: 79.972<br />gdpPercap: 47143.1796<br />continent: Asia","pop:   20378239<br />lifeExp: 72.396<br />gdpPercap:  3970.0954<br />continent: Asia","pop:   19314747<br />lifeExp: 74.143<br />gdpPercap:  4184.5481<br />continent: Asia","pop:   23174294<br />lifeExp: 78.400<br />gdpPercap: 28718.2768<br />continent: Asia","pop:   65068149<br />lifeExp: 70.616<br />gdpPercap:  7458.3963<br />continent: Asia","pop:   85262356<br />lifeExp: 74.249<br />gdpPercap:  2441.5764<br />continent: Asia","pop:    4018332<br />lifeExp: 73.422<br />gdpPercap:  3025.3498<br />continent: Asia","pop:   22211743<br />lifeExp: 62.698<br />gdpPercap:  2280.7699<br />continent: Asia"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(0,191,125,1)","opacity":0.4,"size":[6.03159870097163,18.435139617496,6.62622512985187,7.01224957779167,9.6160247359879,20.7215863129334,7.75741401140131,8.6522538382964,12.8584975709654,9.30342603322482,17.3330060104691,18.8898261210536,9.33521153419374,6.87341506468131,16.7359820675249,22.278250081354,12.3875922731096,13.1913851358154,8.30792276607732,5.98164270774655,6.21295163081251,16.4428961884793,7.89562114183677,8.38338434413266,16.2514404137721,22.2460051536332,8.96298643026723,9.11138267962979,18.1651008955863,11.0079679526943,7.74767623078609,8.25098643321227,7.59739597361082],"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(0,191,125,1)"}},"hoveron":"points","name":"Asia","legendgroup":"Asia","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[6.55636558952193,6.9138023593167,7.01670858278068,6.65822114364125,6.86468061266828,6.65256657554249,7.00982230943478,6.73783803676008,6.71920363205983,7.78592687125726,7.91593246114723,7.02963900288956,6.99808959902029,5.47990770539729,6.61374523080546,7.76453278759787,5.83552316150299,7.21933857465912,6.66538640601683,7.5856664461203,7.02705736997367,7.34783830099634,7.00647738082414,6.73619739836553,6.30303289626077,7.60689910304876,6.95574007411558,6.87821498103341,7.85222768187928,7.78373381409855],"y":[76.423,79.829,79.441,74.852,73.005,75.748,76.486,78.332,79.313,80.657,79.406,79.483,73.338,81.757,78.885,80.546,74.543,79.762,80.196,75.563,78.098,72.476,74.002,74.663,77.926,80.941,80.884,81.701,71.777,79.425],"text":["pop:    3600523<br />lifeExp: 76.423<br />gdpPercap:  5937.0295<br />continent: Europe","pop:    8199783<br />lifeExp: 79.829<br />gdpPercap: 36126.4927<br />continent: Europe","pop:   10392226<br />lifeExp: 79.441<br />gdpPercap: 33692.6051<br />continent: Europe","pop:    4552198<br />lifeExp: 74.852<br />gdpPercap:  7446.2988<br />continent: Europe","pop:    7322858<br />lifeExp: 73.005<br />gdpPercap: 10680.7928<br />continent: Europe","pop:    4493312<br />lifeExp: 75.748<br />gdpPercap: 14619.2227<br />continent: Europe","pop:   10228744<br />lifeExp: 76.486<br />gdpPercap: 22833.3085<br />continent: Europe","pop:    5468120<br />lifeExp: 78.332<br />gdpPercap: 35278.4187<br />continent: Europe","pop:    5238460<br />lifeExp: 79.313<br />gdpPercap: 33207.0844<br />continent: Europe","pop:   61083916<br />lifeExp: 80.657<br />gdpPercap: 30470.0167<br />continent: Europe","pop:   82400996<br />lifeExp: 79.406<br />gdpPercap: 32170.3744<br />continent: Europe","pop:   10706290<br />lifeExp: 79.483<br />gdpPercap: 27538.4119<br />continent: Europe","pop:    9956108<br />lifeExp: 73.338<br />gdpPercap: 18008.9444<br />continent: Europe","pop:     301931<br />lifeExp: 81.757<br />gdpPercap: 36180.7892<br />continent: Europe","pop:    4109086<br />lifeExp: 78.885<br />gdpPercap: 40675.9964<br />continent: Europe","pop:   58147733<br />lifeExp: 80.546<br />gdpPercap: 28569.7197<br />continent: Europe","pop:     684736<br />lifeExp: 74.543<br />gdpPercap:  9253.8961<br />continent: Europe","pop:   16570613<br />lifeExp: 79.762<br />gdpPercap: 36797.9333<br />continent: Europe","pop:    4627926<br />lifeExp: 80.196<br />gdpPercap: 49357.1902<br />continent: Europe","pop:   38518241<br />lifeExp: 75.563<br />gdpPercap: 15389.9247<br />continent: Europe","pop:   10642836<br />lifeExp: 78.098<br />gdpPercap: 20509.6478<br />continent: Europe","pop:   22276056<br />lifeExp: 72.476<br />gdpPercap: 10808.4756<br />continent: Europe","pop:   10150265<br />lifeExp: 74.002<br />gdpPercap:  9786.5347<br />continent: Europe","pop:    5447502<br />lifeExp: 74.663<br />gdpPercap: 18678.3144<br />continent: Europe","pop:    2009245<br />lifeExp: 77.926<br />gdpPercap: 25768.2576<br />continent: Europe","pop:   40448191<br />lifeExp: 80.941<br />gdpPercap: 28821.0637<br />continent: Europe","pop:    9031088<br />lifeExp: 80.884<br />gdpPercap: 33859.7484<br />continent: Europe","pop:    7554661<br />lifeExp: 81.701<br />gdpPercap: 37506.4191<br />continent: Europe","pop:   71158647<br />lifeExp: 71.777<br />gdpPercap:  8458.2764<br />continent: Europe","pop:   60776238<br />lifeExp: 79.425<br />gdpPercap: 33203.2613<br />continent: Europe"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(0,176,246,1)","opacity":0.4,"size":[10.1967193493677,19.9303636048719,19.3724644995533,11.0018765317884,12.47996668734,13.9949591451742,16.5906016888519,19.7381810266616,19.258767306528,18.601504339918,19.0131547252171,17.8635471452086,15.138218073671,19.9425899341509,20.924593730441,18.1274809395435,11.861299555195,20.0809122590826,22.6771653543307,14.2658491772125,15.9127813422167,12.5331957000679,12.097622560419,15.3506304084744,17.3986076440197,18.1910727077824,19.4114140626975,20.238274008765,11.4948259700202,19.2578687102792],"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(0,176,246,1)"}},"hoveron":"points","name":"Europe","legendgroup":"Europe","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[7.31035712964887,6.61445120277575],"y":[81.235,80.204],"text":["pop:   20434176<br />lifeExp: 81.235<br />gdpPercap: 34435.3674<br />continent: Oceania","pop:    4115771<br />lifeExp: 80.204<br />gdpPercap: 25185.0091<br />continent: Oceania"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(231,107,243,1)","opacity":0.4,"size":[19.5448148276253,17.2418981243488],"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(231,107,243,1)"}},"hoveron":"points","name":"Oceania","legendgroup":"Oceania","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null}],"layout":{"margin":{"t":43.7625570776256,"r":7.30593607305936,"b":40.1826484018265,"l":37.2602739726027},"font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"title":"Life Expectancy over population Worldwide","titlefont":{"color":"rgba(0,0,0,1)","family":"","size":17.5342465753425},"xaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[5.10911356240102,9.31114171890954],"tickmode":"array","ticktext":["1,000,000","10,000,000","100,000,000","1,000,000,000"],"tickvals":[6,7,8,9],"categoryorder":"array","categoryarray":["1,000,000","10,000,000","100,000,000","1,000,000,000"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":3.65296803652968,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(235,235,235,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"y","title":"Population","titlefont":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"hoverformat":".2f"},"yaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[37.4635,84.7525],"tickmode":"array","ticktext":["40","50","60","70","80"],"tickvals":[40,50,60,70,80],"categoryorder":"array","categoryarray":["40","50","60","70","80"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":3.65296803652968,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(235,235,235,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"x","title":"Life Expectancy","titlefont":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"hoverformat":".2f"},"shapes":[{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0,"x1":1,"y0":0,"y1":1}],"showlegend":true,"legend":{"bgcolor":null,"bordercolor":null,"borderwidth":0,"font":{"color":"rgba(0,0,0,1)","family":"","size":11.689497716895},"y":0.826771653543307},"annotations":[{"text":"continent<br />gdpPercap","x":1.02,"y":1,"showarrow":false,"ax":0,"ay":0,"font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"xref":"paper","yref":"paper","textangle":-0,"xanchor":"left","yanchor":"bottom","legendTitle":true}],"hovermode":"closest","barmode":"relative"},"config":{"doubleClick":"reset","modeBarButtonsToAdd":[{"name":"Collaborate","icon":{"width":1000,"ascent":500,"descent":-50,"path":"M487 375c7-10 9-23 5-36l-79-259c-3-12-11-23-22-31-11-8-22-12-35-12l-263 0c-15 0-29 5-43 15-13 10-23 23-28 37-5 13-5 25-1 37 0 0 0 3 1 7 1 5 1 8 1 11 0 2 0 4-1 6 0 3-1 5-1 6 1 2 2 4 3 6 1 2 2 4 4 6 2 3 4 5 5 7 5 7 9 16 13 26 4 10 7 19 9 26 0 2 0 5 0 9-1 4-1 6 0 8 0 2 2 5 4 8 3 3 5 5 5 7 4 6 8 15 12 26 4 11 7 19 7 26 1 1 0 4 0 9-1 4-1 7 0 8 1 2 3 5 6 8 4 4 6 6 6 7 4 5 8 13 13 24 4 11 7 20 7 28 1 1 0 4 0 7-1 3-1 6-1 7 0 2 1 4 3 6 1 1 3 4 5 6 2 3 3 5 5 6 1 2 3 5 4 9 2 3 3 7 5 10 1 3 2 6 4 10 2 4 4 7 6 9 2 3 4 5 7 7 3 2 7 3 11 3 3 0 8 0 13-1l0-1c7 2 12 2 14 2l218 0c14 0 25-5 32-16 8-10 10-23 6-37l-79-259c-7-22-13-37-20-43-7-7-19-10-37-10l-248 0c-5 0-9-2-11-5-2-3-2-7 0-12 4-13 18-20 41-20l264 0c5 0 10 2 16 5 5 3 8 6 10 11l85 282c2 5 2 10 2 17 7-3 13-7 17-13z m-304 0c-1-3-1-5 0-7 1-1 3-2 6-2l174 0c2 0 4 1 7 2 2 2 4 4 5 7l6 18c0 3 0 5-1 7-1 1-3 2-6 2l-173 0c-3 0-5-1-8-2-2-2-4-4-4-7z m-24-73c-1-3-1-5 0-7 2-2 3-2 6-2l174 0c2 0 5 0 7 2 3 2 4 4 5 7l6 18c1 2 0 5-1 6-1 2-3 3-5 3l-174 0c-3 0-5-1-7-3-3-1-4-4-5-6z"},"click":"function(gd) { \n        // is this being viewed in RStudio?\n        if (location.search == '?viewer_pane=1') {\n          alert('To learn about plotly for collaboration, visit:\\n https://cpsievert.github.io/plotly_book/plot-ly-for-collaboration.html');\n        } else {\n          window.open('https://cpsievert.github.io/plotly_book/plot-ly-for-collaboration.html', '_blank');\n        }\n      }"}],"cloud":false},"source":"A","attrs":{"10c056f386252":{"x":{},"y":{},"size":{},"colour":{},"type":"scatter"}},"cur_data":"10c056f386252","visdat":{"10c056f386252":["function (y) ","x"]},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"base_url":"https://plot.ly"},"evals":["config.modeBarButtonsToAdd.0.click"],"jsHooks":[]}</script>

<!--/html_preserve-->

And thanks to
[`huangjieying`](https://github.com/STAT545-UBC-students/hw05-huangjieying/blob/master/hw05_factor_and_figure_management.md),
I found out about this amazing interactive visualization tool called
[`gganimate`](https://github.com/thomasp85/gganimate). In order to
install the animation package use:

``` r
# library(devtools) # load devtools package
# devtools::install_github('thomasp85/gganimate',force = TRUE) 
```

``` r

ggplot(gapminder, aes(pop, lifeExp, size = gdpPercap, color = continent)) +
geom_point(alpha=0.4) +
scale_x_log10(labels=comma_format()) +
theme_minimal() +
labs(title = 'Year: {frame_time}', x = 'Population', y = 'Life Expectancy') +
transition_time(year) # This needs to be added for gganimate
```

![](hw05-exercise_files/figure-gfm/unnamed-chunk-19-1.gif)<!-- -->

# Part 4: Writing figures to file

> Use ggsave() to explicitly save a plot to file. Then use `![Alt
> text](/path/to/img.png)` to load and embed it in your report. You can
> play around with various options, such as:

> Arguments of ggsave(), such as width, height, resolution or text
> scaling. Various graphics devices, e.g. a vector vs. raster format.
> Explicit provision of the plot object p via ggsave(…, plot = p). Show
> a situation in which this actually matters.

Let’s make another plot and save it:

``` r
Life_Expectancy_viz <- gapminder %>% 
  filter(year == "2007") %>% 
  ggplot(aes(gdpPercap, lifeExp, size = pop, colour = country)) +
  geom_point(alpha = 0.7, show.legend = FALSE) +
  scale_colour_manual(values = country_colors) +
  scale_size(range = c(2, 12)) +
  scale_x_log10(labels=comma_format()) +
  facet_wrap(~continent) +
  labs(title = 'Worldwide life expectancy over gdp in 2007', x = 'gdp per capita', y = 'life expectancy')

ggsave("./plot/life_expectancy_viz.png")
#> Saving 7 x 5 in image
```

![Load the plot here:](./plot/life_expectancy_viz.png)

Different heights
andresolutions:

``` r
ggsave("./plot/viz_small.png", plot = Life_Expectancy_viz, width = 2, height = 2)

ggsave("./plot/viz_tiny.jpeg", plot = Life_Expectancy_viz, device="jpeg")
#> Saving 7 x 5 in image

ggsave("./plot/viz_high_resolution.png", plot = Life_Expectancy_viz, dpi = 400)
#> Saving 7 x 5 in image

ggsave("./plot/viz_low_resolution.png", plot = Life_Expectancy_viz, dpi = 10)
#> Saving 7 x 5 in image
```

![Small:](./plot/viz_small.png)

Wee can see, that ggsave is only reducing the size of the graphics, but
not the size of the text dimension. In comparison changing the
resolution scales the size of graphic elements accordingly:

![Small:](./plot/viz_high_resolution.png)

![Small:](./plot/viz_low_resolution.png)

It is also possible to use a different format:

![Small:](./plot/viz_tiny.jpeg)
