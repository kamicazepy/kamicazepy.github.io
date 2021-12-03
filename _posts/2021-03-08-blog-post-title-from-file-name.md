## Scooby-Do!
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)

# Or read in the data manually

scoobydoo <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-13/scoobydoo.csv')

library(tidyverse)
library(scales)
```

## Scooby Do, where are you?!
Even though I am already an adult, I do love cartoons! Scooby-Do was one of favorites since I was a child. Every Saturday morning I would change the channel from the news and I would sit on the couch to watch one more episode while eating breakfast. Thanks to one of the Tidy Tuesday data sets recommended by Dr. Sara Stoudt and the open source webpage Kable, I was able to bring back some memories with pretty cool facts about this tv show using R.

![](scooby.jpeg){width=50%}
## Mistery solved
While going through the available data, I focus on figuring out the following: 

*The top 5 most common monsters shown on the episodes. 
*The most common reasons behind the crimes done by the monsters/villains. 
*The most common setting terrain for each episode. 
*Was the old Scooby-Do most watched than the new seasons? 

Let's find out. 

```{r cars,include = FALSE }
#Data wrangling perfomed to get the first barplot.
monster_types <- scoobydoo %>%
  select(monster_type,monster_species,monster_species,monster_subtype) %>%
  group_by(monster_type) %>%
  summarize(numberoftimes = n()) %>%
  top_n(n = 6, wt = numberoftimes)
monster_types <- monster_types %>%
  filter(monster_type != "NULL")
monster_types

#Data wrangling perfomed to get sum of how many times a crime was done under the same purpose to later showcase it through a horizontal barplot. 
culprits <- scoobydoo %>%
select(culprit_amount,motive) %>%
group_by(motive) %>%
summarize(sum = sum(culprit_amount)) %>%
arrange(desc(sum))
culprits


#Most Famous settings terrains where Scooby Do episodes were developed. For this visualization, I reduced the terrain scenarios to only those that happened in the United States. 

where_scooby <- scoobydoo %>% 
  select(setting_terrain,setting_country_state) %>% 
  group_by(setting_country_state, setting_terrain) %>% 
  summarize(numberoftimes = n()) %>%
  top_n(n = 10, wt = numberoftimes) %>%
  filter(setting_country_state == "United States")
where_scooby

#The engagement variable is the one that counted how many people watched the episode on the date it was on air. 
engagement_top<- scoobydoo %>% 
  select(title,imdb,engagement,date_aired) %>%
  mutate(imdb = as.numeric(imdb)) %>%
  mutate(engagement = as.numeric(engagement))
engagement_top


```

## _Scooby-Dooby-Doo!_ *Don't be scared!* 
From the very first appearance of the mysterious monster at the beggining of each episode, I was interested to see if there was a pattern within the type of monsters this show presented. Of course, as our dear Scooby always was scared of, Ghosts were the most popular monsters throughout the series. 

```{r pressure, echo=FALSE}

ggplot(monster_types, aes(x = reorder(monster_type, numberoftimes), y = numberoftimes, fill = monster_type)) + geom_col() + labs(x = "Type of Monster", y = "Number of times shown", title = "Top 5 types of monsters shown in Scooby Doo episodes", subtitle = "Null data not shown" )+ theme(element_blank()) + guides(fill = "none")
```
## Get in, let's solve the mistery. The Mistery machine awaits. 

Known by almost any fan and non-fan of this tv show can recognize the colorful van drove by the handsome and popular Greg, the leader of the gang. This group of friends traveled mostly all around the world to unmask the villains, yet for this plot, I decided to reduce the terrains to those within the United States. So, to which terrain did our gang traveled the most? 

```{r 3, echo=FALSE}
ggplot(where_scooby, aes(y = numberoftimes, x = setting_country_state, col = setting_terrain)) + geom_jitter()+ labs(x = "", y = "Number of times", title = "Most common setting terrain where episode was located", col = "Terrain")+ theme(element_blank()) + scale_color_brewer(palette = "Paired")
```
There was nothing more exciting than the end of each episode, where the gang explained what really happened and then asked the, most of the times, unexpected villain to explain their reasons behind their maleficent intentions. Turns out that Competition was the main motive...

```{r 4, echo=FALSE}
ggplot(culprits, aes(x = sum, y = reorder(motive, sum))) + geom_col()+labs(title = "Most famous motive behind crimes",x= "Sum of times crime was perfomed", y = "Motive behind crime")+ theme(element_blank())
```
## Old Scooby vs New Scooby. 
Seems that Scooby Do was a really great hit between 2005 and 2006. Exactly the age at which I was five years old eating breakfast and watching it from my living room couch. 

```{r, echo=FALSE}
ggplot(engagement_top, aes(x = date_aired, y = engagement)) + geom_line()+ theme_bw() + labs(x = "Date aired", y = "Engagement", title = "When was Scooby Doo most watched?")
```
![alt text](scoobywhereareyou.jpg)



