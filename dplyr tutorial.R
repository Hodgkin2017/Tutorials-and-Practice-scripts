###########
## dplyr Tutorial (http://genomicsclass.github.io/book/pages/dplyr_tutorial.html)
#########

library(dplyr)
library(downloader)

url<- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/msleep_ggplot2.csv"
url
filename <- "msleep_ggplot2.csv"
if (!file.exists(filename)) download(url,filename)
dir()
msleep <- read.csv("msleep_ggplot2.csv")
head(msleep)
dim(msleep)


################
## select()
sleepData<- select(msleep, name, sleep_total)
sleepData
msleep[,c(1,6)]


head(select(msleep, -name))
msleep[,-1]

head(select (msleep, name:order))
head(msleep[,1:4])

head (select(msleep, starts_with("sl")))
head(select(msleep, ends_with("e")))
head(select(msleep, contains("ee")))
head(select(msleep, matches("sleep*")))
a<- "sleep_cycle"
head(select(msleep, one_of(a)))

################
## filter()
filter(msleep, sleep_total >= 16)
rows.to.keep<- msleep$sleep_total >= 16
msleep[rows.to.keep,]

filter(msleep, sleep_total >= 16, bodywt >=1)

filter(msleep, order %in% c("Perissodactyla", "Primates"))

filter(msleep, order == "Primates")

# ##############
# Pipe operator: %>% and arrange()

head(select (msleep, name, sleep_total))

msleep %>% select(name, sleep_total) %>% head

head(msleep)
msleep %>% arrange(order) %>% head

msleep %>% select(name, order, sleep_total) %>% arrange(order, sleep_total) %>% head
msleep %>% 
  select(name, order, sleep_total) %>% 
  arrange(order, sleep_total) %>% 
  filter(sleep_total >= 16)

#############
## mutate()

msleep %>%
  mutate(rem_proportion = sleep_rem / sleep_total) %>%
  head

b<- msleep %>%
  mutate(rem_proportion = sleep_rem / sleep_total) %>%
  head
b

msleep %>% 
  mutate(rem_proportion = sleep_rem / sleep_total, bodywt_grams = bodywt * 1000) %>%
  head

############
## Summarise()

msleep %>% summarise(avg_sleep = mean(sleep_total))

msleep %>% summarise(avg_sleep = mean (sleep_total),
                     min_sleep = min (sleep_total),
                     max_sleep = max (sleep_total), 
                     total = n())

###########
## group_by()

msleep %>% 
  group_by(order) %>%
  summarise(avg_sleep = mean(sleep_total),
            min_sleep = min(sleep_total),
            max_sleep = max(sleep_total),
            total = n())
  
  
###########
## Introduction to dplyr (https://cran.rstudio.com/web/packages/dplyr/vignettes/introduction.html)
#########

?nycflights13
library(nycflights13)
dim(flights)
head(flights)

flight<- tbl_df(flights)
dim(flight)
head(flight)

#############
## filter() and slice()

filter(flights, month == 1, day == 1)

flights[flights$month == 1 & flights$day == 1,]

tail(filter(flights, month == 1 | month == 2))

slice(flights,1:10)

flights[1:10,]

#############
## arrange()

arrange(flights, year, month, day)
?arrange
arrange(flights, year, desc(month), day)

desc(1:10)

arrange(flights, desc(arr_delay))
arrange(flights, arr_delay)

flights[order(flights$arr_delay, decreasing = TRUE), ]

##############
## Select()

head(flights)

select(flights, tail_num = tailnum)

rename(flights, tail_num = tailnum)

distinct(flights, tailnum)

distinct(flights, tailnum, dest)

############
## mutate()

mutate(flights,
       gain = arr_delay - dep_delay,
       speed = distance / air_time * 60)

dim(flight)

mutate(flights,
       gain = arr_delay - dep_delay,
       speed = distance / air_time * 60) %>%
  select(distance:speed)

transform(flights,
          gain = arr_delay - delay,
          gain_per_hour = gain / (air_time / 60)
)

transmute(flights,
          gain = arr_delay - dep_delay,
          gain_per_hour = gain / (air_time / 60)
          )

##########
## summarise()

summarise(flights, 
          delay = mean(dep_delay, na.rm = TRUE))

sample_n(flights,10)
sample_frac(flights, 0.01)

vignette("window-functions")

############
## Grouped operations

by_tailnum<- group_by(flights,tailnum)
select(by_tailnum,tailnum)

delay <- summarise(by_tailnum,
                   count = n(),
                   dist = mean(distance, na.rm = TRUE),
                   delay = mean(arr_delay, na.rm = TRUE)
                   )
delay

delay<- filter(delay, count >20, dist < 2000)
delay

library(ggplot2)

ggplot(delay, aes(dist, delay)) +
  geom_point(aes(size = count), alpha = 1/2) +
  geom_smooth() +
  scale_size_area()

destinations<- group_by(flights, dest)
destinations

summarise(destinations,
          planes = n_distinct(tailnum),
          flights = n())

daily<- group_by(flights, year, month, day)
daily
class(daily)
str(daily)
View(daily)
glimpse(daily)

per_day<- summarise(daily, flights = n())
per_day
View(per_day)
print(tbl_df(per_day), n=40)


per_month<- summarise(per_day, flights = sum(flights))
per_month

per_year<- summarise(per_month, flights = sum(flights))
per_year

################
## Chaining

flights %>%
  group_by(year, month, day) %>%
  select(arr_delay, dep_delay) %>%
  summarise(
    arr = mean(arr_delay, na.rm = TRUE),
    dep = mean(dep_delay, na.rm = TRUE)
  ) %>%
  filter(arr > 30 | dep > 30) %>%
  print(n = nrow(.))


###############################
## Data manipulation with tidyr (https://www.r-bloggers.com/data-manipulation-with-tidyr/)
##############################

library(tidyr)
library(dplyr)
head(mtcars)

rownames(mtcars)
colnames(mtcars)

mtcars$car <- rownames(mtcars)
head(mtcars)
mtcars<- mtcars[,c(12,1:11)]
head(mtcars)
glimpse(mtcars)

mtcarsNew<- mtcars %>% gather(attribute, value, -car)
head(mtcarsNew)

tail(mtcarsNew)
View(mtcarsNew)

mtcarsNew<- mtcars %>%
  gather(attribute, value, mpg:gear)

head(mtcarsNew)
View (mtcarsNew)

############
## spread()

mtcarsSpread<- mtcarsNew %>% spread(attribute, value)
head(mtcarsSpread)

set.seed(1)
date <- as.Date('2016-01-01') + 0:14
hour <- sample(1:24, 15)
min <- sample(1:60, 15)
second <- sample(1:60, 15)
event <- sample(letters, 15)
data <- data.frame(date, hour, min, second, event)
data

dataNew<- data %>%
  unite(datehour, date,hour, sep= ' ') %>%
  unite(datetime, datehour, min, second, sep = ':')
dataNew

##############
## separate()

data1<- dataNew %>%
  separate(datetime, c("date", "time"), sep = " ") %>%
  separate(time, c("hour", "min", "second"), sep = ":")
data1

identical(data1, data)
table(data1 == data, useNA = 'ifany')
dim(data1)
15*5

?all.equal
all.equal(data1,data)
all.equal(data,data)

data

system.time(data_frame(a=1:1000000, b = 1:1000000, c = 1:1000000, 
                       d = 1:1000000, e = 1:1000000))

system.time(data.frame(a=1:1000000, b = 1:1000000, c = 1:1000000, 
                       d = 1:1000000, e = 1:1000000))


quickadd <- function(g){ 
  return(g+1)
}

slowadd <- function(g){
  h <- rep(NA, length(g))
  for (i in 1:length(g)){
    h[i] <- g[i] + 1
  }
  return(h)
}

g <- rnorm(10000000)
system.time(a <- slowadd(g))

system.time(a <- quickadd(g))





















