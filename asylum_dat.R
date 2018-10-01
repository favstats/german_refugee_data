
pacman::p_load(tidyverse, tabulizer, magrittr)

asylum_dat <- data.frame(string_dat = c(
  "Jan 2010 - 3.005",
  "Feb 2010 - 2.777",
  "Mar 2010 - 3.291",
  "Apr 2010 - 2.851",
  "May 2010 - 2.663",
  "Jun 2010 - 3.265",
  "Jul 2010 - 3.699",
  "Aug 2010 - 4.466",
  "Sep 2010 - 5.301",
  "Oct 2010 - 5.789",
  "Nov 2010 - 5.469",
  "Dec 2010 - 4.282",
  "Jan 2011 - 4.245",
  "Feb 2011 - 3.736",
  "Mar 2011 - 4.076",
  "Apr 2011 - 3.365",
  "May 2011 - 3.874",
  "Jun 2011 - 3.593",
  "Jul 2011 - 3.899",
  "Aug 2011 - 4.568",
  "Sep 2011 - 4.919",
  "Oct 2011 - 5.098",
  "Nov 2011 - 5.830",
  "Dec 2011 - 5.063",
  "Jan 2012 - 5.374",
  "Feb 2012 - 4.588",
  "Mar 2012 - 4.342",
  "Apr 2012 - 3.953",
  "May 2012 - 4.199",
  "Jun 2012 - 4.661",
  "Jul 2012 - 5.380",
  "Aug 2012 - 6.529",
  "Sep 2012 - 8.483",
  "Oct 2012 - 12.187",
  "Nov 2012 - 9.986",
  "Dec 2012 - 5.583",
  "Jan 2013 - 8.186",
  "Feb 2013 - 6.636",
  "Mar 2013 - 6.295",
  "Apr 2013 - 8.557",
  "May 2013 - 8.358",
  "Jun 2013 - 9.510",
  "Jul 2013 - 11.063",
  "Aug 2013 - 11.177",
  "Sep 2013 - 13.752",
  "Oct 2013 - 15.251",
  "Nov 2013 - 14.147",
  "Dec 2013 - 11.028",
  "Jan 2014 - 14.463",
  "Feb 2014 - 11.220",
  "Mar 2014 - 11.280",
  "Apr 2014 - 11.503",
  "May 2014 - 12.457",
  "Jun 2014 - 14.019",
  "Jul 2014 - 19.431",
  "Aug 2014 - 17.695",
  "Sep 2014 - 19.043",
  "Oct 2014 - 21.279",
  "Nov 2014 - 22.075",
  "Dec 2014 - 20.384",
  "Jan 2015 - 25.042",
  "Feb 2015 - 26.083",
  "Mar 2015 - 32.054",
  "Apr 2015 - 27.178",
  "May 2015 - 25.992",
  "Jun 2015 - 35.449",
  "Jul 2015 - 37.531",
  "Aug 2015 - 36.422",
  "Sep 2015 - 43.071",
  "Oct 2015 - 54.877",
  "Nov 2015 - 57.816",
  "Dec 2015 - 48.277",
  "Jan 2016 - 52.103",
  "Feb 2016 - 67.797",
  "Mar 2016 - 59.975",
  "Apr 2016 - 60.943",
  "May 2016 - 55.259",
  "Jun 2016 - 74.637",
  "Jul 2016 - 74.454",
  "Aug 2016 - 91.331",
  "Sep 2016 - 76.400",
  "Oct 2016 - 32.640",
  "Nov 2016 - 26.438",
  "Dec 2016 - 20.575",
  "Jan 2017 - 17.964",
  "Feb 2017 - 16.568",
  "Mar 2017 - 20.136",
  "Apr 2017 - 14.848",
  "May 2017 - 16.641",
  "Jun 2017 - 15.261",
  "Jul 2017 - 16.844",
  "Aug 2017 - 18.651",
  "Sep 2017 - 16.520",
  "Oct 2017 - 17.028",
  "Nov 2017 - 18.711",
  "Dec 2017 - 14.293",
  "Jan 2018 - 15.077",
  "Feb 2018 - 12.490",
  "Mar 2018 - 12.622",
  "Apr 2018 - 13.163",
  "May 2018 - 12.494",
  "Jun 2018 - 13.254",
  "Jul 2018 - 15.199",
  "Aug 2018 - 15.122"))

asylum_dat %<>% tidyr::separate(string_dat,  into = c("time", "number"), sep = "-") %>% 
  # mutate(number = str_replace(number, ".", "")) %>%
  mutate(number = as.numeric(number)*1000) %>%
  mutate(time = lubridate::dmy(paste("01",time)))

save(asylum_dat, file = "data/asylum_dat.Rdata")


# Location of WARN notice pdf file
refugee_decisions1 <- 'http://biaj.de/images/2018-09-22_bamf-asyl-entscheidungen-bis-082018.pdf'

# Extract the table
out1 <- extract_tables(refugee_decisions1, pages = 2)

first_wave <- do.call(rbind, out1[-length(out1)]) %>% as_tibble()

clean_numbers <- function(x){
  str_replace(x, ",", "\\.") %>% 
    str_remove("%") %>% 
    as.numeric
}


first_wave <- first_wave %>% 
  select(V1, V7, V10) %>% 
  .[-c(1:17),] %>% 
  separate(V7, into = c("pos_til_jan", 
                        "pos_gleitend", 
                        "pos_percent"), sep = " ") %>% 
  separate(V10, into = c("neg_til_jan", 
                         "neg_gleitend", 
                         "neg_percent"), sep = " ") %>% 
  mutate_all(clean_numbers) %>% 
  rename(time = V1) %>% 
  drop_na(time) %>% 
  mutate(pos = c(33.501, diff(pos_til_jan))) %>% 
  mutate(pos = ifelse(time == 201801, pos_til_jan, pos)) %>% 
  mutate(neg = c(37.249, diff(neg_til_jan))) %>% 
  mutate(neg = ifelse(time == 201801, neg_til_jan, neg))  %>%
  mutate(time = lubridate::dym(paste("01",time))) %>% 
  select(time, pos, neg, pos_percent, neg_percent) %>% 
  mutate_at(vars(pos, neg), function(x)x*1000)


# Location of WARN notice pdf file
refugee_decisions2 <- 'http://biaj.de/images/2017-12-11_bamf-asyl-entscheidungen-bis-112017.pdf'

# Extract the table
out2 <- extract_tables(refugee_decisions2, pages = 2)

second_wave <- do.call(rbind, out2[-length(out2)]) %>% as_tibble()

second_wave <- second_wave %>% 
  select(V1, V7, V10) %>% 
  filter(str_detect(V1, "2016")) %>% 
  separate(V7, into = c("pos_til_jan", 
                        "pos_gleitend", 
                        "pos_percent"), sep = " ") %>% 
  separate(V10, into = c("neg_til_jan", 
                         "neg_gleitend", 
                         "neg_percent"), sep = " ") %>% 
  mutate_all(clean_numbers) %>% 
  rename(time = V1) %>% 
  drop_na(time) %>% 
  mutate(pos = c(31.623, diff(pos_til_jan))) %>%  
  mutate(neg = c(17.761, diff(neg_til_jan))) %>% 
  mutate(time = lubridate::dym(paste("01",time))) %>% 
  select(time, pos, neg, pos_percent, neg_percent) %>% 
  mutate_at(vars(pos, neg), function(x)x*1000)

refugee_decisions3 <- 'http://biaj.de/images/2016-07-12_bamf-asyl-entscheidungen-bis-062016.pdf'

# Extract the table
out3 <- extract_tables(refugee_decisions3, pages = 3)

third_wave <- do.call(rbind, out3[-length(out3)]) %>% as_tibble()

third_wave1 <- third_wave %>% 
  select(V1, pos_til_jan = V10 , pos_percent = V12, 
         neg_til_jan = V17, neg_percent = V19) %>% 
  filter(str_detect(V1, "2015")) %>% 
  mutate_all(clean_numbers) %>% 
  rename(time = V1) %>% 
  drop_na(time) %>% 
  mutate(pos = c(8.041, diff(pos_til_jan))) %>%  
  mutate(neg = c(9.794, diff(neg_til_jan))) %>% 
  mutate(time = lubridate::dym(paste("01",time))) %>% 
  select(time, pos, neg, pos_percent, neg_percent) %>% 
  mutate_at(vars(pos, neg), function(x)x*1000)



third_wave2 <- third_wave %>% 
  select(V1, pos_til_jan = V10 , pos_percent = V12, 
         neg_til_jan = V17, neg_percent = V19) %>% 
  filter(str_detect(V1, "2014")) %>% 
  mutate_all(clean_numbers) %>% 
  rename(time = V1) %>% 
  drop_na(time) 


## simulating jan through jun

# pos_til_jun <- third_wave2$pos_til_jan[7] - third_wave2$pos_til_jan[1]
# neg_til_jun <- third_wave2$neg_til_jan[7] - third_wave2$neg_til_jan[1]
# 
# avg_pos_til_jun <- pos_til_jun / 5
# avg_neg_til_jun <- neg_til_jun / 5
# 
# times <- lubridate::dym(paste0("01",201401:201405))
# 
# avg_data <- tibble(time = times, pos = avg_pos_til_jun, neg = avg_neg_til_jun) %>% 
#   mutate_at(vars(pos, neg), function(x) round(x*1000))
# 
# avg_data

third_wave2 <- third_wave2 %>% 
  mutate(pos = c(NA, diff(pos_til_jan))) %>%  
  mutate(neg = c(NA, diff(neg_til_jan))) %>% 
  mutate(time = lubridate::dym(paste("01",time))) %>% 
  select(time, pos, neg, pos_percent, neg_percent) %>% 
  mutate_at(vars(pos, neg), function(x)x*1000)


refugee_decisions <- bind_rows(third_wave2, third_wave1, second_wave, first_wave)

save(refugee_decisions, file = "data/refugee_decisions.Rdata")