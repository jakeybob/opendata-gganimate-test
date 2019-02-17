library(tidyverse)
library(jsonlite)
library(lubridate)
library(rvest)
library(xml2)
library(ggthemes)
library(gganimate)

#### download and format MSP PQ data from Scot Parl opendata ####
generate_archive_opendata <- function(start_year=2017, save=TRUE, ...){
  
  current_year <- as.integer(format(Sys.time(), "%Y"))
  years <- as.character(seq(from=start_year, to=current_year))
  
  #### URLS ####
  event_subtypes_url <- "https://data.parliament.scot/api/motionsquestionsanswerseventsubtypes/json"
  MSP_url <- "https://data.parliament.scot/api/members/json"
  constituency_url <- "https://data.parliament.scot/api/constituencies/json"
  region_url <- "https://data.parliament.scot/api/regions/json"
  PQ_urls <- paste0("https://data.parliament.scot/api/motionsquestionsanswersquestions?year=", years)
  
  
  #### PQ DATA ####
  
  current_year_PQ_IDs <- dplyr::as_tibble(jsonlite::fromJSON(txt=url(PQ_urls[length(PQ_urls)]), simplifyDataFrame = TRUE))$EventID
  if(save==TRUE){
    saveRDS(current_year_PQ_IDs, "recentPQ_IDs.rds")
  }
  
  PQ_data <- dplyr::tibble()
  for(PQ_url in PQ_urls){
    PQ_data <- dplyr::bind_rows(PQ_data, dplyr::as_tibble(jsonlite::fromJSON(txt=url(PQ_url), simplifyDataFrame = TRUE)))
    print(paste(stringr::str_sub(PQ_url, -4), "data added."))
  }
  
  # will hardcode cols_to_keep as below to keep consistency 
  cols_to_keep <- c("UniqueID", "EventID", "EventTypeID", "EventSubTypeID", "MSPID", "Party", "RegionID", "ConstituencyID", 
                    "ApprovedDate", "SubmissionDateTime", "ItemText", "AnswerText", "AnswerDate", 
                    "ExpectedAnswerDate", "MeetingDate", "AnsweredByMSP", "RegisteredInterest")
  
  PQ_data <- PQ_data %>%
    select(cols_to_keep) %>%
    mutate_at(vars(contains("Date")), ~as_datetime(.))
  
  
  #### EVENT CLASSIFICATION ####
  
  event_subtypes_data <- dplyr::as_tibble(jsonlite::fromJSON(txt=url(event_subtypes_url), simplifyDataFrame = TRUE))
  
  event_subtypes_df <- event_subtypes_data %>%
    select(-IntroText) %>%
    filter(str_detect(EventSubType, fixed("question", ignore_case=TRUE)))
  
  
  
  #### REGION CLASSIFICATION ####
  # RegionID variable already takes care of region names changing over time etc, so don't require extra logic
  region_data <- dplyr::as_tibble(jsonlite::fromJSON(txt=url(region_url), simplifyDataFrame = TRUE))
  
  region_df <- region_data %>%
    mutate_at(vars(contains("Date")), ~as_datetime(.))  #  convert date/time columns from char to datetime
  
  
  #### CONSTITUENCY CLASSIFICATION ####
  # Likewise, constituency IDs are also unique over time so shouldn't have to take account of
  constituency_data <- dplyr::as_tibble(jsonlite::fromJSON(txt=url(constituency_url), simplifyDataFrame = TRUE))
  
  constituency_df <- constituency_data %>%
    mutate_at(vars(contains("Date")), ~as_datetime(.)) %>%  #  convert date/time columns from char to datetime
    select(-RegionID)
  
  
  #### MSP DATA ####  
  MSP_data <- dplyr::as_tibble(jsonlite::fromJSON(txt=url(MSP_url), simplifyDataFrame = TRUE))
  
  MSP_df <- MSP_data %>%
    select(-BirthDateIsProtected) %>%
    mutate_at(vars(contains("Date")), ~as_datetime(.)) %>%  #  convert date/time columns from char to datetime
    separate(ParliamentaryName, into=c("Surname", "FirstName"), sep=", ", extra="merge", remove=TRUE) %>%
    mutate(FullPreferredName = paste(PreferredName, Surname),
           age_years = year(as.period(interval(BirthDate, today()), unit = "years")),
           refresh_date = today()) %>%
    rename(MSPID = PersonID)
  
  
  #### DATA MERGE ####
  df <- PQ_data %>%
    
    # merge in the definitions of the different question types (will automatically drop e.g. motions)
    left_join(event_subtypes_df, by=c("EventTypeID", "EventSubTypeID")) %>%
    select(-c("EventTypeID", "EventSubTypeID")) %>%
    
    # merge in Region info
    left_join(region_df, by=c("RegionID" = "ID")) %>%
    select(-c("StartDate", "EndDate")) %>%
    rename(RegionName = Name) %>%
    
    # merge in Constituency info
    left_join(constituency_df, by=c("ConstituencyID" = "ID")) %>%
    select(-c("ShortName", "ValidFromDate", "ValidUntilDate")) %>%
    rename(ConstituencyName = Name) %>%
    
    # merge in MSP info
    left_join(MSP_df, df, by="MSPID") %>%
    
    arrange(ApprovedDate)
  
  if(save==TRUE){
    saveRDS(df, "opendata_archive.rds")}
  
  return(df)
}

# df <- generate_archive_opendata(start_year = 1999, save = TRUE)
df <- readRDS("opendata_archive.rds")

#### PQ Activity Plot ####
MSP_PQs_per_year <- df %>%
  filter(Party %in% c("Member for Falkirk West", "")==FALSE) %>%
  select(SubmissionDateTime, MSPID, FullPreferredName, Party) %>%
  mutate(year = year(SubmissionDateTime)) %>%
  group_by(year, FullPreferredName) %>%
  summarise(tot=n(),
            party=last(Party))

# 21 years, 300 unique MSP names = 6300 combinations; necessary to pad so that every MSP appears in the 
# data for each year (even if they have zero questions)
MSP_PQs_per_year_padded <- MSP_PQs_per_year %>%
  ungroup() %>%
  complete(FullPreferredName, nesting(year), fill=list(tot=0, party="dummy")) %>%
  filter(party != "dummy") %>%
  arrange(party, FullPreferredName) %>%
  mutate(FullPreferredName = factor(FullPreferredName, levels=unique(FullPreferredName)))

# Plot PQs asked per year as columns, party = fillcolour
p <- ggplot(MSP_PQs_per_year_padded, aes(x=FullPreferredName, y=tot, fill=party)) +
  geom_col() +
  labs(title = "{round(frame_time, 0)}", x = "", y = "PQs asked") +
  theme(legend.title=element_blank(),
        legend.position="bottom",
        axis.text.x = element_text(angle = 90, hjust = 1, size = 2),
        plot.title = element_text(face="bold")) +
  scale_fill_tableau(palette = "Classic 10") +
  # gganimate stuff
  transition_time(year) +
  enter_fade() +
  exit_shrink() +
  ease_aes("linear")

animate(p, width = 1000, height = 800)
anim_save("PQs.gif")


#### Age / Gender Plot ####

# note: 1 = female, 2 = male, no other values in dataset

MSPs_PQs_age_gender <- df %>%
  filter(is.na(age_years) == FALSE) %>%  #  drops those with no defined d.o.b
  mutate(year = year(SubmissionDateTime)) %>%
  select(year, age_years, GenderTypeID) %>%
  group_by(year, age_years, GenderTypeID)

# animated histogram
p2 <- ggplot(MSPs_PQs_age_gender, aes(x=age_years, fill=factor(GenderTypeID))) +
  geom_histogram(binwidth = 5, alpha = 1,  position="dodge") +
  labs(title = "{round(frame_time, 0)}", x = "age", y = "PQs asked") +
  theme(legend.title=element_blank(),
        plot.title = element_text(face="bold")) +
  scale_fill_manual(values=c("orange", "turquoise"), labels=c("female", "male")) +
transition_time(year) +
enter_fade() +
exit_shrink() +
ease_aes("linear")

animate(p2, width = 1000, height = 800)
anim_save("age_gender_hist.gif")

# animated density plot
p3 <- ggplot(MSPs_PQs_age_gender, aes(x=age_years, fill=factor(GenderTypeID))) +
  geom_density(bw=4, alpha=.5, linetype=0)  +
  labs(title = "{round(frame_time, 0)}", x = "age", y = "PQ density") +
  theme(legend.title=element_blank(),
        plot.title = element_text(face="bold")) +
  scale_fill_manual(values=c("orange", "turquoise"), labels=c("female", "male")) +
  transition_time(year) +
  enter_fade() +
  exit_shrink() +
  ease_aes('sine-in-out')

animate(p3, width = 1000, height = 800)
anim_save("age_gender_density.gif")

# # p3 but with no gridlines etc
# p4 <- ggplot(MSPs_PQs_age_gender, aes(x=age_years, fill=factor(GenderTypeID))) +
#   geom_density(bw=4, alpha=.5, linetype=0)  +
#   labs(title = "{round(frame_time, 0)}", x = "", y = "") +
#   theme(legend.title=element_blank(),
#         plot.title = element_blank(),
#         legend.position = "none") +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#        panel.background = element_blank(), axis.line = element_blank(),
#        axis.title.x=element_blank(),
#        axis.text.x=element_blank(),
#        axis.ticks.x=element_blank(),
#        axis.title.y=element_blank(),
#        axis.text.y=element_blank(),
#        axis.ticks.y=element_blank()) +
#   scale_fill_manual(values=c("orange", "turquoise"), labels=c("female", "male")) +
#   transition_time(year) +
#   enter_fade() +
#   exit_shrink() +
#   ease_aes('sine-in-out')
# 
# animate(p4, width = 1000, height = 800)
# anim_save("age_gender_density_02.gif")
