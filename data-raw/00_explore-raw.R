#' ---
#' title: "Getting you started with the candy data"
#' author: "Jenny Bryan"
#' date: "`r format(Sys.Date())`"
#' output:
#'   html_document:
#'     keep_md: TRUE
#' ---

#' I'm putting the cleaning done during class plus a bit more here, in case it's
#' helpful to you re: homework 7.

library(readr)
suppressPackageStartupMessages(library(dplyr))
library(tidyr)
library(stringr)
library(ggplot2)

raw <- read_csv("CANDY-HIERARCHY-2015 SURVEY-Responses.csv",
                col_types = cols(
                  Timestamp = col_datetime("%m/%d/%Y %H:%M:%S")
                ))
raw
#glimpse(raw)
nrow(raw)
ncol(raw)

#' Create an ID variable for each survey respondent. Bring that variable to the
#' front. Rename "How old are you?" to `age` and make it the second variable.

raw_with_id <- raw %>%
  mutate(id = sprintf("ID%04d", row_number())) %>%
  select(id,
         age = starts_with("How"),
         everything())

#' Clean `age`. I only expect two characters and they should coerce nicely to
#' integer. Drop rows with `NA` for `age`.

tmp <- raw_with_id %>%
  mutate(age_nchar = str_length(age))

ggplot(tmp %>% filter(age_nchar < 8), aes(age_nchar)) + geom_bar()

raw_with_id <- raw_with_id %>%
  mutate(age_nchar = str_length(age)) %>%
  filter(age_nchar <= 2) %>%
  select(-age_nchar) %>%
  mutate(age = as.integer(age)) %>%
  filter(!is.na(age))
nrow(raw_with_id)

#' First cleaning pass on the candy variable names. In tidy formats, these are
#' also destined to be factor levels.

tmp_names <-
  data_frame(orig_name = names(raw_with_id)) %>%
  mutate(is_candy = str_detect(orig_name,"^\\["),
         new_name = str_replace_all(orig_name, "(^\\[)(.*)(\\]$)", "\\2"),
         new_name = str_replace_all(new_name, '["’]', "'"),
         is_changed = orig_name != new_name)
sum(tmp_names$is_candy)
table(tmp_names$is_candy, tmp_names$is_changed)

names(raw_with_id) <- tmp_names$new_name

#' Write `id` and the candy variables out to file.

candy_names <- tmp_names %>%
  filter(is_candy) %>%
  .$new_name
length(candy_names)

candy_dat_untidy <- raw_with_id %>%
  select(one_of(c("id", candy_names)))

write_csv(candy_dat_untidy, "candy-joy-untidy.csv")

#' Write a selective table to file in which there are no candy variables and
#' each row is a respondent. Dropping variables that I can't imagine using any
#' time soon.

id_dat <- raw_with_id %>%
  select(-one_of(candy_names))
id_dat
names(id_dat)

df <- data_frame(
  orig_name = c(
    "id",
    "age",
    "Timestamp",
    "Are you going actually going trick or treating yourself?",
    "Guess the number of mints in my hand.",
    "'That dress* that went viral early this year - when I first saw it, it was ________'",
    "What is your favourite font?"),
  new_name = c(
    "id",
    "age",
    "timestamp",
    "trick_or_treat",
    "n_mints",
    "dress",
    "font")
)

id_dat <- id_dat %>%
  select(one_of(df$orig_name))
id_dat
names(id_dat) <- df$new_name[match(df$orig_name, names(id_dat))]
id_dat

id_dat %>%
  mutate(n_mints = str_sub(1, 53))

write_csv(id_dat, "candy-id.csv")

#' Tidy the candy data. This makes a rather large file and I won't be committing
#' and pushing it.

candy_dat <- candy_dat_untidy %>%
  gather(candy, joy, -id) %>%
  mutate(joy = joy == "JOY")
write_csv(candy_dat, "candy-joy.csv")

#' Create some small datasets, suitable for practicing joins.
candy_keep <- c("Snickers", "Hugs (actual physical hugs)", "Twix", "Vicodin")
id_keep <- c(178, 2254, 4817, 5522) %>%
  sprintf("ID%04d", .)

candy_mini <- candy_dat %>%
  filter(candy %in% candy_keep, id %in% id_keep)

write_csv(candy_mini, "candy-joy-mini.csv")

id_mini <- id_dat %>%
  filter(id %in% id_keep)

write_csv(id_mini, "candy-id-mini.csv")

#' Food for thought on first day we looked at this data:

#' How many variables are there?
#' Can you clump them into groups for similar exploration and cleaning?

#' Do you like the variable names? No? What are you going to do about it? Will
#' each candy even remain as a variable or will it becomes ... IDK maybe a
#' factor level in a reshaped data frame? How will you track the relationship
#' between original variable names and the eventual variable names or factor
#' levels?

#' Timestamp ... can you make this a proper POSIXct variable?
#' How old are you? ... why is this character instead of integer?

#' How hair-raising are the free text fields, such as "Please leave any remarks
#' or comments regarding your choices."?

#' What do you think the clean, ready-to-analyze dataset(s) should look like?

#' Should we build some sort of ID for each respondent? What should that look
#' like?

#' What are some intermediate datasets you could form? Maybe you don't need
#' *all* the candies, maybe you can ignore certain types of questions at first,
#' etc. Give yourself some achievable goals!
