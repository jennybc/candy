#' ---
#' title: "Project M.I.N.T.S. "
#' author: "Kieran Samuk"
#' date: "`r format(Sys.Date())`"
#' output:
#'   html_document:
#'     keep_md: TRUE
#' ---

## This file does not contain the code to make the (mildly) cleaner datasets in
## this directory, since that's your homework.

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

setwd("/Users/Kieran/Documents/github repos/candy/data-raw")

raw <- read_csv("CANDY-HIERARCHY-2015 SURVEY-Responses.csv",
								col_types = cols(
									Timestamp = col_datetime("%m/%d/%Y %H:%M:%S")
								))
raw
glimpse(raw)

# select only the candy columns
candy_only  <- raw %>%
	select(matches("^\\[.+\\]"))

# clean the column names
names(candy_only) <- gsub("\\[|\\]", "", names(candy_only))

# add in an ID variable
candy_only$id <- 1:nrow(candy_only)

# wide to long
candy_dat <- gather(candy_only, key = "candy", value = "emotion", -id)

# classify the mints
mint_pattern <- "[Mm]int"
candy_dat$is_mint <- grepl(mint_pattern, candy_dat$candy)

# logical version of emotions
candy_dat$emotion_logical <- grepl("JOY", candy_dat$emotion)

# summarise the mean emotion for each candy (keeping is_mint)
mint_dat <- candy_dat %>%
	group_by(is_mint, candy) %>%
	summarise(mean_emotion = mean(emotion_logical))

# plot the mint results
mint_dat %>%
	ggplot(aes(x = is_mint, y = mean_emotion, color = is_mint)) +
	geom_jitter(size = 3)

# t-test
mint_dat %>%
	lm(mean_emotion ~ is_mint, data = .) %>%
	anova()