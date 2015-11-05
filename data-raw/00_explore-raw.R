#' ---
#' title: "Smell test the raw candy data"
#' author: "Jenny Bryan"
#' date: "`r format(Sys.Date())`"
#' output:
#'   html_document:
#'     keep_md: TRUE
#' ---

## This file does not contain the code to make the (mildly) cleaner datasets in
## this directory, since that's your homework.

library(readr)
library(dplyr)

raw <- read_csv("CANDY-HIERARCHY-2015 SURVEY-Responses.csv",
                col_types = cols(
                  Timestamp = col_datetime("%m/%d/%Y %H:%M:%S")
                ))
raw
glimpse(raw)

## How many variables are there?
## Can you clump them into groups for similar exploration and cleaning?

## Do you like the variable names? No? What are you going to do about it? Will
## each candy even remain as a variable or will it becomes ... IDK maybe a
## factor level in a reshaped data frame? How will you track the relationship
## between original variable names and the eventual variable names or factor
## levels?

## Timestamp ... can you make this a proper POSIXct variable?
## How old are you? ... why is this character instead of integer?

## How hair-raising are the free text fields, such as "Please leave any remarks
## or comments regarding your choices."?

## What do you think the clean, ready-to-analyze dataset(s) should look like?

## Should we build some sort of ID for each respondent? What should that look
## like?

## What are some intermediate datasets you could form? Maybe you don't need
## *all* the candies, maybe you can ignore certain types of questions at first,
## etc. Give yourself some achievable goals!
