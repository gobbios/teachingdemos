This repository contains code examples and shiny apps I wrote for teaching purposes.

In order to start any of the apps remotely, you need the `shiny` package.

## network null models

This app illustrates the use of randomizations for hypothesis testing in the context of social networks. It requires the `igraph` package installed.

`shiny::runGitHub("teachingdemos", "gobbios", subdir = "shiny_apps/network_null_models")`


## interactive interactions

This is one of the first apps I wrote and just let's you *slice* through a three-dimensional display of a regression model with a two-way interaction. It requires the `effects` package.

`shiny::runGitHub("teachingdemos", "gobbios", subdir = "shiny_apps/interactiveinteraction")`

## 2-way interaction

This is another app illustrating visually what is happening in models with interactions. It requires the `lme4` and `effects` packages.


## shiny network

I wrote this app in response to a friend asking about a nice way of illustrating knock-out simulations in the context of networks. It requires the `igraph` package.

`shiny::runGitHub("teachingdemos", "gobbios", subdir = "shiny_apps/shinynetwork1")`
