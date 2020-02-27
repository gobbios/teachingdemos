library(shiny)

shinyUI(navbarPage("association null models", selected = "flipping/swapping", inverse = TRUE,
                   tabPanel("read me",
                            fluidPage(
                                includeMarkdown("instructions.Rmd")
                            )
                            ),
                   tabPanel("flipping/swapping",
                            tags$head(
                              tags$style(HTML("hr {border-top: 1px solid #000000;}"))
                            ),
                            sidebarLayout(
                                sidebarPanel(
                                    h4("data settings"),
                                    fluidRow(
                                        column(6, numericInput("nind", "individuals", min = 4, max = 20, value = 6, step = 1)),
                                        column(6, numericInput("nobs", "observations", min = 6, max = 1000, value = 50, step = 1))
                                    ),
                                    sliderInput("npres", "proportion of '1's in matrix", min = 0.2, max = 0.8, step = 0.01, value = 0.3),
                                    fluidRow(
                                        column(6, checkboxInput("prefdyad", "A and B are strong associates", value = TRUE)),
                                        column(6, checkboxInput("avoiddyad", "C and D avoid each other", value = FALSE))
                                    ),
                                    actionButton("gendata", "generate data set"),

                                    hr(),
                                    h4("analysis options"),
                                    # radioButtons("ai_meth", "association index", choices = c("HWI", "SRI"), selected = "HWI", inline = TRUE),
                                    sliderInput("nrand", "number of randomizations", min = 1, max = 5000, step = 1, value = 1000),
                                    fluidRow(
                                        column(6, checkboxInput("checkerboardsonly", "use only checkerboards", value = TRUE)),
                                        column(6, actionButton("genrand", "run randomizations"))
                                    ),

                                    hr(),
                                    h4("display options"),
                                    sliderInput("display_step", "visualization step", min = 1, max = 5000, step = 1, value = 1, animate = animationOptions(interval = 500)),
                                    sliderInput("textcex", "text size", min = 0.6, max = 2, step = 0.1, value = 1.8)
                                ),

                                mainPanel(
                                    plotOutput("distPlot")
                                )
                            )
                            ),
                   tabPanel("multiple chains",
                            sidebarLayout(
                                sidebarPanel(
                                    h4("randomization settings"),
                                    checkboxInput("checkerboardsonly2", "use only checkerboards", value = TRUE),
                                    sliderInput("nswaps2", "swap/flip trials per chain", min = 10, max = 1000, step = 1, value = 100),
                                    sliderInput("nchains", "number of chains", min = 10, max = 1000, step = 1, value = 20),
                                    actionButton("runchains", "run chains"),
                                ),
                                mainPanel(
                                    plotOutput("chainplot")
                                )
                            )
                   ),
                   tabPanel("dominance and centrality",
                            sidebarLayout(
                                sidebarPanel(
                                    h4("settings"),
                                    fluidRow(
                                      column(6, numericInput("nind_domcentr", "individuals", min = 4, max = 20, value = 10, step = 1)),
                                      column(6, numericInput("nobs_domcentr", "observations", min = 10, max = 2000, value = 200, step = 1))
                                    ),
                                    sliderInput("nruns_dom", "number of randomizations", min = 1, max = 5000, step = 1, value = 1000),
                                    fluidRow(
                                      column(6, actionButton("gen_domcentr", "generate data")),
                                      column(6, actionButton("run_domcentr", "randomize"))
                                    )


                                ),
                                mainPanel(
                                    plotOutput("domcentr")
                                )
                            )

                            )



                   )

    )
