body <- dashboardBody(tabItems(
  # First tab content
  tabItem(
    tabName = "uni",
    class = "active",
    h2("Univariate statistics"),
    fluidRow(
      align = "center",
      selectInput(
        inputId = "varUni",
        label = "Choose a variable",
        choices = gsub(
          pattern = "[.]",
          replacement = " ",
          x = colnames(df)
        ),
        selected = gsub(
          pattern = "[.]",
          replacement = " ",
          x = target
        )
      ),
      withSpinner(plotOutput(outputId = "plot1"), color="#0dc5c1")
    )
  ),
  # Second tab content
  tabItem(
    tabName = "bi",
    h2("Bivariate statistics"),
    fluidRow(
      align = "center",
      selectInput(
        inputId = "varBi1",
        label = "Choose a variable",
        choices = gsub(
          pattern = "[.]",
          replacement = " ",
          x = colnames(df)
        ),
        selected = gsub(
          pattern = "[.]",
          replacement = " ",
          x = col_num[1]
        )
      ),
      selectInput(
        inputId = "varBi2",
        label = "Choose another variable",
        choices = gsub(
          pattern = "[.]",
          replacement = " ",
          x = colnames(df)
        ),
        selected = gsub(
          pattern = "[.]",
          replacement = " ",
          x = col_num[1]
        )
      ),
      withSpinner(plotOutput(outputId = "plot2"), color="#0dc5c1")
    )
  )
))
