server <- function(input, output) {
  output$plot1 <- renderPlot({
    req(input$varUni)
    v0 <- gsub(pattern = " ", replacement = ".", input$varUni)
    if (v0 %in% col_num) {
      hist(
        df[, v0],
        freq = F,
        xlab = input$varUni,
        ylab = "Count",
        main = paste("Histogram of", input$varUni)
      )
    } else {
      barplot(table(df[!(is.na(df[, v0]) | (df[, v0] == "")), v0]),
              ylab = "Count",
              main = paste("Bar plot of", input$varUni))
    }
  })
  output$plot2 <- renderPlot({
    req(input$varBi1, input$varBi2)
    v1 <- gsub(pattern = " ",
               replacement = ".",
               input$varBi1)
    v2 <- gsub(pattern = " ",
               replacement = ".",
               input$varBi2)
    if (v1 %in% col_num) {
      if (v2 %in% col_num) {
        # num_num ----
        plot(
          df[v1][!(is.na(df[v1]) | is.na(df[v2]))],
          df[v2][!(is.na(df[v1]) | is.na(df[v2]))],
          pch = 5,
          xlab = input$varBi1,
          ylab = input$varBi2,
          main = paste(
            "Distribution of",
            input$varBi1,
            "and",
            input$varBi2,
            "\n Correlation value of",
            round(cor(df[v1][!(is.na(df[v1]) | is.na(df[v2]))],
                      df[v2][!(is.na(df[v1]) | is.na(df[v2]))]), 3)
          )
        )
      } else {
        # num_cat ----
        df0 <- df[!(is.na(df[, v1]) | is.na(df[, v2]) | (df[, v2] == "")), ]
        vv1 <- df0[, v1]
        vv2 <- df0[, v2]
        vv2 <- factor(vv2)
        grp <-
          cut(
            vv1,
            breaks = seq(
              from = min(vv1, na.rm = T),
              to = max(vv1, na.rm = T),
              length.out = nbin
            ),
            include.lowest = TRUE
          )
        df2 <- data.frame(vv1, vv2, grp)
        ggplot(df2, aes(x = grp, fill = vv2)) + geom_bar(position = "dodge") + ggtitle(
          paste(
            "Plot of",
            input$varBi1,
            "by",
            input$varBi2,
            "\n Correlation ratio of",
            round(DiscriMiner::corRatio(variable = vv1, group = vv2), 3)
          )
        ) + xlab(input$varBi2) + ylab("Count") + labs(fill = input$varBi2) + theme(plot.title = element_text(hjust = 0.5))
      }
    } else {
      if (v2 %in% col_num) {
        # cat_num ----
        df0 <- df[!(is.na(df[, v1]) | is.na(df[, v2]) | (df[, v1] == "")), ]
        vv1 <- df0[, v2]
        vv2 <- df0[, v1]
        vv2 <- factor(vv2)
        grp <-
          cut(
            vv1,
            breaks = seq(
              from = min(vv1, na.rm = T),
              to = max(vv1, na.rm = T),
              length.out = nbin
            ),
            include.lowest = TRUE
          )
        df2 <- data.frame(vv1, vv2, grp)
        ggplot(df2, aes(x = grp, fill = vv2)) + geom_bar(position = "dodge") + ggtitle(
          paste(
            "Plot of",
            input$varBi2,
            "by",
            input$varBi1,
            "\n Correlation ratio of",
            round(DiscriMiner::corRatio(variable = vv1, group = vv2), 3)
          )
        ) + xlab(input$varBi1) + ylab("Count") + labs(fill = input$varBi1) + theme(plot.title = element_text(hjust = 0.5))
      } else {
        # cat_cat ----
        df0 <-
          df[!(is.na(df[, v1]) |
                 is.na(df[, v2]) | (df[, v1] == "") | (df[, v2] == "")), ]
        vv1 <- df0[, v2]
        vv2 <- df0[, v1]
        dat <- data.frame(table(df0[, v1] , df0[, v2]))
        names(dat) <- c("Bi1", "Bi2", "Count")
        ggplot(data = dat, aes(
          x = Bi1,
          y = Count,
          fill = Bi2
        )) + geom_bar(stat = "identity") + xlab(input$varBi1) + labs(fill = input$varBi2) + ggtitle(
          paste(
            "Count of",
            input$varBi2,
            "by",
            input$varBi1,
            "\n Cramér\'s V of",
            round(lsr::cramersV(vv1, vv2), 3)
          )
        ) + theme(plot.title = element_text(hjust = 0.5))
      }
    }
  })
}