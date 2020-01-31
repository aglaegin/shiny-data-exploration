sidebar <- dashboardSidebar(sidebarMenu(
  menuItem(
    "Univariate statistics",
    tabName = "uni",
    icon = icon("dashboard")
  ),
  menuItem("Bivariate statistics", tabName = "bi", icon = icon("th"))
))