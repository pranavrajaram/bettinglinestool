library(tidyverse)
library(gt)
library(gtExtras)
library(shiny)
library(nflreadr)
library(scales)

game_lines <- read_csv("nfl_game_lines.csv")

cleaned_names <- game_lines %>%
  mutate(team_name = case_when(
    str_detect(team, fixed("Patriots")) ~ "Patriots",
    str_detect(team, fixed("Jets")) ~ "Jets",
    str_detect(team, fixed("Bills")) ~ "Bills",
    str_detect(team, fixed("Dolphins")) ~ "Dolphins",
    str_detect(team, fixed("Steelers")) ~ "Steelers",
    str_detect(team, fixed("Browns")) ~ "Browns",
    str_detect(team, fixed("Ravens")) ~ "Ravens",
    str_detect(team, fixed("Bengals")) ~ "Bengals",
    str_detect(team, fixed("Titans")) ~ "Titans",
    str_detect(team, fixed("Colts")) ~ "Colts",
    str_detect(team, fixed("Texans")) ~ "Texans",
    str_detect(team, fixed("Jaguars")) ~ "Jaguars",
    str_detect(team, fixed("Chiefs")) ~ "Chiefs",
    str_detect(team, fixed("Chargers")) ~ "Chargers",
    str_detect(team, fixed("Broncos")) ~ "Broncos",
    str_detect(team, fixed("Raiders")) ~ "Raiders",
    str_detect(team, fixed("Eagles")) ~ "Eagles",
    str_detect(team, fixed("Cowboys")) ~ "Cowboys",
    str_detect(team, fixed("Commanders")) ~ "Commanders",
    str_detect(team, fixed("Giants")) ~ "Giants",
    str_detect(team, fixed("Lions")) ~ "Lions",
    str_detect(team, fixed("Packers")) ~ "Packers",
    str_detect(team, fixed("Bears")) ~ "Bears",
    str_detect(team, fixed("Vikings")) ~ "Vikings",
    str_detect(team, fixed("Saints")) ~ "Saints",
    str_detect(team, fixed("Buccaneers")) ~ "Buccaneers",
    str_detect(team, fixed("Panthers")) ~ "Panthers",
    str_detect(team, fixed("Falcons")) ~ "Falcons",
    str_detect(team, fixed("Rams")) ~ "Rams",
    str_detect(team, fixed("Seahawks")) ~ "Seahawks",
    str_detect(team, fixed("Cardinals")) ~ "Cardinals",
    str_detect(team, fixed("49ers")) ~ "49ers"
  )) %>%
  filter(updated >= "2023-05-24")

pivoted_data <- cleaned_names %>%
  select(team_name, week,  bet_type, line, odds, over_under, provider) %>% 
  filter(!is.na(team_name)) %>%
  pivot_wider(names_from = provider, values_from = c(line, odds)) %>%
  select(-over_under)

tcl <- nflreadr::load_teams() %>%
  select(team_nick, team_wordmark)

moneylines_spreads <- pivoted_data %>%
  mutate(Consensus = case_when(bet_type == "Moneyline" ~ odds_Consensus,
                          bet_type == "Spread" ~ line_Consensus)) %>% 
  mutate(DraftKings = case_when(bet_type == "Moneyline" ~ odds_DraftKings,
                               bet_type == "Spread" ~ line_DraftKings)) %>% 
  mutate(FanDuel = case_when(bet_type == "Moneyline" ~ odds_FanDuel,
                               bet_type == "Spread" ~ line_FanDuel)) %>%
  mutate(Caesars = case_when(bet_type == "Moneyline" ~ odds_Caesars,
                               bet_type == "Spread" ~ line_Caesars)) %>%
  mutate(PointsBet = case_when(bet_type == "Moneyline" ~ odds_PointsBet,
                               bet_type == "Spread" ~ line_PointsBet)) %>% 
  mutate(BetMGM = case_when(bet_type == "Moneyline" ~ odds_BetMGM,
                               bet_type == "Spread" ~ line_BetMGM)) %>% 
  select(team_name, week, bet_type, 17:21, 16)  %>%
  arrange(week)

#——————————————————————————————————————————————————————————#

ui <- shinyUI(fluidPage(
  h1("4for4 NFL Betting Lines Tool"),
  h4("See consensus betting lines for the 2023-24 NFL season"),
  h5("Press 'delete' in team drop-down to reset table to show all teams"),
  fluidRow(
    sidebarLayout(
      sidebarPanel(                                     
        selectizeInput(
          'team', 'Team', choices = sort(tcl$team_nick),
          options = list(
            placeholder = 'Select a team',
            onInitialize = I('function() { this.setValue(""); }')
          )
        ),
        selectInput("type", "Choose a Bet Type", choices = c("Moneyline", "Spread"))
      ),
      mainPanel(
        gt_output(outputId = "table")
      )
    )
  )
))

server <- function(input, output) {
  
  selectedData <- reactive({
  
    # Default table when no team selected
    if (input$team == "") {
        moneylines_spreads %>%
          filter(bet_type == input$type) %>%
          select(team_name, week, Consensus) %>%
          arrange(week) %>%
          pivot_wider(names_from = week, values_from = Consensus) %>% 
          arrange(team_name) %>%
          left_join(tcl, by = c("team_name" = "team_nick")) %>% 
          select(team_wordmark, 2:19) %>%
          gt() %>%
          sub_missing(missing_text = "BYE") %>%
          cols_label(team_wordmark = "Week") %>%
          gt_img_rows(columns = team_wordmark, height = 25) %>%
          gt_theme_538() %>%
          data_color(columns = 2:19, 
                     colors = col_numeric(palette = c(
                       "#762a83", "#af8dc3", "#e7d4e8", "#f7f7f7",
                       "#d9f0d3", "#7fbf7b", "#1b7837"), 
                       reverse = TRUE, domain = case_when(input$type == "Moneyline" ~ c(-500,500),
                                                          input$type == "Spread" ~ c(-11,11))))
    
    } else {
        moneylines_spreads %>%
          filter(bet_type == input$type) %>%
          select(team_name, week, Consensus) %>%
          arrange(week) %>%
          pivot_wider(names_from = week, values_from = Consensus) %>% 
          left_join(tcl, by = c("team_name" = "team_nick")) %>% 
          filter(team_name == input$team) %>%
          arrange(team_name) %>%
          select(team_wordmark, 2:19) %>%
          gt() %>%
          sub_missing(missing_text = "BYE") %>%
          cols_label(team_wordmark = "Week") %>%
          gt_img_rows(columns = team_wordmark, height = 25) %>%
          gt_theme_538() %>%
          data_color(columns = 2:19, 
                     colors = col_numeric(palette = c(
                       "#762a83", "#af8dc3", "#e7d4e8", "#f7f7f7",
                       "#d9f0d3", "#7fbf7b", "#1b7837"), 
                       reverse = TRUE, domain = case_when(input$type == "Moneyline" ~ c(-500,500),
                                                          input$type == "Spread" ~ c(-11,11))))
    } 
    
  })
  
  output$table <- render_gt(expr = selectedData(), height = "100%", width = "100%")

}

shinyApp(ui, server)
