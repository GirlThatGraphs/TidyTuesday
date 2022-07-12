install.packages("tidytuesdayR")
tidytuesdayR::tt_load('2022-07-12')
flights <- tuesdata$flights

flights <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-07-12/flights.csv')

library(tidyverse)
library(ggrepel)
library(ggtext)
library(ggplot2)
library(viridis)

PNGWidth <- 10
PNGHeight <- 10
PNGDPI <- 300

FlightsData <-
  flights |> 
  select(`Year` = `YEAR`,
         `Country` = `STATE_NAME`,
         `TotalFlights` = `FLT_TOT_1`) |> 
  group_by(`Year`,
           `Country`) |> 
  summarise(`TotalFlights` = sum(`TotalFlights`)) |> 
  ungroup() |> 
  filter(`Year` >= 2019 &
           `Year` <= 2021) |> 
  group_by(Year)  |> 
  top_n(7,
        TotalFlights) 

TotalFlightsPlot <-
  ggplot(data = FlightsData,
         aes(x = `Year`,
             y = `TotalFlights`,
             colour = `Country`)) +
  #geom_area(alpha = 0.2) +
  geom_line(size = 0.8) +
  # scale_colour_manual("legend",
  #                     values = c("Somerset" = NHSDarkBlue)) +
  # scale_fill_manual("legend",
  #                   values = c("Somerset" = NHSDarkBlue)) +
  geom_text_repel(data = FlightsData |> 
                    filter(`Year` == 2021),
    aes(color = `Country`,
        label = `Country`),
    size = 4,
    direction = "y",
    xlim = c(2021.1, NA),
    hjust = 0,
    segment.size = .7,
    segment.alpha = .5,
    segment.linetype = "dotted",
    box.padding = .4,
    segment.curvature = -0.1,
    segment.ncp = 3,
    segment.angle = 20
  ) +
  scale_colour_manual("legend",
                      values = c("Spain" = "#49006a",
                                 "France" = "#7a0177",
                                 "Germany" = "#ae017e",
                                 "United Kingdom" = "#dd3497",
                                 "Türkiye" = "#f768a1",
                                 "Italy" = "#fa9fb5",
                                 "Norway" = "#fcc5c0"))  +
  ## coordinate system + scales
  coord_cartesian(
    clip = "off",
    xlim = c(2019,
             2021.4)
  ) +
  scale_x_continuous(breaks = c(2019,
                                2020,
                                2021)) +
  scale_y_continuous(limits = c(0,
                                2500000),
                     expand = c(0,
                                0),
                     labels = comma) +
  labs(title = paste0("Commercial Flights at top EU airports have not recovered from the C19 pandemic"),
       subtitle = paste0("Total IFR movements"),
       caption = paste0("Source: Eurocontrol | TidyTuesday 2022 W28 | @GirlThatGraphs")) +
  LinePlotTheme()

ggsave(file = paste0("TotalFlightsPlot.png"),
       plot = TotalFlightsPlot,
       width = PNGWidth,
       height = PNGHeight,
       dpi = PNGDPI)


FlightsData2 <-
  flights |> 
  select(`Year` = `YEAR`,
         `Country` = `STATE_NAME`,
         `TotalFlights` = `FLT_TOT_1`) |> 
  group_by(`Year`,
           `Country`) |> 
  summarise(`TotalFlights` = sum(`TotalFlights`)) |> 
  ungroup() |> 
  filter(`Year` >= 2019 &
           `Year` <= 2021) |> 
  group_by(Year)  |> 
  top_n(7,
        TotalFlights) |>
  ungroup() |> 
  pivot_wider(everything(),
              names_from = `Year`,
              values_from = `TotalFlights`) |> 
  mutate(`2019P` = 1,
         `2020P` = `2020`/`2019`,
         `2021P` = `2021`/`2019`) |> 
  select(`Country`,
         `2019` = `2019P`,
         `2020` = `2020P`,
         `2021` = `2021P`) |> 
  pivot_longer(!`Country`,
               names_to = "Year",
               values_to = "TotalFlightPercentage") |> 
  mutate(`Year` = as.numeric(`Year`))

TotalFlightsPlot2 <-
  ggplot(data = FlightsData2,
         aes(x = `Year`,
             y = `TotalFlightPercentage`,
             colour = `Country`)) +
  #geom_area(alpha = 0.2) +
  geom_line(size = 0.8) +
  # scale_colour_manual("legend",
  #                     values = c("Somerset" = NHSDarkBlue)) +
  # scale_fill_manual("legend",
  #                   values = c("Somerset" = NHSDarkBlue)) +
  geom_text_repel(data = FlightsData2 |> 
                    filter(`Year` == 2021),
                  aes(color = `Country`,
                      label = `Country`),
                  size = 4,
                  direction = "y",
                  xlim = c(2021.1, NA),
                  hjust = 0,
                  segment.size = .7,
                  segment.alpha = .5,
                  segment.linetype = "dotted",
                  box.padding = .4,
                  segment.curvature = -0.1,
                  segment.ncp = 3,
                  segment.angle = 20
  ) +
  scale_colour_manual("legend",
                      values = c("Spain" = "#49006a",
                                 "France" = "#7a0177",
                                 "Germany" = "#ae017e",
                                 "United Kingdom" = "#dd3497",
                                 "Türkiye" = "#f768a1",
                                 "Italy" = "#fa9fb5",
                                 "Norway" = "#fcc5c0"))  +
  ## coordinate system + scales
  coord_cartesian(
    clip = "off",
    xlim = c(2019,
             2021.4)
  ) +
  scale_x_continuous(breaks = c(2019,
                                2020,
                                2021)) +
  scale_y_continuous(limits = c(0,
                                1),
                     expand = c(0,
                                0),
                     labels = percent) +
  labs(title = paste0("The UK has the worst commercial flights recovery of the top EU airports"),
       subtitle = paste0("Percentage of 2019 total IFR movements"),
       caption = paste0("Source: Eurocontrol | TidyTuesday 2022 W28 | @GirlThatGraphs")) +
  LinePlotTheme()


ggsave(file = paste0("TotalFlightsPlot2.png"),
       plot = TotalFlightsPlot2,
       width = PNGWidth,
       height = PNGHeight,
       dpi = PNGDPI)
