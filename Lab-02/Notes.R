pop = readxl:::read_excel("data/populationEstimates.xls", skip = 2) %>%
  select(pop2019 = POP_ESTIMATE_2019, State = State, fips = FIPStxt)

pop_ca2019 = pop %>%
  filter(State == "CA") %>%
  slice_max(pop2019, n = 1)

cases = covid %>%
  filter(state %in% c("California")) %>%
  group_by(county) %>%
  mutate(newCases = cases - lag(cases)) %>%
  ungroup() %>%
  filter(date == max(date))

most_cases = cases %>%
  slice_max(cases, n = 5) %>%
  select(county, cases)

knitr::kable(most_cases,
             caption = "Most Cases California Counties",
             col.names = c("County", "Cases"))

most_new_cases = cases %>%
  slice_max(newCases, n = 5) %>%
  select(county, newCases)

knitr::kable(most_new_cases,
             caption = "Most New Cases California Counties",
             col.names = c("County", "New Cases"))



pop_data1 = right_join(pop, cases, by = "fips") %>%
  mutate(cases_percapita = (cases / pop2019) * 100000,
         newCases_percapita = (newCases / pop2019) * 100000)

most_cases_percapita = pop_data1 %>%
  slice_max(cases_percapita, n = 5) %>%
  select(county, cases_percapita)

knitr::kable(most_cases_percapita,
             caption = "Most Cumulative Cases Per Capita",
             col.names = c("County", "Cases"))




##Notes from labs
newData = filter(state ('CA')) %>% group_by(county) %>% mutate(newCase = cases - lag(cases))




regions_cases_deaths = covid %>%
  right_join(region, by = 'state') %>%
  group_by(region, date) %>%
  summarize(cases = sum(cases), deaths = sum(deaths)) %>%
  ungroup() %>%
  pivot_longer(cols = c('cases', 'deaths')) #pivot the frame to long


p1 = ggplot(data=regions_cases_deaths, aes(x = date, y = value)) +
  geom_line(aes(col = region)) +
  facet_grid(name~region, scale = "free_y") +
  ggthemes::theme_few() +
  labs(title = "Cummulative Cases and Deaths: Region",
       subtitle = "Data Source: NY-Times",
       x = "Date",
       y = "Daily Cummulative Count",
       caption = "Daily Exercise 07: Abigail Porter")





ggplot(covid_region)

regions_cases_deaths %>%
  pivot_wider(names_from = "name", values_from = "value") %>%
  ggplot(aes(x=date)) +
  geom_line(aes(y = cases), col = "blue") +
  geom_line(aes(y = deaths), col = "red") +
  facet_grid(~region)

covid %>%
  filter(county == "Unknown", state == "Florida", date == max(date)) %>%
  arrange(-deaths)







homes = read_csv('data/landdata-states.csv')

pop = readxl::read_excel("data/PopulationEstimates.xls", skip = 2) %>% select(pop2019 = POP_ESTIMATE_2019, fips = FIPStxt)
