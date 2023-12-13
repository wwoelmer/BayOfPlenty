remotes::install_github("FLARE-forecast/RopenMeteo")

df <- RopenMeteo::get_forecast(latitude = -38.1387,
                               longitude = 176.2452,
                               forecast_days = 7, 
                               past_days = 2, 
                               model = "generic",
                               variables = c("temperature_2m"))
head(df)

df |> 
  mutate(variable = paste(variable, unit)) |> 
  ggplot(aes(x = datetime, y = prediction)) + 
  geom_line(color = "#F8766D") + 
  geom_vline(aes(xintercept = reference_datetime)) + 
  facet_wrap(~variable, scale = "free")

df <- RopenMeteo::get_ensemble_forecast(
  latitude = -38.1387,
  longitude = 176.2452,
  forecast_days = 7,
  past_days = 2,
  model = "gfs_seamless",
  variables = c("temperature_2m"))

head(df)

df |> 
  mutate(variable = paste(variable, unit)) |> 
  ggplot(aes(x = datetime, y = prediction, color = ensemble)) + 
  geom_line() + 
  geom_vline(aes(xintercept = reference_datetime)) + 
  facet_wrap(~variable, scale = "free", ncol = 2)

df <- RopenMeteo::get_ensemble_forecast(
  latitude = -38.1387,
  longitude = 176.2452,
  forecast_days = 7,
  past_days = 2,
  model = "gfs_seamless",
  variables = RopenMeteo::glm_variables(product = "ensemble_forecast", 
                                        time_step = "hourly"))
df |> 
  mutate(variable = paste(variable, unit)) |> 
  ggplot(aes(x = datetime, y = prediction, color = ensemble)) + 
  geom_line() + 
  geom_vline(aes(xintercept = reference_datetime)) + 
  facet_wrap(~variable, scale = "free", ncol = 2)

path <- tempdir()
df |> 
  RopenMeteo::add_longwave() |>
  RopenMeteo::write_glm_format(path = path)
head(read.csv(list.files(path = path, full.names = TRUE, pattern = ".csv")[1]))

df <- RopenMeteo::get_seasonal_forecast(
  latitude = -38.1387,
  longitude = 176.2452,
  forecast_days = 274,
  past_days = 5,
  variables = c("temperature_2m"))
head(df)

df |> 
  mutate(variable = paste(variable, unit)) |> 
  ggplot(aes(x = datetime, y = prediction, color = ensemble)) + 
  geom_line() + 
  geom_vline(aes(xintercept = reference_datetime)) +
  facet_wrap(~variable, scale = "free")

models <- c("CMCC_CM2_VHR4","FGOALS_f3_H","HiRAM_SIT_HR","MRI_AGCM3_2_S","EC_Earth3P_HR","MPI_ESM1_2_XR","NICAM16_8S")

df <- purrr::map_df(models, function(model){
  RopenMeteo::get_climate_projections(
    latitude = -38.1387,
    longitude = 176.2452,
    start_date = Sys.Date(),
    end_date = Sys.Date() + lubridate::years(1),
    model = model,
    variables = c("temperature_2m_mean"))
})

df |> 
  mutate(variable = paste(variable, unit)) |> 
  ggplot(aes(x = datetime, y = prediction, color = model_id)) + 
  geom_line() +
  facet_wrap(~variable, scale = "free")
