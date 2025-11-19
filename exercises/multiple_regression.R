df_plants <- read.csv("./data/alpineplants.csv")
# Temperatures are measured in degrees Celsius, light intensity as the % of sunlight that reaches through the vegetation, snow cover in cm, altitude in m, and soil moisture in %.

mean(df_plants$Carex.bigelowii) # [1] 1.359375
mean(df_plants$Thalictrum.alpinum) # [1] 1.223958

df_plants$Carex.bigelowii[df_plants$Carex.bigelowii == 0] <- NA
df_plants$Thalictrum.alpinum[df_plants$Thalictrum.alpinum == 0] <- NA

plot(
  df_plants$Carex.bigelowii ~ df_plants$light,
  las = "1",
  ylab = "",
  col = "red",
  # xlim = c(30,96)
)
points(
  df_plants$Thalictrum.alpinum ~ df_plants$light,
  ylab = "",
  col = "skyblue"
)

model_light <- lm()

plot(df_plants$Carex.bigelowii ~ df_plants$light)
