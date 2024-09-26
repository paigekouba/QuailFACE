# Process raw data for TinyFACE Methods Paper

# load the data: testing period from 4/9 to 4/19/24
data_4.19.24 <- read_csv("/Users/paigekouba/Documents/UC_Davis/2021_Winter/Quals/Proposal/Chapter 1/TinyFACE/CO2_control/CR3000_Data20s_4.19.24.csv", skip = 4, col_names = c("TIMESTAMP",	"RECORD",	"BattV_Avg",	"CO2signal",	"CO2ref",	"CO2elev",	"CO2test",	"DeltaObs",	"DeltaSet",	"LicorSig2",	"FlowMFC",	"AirTC",	"RH",	"WS_ms",	"PARuE",	"TurnCO2On",	"SpanObs",	"SpanAct",	"ZeroObs",	"ZeroAct"))

attr(data_4.19.24$TIMESTAMP, "tzone") # starts out as UTC, though clock time is correct
# force_tz keeps the clock time but reassigns the tz to be accurate
data_4.19.24$TIMESTAMP <- force_tz(data_4.19.24$TIMESTAMP, "America/Los_Angeles")

# examine CO2 performance after 4.9.24
write.csv(data_4.19.24 %>% 
            filter(TIMESTAMP > "2024-04-09 08:10:00") %>%
            mutate(DeltaTest = CO2test - CO2ref) %>%
            distinct(),
          file = "TinyFACE_performance.csv")
