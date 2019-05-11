# Load relevant packages 
library(StatsBombR) 
library(tidyr) 
library(dplyr)
library(ggplot2)

# Create dataframe with events
events <- StatsBombFreeEvents() # Download the events data from Statsbomb 

saveRDS(events, "SB_events_WSL.RDS") # Save dataframe as a file

events <- cleanlocations(events) # Split the location data for relevant events 
events <- events %>% filter(competition_id == 37) # Select only the WSL 2018/19 season data
events <- events %>% filter(position.name %in% c("Center Forward", "Right Center Forward", "Right Wing", "Center Attacking Midfield", "Left Center Forward", "Left Wing", "Secondary Striker")) # Select the events of the positions Nikita Parris has played, including two additional attacking positions: Left Wing & Secondary Striker

# Function for minutes played
get.minutesplayed <- function(events){
  
  MatchIDs <- unique(events$match_id)
  
  get.minutesplayed.single.match <- function(MatchSelect){
    
    per.match <- events %>% filter(match_id == MatchSelect)
    match.length <- max(per.match$minute)
    line.up1 <- per.match[1,]$tactics.lineup[[1]]
    line.up1$team.name <- per.match[1,]$team.name
    
    line.up2 <- per.match[2,]$tactics.lineup[[1]]
    line.up2$team.name <- per.match[2,]$team.name
    
    line.ups <- bind_rows(line.up1, line.up2)
    line.ups <- line.ups %>% select(player.id, player.name, team.name)
    
    line.ups$time.start <- 0
    
    substitutions <- per.match %>% filter(type.name == "Substitution") %>% select(minute, team.name, player.name, player.id, substitution.replacement.name)
    substitutions.out <- substitutions %>% select(-substitution.replacement.name, -team.name, -player.id)
    
    line.ups <- merge(line.ups, substitutions.out, by = "player.name", all = T)
    
    substitutions.in <- data.frame(player.name = substitutions$substitution.replacement.name, team.name = substitutions$team.name, time.start = substitutions$minute, minute = match.length, stringsAsFactors = F)
    
    playerIDS <- per.match %>% group_by(player.name) %>% select(player.id)
    playerIDS <- unique(playerIDS)  
    playerIDS <- playerIDS %>% filter(player.name %in% substitutions.in$player.name)
    
    substitutions.in <- merge(substitutions.in, playerIDS, by = "player.name")
    
    line.ups <- bind_rows(line.ups, substitutions.in)
    line.ups[is.na(line.ups)] <- match.length
    line.ups$Minutes.Played <- line.ups$minute - line.ups$time.start
    
    line.ups$match_id <- MatchSelect
    colnames(line.ups) <- c("player.name", "player.id", "team.name", "time.in", "time.out", "minutes.played", "match_id")
    
    return(line.ups)
    
  } 
  
  MinsPlayedCatch <- MatchIDs %>% 
    split(1:length(.)) %>% 
    purrr::map(get.minutesplayed.single.match) %>% 
    dplyr::bind_rows()
  
  return(MinsPlayedCatch)
  
}

# Get minutes played data 
minutes.played <- get.minutesplayed(events)

# Sumarise as total minutes played 
minutes.played.per.player <- 
  minutes.played %>% 
  group_by(player.name) %>% 
  summarise(total.minutes.played = sum(minutes.played))

# Add total minutes to events dataframe 
events <- merge(events, minutes.played.per.player, by = "player.name")

## Add pass in the box  
events <- events %>% 
  mutate(type.name = ifelse(type.name == "Pass" & pass.length >= quantile(events$pass.length, probs = c(0.95), na.rm = T), "Long.Pass", type.name)) %>%
  mutate(type.name = ifelse(type.name == "Pass" & pass.end_location.x >= 102 & pass.end_location.y > 18 & pass.end_location.y < 62, "Pass.into.Box", type.name)) %>% 
  mutate(type.name = ifelse(type.name == "Shot" & location.x >= 102 & location.y > 18 & location.y < 62, "Shot.in.box", type.name)) %>% 
  mutate(type.name = ifelse(type.name == "Shot", "Shot.outside.box", type.name)) %>% 
  mutate(type.name = ifelse(type.name == ))
  mutate(type.name = ifelse(type.name == "Pressure" & location.x >= 80, "High.Pressure", type.name)) %>%
  mutate(type.name = ifelse(type.name == "Pressure" & location.x >= 40 & location.x < 80, "Middle.Pressure", type.name))


# All in one 
player.summaries <- events %>% 
  filter(total.minutes.played > 270 & position.name != "Goalkeeper" & competition_id == 37) %>% 
  group_by(player.name, type.name) %>%
  summarise(Total = (n() / max(total.minutes.played) * 90), 
            total.minutes.played = max(total.minutes.played)) %>% 
  group_by(player.name) %>% 
  spread(type.name, Total)

# Replace NAs with zero
player.summaries[is.na(player.summaries)] <- 0

# See all variables  
allVars <- colnames(player.summaries)

events %>%
  group_by(player.name, shot.outcome.name) %>%
  summarise(Total = (n() / max(total.minutes.played) * 90), 
                     total.minutes.played = max(total.minutes.played)) %>% 
  group_by(player.name) %>%
  spread(shot.outcome.name, Total)

# Load psych package
library(psych)

x <- scale(player.summaries[, 3:32])


# Run PCA
# Varimax Rotated Principal Components retaining 6 components
fit <- principal(x, nfactors = 5, rotate = "varimax", method = "regression")

# Print factor loadings with cutoff .6
print(fit$loadings, cutoff = .6, sort = TRUE)

