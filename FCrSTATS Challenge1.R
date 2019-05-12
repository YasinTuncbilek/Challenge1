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

## Add additional relevant types: passes, shots & pressures   
events <- events %>% 
  mutate(type.name = ifelse(type.name == "Pass" & pass.length >= quantile(events$pass.length, probs = c(0.95), na.rm = T), "Long.Pass", type.name)) %>% # Add long pass
  mutate(type.name = ifelse(type.name == "Pass" & pass.end_location.x >= 102 & pass.end_location.y > 18 & pass.end_location.y < 62, "Pass.into.Box", type.name)) %>% # Add pass into box
  mutate(type.name = ifelse(type.name == "Pass" & pass.end_location.x > 80, "Pass.into.Finalthird", type.name)) %>% # Add pass into final third
  mutate(type.name = ifelse(type.name == "Pass" & location.x > 80 & pass.end_location.x > 80 & pass.end_location.x <= 120, "Pass.in.Finalthird", type.name)) %>% # Add pass in final third 
  mutate(type.name = ifelse(type.name == "Shot" & location.x >= 102 & location.y > 18 & location.y < 62, "Shot.in.box", type.name)) %>% # Add shot in box
  mutate(type.name = ifelse(type.name == "Shot", "Shot.outside.box", type.name)) %>% # Add shot outside box
  mutate(type.name = ifelse(type.name == "Pressure" & location.x <= 40, "Low.Pressure", type.name)) %>% # Add low pressure
  mutate(type.name = ifelse(type.name == "Pressure" & location.x > 40 & location.x <= 80, "Middle.Pressure", type.name)) %>% # Add middle pressure
  mutate(type.name = ifelse(type.name == "Pressure" & location.x > 80, "High.Pressure", type.name)) # Add high pressure 

# Player summaries of type name
summaries.type.name <- events %>% 
  filter(position.name %in% c("Center Forward", "Right Center Forward", "Right Wing", "Center Attacking Midfield", "Left Center Forward", "Left Wing", "Secondary Striker")) %>% # Select the events of the positions Nikita Parris has played, including two additional attacking positions: Left Wing & Secondary Striker
  group_by(player.name, type.name) %>%
  summarise(Total = (n() / max(total.minutes.played) * 90), 
            total.minutes.played = max(total.minutes.played)) %>% 
  group_by(player.name) %>% 
  spread(type.name, Total)

# Player summaries of shot outcome
summaries.shot.outcome <- events %>%
  filter(position.name %in% c("Center Forward", "Right Center Forward", "Right Wing", "Center Attacking Midfield", "Left Center Forward", "Left Wing", "Secondary Striker")) %>% # Select the events of the positions Nikita Parris has played, including two additional attacking positions: Left Wing & Secondary Striker
  group_by(player.name, shot.outcome.name) %>%
  summarise(Total = (n() / max(total.minutes.played) * 90), 
            total.minutes.played = max(total.minutes.played)) %>% 
  group_by(player.name) %>%
  spread(shot.outcome.name, Total)

# Player summaries of passes
summaries.passes <- events %>%
  filter(position.name %in% c("Center Forward", "Right Center Forward", "Right Wing", "Center Attacking Midfield", "Left Center Forward", "Left Wing", "Secondary Striker")) %>% # Select the events of the positions Nikita Parris has played, including two additional attacking positions: Left Wing & Secondary Striker
  group_by(player.name) %>%
  summarise_at(c("pass.switch", "pass.aerial_won", "pass.cross", "pass.shot_assist", "pass.miscommunication", "pass.through_ball",
                             "pass.cut_back", "pass.backheel", "pass.deflected", "pass.goal_assist"), .funs = sum, na.rm = TRUE)  

summaries.passes <- merge(summaries.passes, minutes.played.per.player, by = "player.name") # Add minutes played per player

summaries.passes <- summaries.passes %>% # Calculate per 90 min.
  mutate(pass.switch = (pass.switch / total.minutes.played) * 90, pass.aerial_won = (pass.aerial_won / total.minutes.played) * 90,
         pass.cross = (pass.cross / total.minutes.played) * 90, pass.shot_assist = (pass.shot_assist / total.minutes.played) * 90,
         pass.miscommunication = (pass.miscommunication / total.minutes.played) * 90, pass.through_ball = (pass.through_ball / total.minutes.played) * 90,
         pass.cut_back = (pass.cut_back / total.minutes.played) * 90, pass.backheel = (pass.backheel / total.minutes.played) * 90,
         pass.deflected = (pass.deflected / total.minutes.played) * 90, pass.goal_assist = (pass.goal_assist / total.minutes.played) * 90)
  
# Player summaries of dribbles
summaries.dribbles <- events %>%
  filter(position.name %in% c("Center Forward", "Right Center Forward", "Right Wing", "Center Attacking Midfield", "Left Center Forward", "Left Wing", "Secondary Striker")) %>% # Select the events of the positions Nikita Parris has played, including two additional attacking positions: Left Wing & Secondary Striker
  group_by(player.name) %>%
  summarise_at(c("dribble.overrun", "dribble.nutmeg"), .funs = sum, na.rm = TRUE)  
  
summaries.dribbles <- merge(summaries.dribbles, minutes.played.per.player, by = "player.name") # Add minutes player per player

summaries.dribbles <- summaries.dribbles %>% # Calculate per 90 min.
  mutate(dribble.overrun = (dribble.overrun / total.minutes.played) * 90, dribble.nutmeg = (dribble.nutmeg / total.minutes.played) * 90)

# Calculate xG & xG ecluding penalties per 90 min.
xG <- events %>%
  filter(position.name %in% c("Center Forward", "Right Center Forward", "Right Wing", "Center Attacking Midfield", "Left Center Forward", "Left Wing", "Secondary Striker")) %>% # Select the events of the positions Nikita Parris has played, including two additional attacking positions: Left Wing & Secondary Striker
  group_by(player.name) %>%
  summarise(xG = sum(shot.statsbomb_xg, na.rm = TRUE))

xG.without.penalties <- events %>%
  filter(position.name %in% c("Center Forward", "Right Center Forward", "Right Wing", "Center Attacking Midfield", "Left Center Forward", "Left Wing", "Secondary Striker")) %>% # Select the events of the positions Nikita Parris has played, including two additional attacking positions: Left Wing & Secondary Striker
  filter(shot.type.name != "Penalty") %>%
  group_by(player.name) %>%
  summarise(xG.without.penalties = sum(shot.statsbomb_xg, na.rm = TRUE))

xG <- merge(xG, xG.without.penalties, by = "player.name", all = TRUE) # Merge both xG calculations
xG <- merge(xG, minutes.played.per.player, by = "player.name") # Add minutes player per player

xG <- xG %>% # Calculate per 90 min.
  mutate(xG = (xG / total.minutes.played) * 90, xG.without.penalties = (xG.without.penalties / total.minutes.played) * 90)

# Player summaries of shots
summaries.shots <- events %>%
  filter(position.name %in% c("Center Forward", "Right Center Forward", "Right Wing", "Center Attacking Midfield", "Left Center Forward", "Left Wing", "Secondary Striker")) %>% # Select the events of the positions Nikita Parris has played, including two additional attacking positions: Left Wing & Secondary Striker
  group_by(player.name) %>%
  summarise_at(c("shot.follows_dribble", "shot.open_goal", "shot.first_time", "shot.aerial_won", "shot.deflected", "shot.one_on_one"), .funs = sum, na.rm = TRUE)

summaries.shots <- merge(summaries.shots, minutes.played.per.player, by = "player.name") # Add minutes player per player

summaries.shots <- summaries.shots %>% # Caculate per 90 min.
  mutate(shot.follows_dribble = (shot.follows_dribble / total.minutes.played) * 90, shot.open_goal = (shot.open_goal / total.minutes.played) * 90, shot.first_time = (shot.first_time / total.minutes.played) * 90,
         shot.aerial_won = (shot.aerial_won / total.minutes.played) * 90, shot.deflected = (shot.deflected / total.minutes.played) * 90, shot.one_on_one = (shot.one_on_one / total.minutes.played) * 90)

# Player summaries of shot body part name
summaries.shot.body_part.name <- events %>%
  filter(position.name %in% c("Center Forward", "Right Center Forward", "Right Wing", "Center Attacking Midfield", "Left Center Forward", "Left Wing", "Secondary Striker")) %>% # Select the events of the positions Nikita Parris has played, including two additional attacking positions: Left Wing & Secondary Striker
  group_by(player.name, shot.body_part.name) %>%
  summarise(Total = (n() / max(total.minutes.played) * 90), 
            total.minutes.played = max(total.minutes.played)) %>% 
  group_by(player.name) %>%
  spread(shot.body_part.name, Total)

# Calculate shots per 90 min.
shots <- events %>%
  filter(position.name %in% c("Center Forward", "Right Center Forward", "Right Wing", "Center Attacking Midfield", "Left Center Forward", "Left Wing", "Secondary Striker")) %>% # Select the events of the positions Nikita Parris has played, including two additional attacking positions: Left Wing & Secondary Striker
  group_by(player.name, shot.body_part.name) %>%
  filter(shot.body_part.name != "NA") %>%
  group_by(player.name) %>%
  summarise(Shots = n()) 

summaries.shots <- merge(shots, summaries.shots, by = "player.name", all = TRUE) # Merge shots data frames

summaries.shots <- summaries.shots %>% # Calculate per 90 min.
  mutate(Shots = (Shots / total.minutes.played) * 90)

# Calculate xA per 90 min.
events$pass.goal_assist <- ifelse(is.na(events$pass.goal_assist), 0, 1)

xA <- list()
for (a in 1:nrow(events)) {
  print(a)
  if(events[a,]$pass.goal_assist == 1){
    xA <- append(unlist(xA), 
                 as.numeric(events[which(events$id == events[a,]$pass.assisted_shot_id),]$shot.statsbomb_xg)
    )}else{
      xA <- append(unlist(xA), 0)
    }
}

events$xA <- xA

xA <- events %>% # Sum all xA
  filter(position.name %in% c("Center Forward", "Right Center Forward", "Right Wing", "Center Attacking Midfield", "Left Center Forward", "Left Wing", "Secondary Striker")) %>% # Select the events of the positions Nikita Parris has played, including two additional attacking positions: Left Wing & Secondary Striker
  group_by(player.name) %>%
  summarise(xA = sum(xA))

xA <- merge(xA, minutes.played.per.player, by = "player.name") # Add minutes played per played

xA <- xA %>% # Calculate per 90 min.
  mutate(xA = (xA / total.minutes.played) * 90) 

# Player summaries of pass height name
summaries.pass.height.name <- events %>%
  filter(position.name %in% c("Center Forward", "Right Center Forward", "Right Wing", "Center Attacking Midfield", "Left Center Forward", "Left Wing", "Secondary Striker")) %>% # Select the events of the positions Nikita Parris has played, including two additional attacking positions: Left Wing & Secondary Striker
  group_by(player.name, pass.height.name) %>%
  summarise(Total = (n() / max(total.minutes.played) * 90), 
            total.minutes.played = max(total.minutes.played)) %>% 
  group_by(player.name) %>%
  spread(pass.height.name, Total)

# Calculate dribbles per 90 min.
dribbles <- events %>%
  filter(position.name %in% c("Center Forward", "Right Center Forward", "Right Wing", "Center Attacking Midfield", "Left Center Forward", "Left Wing", "Secondary Striker")) %>% # Select the events of the positions Nikita Parris has played, including two additional attacking positions: Left Wing & Secondary Striker
  group_by(player.name, dribble.outcome.name) %>%
  filter(dribble.outcome.name == "Complete") %>%
  group_by(player.name) %>%
  summarise(Dribbles = n()) 

summaries.dribbles <- merge(dribbles, summaries.dribbles, by = "player.name", all = TRUE) # Merge dribbles data frames

summaries.dribbles <- summaries.dribbles %>% # Calculate per 90 min.
  mutate(Dribbles = (Dribbles / total.minutes.played) * 90)

# Delete irrelevant columns
summaries.dribbles <- summaries.dribbles[, -5]
summaries.pass.height.name <- summaries.pass.height.name[, -6]
summaries.passes <- summaries.passes[, -12]
summaries.shot.body_part.name <- summaries.shot.body_part.name[, -c(2, 6)]
summaries.shot.outcome <- summaries.shot.outcome[, c(1,4)]
summaries.shots <- summaries.shots[, -9]
summaries.type.name <- summaries.type.name[, -2]
xA <- xA[, -3]
xG <- xG[, -4]

# Merge summaries of passes
summaries.passes <- merge(summaries.pass.height.name, summaries.passes, by = "player.name")

# Merge summaries of shots
summaries.shots <- merge(summaries.shots, summaries.shot.body_part.name, by = "player.name")
summaries.shots <- merge(summaries.shots, summaries.shot.outcome, by = "player.name")

# Merge all summaries
player.summaries <- merge(summaries.type.name, summaries.dribbles, by = "player.name")
player.summaries <- merge(player.summaries, summaries.passes, by = "player.name")
player.summaries <- merge(player.summaries, summaries.shots, by = "player.name")
player.summaries <- merge(player.summaries, xA, by = "player.name")
player.summaries <- merge(player.summaries, xG, by = "player.name")

# Reorder columns
player.summaries <- player.summaries[, c(1, 36, 2:35, 37:63)]

# Replace NAs with zero
player.summaries[is.na(player.summaries)] <- 0

# See all variables  
allVars <- colnames(player.summaries)

### PCA: prcomp & princomp ###
# Select data for PCA 
data.pca <- player.summaries[, c(1, 2, 5:8, 12, 14:16, 18:21, 23, 25:27, 30:32, 34:43, 45:47, 49:54, 56:63)]

# Use prcomp function
results <- prcomp(data.pca[, 3:48], scale = TRUE)
results

# Get eigenvalues of principa components (> 1)
eig <- (results$sdev)^2

# Get variances of principal components
variance <- eig*100/sum(eig)

# Get cumulative variances
cumvar <- cumsum(variance)

# Combine to dataframe
eig.var.cum <- data.frame(eig = eig, variance = variance, cumvariance = cumvar)
eig.var.cum

# Use princomp to compare results with prcomp
princomp(data.pca[, 3:48], cor = TRUE)
summary(princomp(data.pca[, 3:48], cor = TRUE))

### PCA: principal ###
# Load psych package
library(psych)

# Scale data
x <- scale(data.pca[, 3:48])

# PCA: Varimax Rotated Principal Components retaining 13 components
fit <- principal(x, nfactors = 13, rotate = "varimax", method = "regression")

# Print factor loadings with cutoff .6
print(fit$loadings, cutoff = .6, sort = TRUE)

### Cluster analysis ###
# Select data for cluster analysis: use variables that loaded on the factors
data.ca <- data.pca[, c(1, 2, 20, 33, 36, 39, 41, 43, 44, 45, 47, 48, 8, 11, 14, 21, 12, 26, 28, 34,
                        18, 22, 25, 31, 5, 6, 9, 35, 46, 30, 32, 40, 42, 24, 37, 19, 15)]

# Scale data
data.ca1 <- scale(data.ca[, 3:37])

# Perform cluster analysis with hclust
d_data <- dist(data.ca1, method = "euclidean")
hc_data <- hclust(d_data, method = "ward.D2") 
plot(hc_data, labels = data.ca$player.name, hang = -1, cex = 1)

# Save as dendrogram
dend <- hc_data %>% as.dendrogram 

# Use player names as labels
labels(dend) <- data.ca$player.name[hc_data$order]

# Install and load dendextend 
install.packages("dendextend")
library(dendextend)

# Install and load circlize package
install.packages("circlize")
library(circlize)

# Install and load dynamicTreeCut package
install.packages("dynamicTreeCut")
library(dynamicTreeCut)

# Use cutreeDynamic to determine numbers of clusters
clusters <- cutreeDynamic(hc_data, cutHeight = NULL, minClusterSize = 3, method = "hybrid", distM = as.matrix(d_data))
clusters

# Provide colors to the branches and labels
dend <- dend %>% 
  color_branches(k = 3) %>%
  color_labels(col = "black") 

# Create final plot  
par(cex = 0.8, mar = c(5, 8, 4, 1))
circlize_dendrogram(dend, labels = TRUE)
title(main = "Playing style partners of players in WSL season 2018/2019")

# Check stats of players in same cluster
shortlist <- player.summaries %>%
  select("player.name", "total.minutes.played", "Goal", "pass.goal_assist", "xG", "xA") %>%
  filter(player.name %in% c("Lauren Kemp", "Francesca Kirby", "Nikita Parris", "Vivianne Miedema",
                            "Adriana Leon", "Ellen White", "Bethany England", "Millie Farrow"))
