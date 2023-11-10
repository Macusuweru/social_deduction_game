#! /bin/sh
library(tidyverse)
# FUNCTIONS
# Makes it so I don't have to type out a fiddly function every time I want to read or write to player_data
read_player_data <- function() return(read.csv("./gamedata/player_data.csv"))
write_player_data <- function(player_data) write.csv(player_data, "./gamedata/player_data.csv", row.names = FALSE)
is_id <- function(input) return(grepl("^[0-9]+$", input) && input %in% read_player_data()$id)
to_id <- function(input) {
	player_data <- read_player_data()
	if (input %in% player_data$id) {return(input)}
	else if (input %in% player_data$name) {return(player_data[player_data$name == input, "id"])}
	else return(-1) 
}
to_name <- function(id) {
	player_data <- read_player_data()
	return(player_data[player_data$id == id, "name"])
}

get_date <- function() return(Sys.time() |> as.Date() |> as.character())
# adds a player to player_data.csv
add_player <- function(name, game = NA) {
	if (file.exists("./gamedata/player_data.csv")) {
		player_data <- read_player_data()
		# Exits in case of duplicate name
		if (name %in% player_data$name) return("Error, duplicate name")
		new_id <- max(player_data$id) + 1
		
		# A new observation for the player, with default settings
		new_data <- c(new_id, name, FALSE, FALSE, get_date(), played = 0, won = 0)
		
		player_data |> rbind(new_data) |> write_player_data()
		
		return(new_id)
	} else {
		#mostly redundant system to create a new player_data file
		#in the case which one does not exist, which isn't really
		#something that is going to happen. Except it has. A lot.
		new_data <- data.frame(id = 0, name = name, queued = FALSE, playing = FALSE, date = get_date(), played = 0, won = 0)
		write.csv(new_data, "./gamedata/player_data.csv", row.names = FALSE)
		return(0)
	}
}

backup_file <- function(name, path = "records") {
	if (file.exists(paste("./gamedata/", name, sep = "")))
		read.csv(paste("./gamedata/", name, sep = "")) |> write.csv(paste(path, name, sep = "/"), row.names = FALSE)
	else print("Error, file does not exist to be backed up dumbass")
}


# Queue players for an upcoming game
queue <- function(input) {
	player_data <- read_player_data()
	id <- to_id(input)
	if (id > -1) {
		player_data[player_data$id == id, "queued"] <- TRUE
		write_player_data(player_data)
	} else print("Error, id or name not in system")
	print(to_id(input))
}


#				STARTING THE GAME
# ASSIGN ROLES, ASSIGN TEAMS

read_game_data <- function() {
	source("./settings.sh")
	return(read.csv(paste("./gamedata/game_data", current_game, ".csv", sep = "")))
}

write_game_data <- function(game_data) {
	source("./settings.sh")
	game_data |> write.csv(paste("./gamedata/game_data", current_game, ".csv", sep = ""), row.names = FALSE)
}
read_this_game_log <- function() {
	source("./settings.sh")
	placeholder <- read.csv("./game_log.csv")
	return(placeholder[placeholder$code == current_game,])
}

write_this_game_log <- function(log) {
	source("./settings.sh")
	game_logs <- read.csv("./game_log.csv") |> filter(code != current_game) |> rbind(log)
	write.csv(game_logs, "./game_log.csv", row.names = FALSE)
}

start_game <- function() {
	# All queued players are stored and set to playing
	player_data <- read_player_data()
	player_data[player_data$queued, "playing"] <- TRUE
	# increment tracker of games played
	player_data[player_data$queued, "played"] <- player_data[player_data$queued, "played"] + 1
	
	# get settings info
	source("./settings.sh")
	# create a new game log entry
	# code, teams, phase, active, winning team
	game_log <- read.csv("./game_log.csv")
	if (TRUE %in% game_log$active) return("Error, game in progress. End game before starting a new one.")
	if (current_game %in% game_log$code) return("Error, game code is taken. Manually edit the current_game variable in settings.sh")
	new_log <- c(code = current_game, teams = paste(good_team, evil_team, sep = "~"), phase = 0, active = TRUE, victor = NA)
	game_log |> rbind(new_log) |> write.csv("./game_log.csv", row.names = FALSE)

	# create a dedicated game_data file
	game_data <- player_data[player_data$queued,] |> 
		select(id, name) |> 
		mutate(alive = TRUE, total_votes = 0)
		
	# assign roles in that file
	roles <- rep(default_good_role, sum(player_data$queued) - evil_n) |> c(rep(default_evil_role, evil_n)) |> sample()
	
	game_data <- game_data |>
		mutate(roles = roles, team = ifelse(roles == default_good_role, good_team, evil_team))
	
		
	game_data |> write.csv(paste("./gamedata/game_data", current_game, ".csv", sep = ""), row.names = FALSE)
	# change all players marked as queued to being marked as alive 
	# & increment games played
	player_data$queued <- FALSE
	write_player_data(player_data)
	
	
}

vote <- function(voter, voted, redo = FALSE) {
	voter_id <- to_id(voter)
	voted_id <- to_id(voted)
	source("./settings.sh")
	vote_data_path <- paste("./gamedata/vote_data", current_game, ".csv", sep = "")
	game_log <- read_this_game_log()
	game_data <- read_game_data()
	# disable if there isn't a game
	if (!game_log$active) return("Error, no ongoing game to vote in dumbass")
	if (!(voter_id %in% game_data$id)) return("Error, invalid voter")
	if (!(voted_id %in% game_data$id)) return("Error, invalid vote target")
	if (!game_data[game_data$id == voter_id, "alive"]) return("Ghosts can't vote")
	if (!game_data[game_data$id == voted_id, "alive"]) return("Can't vote ghosts")
	
	
	# if there isn't an associated file, create one.
	if (file.exists(vote_data_path)) {
		vote_data <- read.csv(vote_data_path)
		if (voter_id %in% vote_data$voters) {
			if (!redo) return("This person has already voted. Specify redo = TRUE in order to change the vote.")
			else vote_data <- vote_data |> filter(voters != voter_id)
		}
		new_data <- c(voters = voter_id, voted = voted_id, date = get_date(), phase = game_log$phase)
		vote_data |> rbind(new_data) |> write.csv(vote_data_path, row.names = FALSE)
	} else {
		vote_data <- data.frame(voters = voter_id, voted = voted_id, date = get_date(), phase = game_log$phase)
		vote_data |> write.csv(vote_data_path, row.names = FALSE)
	}
}

action <- function(actor, target, redo = FALSE) {
	actor_id <- to_id(actor)
	target_id <- to_id(target)
	source("./settings.sh")
	action_data_path <- paste("./gamedata/action_data", current_game, ".csv", sep = "")
	game_log <- read_this_game_log()
	game_data <- read_game_data()
	# disable if there isn't a game
	if (!game_log$active) return("Error, no ongoing game to act in dumbass")
	if (!(actor_id %in% game_data$id)) return("Error, invalid actor")
	if (!(target_id %in% game_data$id)) return("Error, invalid target")
	if (!game_data[game_data$id == actor_id, "alive"]) return("Ghosts can't act")
	

	# if there isn't an associated file, create one.
	if (file.exists(action_data_path)) {
		action_data <- read.csv(action_data_path)
		if (actor_id %in% action_data$actors) {
			if (!redo) return("This person has already used their action. Specify redo = TRUE in order to change the action.")
			else action_data <- action_data |> filter(actors != actor_id)
		}
		new_data <- c(actors = actor_id, target = target_id, role = game_data[game_data$id == actor_id, "role"], date = get_date(), phase = game_log$phase)
		action_data |> rbind(new_data) |> write.csv(action_data_path, row.names = FALSE)
	} else {
		action_data <- data.frame(actors = to_id(actor), target = to_id(target), role = game_data[game_data$id == actor_id, "roles"], date = get_date(), phase = game_log$phase)
		write.csv(action_data, action_data_path, row.names = FALSE)
	}
	# the evaluation occurs later
}

remove_player <- function(input) {
	this_id <- to_id(input)
	game_data <- read_game_data()
	player_data <- read_player_data()
	if (id %in% game_data$id) {
		game_data <- game_data |> filter(id != this_id)
		player_data[player_data$id == this_id, "playing"] <- FALSE
		write_game_data(game_data)
		write_player_data(player_data)
	} else return("Error, that player is not registered as part of the current game")
}

end_phase <- function() {
	# backup all files
	source("./settings.sh")
	game_backup_path <- paste("./records", current_game, sep = "/")
	phase_backup_path <- paste(game_backup_path, get_date(), sep = "/")
	if (!file.exists(game_backup_path)) dir.create(game_backup_path)
	if (!file.exists(phase_backup_path)) dir.create(phase_backup_path)
	backup_file(paste("vote_data", current_game, ".csv", sep = ""), phase_backup_path)
	backup_file(paste("action_data", current_game, ".csv", sep = ""), phase_backup_path)
	backup_file(paste("game_data", current_game, ".csv", sep = ""), phase_backup_path)
	# evaluate vote results, action results, check for victory
	vote_data <- read.csv(paste("./gamedata/vote_data", current_game, ".csv", sep = ""))
	action_data <- read.csv(paste("./gamedata/action_data", current_game, ".csv", sep = ""))
	game_data <- read_game_data()
	game_log <- read_this_game_log()
	
	# evaluate the vote first
	votes <- vote_data |>
		filter(phase == game_log$phase)
	if (count(votes) == 0) {
		print("No one voted. Disappointing!")
	} else {
		top_voted <- votes |>
			group_by(voted) |>
			summarize(n = n()) |>
			arrange(desc(n)) |>
			filter(n == max(n))
		top_voted_names <- sapply(top_voted$voted, to_name)
		if (count(top_voted) > 1) print(paste("No one was voted out due to a tie in votes between ", paste(top_voted_names, collapse = " and "), ".", sep = "") |> paste(". Each received", max(top_voted$n), "votes.")) 
		else print(paste(top_voted_names, "was voted out, receiving", max(top_voted$n), "votes"))
		game_data[game_data$id == head(top_voted, n = 1)$voted, "alive"] <- FALSE
	}	
	# also check for missed votes
	missed_voters <- game_data |> 
		filter(!(game_data$id %in% votes$voters)) |>
		filter(alive) |>
		pull(name)
		
	if (length(missed_voters) > 0) missed_voters |>
		paste(collapse = ", ") |>
		paste("did not vote this phase.") |>
		print()
	
	# evaluate actions, starting with wolf kill
	night_kill <- action_data |> 
		filter(role == "wolf" && phase == game_log$phase) |>
		group_by(target) |>
		summarize(n = n()) |>
		sample() |>
		arrange(desc(n)) |>
		head(n = 1) |>
		pull(target) 
	if (length(night_kill) > 0) {
		night_kill |> 
			to_name() |>
			paste("died in the night.") |>
			print()
		game_data[game_data$id == night_kill, "alive"] <- FALSE
	} else print("Nobody died in the night")
	
	
	# check for wolf/town victory
	alive_players <- game_data |>
		group_by(team) |>
		summarise(n = sum(alive))
	
	if (alive_players[alive_players$team == evil_team, "n"] == 0) print("GAME OVER. All members of the evil team have been eliminated")
	else if (alive_players[alive_players$team == evil_team, "n"] >= alive_players[alive_players$team == good_team, "n"]) print("GAME OVER. The evil team composes at least half of all remaining players.")
	# increment the phase counter
	game_log$phase <- game_log$phase |> as.numeric() + 1
	
	#save all changes
	write_game_data(game_data)
	write_this_game_log(game_log)
	
	night_kill <- c()
	missed_voters  <- c()
	votes <- c()
	top_voted <- c()
	top_voted_names <- c()
	
}
end_game <- function() {}
	# this is going to both end the game and perform a number of regulatory functions.
	# backup files, make sure that all files are formatted properly, etc.
	
	#player_data table, game_log, backup files, evaluate settings.
