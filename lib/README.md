## Lib

Various scripts used to make running `doc` a little cleaner.

I will be describing files in the order that they are utilized

`get_client_secret.R` is required to access Twitch's video game database API. **This file must be modified if attempting to reproduce from scratch**
`twitch_game_and_channel.R` is tasked with processing the `csvs` from `/data`.
`manual_game_additions.R` is needed for a few manual overrides if a race in the upcoming year will feature games neither streamer has played (meaning it has no records in `/data`)
`get_game_metadata.R` obtains all of the relevant metadata for each game from the vgdb API and places it in `output/MOMAM.sqlite`.