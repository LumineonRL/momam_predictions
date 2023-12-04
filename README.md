## Overview

**MOMAM** or **M**onth **o**f **M**ushrooms **a**nd **M**ayhem is an annual event hosted by two streamers on Amazon's popular video game streaming platform, twitch.tv. During the month of March each year, iateyourpie and SpikeVegeta will compete in different head-to-head video game challenges and races every day. Viewers will also have the opportunity to submit a list of their predictions for who will win each individual challenge, with the winner often receiving a prize. Competition tends to be fierce, as upwards of 4000 users will submit their predictions each year, making it difficult to place well.

The objective of this project is to create a list of predictions for who will win each individual challenge for the upcoming year. To do that, all of the data from previous MOMAMs will be used, as well as each streamer's general play time preferences. This will be done by collecting publicly available playtime data, obtaining meta data on every game they've played via a publicly-accessible game database API, structuring the data to formalize how each streamer engages with certain genres, franchises, etc., and finally using this information to create a prediction framework that can produce a competitive set of predictions no matter what the game list for the upcoming year ends up being.


## Setup

In order to completely reporduce the contents of this project's `output` folder from scratch, access to Twitch.tv's developer API is required.

1. Please follow the instructions needed to generate a `client_id` and `client_secret` here: https://dev.twitch.tv/docs/authentication/register-app/
    - A twitch.tv account is required. You will need to sign up for one (for free) and enable 2FA.
    - You will need to create an application in the developer console. 
      The name does not matter, but mine is called `momam_game_info_collector` and its URL is `http://localhost`
    - The end of the instructions has a "Next Steps" section that says: `After registering your app, the next step is to get a token.`.
      Do not worry about this - the `get_client_secret.R` R script will take of this part for you automatically.
2. Open the `get_client_secret.R` file and paste your id and secret into the return statements of the appropriate functions.

Note that if someone going through this project does not wish to go through the above setup process, they can simply ensure that they clone `MOMAM.sqlite` and and skip from `Step 0: Setup` straight to `Step 2: Prepare Data for Analysis` code blocks when running `doc/Run_MOMAM.Rmd`.