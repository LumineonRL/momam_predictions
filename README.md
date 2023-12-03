## Setup

In order to run this project, access to Twitch.tv's developer API is required.

1. Please follow the instructions needed to generate a `client_id` and `client_secret` here: https://dev.twitch.tv/docs/authentication/register-app/
    - A twitch.tv account is required. You will need to sign up for one (for free) and enable 2FA.
    - You will need to create an application in the developer console. 
      The name does not matter, but mine is called `momam_game_info_collector` and its URL is `http://localhost`
    - The end of the instructions has a "Next Steps" section that says: `After registering your app, the next step is to get a token.`.
      Do not worry about this - the `get_client_secret.R` R script will take of this part for you automatically.
2. Open the `get_client_secret.R` file and paste your id and secret into the return statements of the appropriate functions.