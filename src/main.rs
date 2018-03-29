// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

#[macro_use(crate_name, crate_version, crate_description)]
extern crate clap;
extern crate gumby;
extern crate slack;
extern crate slack_api;
extern crate openssl_probe;

use clap::{App, Arg};

use gumby::slack_handler::GumbyHandler;

fn main() {
    openssl_probe::init_ssl_cert_env_vars();

    let app_args = App::new(crate_name!())
        .version(crate_version!())
        .about(crate_description!())
        .arg(
            Arg::with_name("slack_api_token")
                .long("--slack-api-token")
                .short("-s")
                .takes_value(true)
                .required(true)
                .env("SLACK_API_TOKEN")
                .hide_env_values(true),
        )
        .get_matches();

    let slack_api_token = app_args.value_of("slack_api_token").unwrap(); // unwrap is safe: slack_api_token is .required(true)
    let rtm_client = slack::RtmClient::login(slack_api_token).unwrap();
    let web_client = slack_api::requests::Client::new().unwrap();

    let mut gumby_handler = GumbyHandler::new(&rtm_client, slack_api_token, web_client);
    match rtm_client.run(&mut gumby_handler) {
        Ok(()) => (),
        Err(err) => panic!("Ooops: {:?}", err),
    }
}
