// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

extern crate chrono_tz;
#[macro_use(crate_name, crate_version, crate_description)]
extern crate clap;
extern crate gumby_ability;
extern crate gumby_app;
extern crate gumby_cmd;
extern crate gumby_tz;
extern crate openssl_probe;
extern crate slack;
extern crate slack_api;
#[macro_use]
extern crate slog;
extern crate sloggers;

use clap::{App, Arg};
use gumby_ability::Ability;
use sloggers::Build;

fn main() {
    let mut logger = sloggers::terminal::TerminalLoggerBuilder::new();
    logger.level(sloggers::types::Severity::Debug);
    logger.destination(sloggers::terminal::Destination::Stderr);
    let logger = logger.build().unwrap();

    debug!(logger, "Initializing SSL certificate env vars");
    openssl_probe::init_ssl_cert_env_vars();

    debug!(logger, "Initializing ability: TZ");
    let ability_tz = gumby_tz::TZ::new(
        vec![
            chrono_tz::America::Los_Angeles,
            chrono_tz::America::Chicago,
            chrono_tz::America::New_York,
            chrono_tz::UTC,
            chrono_tz::Europe::Warsaw,
            chrono_tz::Europe::Saratov,
        ],
        Some(&logger),
    );

    debug!(logger, "Initializing ability: CMD");
    let ability_cmd = gumby_cmd::CMD::new(
        "mod_passwd",
        "ls",
        vec!["-alhd", "/etc/passwd"],
        Some(&logger),
    );

    let abilities: Vec<&Ability> = vec![&ability_tz, &ability_cmd];

    debug!(logger, "Initializing & parsing cmdline args");
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

    debug!(logger, "Retrieving slack_api_token");
    // unwrap is safe: slack_api_token is .required(true)
    let slack_api_token = app_args.value_of("slack_api_token").unwrap();

    info!(logger, "Logging into slack");
    let rtm_client = slack::RtmClient::login(slack_api_token).unwrap();
    debug!(logger, "Creating web API client");
    let web_client = slack_api::requests::Client::new();

    debug!(logger, "Creating GumbyHandler");
    let mut gumby_handler = gumby_app::GumbyHandler::new(
        &rtm_client,
        slack_api_token,
        web_client,
        &abilities,
        Some(&logger),
    );

    info!(logger, "Running slack message receive-loop");
    match rtm_client.run(&mut gumby_handler) {
        Ok(()) => info!(logger, "Slack message receive-loop closed successfuly"),
        Err(err) => {
            error!(logger, "Slack message receive-loop failed: {:?}", err);
            panic!("Slack message receive-loop error");
        }
    }
}
