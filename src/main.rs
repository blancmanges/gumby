// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

extern crate slack;
extern crate slack_api;

#[allow(dead_code)]
#[derive(Debug)]
struct GumbyHandler {
    slack_api_token: String,
    slack_web_client: slack_api::requests::Client,
    gumby_id: String,
    gumby_callout: String,
}
use slack::{Event, Message, RtmClient};
use slack_api::MessageStandard;

#[macro_use(crate_name, crate_version, crate_description)]
extern crate clap;
use clap::{App, Arg};

extern crate chrono;
use chrono::{DateTime, Utc};

extern crate chrono_tz;
use chrono_tz::{Tz, UTC};
use chrono_tz::US::Pacific;

extern crate itertools;
use itertools::Itertools;

impl slack::EventHandler for GumbyHandler {
    fn on_event(&mut self, cli: &RtmClient, event: Event) {
        println!("on_event(event: {:?})", event);
        match event {
            Event::Hello => println!("  Connected"),
            Event::Message(boxed_msg) => {
                println!("  Message!");
                if let Message::Standard(smsg) = *boxed_msg {
                    println!("  Message standard!");
                    if let Some(text) = smsg.text {
                        println!("  Value: {}", text);
                        if text.starts_with(&self.gumby_callout) {
                            // https://github.com/rust-lang/rfcs/issues/1262
                            let len = self.gumby_callout.len();
                            let msg = &text[len..];
                            println!("  Msg to us!: {}", msg);
                            match msg {
                                "tz" => {
                                    println!("TZ");
                                    let utc_now: DateTime<Utc> = Utc::now();
                                    println!("{:?} UTC", utc_now);
                                    let tzs = [
                                        chrono_tz::America::Los_Angeles,
                                        chrono_tz::America::Chicago,
                                        chrono_tz::America::New_York,
                                        chrono_tz::UTC,
                                        chrono_tz::Europe::Warsaw,
                                        chrono_tz::Europe::Saratov,
                                    ];
                                    let tzs_max_len = tzs.into_iter()
                                        .map(|tz| tz.name().len())
                                        .max()
                                        .unwrap_or(0);
                                    let tzs_txt = format!(
                                        "```{}```",
                                        tzs.into_iter()
                                            .map(|tz| format!(
                                                "{tzname:<width$}   {tzfmt}",
                                                tzname = tz.name(),
                                                width = tzs_max_len,
                                                tzfmt = utc_now.with_timezone(tz).format(
                                                    "--   %a, %Y-%m-%d at %H:%M   --   %Z (%z)"
                                                )
                                            ))
                                            .join("\n")
                                    );

                                    cli.sender()
                                        .send_message(&smsg.channel.unwrap(), &tzs_txt)
                                        .unwrap();
                                }
                                _ => (),
                            }
                        }
                    }
                }
            }
            _ => (),
        }
    }
    fn on_close(&mut self, _cli: &RtmClient) {
        println!("on_close");
    }
    fn on_connect(&mut self, cli: &RtmClient) {
        println!("on_connect");
        let resp = slack_api::bots::info(
            &self.slack_web_client,
            &self.slack_api_token,
            &slack_api::bots::InfoRequest::default(),
        );
        println!(" --> {:?}", resp);
        let resp = slack_api::users::identity(&self.slack_web_client, &self.slack_api_token);
        println!(" --> {:?}", resp);
        let xx = cli.start_response();
        println!(" >>> {:?}", xx);
    }
}

fn main() {
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

    let rtm_client = RtmClient::login(slack_api_token).unwrap();
    let bot_id: &str = &rtm_client
        .start_response()
        .slf
        .as_ref()
        .unwrap()
        .id
        .as_ref()
        .map_or("gumby", |s| s);

    let mut gumby_handler = GumbyHandler {
        slack_api_token: slack_api_token.to_string(),
        slack_web_client: slack_api::requests::Client::new().unwrap(),
        gumby_id: bot_id.to_string(),
        gumby_callout: format!("<@{}> ", bot_id),
    };

    println!("{:?}", gumby_handler);

    match rtm_client.run(&mut gumby_handler) {
        Ok(()) => (),
        Err(err) => panic!("Ooops: {:?}", err),
    }
}
