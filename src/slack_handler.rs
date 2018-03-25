// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use slack::{Event, Message, RtmClient};
use slack_api;
use slack;

use abilities;

#[derive(Debug)]
pub struct GumbyHandler {
    slack_api_token: String,
    slack_web_client: slack_api::requests::Client,
    gumby_callout: String,
}

impl GumbyHandler {
    pub fn new(
        rtm_client: &slack::RtmClient,
        slack_api_token: &str,
        slack_web_client: slack_api::requests::Client,
    ) -> GumbyHandler {
        let bot_id: &str = rtm_client
            .start_response()
            .slf
            .as_ref()
            .unwrap()
            .id
            .as_ref()
            .map_or("gumby", |s| s);
        GumbyHandler {
            slack_api_token: slack_api_token.to_string(),
            slack_web_client,
            gumby_callout: format!("<@{}> ", bot_id),
        }
    }
}

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
                            if let "tz" = msg {
                                let response = abilities::tz::tz();
                                cli.sender()
                                    .send_message(&smsg.channel.unwrap(), &response)
                                    .unwrap();
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
