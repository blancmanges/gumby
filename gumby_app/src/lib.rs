// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

extern crate gumby_ability;
extern crate slack;
extern crate slack_api;

use gumby_ability::Ability;
use slack::{Event, Message, RtmClient};

pub struct GumbyHandler<'a> {
    slack_api_token: String,
    slack_web_client: slack_api::requests::Client,
    gumby_callout: String,
    abilities: &'a [&'a Ability],
}

impl<'a> std::fmt::Debug for GumbyHandler<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            "GumbyHandler {{ gumby_callout: {}, slack_api_token: …, \
             slack_web_client: {:?}, abilities: (…) }}",
            self.gumby_callout, self.slack_web_client
        )
    }
}

impl<'a> GumbyHandler<'a> {
    pub fn new(
        rtm_client: &slack::RtmClient,
        slack_api_token: &str,
        slack_web_client: slack_api::requests::Client,
        abilities: &'a [&'a Ability],
    ) -> GumbyHandler<'a> {
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
            gumby_callout: format!("<@{}>", bot_id),
            abilities,
        }
    }

    /// Tries to retrieve command & arguments from text.
    fn text_to_call(&'a self, message: &'a str) -> Option<(&'a str, &'a str)> {
        let mut splitter = message.splitn(3, char::is_whitespace);
        if Some(self.gumby_callout.as_str()) == splitter.next() {
            if let Some(command) = splitter.next() {
                let args = splitter.next().unwrap_or("");
                return Some((command, args));
            }
        }
        return None;
    }

    /// Executes the command.
    ///
    /// Will execute all matching abilities.
    fn exec_command(
        &'a self,
        cli: &RtmClient,
        command: &'a str,
        args: &'a str,
        message_chan: &'a str,
    ) {
        for ability in self.abilities {
            if ability.callout() == command {
                let response = ability.reply_to(args);
                cli.sender().send_message(message_chan, &response).unwrap();
            }
        }
    }
}

impl<'a> slack::EventHandler for GumbyHandler<'a> {
    fn on_event(&mut self, cli: &RtmClient, event: Event) {
        println!("on_event(event: {:?})", event);
        match event {
            Event::Hello => println!("  Connected"),
            Event::Message(boxed_msg) => {
                println!("  Message!");
                if let Message::Standard(std_message) = *boxed_msg {
                    let message_chan = std_message.channel.unwrap();
                    println!("  Message standard!");
                    if let Some(text) = std_message.text {
                        if let Some((command, args)) = self.text_to_call(&text) {
                            self.exec_command(cli, command, args, &message_chan);
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
