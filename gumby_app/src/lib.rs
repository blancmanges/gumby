// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

extern crate gumby_ability;
extern crate itertools;
extern crate slack;
extern crate slack_api;
#[macro_use]
extern crate slog;

use gumby_ability::Ability;
use itertools::Itertools;
use slack::{Event, Message, RtmClient};

pub struct GumbyHandler<'a> {
    #[allow(dead_code)]
    slack_api_token: String,
    slack_web_client: slack_api::requests::Client,
    gumby_callout: String,
    abilities: &'a [&'a Ability],
    logger: slog::Logger,
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
        logger: Option<&slog::Logger>,
    ) -> GumbyHandler<'a> {
        let bot_id: &str = rtm_client
            .start_response()
            .slf
            .as_ref()
            .unwrap()
            .id
            .as_ref()
            .map_or("gumby", |s| s);
        let gumby_callout = format!("<@{}>", bot_id);
        let available_commands = format!(
            "[{}]",
            abilities.into_iter().map(|a| a.callout()).join(", ")
        );
        let logger_ctx = o!(
            "bot_id" => bot_id.to_string(),
            "available_commands" => available_commands,
            "gumby_callout" => gumby_callout.clone(),
            "event_source" => "slack_rtm",
        );
        GumbyHandler {
            slack_api_token: slack_api_token.to_string(),
            slack_web_client,
            gumby_callout,
            abilities,
            logger: logger
                .map_or(slog::Logger::root(slog::Discard, o!()), |l| {
                    l.new(o!())
                })
                .new(logger_ctx),
        }
    }

    /// Tries to retrieve command & arguments from text.
    fn text_to_call(
        &'a self,
        message: &'a str,
        logger: &slog::Logger,
    ) -> Option<(&'a str, &'a str, slog::Logger)> {
        let mut splitter = message.splitn(3, char::is_whitespace);
        if Some(self.gumby_callout.as_str()) == splitter.next() {
            debug!(logger, "Message is addressed to us: {}", &message);
            if let Some(command) = splitter.next() {
                debug!(logger, "Retrieved command: {}", &command);
                let args = splitter.next().unwrap_or("");
                let logger = logger.new(o!(
                    "slack_command" => command.to_string(),
                    "slack_command_args" => args.to_string(),
                ));
                return Some((command, args, logger));
            }
        }
        return None;
    }

    fn prepare_help(&self, logger: &slog::Logger) -> String {
        debug!(logger, "Preparing help message");
        let mut msg = "*Available commands:*\n\n".to_string();
        for ability in self.abilities {
            msg.push_str(&format!("- `{}`\n", ability.callout()));
        }
        msg.push_str("- `help`");
        msg
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
        logger: &slog::Logger,
    ) {
        if command == "help" {
            let response = self.prepare_help(&logger);
            info!(logger, "Sending response (help text)");
            cli.sender().send_message(message_chan, &response).unwrap();
        } else {
            for ability in self.abilities {
                if ability.callout() == command {
                    info!(logger, "Executing ability {}", ability.callout());
                    let response = ability.reply_to(args);
                    info!(logger, "Sending response from ability: {}", response);
                    cli.sender().send_message(message_chan, &response).unwrap();
                }
            }
        }
    }
}

impl<'a> slack::EventHandler for GumbyHandler<'a> {
    fn on_event(&mut self, cli: &RtmClient, event: Event) {
        let logger = self.logger.new(o!(
            "event" => "on_event",
        ));
        match event {
            Event::Hello => {
                let logger = logger.new(o!(
                    "event_type" => "hello",
                ));
                info!(logger, "Got Hello")
            }
            Event::Message(boxed_msg) => {
                let logger = logger.new(o!(
                    "event_type" => "message",
                ));
                debug!(logger, "Got Message");
                trace!(logger, "Message: {:?}", boxed_msg);

                if let Message::Standard(std_message) = *boxed_msg {
                    debug!(logger, "A standard message");
                    if let Some(message_chan) = std_message.channel {
                        let logger = logger.new(o!(
                            "slack_channel" => message_chan.clone(),
                            "slack_message_ts" => std_message.ts.unwrap_or("?".to_string()),
                            "slack_message_user" => std_message.user.unwrap_or("?".to_string()),
                        ));

                        if let Some(text) = std_message.text {
                            debug!(logger, "Retrieved text: {}", &text);
                            if let Some((command, args, logger)) = self.text_to_call(&text, &logger)
                            {
                                self.exec_command(cli, command, args, &message_chan, &logger);
                            } else {
                                debug!(logger, "No command found");
                            }
                        } else {
                            debug!(logger, "Message without text");
                        }
                    } else {
                        debug!(logger, "Message without channel");
                    }
                } else {
                    debug!(logger, "A non-standard message");
                }
            }
            _ => (),
        }
    }
    fn on_close(&mut self, _cli: &RtmClient) {
        let logger = self.logger.new(o!(
            "event" => "on_close",
        ));
        info!(logger, "Closed connection");
    }
    fn on_connect(&mut self, _cli: &RtmClient) {
        let logger = self.logger.new(o!(
            "event" => "on_connect",
        ));
        info!(logger, "Connected");
    }
}
