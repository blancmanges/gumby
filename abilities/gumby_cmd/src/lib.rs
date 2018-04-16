// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

extern crate gumby_ability;
extern crate itertools;
#[macro_use]
extern crate slog;

use std::process::Command;
use itertools::Itertools;

pub struct CMD {
    slack_cmd: &'static str,
    cmd: &'static str,
    args: Vec<&'static str>,
    logger: slog::Logger,
}

impl CMD {
    pub fn new(
        slack_cmd: &'static str,
        cmd: &'static str,
        args: Vec<&'static str>,
        logger: Option<&slog::Logger>,
    ) -> CMD {
        let logger_ctx = o!(
            "ability" => "CMD".to_string(),
            "cmd" => cmd.to_string(),
            "args" => args.clone().into_iter().join(","),
        );
        CMD {
            slack_cmd,
            cmd,
            args,
            logger: logger
                .map_or(slog::Logger::root(slog::Discard, o!()), |l| {
                    l.new(o!())
                })
                .new(logger_ctx),
        }
    }
}

impl gumby_ability::Ability for CMD {
    fn callout(&self) -> &'static str {
        self.slack_cmd
    }

    fn reply_to(&self, msg: &str) -> String {
        let logger = self.logger.new(o!(
            "message" => msg.to_string(),
        ));
        let output = Command::new(&self.cmd).args(&self.args).output();
        match output {
            Ok(output) => {
                let exit_code = match output.status.code() {
                    Some(code) => code.to_string(),
                    None => "killed".to_string(),
                };
                let logger = logger.new(o!(
                    "exit_code" => exit_code.clone(),
                ));
                debug!(logger, "Done");
                format!(
                    "*stdout* ```\n{}```\n\n*stderr* ```\n{}```\n\n*exit code:* `{}`",
                    String::from_utf8_lossy(&output.stdout),
                    String::from_utf8_lossy(&output.stderr),
                    exit_code,
                )
            }
            Err(err) => {
                warn!(logger, "Execution error: {}", err);
                "Command execution error".to_string()
            }
        }
    }
}
