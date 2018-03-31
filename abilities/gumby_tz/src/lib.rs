// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

extern crate chrono;
extern crate chrono_tz;
extern crate gumby_ability;
extern crate itertools;

use itertools::Itertools;
use chrono::{DateTime, Utc};

pub struct TZ {
    timezones: Vec<chrono_tz::Tz>,
}

impl TZ {
    /// Constructs a new `TZ` with a given list of timezones.
    pub fn new(timezones: Vec<chrono_tz::Tz>) -> TZ {
        TZ { timezones }
    }

    /// Creates a reply text basing on given time and timezones.
    ///
    /// # Example output
    ///
    /// ```
    /// America/New_York      --   Thu, 2018-03-29 at 16:30   --   EDT (-0400)
    /// UTC                   --   Thu, 2018-03-29 at 20:30   --   UTC (+0000)
    /// Europe/Warsaw         --   Thu, 2018-03-29 at 22:30   --   CEST (+0200)
    /// ```
    fn reply_time(time: &DateTime<Utc>, timezones: &[chrono_tz::Tz]) -> String {
        let tz_name_max_len = timezones
            .into_iter()
            .map(|tz| tz.name().len())
            .max()
            .unwrap_or(0);
        let date_fmt = "   --   %a, %Y-%m-%d at %H:%M   --   %Z (%z)";

        format!(
            "```{}```",
            timezones
                .into_iter()
                .map(|tz| format!(
                    "{tz_name:<tz_name_width$}{time}",
                    tz_name = tz.name(),
                    tz_name_width = tz_name_max_len,
                    time = time.with_timezone(tz).format(date_fmt)
                ))
                .join("\n")
        )
    }
}

impl gumby_ability::Ability for TZ {
    fn callout(&self) -> &'static str {
        "tz"
    }

    fn reply_to(&self, msg: &str) -> String {
        match msg {
            "" => {
                let utc_now: DateTime<Utc> = Utc::now();
                TZ::reply_time(&utc_now, &self.timezones)
            }
            _ => unimplemented!(""),
        }
    }
}
