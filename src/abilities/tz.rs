// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use chrono::{DateTime, Utc};
use itertools::Itertools;
use chrono_tz;

pub fn tz() -> String {
    let utc_now: DateTime<Utc> = Utc::now();

    let timezones = [
        chrono_tz::America::Los_Angeles,
        chrono_tz::America::Chicago,
        chrono_tz::America::New_York,
        chrono_tz::UTC,
        chrono_tz::Europe::Warsaw,
        chrono_tz::Europe::Saratov,
    ];
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
                time = utc_now.with_timezone(tz).format(date_fmt)
            ))
            .join("\n")
    )
}
