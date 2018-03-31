// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

pub trait Ability {
    /// Returns a slice describing the word,
    /// by which the ability will be invoked.
    fn callout(&self) -> &'static str;

    /// When ability is invoked, this method will receive
    /// the remaining (stripped) invocation line.
    ///
    /// # Example
    ///
    /// For ability with `callout() == FOO`, when
    /// user sends:
    ///
    /// > FOO 1 2 3
    ///
    /// then the ability will be invoked as:
    ///
    /// ```
    /// foo.reply_to("1 2 3")
    /// ```
    fn reply_to(&self, msg: &str) -> String;
}
