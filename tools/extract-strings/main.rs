// Copyright © SixtyFPS GmbH <info@slint-ui.com>
// SPDX-License-Identifier: GPL-3.0-only OR LicenseRef-Slint-commercial

use clap::Parser;
use i_slint_compiler::diagnostics::{BuildDiagnostics, Spanned};
use i_slint_compiler::parser::{SyntaxKind, SyntaxNode};
use messages::{Message, Messages};

mod generator;
mod messages;

#[derive(clap::Parser)]
#[command(author, version, about, long_about = None)]
struct Cli {
    #[arg(name = "path to .slint file(s)", action)]
    paths: Vec<std::path::PathBuf>,

    #[arg(long = "default-domain", short = 'd')]
    domain: Option<String>,

    #[arg(
        name = "file",
        short = 'o',
        help = "Write output to specified file (instead of messages.po)."
    )]
    output: Option<std::path::PathBuf>,

    #[arg(long = "omit-header", help = r#"Don’t write header with ‘msgid ""’ entry"#)]
    omit_header: bool,

    #[arg(long = "copyright-holder", help = "Set the copyright holder in the output")]
    copyright_holder: Option<String>,

    #[arg(long = "package-name", help = "Set the package name in the header of the output")]
    package_name: Option<String>,

    #[arg(long = "package-version", help = "Set the package version in the header of the output")]
    package_version: Option<String>,

    #[arg(
        long = "msgid-bugs-address",
        help = "Set the reporting address for msgid bugs. This is the email address or URL to which the translators shall report bugs in the untranslated strings"
    )]
    msgid_bugs_address: Option<String>,
}

fn main() -> std::io::Result<()> {
    let args = Cli::parse();

    let mut messages = Messages::new();

    for path in args.paths {
        let source = std::fs::read_to_string(&path)?;

        process_file(source, path, &mut messages)?
    }

    let output = args.output.unwrap_or_else(|| {
        format!("{}.po", args.domain.as_ref().map(String::as_str).unwrap_or("messages")).into()
    });

    let output_details = generator::OutputDetails {
        omit_header: args.omit_header,
        copyright_holder: args.copyright_holder,
        package_name: args.package_name,
        package_version: args.package_version,
        bugs_address: args.msgid_bugs_address,
        charset: "UTF-8".into(),
        add_location: generator::AddLocation::Full,
    };

    let mut messages: Vec<_> = messages.values().collect();
    messages.sort_by_key(|m| m.index);

    generator::generate(output, output_details, messages)
}

/// FIXME! this is duplicated with the updater
fn process_rust_file(source: String, _messages: &mut Messages) -> std::io::Result<()> {
    let mut source_slice = &source[..];
    let slint_macro = format!("{}!", "slint"); // in a variable so it does not appear as is
    'l: while let Some(idx) = source_slice.find(&slint_macro) {
        // Note: this code ignore string literal and unbalanced comment, but that should be good enough
        let idx2 =
            if let Some(idx2) = source_slice[idx..].find(|c| c == '{' || c == '(' || c == '[') {
                idx2
            } else {
                break 'l;
            };
        let open = source_slice.as_bytes()[idx + idx2].into();
        let close = match open {
            '{' => '}',
            '(' => ')',
            '[' => ']',
            _ => panic!(),
        };
        source_slice = &source_slice[idx + idx2 + 1..];
        let mut idx = 0;
        let mut count = 1;
        while count > 0 {
            if let Some(idx2) = source_slice[idx..].find(|c| {
                if c == open {
                    count += 1;
                    true
                } else if c == close {
                    count -= 1;
                    true
                } else {
                    false
                }
            }) {
                idx += idx2 + 1;
            } else {
                break 'l;
            }
        }
        let code = &source_slice[..idx - 1];
        source_slice = &source_slice[idx - 1..];
        todo!("Rust files");
        /*
        let mut diag = BuildDiagnostics::default();
        let syntax_node = i_slint_compiler::parser::parse(code.to_owned(), None, &mut diag);
        let len = syntax_node.text_range().end().into();
        visit_node(syntax_node, &mut file)?;
        */
    }
    Ok(())
}

fn process_slint_file(
    source: String,
    path: std::path::PathBuf,
    messages: &mut Messages,
) -> std::io::Result<()> {
    let mut diag = BuildDiagnostics::default();
    let syntax_node = i_slint_compiler::parser::parse(source.clone(), Some(&path), &mut diag);
    visit_node(syntax_node, messages);

    Ok(())
}

fn process_file(
    source: String,
    path: std::path::PathBuf,
    messages: &mut Messages,
) -> std::io::Result<()> {
    match path.extension() {
        Some(ext) if ext == "rs" => process_rust_file(source, messages),
        _ => process_slint_file(source, path, messages),
    }
}

fn visit_node(node: SyntaxNode, results: &mut Messages) {
    for n in node.children() {
        if n.kind() == SyntaxKind::AtTr {
            if let Some(msgid) = n
                .child_text(SyntaxKind::StringLiteral)
                .and_then(|s| i_slint_compiler::literals::unescape_string(&s))
            {
                let msgctxt = None; // todo!
                let key =
                    messages::MessageKey::new(msgid.clone(), msgctxt.clone().unwrap_or_default());
                let index = results.len();
                let message = results.entry(key).or_insert_with(|| Message {
                    msgctxt,
                    msgid,
                    index,
                    ..Default::default()
                });

                let span = node.span();
                if span.is_valid() {
                    let (line, _) = node.source_file.line_column(span.offset);
                    if line > 0 {
                        message.locations.push(messages::Location {
                            file: node.source_file.path().into(),
                            line,
                        });
                    }
                }

                /* TODO
                let mut comments = get_comment_before_line(&self.source_lines, line);
                if comments.is_none() {
                    let ident_line = ident_span.start().line;
                    if ident_line != line {
                        comments = get_comment_before_line(&self.source_lines, ident_line);
                    }
                }
                message.comments = comments;
                */
            }
        }
        visit_node(n, results)
    }
}

/*


/* Copyright (C) 2018 Olivier Goffart <ogoffart@woboq.com>

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Affero General Public License as
published by the Free Software Foundation, either version 3 of the
License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.
*/

use quote::ToTokens;
use tr::{tr, tr_init};

use anyhow::{anyhow, Error};
use clap::{App, Arg};
use std::collections::HashMap;
use std::str::FromStr;

mod crate_visitor;
mod extract_messages;
mod generator;

#[derive(Clone, Debug)]
pub enum SpecArg {
    MsgId(u32),
    Context(u32),
}

#[derive(Default, Clone, Debug)]
pub struct Spec {
    pub args: Vec<SpecArg>,
    pub comment: Option<String>,
    pub argnum: Option<u32>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Location {
    pub file: std::path::PathBuf,
    pub line: usize,
}

#[derive(Debug, Clone, Default, Eq, PartialEq, Hash)]
pub struct MessageKey(String, String);

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct Message {
    msgctxt: Option<String>,
    msgid: String,
    plural: Option<String>,
    locations: Vec<Location>,
    comments: Option<String>,
    /// that's just keeping the count, so they can be sorted
    index: usize,
}

/// How much [Message](Message) location information to include in the
/// output.
#[derive(PartialEq, Debug)]
pub enum AddLocation {
    /// Format the locations output as ‘#: filename:line’
    /// This is the default.
    Full,
    /// Format the locations output as ‘#: filename`
    File,
    /// Don't include the message locations.
    Never,
}

impl FromStr for AddLocation {
    type Err = anyhow::Error;

    /// Create an [AddLocation](AddLocation) from a &str. Valid inputs
    /// are: "full", "file" or "never".
    fn from_str(s: &str) -> Result<AddLocation, Self::Err> {
        match s {
            "full" => Ok(AddLocation::Full),
            "file" => Ok(AddLocation::File),
            "never" => Ok(AddLocation::Never),
            _ => Err(anyhow!(
                "\"{0}\" is not a valid --add-location option. Valid \
                    options are \"full\", \"file\" or \"never\".",
                s
            )),
        }
    }
}

pub struct OutputDetails {
    omit_header: bool,
    copyright_holder: Option<String>,
    package_name: Option<String>,
    package_version: Option<String>,
    bugs_address: Option<String>,
    charset: String,
    add_location: AddLocation,
}

fn main() -> Result<(), Error> {
    tr_init!(concat!(env!("CARGO_MANIFEST_DIR"), "/lang/"));

    // The options are made to be compatible with xgetext options
    let matches = App::new("xtr")
        .version(clap::crate_version!())
        .author(clap::crate_authors!())
        .about(tr!("Extract strings from a rust crate to be translated with gettext").as_ref())
        .arg(
            Arg::with_name("domain")
                .short("d")
                .long("default-domain")
                .value_name("domain")
                .help(&tr!("Use name.po for output (instead of messages.po)")),
        )
        .arg(
            Arg::with_name("OUTPUT")
                .short("o")
                .long("output")
                .value_name("file")
                .help(&tr!(
                    "Write output to specified file (instead of messages.po)."
                )),
        )
        .arg(
            Arg::with_name("KEYWORDS")
                .short("k")
                .long("keyword")
                .value_name("keywordspec")
                .use_delimiter(false)
                .multiple(true)
                .help(&tr!(
                    // documentation for keywordspec goes here
                    "Specify keywordspec as an additional keyword to be looked for.\
                     Refer to the xgettext documentation for more info."
                )),
        )
        .arg(
            Arg::with_name("omit-header")
                .long("omit-header")
                .help(&tr!(r#"Don’t write header with ‘msgid ""’ entry"#)),
        )
        .arg(
            Arg::with_name("copyright-holder")
                .long("copyright-holder")
                .value_name("string")
                .help(&tr!("Set the copyright holder in the output.")),
        )
        .arg(
            Arg::with_name("package-name")
                .long("package-name")
                .value_name("package")
                .help(&tr!("Set the package name in the header of the output.")),
        )
        .arg(
            Arg::with_name("package-version")
                .long("package-version")
                .value_name("version")
                .help(&tr!("Set the package version in the header of the output.")),
        )
        .arg(
            Arg::with_name("msgid-bugs-address")
                .long("msgid-bugs-address")
                .value_name("email@address")
                .help(&tr!(
                    "Set the reporting address for msgid bugs. This is the email address \
                     or URL to which the translators shall report bugs in the untranslated strings"
                )),
        )
        .arg(
            Arg::with_name("charset")
                .long("charset")
                .value_name("encoding")
                .default_value("UTF-8")
                .help(&tr!(
                    "The encoding used for the characters in the POT file's locale."
                )),
        )
        .arg(
            Arg::with_name("add-location")
                .long("add-location")
                .short("n")
                .help(&tr!(
                    "How much message location information to include in the output. \
                     (default). If the type is ‘full’ (the default), it generates the \
                     lines with both file name and line number: ‘#: filename:line’. \
                     If it is ‘file’, the line number part is omitted: ‘#: filename’. \
                     If it is ‘never’, nothing is generated."
                ))
                .value_name("type")
                .possible_values(&["full", "file", "never"])
                .default_value("full"),
        )
        .arg(
            Arg::with_name("INPUT")
                // documentation for the input
                .help(&tr!("Main rust files to parse (will recurse into modules)"))
                .required(true)
                .multiple(true),
        )
        .get_matches();

    let keywords = matches
        .values_of("KEYWORDS")
        .map(|x| x.collect())
        .unwrap_or_else(|| {
            vec![
                "tr",
                "gettext",
                "dgettext:2",
                "dcgettext:2",
                "ngettext:1,2",
                "dngettext:2,3",
                "dcngettext:2,3",
                "gettext_noop",
                "pgettext:1c,2",
                "dpgettext:2c,3",
                "dcpgettext:2c,3",
                "npgettext:1c,2,3",
                "dnpgettext:2c,3,4",
                "dcnpgettext:2c,3,4",
            ]
        });
    let mut specs = HashMap::new();
    for k in keywords {
        if let Some(colon) = k.find(':') {
            let (name, desc) = k.split_at(colon);
            let spec = desc[1..]
                .split(',')
                .map(|d| {
                    if d.ends_with('c') {
                        return SpecArg::Context(
                            d[..d.len() - 1].parse().expect("invalid keyword spec"),
                        );
                    }
                    SpecArg::MsgId(d.parse().expect("invalid keyword spec"))
                    // TODO: comment or argnum
                })
                .collect();
            specs.insert(
                name.to_owned(),
                Spec {
                    args: spec,
                    comment: None,
                    argnum: None,
                },
            );
        } else {
            specs.insert(k.to_owned(), Spec::default());
        }
    }

    let mut results = HashMap::new();

    let inputs = matches.values_of("INPUT").expect("Missing crate root");
    for i in inputs {
        crate_visitor::visit_crate(i, |path, source, file| {
            extract_messages::extract_messages(
                &mut results,
                &specs,
                file.into_token_stream(),
                source,
                path,
            )
        })?;
    }

    let od = OutputDetails {
        omit_header: matches.is_present("omit-header"),
        copyright_holder: matches.value_of("copyright-holder").map(str::to_owned),
        package_name: matches.value_of("package-name").map(str::to_owned),
        package_version: matches.value_of("package-version").map(str::to_owned),
        bugs_address: matches.value_of("msgid-bugs-address").map(str::to_owned),
        charset: matches
            .value_of("charset")
            .expect("expected charset to have a default value")
            .to_owned(),
        add_location: AddLocation::from_str(
            matches
                .value_of("add-location")
                .expect("expected add-location to have a default value"),
        )
        .expect("expected add-location to be a valid value"),
    };

    let mut messages: Vec<_> = results.values().collect();
    messages.sort_by_key(|m| m.index);
    generator::generate(
        matches
            .value_of("OUTPUT")
            .map(|s| s.to_owned())
            .unwrap_or_else(|| format!("{}.po", matches.value_of("domain").unwrap_or("messages"))),
        od,
        messages,
    )?;

    Ok(())
}
*/
