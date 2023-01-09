use core::iter;
use std::{
  io::{self, Write},
  path::PathBuf,
};

// use ariadne::{Color, Config, Fmt, Label, Report, ReportKind, Source};
use boop::eval::Context;

fn main() {
  let cli: Cli = clap::Parser::parse();

  let mut ctx = Context::default();

  cli
    .libs
    .into_iter()
    .filter_map(|path| match std::fs::read_to_string(path) {
      Ok(source) => Some(source),
      Err(err) => {
        eprintln!("WARN: {}", err);
        None
      }
    })
    .for_each(|source| {
      source.lines().for_each(|s| {
        ctx.eval(s);
      });
    });

  match cli.command {
    Command::Inline { source } => {
      if let Some(result) = ctx.eval(&source) {
        println!("{}", result)
      }
    }
    Command::Repl => {
      iter::from_fn(|| {
        print!("=> ");
        io::stdout().flush().ok()
      })
      .zip(io::stdin().lines())
      // TODO: handle errors
      .map(|(_, line)| line.unwrap())
      .filter_map(|s| ctx.eval(&s))
      .for_each(|result| println!("{}", result));
    }
  }
}

#[derive(Debug, clap::Parser)]
#[command(author, version, about, long_about = None)]
struct Cli {
  #[command(subcommand)]
  command: Command,

  // /// Whether to disable the standard library.
  // #[arg(short, long)]
  // no_std: bool,
  /// List of library files to import.
  #[arg(short, long)]
  libs: Vec<PathBuf>,
}

#[derive(Debug, Clone, PartialEq, Eq, clap::Subcommand)]
enum Command {
  /// Evaluates the provided string [alias: `=`].
  #[command(alias = "=")]
  Inline { source: String },
  /// Starts the interactive REPL [alias: `>`].
  #[command(alias = ">")]
  Repl,
}

// fn handle_errors(source: String, errs: Vec<chumsky::error::Simple<char>>) {
//   errs
//     .into_iter()
//     .map(|e| e.map(|c| c.to_string()))
//     .for_each(|e| {
//       let report = Report::build(ReportKind::Error, (), e.span().start)
//         .with_config(Config::default().with_compact(true).with_cross_gap(true));

//       let report = match e.reason() {
//         chumsky::error::SimpleReason::Unclosed { span, delimiter } => report
//           .with_message(format!(
//             "Unclosed delimiter {}",
//             delimiter.fg(Color::Yellow)
//           ))
//           .with_label(
//             Label::new(span.clone())
//               .with_message(format!(
//                 "Unclosed delimiter {}",
//                 delimiter.fg(Color::Yellow)
//               ))
//               .with_color(Color::Yellow),
//           )
//           .with_label(
//             Label::new(e.span())
//               .with_message(format!(
//                 "Must be closed before this {}",
//                 e.found()
//                   .unwrap_or(&"end of file".to_string())
//                   .fg(Color::Red)
//               ))
//               .with_color(Color::Red),
//           ),
//         chumsky::error::SimpleReason::Unexpected => report
//           .with_message(format!(
//             "{}, expected {}",
//             if e.found().is_some() {
//               "Unexpected token in input"
//             } else {
//               "Unexpected end of input"
//             },
//             if e.expected().len() == 0 {
//               "something else".to_string()
//             } else {
//               e.expected()
//                 .map(|expected| match expected {
//                   Some(expected) => expected.to_string(),
//                   None => "end of input".to_string(),
//                 })
//                 .collect::<Vec<_>>()
//                 .join(", ")
//             }
//           ))
//           .with_label(
//             Label::new(e.span())
//               .with_message(format!(
//                 "Unexpected token {}",
//                 e.found()
//                   .unwrap_or(&"end of file".to_string())
//                   .fg(Color::Red)
//               ))
//               .with_color(Color::Red),
//           ),
//         chumsky::error::SimpleReason::Custom(msg) => {
//           report.with_message(msg).with_label(
//             Label::new(e.span())
//               .with_message(format!("{}", msg.fg(Color::Red)))
//               .with_color(Color::Red),
//           )
//         }
//       };

//       report
//         .finish()
//         .eprint(Source::from(source.clone()))
//         .unwrap();
//     });
// }
