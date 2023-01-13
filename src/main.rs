use boop::{
  eval::{CompileError, Engine, Scope},
  fmt::Formatter,
};
use chumsky::zero_copy::error::Rich;
use clap::Parser;
use lasso::Rodeo;
use reedline::{Prompt, Reedline, Signal};
use std::borrow::Cow;

fn main() {
  let mut engine = Engine::default();
  let mut scope = Scope::default();

  let cli = Cli::parse();

  match cli.command {
    Command::Inline { input } => eval(&mut engine, &mut scope, &input),
    Command::Repl => repl(&mut engine, &mut scope),
  }
}

fn eval(engine: &mut Engine<Rodeo>, scope: &mut Scope, input: &str) {
  let expr = engine.eval_with_scope::<Rich<_>, Rich<_>>(scope, input);

  match expr {
    Ok(expr) => println!("{}", Formatter::new(engine, &expr)),
    // TODO: use ariadne once the inner error data becomes available
    Err(errs) => errs.into_iter().for_each(|e| match e {
      CompileError::Lex(e) => eprintln!("ERROR: {:?}", e),
      CompileError::Parse(e) => eprintln!("ERROR: {:?}", e),
    }),
  }
}

fn repl(engine: &mut Engine<Rodeo>, scope: &mut Scope) {
  let mut line_editor = Reedline::create();
  let prompt = ReplPrompt::default();

  loop {
    match line_editor.read_line(&prompt) {
      Ok(Signal::Success(input)) => eval(engine, scope, &input),
      Ok(Signal::CtrlC) | Ok(Signal::CtrlD) => {
        println!("Aborted!");
        break;
      }
      event => {
        eprintln!("Unknown REPL event: {:?}", event);
        break;
      }
    }
  }
}

#[derive(Debug, clap::Parser)]
#[command(author, version, about, long_about = None)]
struct Cli {
  #[command(subcommand)]
  command: Command,
}

#[derive(Debug, Clone, PartialEq, Eq, clap::Subcommand)]
enum Command {
  /// Evaluates the provided string [alias: `=`].
  #[command(alias = "=")]
  Inline { input: String },
  /// Starts the interactive REPL [alias: `>`].
  #[command(alias = ">")]
  Repl,
}

#[derive(Default)]
struct ReplPrompt;

impl Prompt for ReplPrompt {
  #[inline]
  fn render_prompt_left(&self) -> std::borrow::Cow<str> {
    Cow::Owned("=> ".to_string())
  }

  #[inline]
  fn render_prompt_right(&self) -> std::borrow::Cow<str> {
    Cow::Owned("".to_string())
  }

  #[inline]
  fn render_prompt_indicator(&self, _: reedline::PromptEditMode) -> Cow<str> {
    Cow::Owned("".to_string())
  }

  #[inline]
  fn render_prompt_multiline_indicator(&self) -> Cow<str> {
    Cow::Owned("".to_string())
  }

  fn render_prompt_history_search_indicator(
    &self,
    _: reedline::PromptHistorySearch,
  ) -> Cow<str> {
    Cow::Owned("".to_string())
  }
}

////////////////////////////////////////////////////////////////////////////////

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
