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
    Command::Repl { dumb } => {
      if dumb {
        std::io::stdin()
          .lines()
          // TODO: handle errors
          .map(|input| input.unwrap())
          .for_each(|input| eval(&mut engine, &mut scope, &input))
      } else {
        repl(&mut engine, &mut scope)
      }
    }
    Command::Stdin => {
      if atty::isnt(atty::Stream::Stdin) {
        std::io::stdin()
          .lines()
          // TODO: handle errors
          .map(|input| input.unwrap())
          .for_each(|input| eval(&mut engine, &mut scope, &input))
      }
    }
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
  Repl {
    /// Whether to use a dumb REPL instead.
    #[arg(short, long)]
    dumb: bool,
  },
  /// Evaluates the provided STDIN [alias: `-`].
  #[command(alias = "-")]
  Stdin,
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
