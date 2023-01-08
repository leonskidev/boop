use core::iter;
use std::io::{self, Write};

use ariadne::{Color, Config, Fmt, Label, Report, ReportKind, Source};
use chumsky::Parser;

fn main() {
  let mut ctx = boop::eval::Context::default();

  let mut input = iter::from_fn(|| {
    print!("=> ");
    io::stdout().flush().ok()
  })
  .zip(io::stdin().lines())
  .map(|(_, line)| line);

  while let Some(Ok(line)) = input.next() {
    let (stmt, errs) = boop::parse::parser().parse_recovery(line.clone());

    if !errs.is_empty() {
      errs
        .into_iter()
        .map(|e| e.map(|c| c.to_string()))
        .for_each(|e| {
          let report = Report::build(ReportKind::Error, (), e.span().start)
            .with_config(
              Config::default().with_compact(true).with_cross_gap(true),
            );

          let report = match e.reason() {
            chumsky::error::SimpleReason::Unclosed { span, delimiter } => {
              report
                .with_message(format!(
                  "Unclosed delimiter {}",
                  delimiter.fg(Color::Yellow)
                ))
                .with_label(
                  Label::new(span.clone())
                    .with_message(format!(
                      "Unclosed delimiter {}",
                      delimiter.fg(Color::Yellow)
                    ))
                    .with_color(Color::Yellow),
                )
                .with_label(
                  Label::new(e.span())
                    .with_message(format!(
                      "Must be closed before this {}",
                      e.found()
                        .unwrap_or(&"end of file".to_string())
                        .fg(Color::Red)
                    ))
                    .with_color(Color::Red),
                )
            }
            chumsky::error::SimpleReason::Unexpected => report
              .with_message(format!(
                "{}, expected {}",
                if e.found().is_some() {
                  "Unexpected token in input"
                } else {
                  "Unexpected end of input"
                },
                if e.expected().len() == 0 {
                  "something else".to_string()
                } else {
                  e.expected()
                    .map(|expected| match expected {
                      Some(expected) => expected.to_string(),
                      None => "end of input".to_string(),
                    })
                    .collect::<Vec<_>>()
                    .join(", ")
                }
              ))
              .with_label(
                Label::new(e.span())
                  .with_message(format!(
                    "Unexpected token {}",
                    e.found()
                      .unwrap_or(&"end of file".to_string())
                      .fg(Color::Red)
                  ))
                  .with_color(Color::Red),
              ),
            chumsky::error::SimpleReason::Custom(msg) => {
              report.with_message(msg).with_label(
                Label::new(e.span())
                  .with_message(format!("{}", msg.fg(Color::Red)))
                  .with_color(Color::Red),
              )
            }
          };

          report.finish().print(Source::from(line.clone())).unwrap();
        });
    } else if let Some(stmt) = stmt {
      println!("  {}", stmt.eval(&mut ctx));
    }
  }
}
