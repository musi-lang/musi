use std::env::current_dir;
use std::fs;
use std::io::{self, Read};
use std::path::{Path, PathBuf};
use std::thread;
use std::time::{Duration, SystemTime};

use crate::cli::{
    FmtArgs, FmtMatchArmArrowAlignmentArg, FmtMatchArmIndentArg, FmtOperatorBreakArg, FmtProfileArg,
};
use crate::error::{MusiError, MusiResult};
use crate::workspace::selected_package_roots;
use musi_fmt::{
    FormatError, FormatInputKind, FormatOptions, MatchArmArrowAlignment, MatchArmIndent,
    OperatorBreak, format_markdown, format_paths, format_source,
};
use musi_project::{ProjectOptions, load_project, load_project_ancestor, manifest::FmtProfile};
use musi_tooling::ToolingError;

pub(super) fn fmt_project(args: &FmtArgs) -> MusiResult {
    if args.all > 0 && !args.paths.is_empty() {
        return Err(MusiError::IncompatibleCommandArgs {
            left: "--all".to_owned(),
            right: "PATH".to_owned(),
        });
    }
    if args.paths.iter().any(|path| path == Path::new("-")) {
        return fmt_stdin(args);
    }

    if args.watch > 0 {
        return watch_fmt_project(args);
    }

    let session = fmt_session(args)?;
    run_fmt_once(args, &session)
}

fn run_fmt_once(args: &FmtArgs, session: &FmtSession) -> MusiResult {
    let mut options = session.options.clone();
    options.exclude.extend(args.watch_exclude.iter().cloned());

    let roots = if args.all > 0 {
        fmt_all_roots(session)
    } else if args.paths.is_empty() {
        vec![session.base_dir.clone()]
    } else {
        args.paths
            .iter()
            .map(|path| {
                if path.is_absolute() {
                    path.clone()
                } else {
                    session.invocation_dir.join(path)
                }
            })
            .collect()
    };
    let summary = format_paths(&roots, &session.base_dir, &options, args.check > 0)?;
    if summary.is_empty() && args.permit_no_files == 0 {
        return Err(FormatError::NoFiles.into());
    }
    let changed = summary.changed_paths();
    for path in &changed {
        println!("{}", path.display());
    }
    if args.check > 0 && !changed.is_empty() {
        return Err(MusiError::CheckCommandFailed);
    }
    Ok(())
}

fn fmt_all_roots(session: &FmtSession) -> Vec<PathBuf> {
    let project = load_project_ancestor(&session.base_dir, ProjectOptions::default()).ok();
    let Some(project) = project else {
        return vec![session.base_dir.clone()];
    };
    let roots = selected_package_roots(&project);
    if roots.is_empty() {
        vec![project.root_dir().to_path_buf()]
    } else {
        roots
    }
}

fn fmt_stdin(args: &FmtArgs) -> MusiResult {
    let mut source = String::new();
    let _bytes_read = io::stdin().read_to_string(&mut source).map_err(|source| {
        ToolingError::ToolingIoFailed {
            path: PathBuf::from("-"),
            source,
        }
    })?;
    let options = fmt_session(args)?.options;
    let kind = stdin_format_kind(args)?;
    let formatted = match kind {
        FormatInputKind::Musi => format_source(&source, &options)?,
        FormatInputKind::Markdown => format_markdown(&source, &options)?,
    };
    if args.check > 0 && formatted.changed {
        return Err(MusiError::CheckCommandFailed);
    }
    if args.check == 0 {
        print!("{}", formatted.text);
    }
    Ok(())
}

fn apply_fmt_args(options: &mut FormatOptions, args: &FmtArgs) {
    if let Some(profile) = args.profile {
        options.apply_profile(fmt_profile_arg(profile));
    }
    if let Some(line_width) = args.line_width {
        options.line_width = line_width;
    }
    if let Some(indent_width) = args.indent_width {
        options.indent_width = indent_width;
    }
    if args.use_tabs > 0 {
        options.use_tabs = true;
    }
    if args.use_spaces > 0 {
        options.use_tabs = false;
    }
    if let Some(indent) = args.match_arm_indent {
        options.match_arm_indent = fmt_match_arm_indent_arg(indent);
    }
    if let Some(alignment) = args.match_arm_arrow_alignment {
        options.match_arm_arrow_alignment = fmt_match_arm_arrow_alignment_arg(alignment);
    }
    if let Some(operator_break) = args.operator_break {
        options.operator_break = fmt_operator_break_arg(operator_break);
    }
    if let Some(ext) = &args.ext {
        options.assume_extension = FormatInputKind::from_extension(ext);
    }
    options.exclude.extend(args.ignore.iter().cloned());
}

const fn fmt_profile_arg(arg: FmtProfileArg) -> FmtProfile {
    match arg {
        FmtProfileArg::Standard => FmtProfile::Standard,
        FmtProfileArg::Compact => FmtProfile::Compact,
        FmtProfileArg::Expanded => FmtProfile::Expanded,
    }
}

const fn fmt_match_arm_indent_arg(arg: FmtMatchArmIndentArg) -> MatchArmIndent {
    match arg {
        FmtMatchArmIndentArg::PipeAligned => MatchArmIndent::PipeAligned,
        FmtMatchArmIndentArg::Block => MatchArmIndent::Block,
    }
}

const fn fmt_match_arm_arrow_alignment_arg(
    arg: FmtMatchArmArrowAlignmentArg,
) -> MatchArmArrowAlignment {
    match arg {
        FmtMatchArmArrowAlignmentArg::None => MatchArmArrowAlignment::None,
        FmtMatchArmArrowAlignmentArg::Consecutive => MatchArmArrowAlignment::Consecutive,
        FmtMatchArmArrowAlignmentArg::Block => MatchArmArrowAlignment::Block,
    }
}

const fn fmt_operator_break_arg(arg: FmtOperatorBreakArg) -> OperatorBreak {
    match arg {
        FmtOperatorBreakArg::Before => OperatorBreak::Before,
        FmtOperatorBreakArg::After => OperatorBreak::After,
    }
}

#[derive(Debug, Clone)]
struct FmtSession {
    base_dir: PathBuf,
    invocation_dir: PathBuf,
    options: FormatOptions,
}

fn fmt_session(args: &FmtArgs) -> MusiResult<FmtSession> {
    validate_fmt_extension(args)?;
    let current = current_dir().map_err(|_| MusiError::MissingCurrentDirectory)?;
    if args.no_config > 0 {
        let mut options = FormatOptions::default();
        apply_fmt_args(&mut options, args);
        return Ok(FmtSession {
            base_dir: current.clone(),
            invocation_dir: current,
            options,
        });
    }
    if let Some(config) = &args.config {
        let project = load_project(config, ProjectOptions::default())?;
        let mut options = FormatOptions::from_manifest(project.manifest().fmt.as_ref());
        apply_fmt_args(&mut options, args);
        return Ok(FmtSession {
            base_dir: project.root_dir().to_path_buf(),
            invocation_dir: current,
            options,
        });
    }
    let anchor = args
        .paths
        .iter()
        .find(|path| path.as_os_str() != "-")
        .cloned()
        .unwrap_or_else(|| current.clone());
    let project = load_project_ancestor(&anchor, ProjectOptions::default()).ok();
    let base_dir = project.as_ref().map_or_else(
        || current.clone(),
        |project| project.root_dir().to_path_buf(),
    );
    let mut options = project
        .as_ref()
        .map_or_else(FormatOptions::default, |project| {
            FormatOptions::from_manifest(project.manifest().fmt.as_ref())
        });
    apply_fmt_args(&mut options, args);
    Ok(FmtSession {
        base_dir,
        invocation_dir: current,
        options,
    })
}

fn stdin_format_kind(args: &FmtArgs) -> MusiResult<FormatInputKind> {
    args.ext
        .as_deref()
        .map_or(Ok(FormatInputKind::Musi), |ext| {
            FormatInputKind::from_extension(ext).ok_or_else(|| {
                FormatError::UnsupportedExtension {
                    extension: ext.to_owned(),
                }
                .into()
            })
        })
}

fn validate_fmt_extension(args: &FmtArgs) -> MusiResult {
    if let Some(ext) = &args.ext
        && FormatInputKind::from_extension(ext).is_none()
    {
        return Err(FormatError::UnsupportedExtension {
            extension: ext.to_owned(),
        }
        .into());
    }
    Ok(())
}

fn watch_fmt_project(args: &FmtArgs) -> MusiResult {
    let mut session = fmt_session(args)?;
    session
        .options
        .exclude
        .extend(args.watch_exclude.iter().cloned());
    let mut last = watch_snapshot(&session.base_dir, &session.options)?;
    run_fmt_once(args, &session)?;
    loop {
        thread::sleep(Duration::from_millis(500));
        let next = watch_snapshot(&session.base_dir, &session.options)?;
        if next != last {
            if args.no_clear_screen == 0 {
                print!("\x1b[2J\x1b[H");
            }
            run_fmt_once(args, &session)?;
            last = watch_snapshot(&session.base_dir, &session.options)?;
        }
    }
}

fn watch_snapshot(
    root: &Path,
    options: &FormatOptions,
) -> MusiResult<Vec<(PathBuf, Option<SystemTime>)>> {
    let summary = format_paths(&[root.to_path_buf()], root, options, true)?;
    let mut snapshot = Vec::new();
    for file in summary.files {
        let modified = fs::metadata(&file.path)
            .ok()
            .and_then(|metadata| metadata.modified().ok());
        snapshot.push((file.path, modified));
    }
    snapshot.sort_by(|left, right| left.0.cmp(&right.0));
    Ok(snapshot)
}
