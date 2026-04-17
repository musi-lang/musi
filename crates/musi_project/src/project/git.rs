use std::ffi::OsStr;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

use crate::ProjectResult;
use crate::errors::ProjectError;
use crate::lock::{LockedPackageSource, Lockfile};
use crate::project::storage::ProjectStorage;
use crate::registry::stable_cache_key;

pub(super) struct GitRequirement {
    pub(super) url: String,
    pub(super) reference: String,
}

impl GitRequirement {
    pub(super) fn parse(name: &str, requirement: &str) -> ProjectResult<Self> {
        let Some(rest) = requirement.trim().strip_prefix("git+") else {
            return Err(ProjectError::InvalidGitDependency {
                name: name.into(),
                requirement: requirement.into(),
            });
        };
        if rest.is_empty() {
            return Err(ProjectError::InvalidGitDependency {
                name: name.into(),
                requirement: requirement.into(),
            });
        }
        let (url, reference) = rest
            .split_once('#')
            .map_or((rest, "HEAD"), |(url, reference)| {
                (
                    url,
                    if reference.is_empty() {
                        "HEAD"
                    } else {
                        reference
                    },
                )
            });
        if url.is_empty() {
            return Err(ProjectError::InvalidGitDependency {
                name: name.into(),
                requirement: requirement.into(),
            });
        }
        Ok(Self {
            url: url.into(),
            reference: reference.into(),
        })
    }
}

#[derive(Debug, Clone)]
pub(super) struct GitPackage {
    pub(super) commit: String,
    pub(super) checkout_dir: PathBuf,
}

pub(super) fn locked_or_latest_git_package(
    name: &str,
    requirement: &GitRequirement,
    storage: &ProjectStorage,
    lockfile: &Lockfile,
) -> ProjectResult<GitPackage> {
    if let Some(package) = lockfile.packages.iter().find(|package| {
        package.name == name
            && matches!(
                &package.source,
                LockedPackageSource::Git {
                    url,
                    reference,
                    ..
                } if url == &requirement.url && reference == &requirement.reference
            )
    }) && let LockedPackageSource::Git { commit, .. } = &package.source
    {
        ensure_git_repo(requirement, storage)?;
        return checkout_git_package(name, requirement, storage, commit);
    }
    let commit = resolve_git_commit(name, requirement, storage)?;
    checkout_git_package(name, requirement, storage, &commit)
}

fn resolve_git_commit(
    name: &str,
    requirement: &GitRequirement,
    storage: &ProjectStorage,
) -> ProjectResult<String> {
    let repo_dir = git_repo_dir(storage, requirement);
    ensure_git_repo(requirement, storage)?;
    git_output(&repo_dir, ["rev-parse", requirement.reference.as_str()])
        .map(|commit| commit.trim().to_owned())
        .map_err(|_| ProjectError::GitReferenceNotFound {
            name: name.into(),
            reference: requirement.reference.clone(),
        })
}

fn ensure_git_repo(requirement: &GitRequirement, storage: &ProjectStorage) -> ProjectResult {
    let repo_dir = git_repo_dir(storage, requirement);
    if repo_dir.exists() {
        run_git(&repo_dir, ["fetch", "--quiet", "--all", "--tags"])?;
    } else {
        let parent = repo_dir
            .parent()
            .ok_or_else(|| ProjectError::MissingPackageRoot {
                path: repo_dir.clone(),
            })?;
        fs::create_dir_all(parent).map_err(|source| ProjectError::ProjectIoFailed {
            path: parent.to_path_buf(),
            source,
        })?;
        run_git_at(
            parent,
            ["clone", "--quiet", requirement.url.as_str(), "repo"],
            "git clone --quiet",
        )?;
    }
    Ok(())
}

fn checkout_git_package(
    _name: &str,
    requirement: &GitRequirement,
    storage: &ProjectStorage,
    commit: &str,
) -> ProjectResult<GitPackage> {
    let repo_dir = git_repo_dir(storage, requirement);
    let checkout_dir = repo_dir
        .parent()
        .ok_or_else(|| ProjectError::MissingPackageRoot {
            path: repo_dir.clone(),
        })?
        .join("checkouts")
        .join(commit);
    if !checkout_dir.exists() {
        let parent = checkout_dir
            .parent()
            .ok_or_else(|| ProjectError::MissingPackageRoot {
                path: checkout_dir.clone(),
            })?;
        fs::create_dir_all(parent).map_err(|source| ProjectError::ProjectIoFailed {
            path: parent.to_path_buf(),
            source,
        })?;
        let checkout_name = checkout_dir
            .file_name()
            .and_then(OsStr::to_str)
            .unwrap_or(commit);
        run_git_at(
            parent,
            [
                "clone",
                "--quiet",
                repo_dir.to_string_lossy().as_ref(),
                checkout_name,
            ],
            "git clone --quiet checkout",
        )?;
        run_git(&checkout_dir, ["checkout", "--quiet", commit])?;
    }
    Ok(GitPackage {
        commit: commit.into(),
        checkout_dir,
    })
}

fn git_repo_dir(storage: &ProjectStorage, requirement: &GitRequirement) -> PathBuf {
    storage
        .global_cache_dir
        .join("git")
        .join(stable_cache_key(&requirement.url))
        .join("repo")
}

fn run_git<const N: usize>(path: &Path, args: [&str; N]) -> ProjectResult {
    let command = format!("git {}", args.join(" "));
    run_git_at(path, args, command)
}

fn run_git_at<const N: usize>(
    path: &Path,
    args: [&str; N],
    command: impl Into<String>,
) -> ProjectResult {
    let status = Command::new("git")
        .args(args)
        .current_dir(path)
        .status()
        .map_err(|source| ProjectError::ProjectIoFailed {
            path: path.to_path_buf(),
            source,
        })?;
    if status.success() {
        Ok(())
    } else {
        Err(ProjectError::GitCommandFailed {
            command: command.into(),
            path: path.to_path_buf(),
        })
    }
}

fn git_output<const N: usize>(path: &Path, args: [&str; N]) -> ProjectResult<String> {
    let output = Command::new("git")
        .args(args)
        .current_dir(path)
        .output()
        .map_err(|source| ProjectError::ProjectIoFailed {
            path: path.to_path_buf(),
            source,
        })?;
    if output.status.success() {
        Ok(String::from_utf8_lossy(&output.stdout).into_owned())
    } else {
        Err(ProjectError::GitCommandFailed {
            command: format!("git {}", args.join(" ")),
            path: path.to_path_buf(),
        })
    }
}
