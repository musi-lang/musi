use musi_native::NativeTestReport;
use musi_project::{Project, ProjectTestTarget};

pub(super) struct TestModuleReport {
    pub(super) label: String,
    pub(super) report: NativeTestReport,
}

pub(super) fn print_test_reports(reports: &[TestModuleReport]) -> usize {
    let mut passed_files = 0usize;
    let mut failed_files = 0usize;
    let mut passed_cases = 0usize;
    let mut failed_cases = 0usize;
    for module in reports {
        let module_failed = module
            .report
            .cases
            .iter()
            .any(|test_case| !test_case.passed);
        if module_failed {
            failed_files += 1;
        } else {
            passed_files += 1;
        }
        let status = if module_failed { "×" } else { "✓" };
        println!(" {status} {} ({})", module.label, module.report.cases.len());
        for test_case in &module.report.cases {
            let status = if test_case.passed {
                passed_cases += 1;
                "✓"
            } else {
                failed_cases += 1;
                "×"
            };
            println!(
                "   {status} {} > {}",
                test_case.suite.as_ref(),
                test_case.name.as_ref()
            );
        }
        if module_failed {
            print_captured_output(&module.label, "stdout", module.report.stdout.as_ref());
            print_captured_output(&module.label, "stderr", module.report.stderr.as_ref());
        }
    }
    println!();
    println!(
        " Test Files  {} passed{} ({})",
        passed_files,
        failed_summary_suffix(failed_files),
        reports.len()
    );
    println!(
        "      Tests  {} passed{} ({})",
        passed_cases,
        failed_summary_suffix(failed_cases),
        passed_cases + failed_cases
    );
    failed_cases
}

fn print_captured_output(module: &str, stream: &str, output: &str) {
    if output.is_empty() {
        return;
    }
    println!("   --- {module} {stream} ---");
    for line in output.lines() {
        println!("   {line}");
    }
}

fn failed_summary_suffix(failed: usize) -> String {
    if failed == 0 {
        String::new()
    } else {
        format!(", {failed} failed")
    }
}

pub(super) fn test_target_label(project: &Project, test: &ProjectTestTarget) -> String {
    test.path.strip_prefix(project.root_dir()).map_or_else(
        |_| test.module_key.as_str().to_owned(),
        |path| path.display().to_string(),
    )
}
