pub fn print_help() {
    let tasks = [("codegen", "Generate codes from ungrammar")];

    let max_task_len = tasks.iter().map(|x| x.0.len()).max().unwrap_or(10);

    eprintln!(
        "Tasks:\n\n{}",
        tasks
            .iter()
            .map(|(code, description)| format!(
                "{}{} - {}",
                code,
                " ".repeat(max_task_len - code.len()), // manual pading
                description
            ))
            .collect::<Vec<String>>()
            .join("\n")
    )
}
