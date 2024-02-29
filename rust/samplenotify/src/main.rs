use notify::{Watcher, RecursiveMode, Result};
use std::{path::Path, time::Duration, thread};

fn main() -> Result<()> {
    // Automatically select the best implementation for your platform.
    let mut watcher = notify::recommended_watcher(|res| {
        match res {
           Ok(event) => println!("event: {:?}", event),
           Err(e) => println!("watch error: {:?}", e),
        }
    })?;

    // Add a path to be watched. All files and directories at that path and
    // below will be monitored for changes.
    watcher.watch(Path::new("."), RecursiveMode::Recursive)?;

    // Loop to keep the program running, allowing the watcher to continue monitoring
    loop {
        // Sleep for a certain duration to prevent the loop from consuming too much CPU
        thread::sleep(Duration::from_secs(1));
    }
}
