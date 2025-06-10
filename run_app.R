# ML Model Builder Startup Script
# This script ensures all packages are installed and starts the application

# Function to display startup banner
display_banner <- function() {
  cat("\n")
  cat("╔═══════════════════════════════════════════════════════════════╗\n")
  cat("║                    ML Model Builder                          ║\n")
  cat("║                                                               ║\n")
  cat("║  A comprehensive Shiny application for machine learning      ║\n")
  cat("║  model development, training, and diagnostics                ║\n")
  cat("║                                                               ║\n")
  cat("╚═══════════════════════════════════════════════════════════════╝\n")
  cat("\n")
}

# Function to check R version
check_r_version <- function() {
  r_version <- R.version.string
  cat("R Version:", r_version, "\n")
  
  # Check if R version is sufficient (>= 4.0.0)
  if (getRversion() < "4.0.0") {
    cat("⚠️  Warning: R version 4.0.0 or higher is recommended for best compatibility.\n")
    cat("   Your current version is:", as.character(getRversion()), "\n")
    cat("   Some features may not work as expected.\n\n")
  } else {
    cat("✅ R version is compatible.\n\n")
  }
}

# Function to install packages using pak
install_packages_with_pak <- function() {
  cat("Installing packages using pak (fast dependency resolver)...\n")
  
  # Source the installation script
  source("install_packages.R", local = TRUE)
  
  # Run the installation function
  success <- install_required_packages()
  
  if (success) {
    cat("✅ Package installation completed successfully!\n\n")
    return(TRUE)
  } else {
    cat("❌ Some packages failed to install. Please check the messages above.\n")
    cat("You may need to install them manually or restart R and try again.\n\n")
    return(FALSE)
  }
}

# Function to start the Shiny application
start_application <- function(port = 3838, host = "127.0.0.1") {
  cat("Starting ML Model Builder application...\n")
  cat("Application will be available at: http://", host, ":", port, "\n", sep = "")
  cat("Press Ctrl+C or close this window to stop the application.\n\n")
  
  # Check if we're in the correct directory
  if (!file.exists("app.R")) {
    cat("❌ Error: app.R not found in current directory.\n")
    cat("Please make sure you're in the ml-model-builder directory.\n")
    return(FALSE)
  }
  
  # Start the application
  tryCatch({
    shiny::runApp(
      appDir = ".",
      port = port,
      host = host,
      launch.browser = TRUE
    )
  }, error = function(e) {
    cat("❌ Error starting application:", e$message, "\n")
    cat("Try running the application manually with: shiny::runApp()\n")
    return(FALSE)
  })
}

# Main execution function
main <- function() {
  display_banner()
  
  cat("Initializing ML Model Builder...\n\n")
  
  # Check R version
  check_r_version()
  
  # Check if install_packages.R exists
  if (!file.exists("install_packages.R")) {
    cat("❌ Error: install_packages.R not found.\n")
    cat("Please make sure all files are in the correct location.\n")
    return()
  }
  
  # Install packages
  cat("Step 1: Checking and installing required packages...\n")
  packages_ok <- install_packages_with_pak()
  
  if (!packages_ok) {
    cat("Package installation issues detected. Do you want to continue anyway? (y/n): ")
    response <- readline()
    if (tolower(response) != "y") {
      cat("Exiting. Please resolve package issues and try again.\n")
      return()
    }
  }
  
  # Start application
  cat("Step 2: Starting the application...\n")
  start_application()
}

# Command line arguments handling
args <- commandArgs(trailingOnly = TRUE)

if (length(args) > 0) {
  if (args[1] == "--install-only") {
    display_banner()
    cat("Installing packages only...\n\n")
    source("install_packages.R", local = TRUE)
    install_required_packages()
  } else if (args[1] == "--check-only") {
    display_banner()
    cat("Checking packages only...\n\n")
    source("install_packages.R", local = TRUE)
    check_packages()
  } else if (args[1] == "--help") {
    display_banner()
    cat("Usage:\n")
    cat("  Rscript run_app.R                 # Install packages and start app\n")
    cat("  Rscript run_app.R --install-only  # Only install packages\n")
    cat("  Rscript run_app.R --check-only    # Only check if packages are installed\n")
    cat("  Rscript run_app.R --help          # Show this help message\n")
  } else {
    cat("Unknown argument:", args[1], "\n")
    cat("Use --help for usage information.\n")
  }
} else {
  # Default behavior: install packages and start app
  main()
}
