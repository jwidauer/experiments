#include <csetjmp>
#include <csignal>
#include <cstdio>
#include <iostream>

namespace {

void print_signal_status(const sigset_t& set, int signal, const char* signal_name) {
  bool is_blocked = sigismember(&set, signal) != 0;
  std::cout << signal_name << " (" << signal << ") is " << (is_blocked ? "blocked\n" : "unblocked\n");
}

// Print the whether SIGUSR1 and SIGUSR2 are blocked or unblocked
void print_signal_status() {
  sigset_t set;
  sigemptyset(&set);

  if (sigprocmask(SIG_BLOCK, nullptr, &set) == -1) {
    std::puts("Error checking signals\n");
    return;
  }

  print_signal_status(set, SIGUSR1, "SIGUSR1");
  print_signal_status(set, SIGUSR2, "SIGUSR2");
  std::puts("");
}

// The jump state for setjump/longjmp
sigjmp_buf env;

// Signal handler that just prints the signal status
void signal_handler(int signal) {
  std::cout << "Received signal: " << signal << "\n";
  print_signal_status();
  siglongjmp(env, 1);
}

void setup_signal_handler() {
  struct sigaction sa;
  sa.sa_handler = signal_handler;
  sigemptyset(&sa.sa_mask);
  sa.sa_flags = 0;

  if (sigaction(SIGUSR1, &sa, nullptr) == -1) {
    std::puts("Error setting up SIGUSR1 handler\n");
  }
}

}  // namespace

auto main() -> int {
  // Block SIGUSR2
  sigset_t set;
  sigemptyset(&set);
  sigaddset(&set, SIGUSR2);
  if (sigprocmask(SIG_BLOCK, &set, nullptr) == -1) {
    std::puts("Error blocking SIGUSR1\n");
    return EXIT_FAILURE;
  }

  std::puts("After blocking SIGUSR2");
  print_signal_status();

  // Set up jump state
  if (sigsetjmp(env, 1)) {
    std::puts("Returned from signal handler");
    print_signal_status();
    return EXIT_SUCCESS;
  }

  // Set up signal handler for SIGUSR1
  setup_signal_handler();

  // Send SIGUSR1 to self
  if (kill(getpid(), SIGUSR1) == -1) {
    std::puts("Error sending SIGUSR1\n");
    return EXIT_FAILURE;
  }

  std::puts("After sending SIGUSR1");
  print_signal_status();

  return EXIT_SUCCESS;
}
