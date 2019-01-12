#include <fcntl.h>
#include <unistd.h>

int pipe2(int pipefd[2], int flags) {
  int result = pipe(pipefd);
  if (result == 0 && (flags & O_CLOEXEC) != 0) {
    fcntl(pipefd[0], F_SETFD, FD_CLOEXEC);
    fcntl(pipefd[1], F_SETFD, FD_CLOEXEC);
  }
  return result;
}
