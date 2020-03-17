#include <fcntl.h>
#include <poll.h>
#include <errno.h>
#include <sys/wait.h>

int SPAWN_O_NONBLOCK = O_NONBLOCK;
int SPAWN_O_CLOEXEC = O_CLOEXEC;
int SPAWN_WNOHANG = WNOHANG;
int SPAWN_F_SETFL = F_SETFL;
int SPAWN_EINTR = EINTR;
int SPAWN_EAGAIN = EAGAIN;
unsigned short SPAWN_POLLIN = POLLIN;
unsigned short SPAWN_POLLOUT = POLLOUT;

extern int SPAWN_NONBLOCK;
extern int SPAWN_CLOEXEC;
extern int SPAWN_WNOHANG;
extern int SPAWN_F_SETFL;
extern int SPAWN_EINTR;
extern int SPAWN_EAGAIN;
extern unsigned short SPAWN_POLLIN;
extern unsigned short SPAWN_POLLOUT;
