#include <stdint.h>
#include "config.h"

struct proc {
    uint64_t fds[NR_FDS];
};

struct file {
    uint64_t refcount;
};

struct proc procs[NR_PROCS];
struct file files[NR_FILES];
uint64_t current;

int close(long fd)
{
    struct proc *proc;
    uint64_t fileid;
    struct file *file;

    proc = &procs[current];
    if (fd < 0 || fd >= NR_FDS)
        return -1;
    fileid = proc->fds[fd];
    if (fileid >= NR_FILES)
        return -1;
    file = &files[fileid];
    --file->refcount;
    return 0;
}
