# miniFS, a minimal file-system-like thing

What exactly is a file system? Roughly, it is two maps (one for dirs,
one for files). But there is also the POSIX API etc. This is a minimal
version, suitable to understand single-process use of a filesystem.



## Related packages

<img src="https://docs.google.com/drawings/d/e/2PACX-1vSqzipIxfOtcWhtSEqcBUpEKPVp1ALtHYyVVBldz7WNP3idcaQTY0iHoLBMf9n4vNMUjDvoIi_gr2gE/pub?w=1034&amp;h=520">



## Modules



<img src="https://docs.google.com/drawings/d/e/2PACX-1vQcYK7STiRj8bxZBFvkYaqQtsAXxCrjAKrfD2GV_uDuec5DmEY3qNq1Nbj_h-om3L1HHA5JY_RBd_uF/pub?w=953&amp;h=352">

The code is modularized. So, for example, it is possible to take the FUSE filesystem operations (which are parameterized over an arbitrary implementation of FS ops), and combine with the NFS client ops to get an NFS client mounted via FUSE. On the server, you can take the NFS server (again, paramterized by some backend) and combine with the in-memory implementation to get an in-memory NFS server. Alternatively, you can combine with the "Unix ops" (a passthrough to an underlying POSIX filesystem), to serve local files over the network. Cool!



## Docs

<https://tomjridge.github.io/mini-fs/>




## Dependencies

| Dependency                   | Description  |
| ---------------------------- | ------------ |
| ocamlfuse                    | FUSE support |
| ppx_binprot                  |              |
| ppx_yojson                   |              |
| extunix                      | pread,pwrite |
| tjr_path_resolution, tjr_net |              |



## Executables

In bin/ there are some executables:

| Name              | Description                                      |
| ----------------- | ------------------------------------------------ |
| fuse_in_mem_main  | Fuse filesystem, backed by in-memory store       |
| fuse_nfs_client   | Fuse filesystem, backed by remote "NFS" server   |
| nfs_client_test   | Run some simple commands against a remote server |
| nfs_server_in_mem | "NFS" server, backed by in-memory store          |
| nfs_server_unix   | "NFS" server, backed by local filesystem         |

## Running the executables

The executables typically invoke `Fuse.main Sys.argv`. At such, command line arguments follow those provided by `Fuse.main`. A full list of options can be displayed by typing e.g. `./fuse_in_mem_main.exe --help`



Some examples:

| Flag                 | Description                                               |
| -------------------- | --------------------------------------------------------- |
| -f                   | foreground                                                |
| -s                   | single-threaded (almost certainly necessary)              |
| -o allow_other       | needed if run as user, but losetup as root                |
| -o max_write=1048576 | don't attempt writes larger than this; 1048756 = 2^20, 1M |
| -o auto_unmount      | unmount if/when the process dies                          |
| -o big_writes        | enable larger than 4kB writes                             |

Typical example: `./fuse_nfs_client.exe -s -f -o auto_unmount ./fuse_mount`

NOTE: probably run the server in a separate xterm, so that emacs buffering doesn't kill everything



Actually, now we are dunified, we have to run as "dune exec bin/fuse_in_mem_main.exe" or similar. But this messes up the flags. So we copy the executables from _build:

~~~
find _build -name "*.exe" -exec cp \{\} .
~~~

This is now included in Makefile. Then we run an executable as:

```
./fuse_in_mem_main.exe -f ./fuse_mount
# ./xxx -o allow_other /mnt/fuse
# eg ./fuse_in_mem_main -o allow_other /mnt/fuse
```



## Addendum: Known bugs

* Recursive copies of directories seems to hang in emacs for some reason, but cp works fine from the command line FIXED this was due to multithreading - you need to make sure you run FUSE in single-threaded mode



## Addendum: FUSE options

These are the options listed on my system at 2019-12-02:

~~~
(python3) > ./fuse_in_mem_main.exe --help
usage: ./fuse_in_mem_main.exe mountpoint [options]

general options:
    -o opt,[opt...]        mount options
    -h   --help            print help
    -V   --version         print version

FUSE options:
    -d   -o debug          enable debug output (implies -f)
    -f                     foreground operation
    -s                     disable multi-threaded operation

    -o allow_other         allow access to other users
    -o allow_root          allow access to root
    -o auto_unmount        auto unmount on process termination
    -o nonempty            allow mounts over non-empty file/dir
    -o default_permissions enable permission checking by kernel
    -o fsname=NAME         set filesystem name
    -o subtype=NAME        set filesystem type
    -o large_read          issue large read requests (2.4 only)
    -o max_read=N          set maximum size of read requests

    -o hard_remove         immediate removal (don't hide files)
    -o use_ino             let filesystem set inode numbers
    -o readdir_ino         try to fill in d_ino in readdir
    -o direct_io           use direct I/O
    -o kernel_cache        cache files in kernel
    -o [no]auto_cache      enable caching based on modification times (off)
    -o umask=M             set file permissions (octal)
    -o uid=N               set file owner
    -o gid=N               set file group
    -o entry_timeout=T     cache timeout for names (1.0s)
    -o negative_timeout=T  cache timeout for deleted names (0.0s)
    -o attr_timeout=T      cache timeout for attributes (1.0s)
    -o ac_attr_timeout=T   auto cache timeout for attributes (attr_timeout)
    -o noforget            never forget cached inodes
    -o remember=T          remember cached inodes for T seconds (0s)
    -o nopath              don't supply path if not necessary
    -o intr                allow requests to be interrupted
    -o intr_signal=NUM     signal to send on interrupt (10)
    -o modules=M1[:M2...]  names of modules to push onto filesystem stack

    -o max_write=N         set maximum size of write requests
    -o max_readahead=N     set maximum readahead
    -o max_background=N    set number of maximum background requests
    -o congestion_threshold=N  set kernel's congestion threshold
    -o async_read          perform reads asynchronously (default)
    -o sync_read           perform reads synchronously
    -o atomic_o_trunc      enable atomic open+truncate support
    -o big_writes          enable larger than 4kB writes
    -o no_remote_lock      disable remote file locking
    -o no_remote_flock     disable remote file locking (BSD)
    -o no_remote_posix_lock disable remove file locking (POSIX)
    -o [no_]splice_write   use splice to write to the fuse device
    -o [no_]splice_move    move data while splicing to the fuse device
    -o [no_]splice_read    use splice to read from the fuse device

Module options:

[iconv]
    -o from_code=CHARSET   original encoding of file names (default: UTF-8)
    -o to_code=CHARSET	    new encoding of the file names (default: UTF-8)

[subdir]
    -o subdir=DIR	    prepend this directory to all paths (mandatory)
    -o [no]rellinks	    transform absolute symlinks to relative
~~~

