# mini-fs, a minimal file-system-like thing

What exactly is a file system? Roughly, it is two maps (one for dirs,
one for files). But there is also the POSIX API etc. This is a minimal
version, suitable to understand single-process use of a filesystem.

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
| fuse_in_mem_main  | Fuse filesystem, backed by in-memory stores      |
| fuse_nfs_client   | Fuse filesystem, backed by remote "NFS" server   |
| nfs_client_test   | Run some simple commands against a remote server |
| nfs_server_in_mem | "NFS" server, backed by in-memory stores         |
| nfs_server_unix   | "NFS" server, backed by local filesystem         |

## Running the executables

| Flag                 | Description                                               |
| -------------------- | --------------------------------------------------------- |
| -f                   | foreground                                                |
| -o allow_other       | needed if run as user, but losetup as root                |
| -o max_write=1048576 | don't attempt writes larger than this; 1048756 = 2^20, 1M |

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



## Existence of tmp.txt

This is used to get hold of some dummy permissions etc. Probably a FIXME

