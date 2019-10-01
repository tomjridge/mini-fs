-**- org -**-

mini-fs, a minimal file-system-like thing
=========================================

What exactly is a file system? Roughly, it is two maps (one for dirs,
one for files). But there is also the POSIX API etc. This is a minimal
version, suitable to understand single-process use of a filesystem.

Dependencies
============

opam packages:

  --------------------- ------------------ --------------------
  ocamlfuse             for FUSE support   ocamlfind lib Fuse
  ppx~binprot~          deriving bin~io~   
  ppx~derivingyojson~   deriving yojson    
  extunix               pread, pwrite      
  --------------------- ------------------ --------------------

tjr:

  --------------------- ------------
  tjr~pathresolution~   
  tjr~net~              networking
  --------------------- ------------

old:

  ---------- -------------
  extlib     
  core       
  lwt        nfs~client~
  lwt.unix   
  ---------- -------------
