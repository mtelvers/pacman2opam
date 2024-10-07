# pacman2opam

- opam can't handle a package called "opam"
- package descriptions have depends and provides
- packages can depend upon other packages or other packges by virtue of a provide

# Usage

Run this project with

```
dune exec -- pacman2opam
```

# Test with Ryan's solver

```
git clone https://github.com/RyanGibb/opam-0install-solver
dune exec -- bin/main.exe --repo ~/pacman2opam/archlinux/packages vim
```

# Test with Docker

```
docker run -v ~/pacman2opam/archlinux:/root/archlinux --rm -it archlinux bash
curl -L https://github.com/ocaml/opam/releases/download/2.2.1/opam-2.2.1-x86_64-linux -o /usr/bin/opam
chmod +x /usr/bin/opam
opam init -k local --bare --bypass-checks -a -y /root/archlinux
opam switch create archlinux --empty
OPAMJOBS=1 opam install vim -y
```

