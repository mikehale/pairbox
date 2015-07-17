# pairbox

A vagrant setup for pairing via tmux. Features things commonly needed to pair on ruby and Go projects.

## Using

1. `bin/setup`
2. `vagrant ssh`
3. work on things in `/projects`

Currently `/projects` in the VM is set up as a synced folder to `~/Projects` on the host.

The best way to get together is `vagrant share --ssh --disable-http` and `vagrant connect` but you can also figure out your own ssh tunneling if you want. Tmate.io is a good option.

## TODO

* Pluggable .netrc
* GitHub auth
* git config
* per user emacs config
* shell aliases
