---
title: Persistent SSH sessions with proper scrollback
date: 2017-02-11
---

**TL;DR:** Put the code snippet below in `~/.profile` or whatever startup
script for your shell, and then use `autoscreen <ssh-arguments>` to get
persistent SSH sessions.

As many of us, I often find myself working on remote machines through SSH while on the
go. Doing this is problematic if you're on unreliable connections, or simply want
to close your laptop and continue working later.

The most common solution for this problem is to use a terminal multiplexer
like `screen` or `tmux` to make sure that you won't lose whatever you're
doing if the connection goes down. However, I really don't like terminal
multiplexers, chiefly because I cannot comfortably scroll and copy
paste. I know that both `screen` and `tmux` and `dvtm` and probably many
others incorporate various facilities to be able to achieve scrolling
and copy-pasting, but I've always found them much less convenient than
just using the cursor, the scrollwheel, and page up/down. Also note
that `mosh` does not offer scrollback either.

However, after much tribulation I think we have found the ideal setup
after a nice session with [Niklas Hambüchen](http://github.com/nh2). The
idea is: connect to the remote server and immediately spawn a `screen`
session, but instruct `screen` to display the entire scrollback rather
than just a screenful. Moreover, use `autossh` to automatically reconnect
and reattach to the session.

It all comes together in this `bash` function to put in your `.profile`
or equivalent:

```
function autoscreen() {
  AUTOSSH_GATETIME=5 autossh -M 0 -- -o "ServerAliveInterval 5" -o "ServerAliveCountMax 1" -t $@ $'bash -c \'tmpScreenConfig=$(mktemp); echo "termcapinfo xterm* ti@:te" >> $tmpScreenConfig; echo "altscreen on" >> $tmpScreenConfig; echo "maptimeout 0" >> $tmpScreenConfig; echo "startup_message off" >> $tmpScreenConfig; echo "msgwait 0" >> $tmpScreenConfig; exec screen -c $tmpScreenConfig -S "autosession-'$RANDOM$RANDOM$RANDOM$RANDOM$'" -RD\''
}
```

Now that I'm using NixOS, I just have the following in my `systemPackages`, which is even handier since it'll be available everywhere, not only in `bash`:

```
autoscreen = pkgs.writeScriptBin "autoscreen" ''
  AUTOSSH_GATETIME=5 ${pkgs.autossh}/bin/autossh -M 0 -- -o "ServerAliveInterval 5" -o "ServerAliveCountMax 1" -t $@ $'bash -c \'tmpScreenConfig=$(mktemp); echo "termcapinfo xterm* ti@:te" >> $tmpScreenConfig; echo "altscreen on" >> $tmpScreenConfig; echo "maptimeout 0" >> $tmpScreenConfig; echo "startup_message off" >> $tmpScreenConfig; echo "msgwait 0" >> $tmpScreenConfig; exec screen -c $tmpScreenConfig -S "autosession-'$RANDOM$RANDOM$RANDOM$RANDOM$'" -RD\' '
'';
```

You can then use `autoscreen` like you'd use `ssh`, e.g.

```
$ autoscreen your-server.com
```

and if the connection goes down for whatever reason `autossh` will soon
reconnect and reattach the session. All of this while still having proper
scrollback, copy paste, and reflow!

Note that even if your client dies you can still recover the session
by manually listing them and trying out the ones with an `autosession-`
prefix.

You can check out a video of the above in action at
<http://mazzo.li/dump/autoscreen.mov>.

For the curious, explanation of the parameters:

* `AUTOSSH_GATETIME` makes `autossh` "validate" the connection
  in just 5 seconds rather than the default 30;
* `-M 0` disables `autossh` built-in keepalive, since we leverage `ServerAliveInterval` and `ServerAliveCountMax`
  to notice when the connection dies;
* On connection we immediately invoke `bash` to prepare an adequate `screen`
  config, save it to a temp file, and then start `screen`;
* The `screen` config contains:
    - `termcapinfo xterm* ti@:te`, which makes `screen` not display
      just a screenful of test but rather to have it to just output
      everything to the terminal's scrollback (see <http://aperiodic.net/screen/faq>);
    - A side effect of the above is that you'll get the output
      of `less` or `vim` in the scrollback, which is inconvenient.
      `altscreen on` removes this side effect;
    - `maptimeout 0` minimize the delay when pressing ESC, which
    is very annoying when using vim or in my case kakoune. Note that to make
    kakoune or vim register ESC keys inside `screen` you also need to [turn
    off mouse integration](https://github.com/mawww/kakoune/issues/1858#issuecomment-939427528). 
    - `startup_message off` and `msgwait 0` which removes `screen` startup messages.
* The session name is composed of `autosession-` postfixed
  with 4 random numbers;
* The `-RD` option that we pass to screen makes it so that
  if the session does not exists it gets created, if it does
  `screen` reattaches it, which means that on the first connection
  we'll create a new session, and then we'll reconnect.

Check the man pages for `autossh`, `ssh`, and `screen` for more
info.
