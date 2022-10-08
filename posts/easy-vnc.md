---
title: Quick and dirty remote desktop on a headless server with NixOS
date: 2022-02-18
tags: [post]
---

This setup is entirely due to [Niklas Hambüchen](https://nh2.me/), sharing it here since it is tremendously useful.

Let's say you [have a server without a graphics card,](/posts/hetzner-zfs.html) and you want to use graphical programs directly on it. Here is a 3 step procedure to get a remote desktop supporting OpenGL applications:

1. Install `turbovnc`, for example by putting it into `environment.systemPackages`. TurboVNC supports software rendering through LLVMpipe, which is a software rasterizer for Mesa, which in turn is the most popular open-source implementation of OpenGL and in general the Linux graphics stack.
2. Set `hardware.opengl.enable = true` in `/etc/nixos/configuration.nix`. This will create `/run/opengl-driver`, containing the shared libraries that OpenGL applications will need to load.
3. Start the TurboVNC server with

        server% Xvnc :30 -iglx -httpd "$(dirname $(readlink -f $(command -v Xvnc)))/../share/turbovnc/classes" -depth 24 -rfbwait 120000 -deferupdate 1 -localhost -verbose -securitytypes none

Note that the server will only listen to `localhost`, to defer security to SSH.

All we need to do on the client is to port-forward the TurboVNC port and connect:

```
client% ssh <server> -L 5930:localhost:5930
client% vncviewer -securitytypes none :30 -DesktopSize=2500x1350 -Scale=150&
```

Here I'm setting a size and a scale manually, but you get the idea.

The X server started by TurboVNC is very bare, but one can start a terminal manually and then go from there:

```
server% DISPLAY=:30 xfce-terminal&
```

I use `openbox` for simple window management, by just starting it from the terminal spawned above.
