---
title: Remote, encrypted ZFS storage server with NixOS
date: 2021-10-12
tags: [post]
sidenotes: true
description: I explain how to remotely setup a storage server with full disk encryption. The server is rented from [Hetzner](https://www.hetzner.com/), and many of the details are specific to Hetzner. The system is configured with [NixOS](https://nixos.org/), survives up to one disk failure, and is easy to operate without physical access to the machine. After setup we test the monitoring and redundancy of the server through a variety of tests.
image: https://mazzo.li/assets/images/hetzner-datacenterpark-fsn.jpg
---

In this post, I explain how to remotely setup a storage server with full disk encryption. The server is rented from [Hetzner](https://www.hetzner.com/), and many of the details are specific to Hetzner. The system is configured with [NixOS](https://nixos.org/), survives up to one disk failure, and is easy to operate without physical access to the machine. After setup we test the monitoring and redundancy of the server through a variety of tests.

***

I wanted a machine with:

* At least 20TB of storage, and easy to expand in the future if needed;
* Full-disk encryption, but easy to reboot remotely;
* Resiliency to a single hard drive failure --- if an hard drive should fail,
  the server should continue booting and working;
* email notifications when things of notice happen (shutdown, boot, hardware failure, IO errors, etc.);
* A gigabit, unmetered internet connection.

<div>

I had been using a homemade NAS with a fairly typical [ext4](https://en.wikipedia.org/wiki/Ext4) on [LVM](https://en.wikipedia.org/wiki/Logical_Volume_Manager_(Linux)) on [LUKS](https://en.wikipedia.org/wiki/Linux_Unified_Key_Setup) on [RAID](https://en.wikipedia.org/wiki/RAID) stack.[^xfs]

[^xfs]: If I were to setup the NAS now I would probably use [XFS](https://en.wikipedia.org/wiki/XFS) rather than ext4, which would have been a better suited filesystem for a storage server.

The NAS ticked all the boxes above, but I grew tired of having to carry it around when moving and having to find apartments with gigabit connections. Given that we had been renting storage servers from [Hetzner](https://www.hetzner.com/) at work for years (although running [Ceph](https://en.wikipedia.org/wiki/Ceph_(software))), I decided to setup a personal storage server there as well.

Moreover, after a suggestion by [Niklas](https://github.com/nh2/), I also decided to try out [OpenZFS](https://en.wikipedia.org/wiki/OpenZFS), given that it can replace the ext4-LVM-LUKS-RAID stack entirely, and bring a whole set of additional features on top.

</div>

<div>

I quickly selected a server on [Hetzner's server auction](https://www.hetzner.com/sb). For 75EUR / month, at no setup cost, I rented a machine with four 10TB enterprise hard drives. I didn't consider alternatives, since we've used Hetzner for years and we've found them to be an excellent provider.

I then spent a few days installing and testing the software, and since it is not a very streamlined procedure, this article describes what I did in the hope of saving somebody else some time. If you're familiar with NixOS/Hetzner/ZFS already, you can also skip to the [`configuration.nix` file](https://gist.github.com/bitonic/78529d3dd007d779d60651db076a321a) to get a gist of the setup.

Also refer to the [acknowledgements](#acknowledgements) for a series of useful links on these topics.

</div>

## Setup overview

The system is configured with NixOS, which means that the whole configuration will end up being a single ~250 lines file.

The root filesystem is going to be the ZFS filesystem.

We use the `raidz` ZFS configuration to achieve redundancy. This is equivalent to RAID5: one disk is used for parity, which means that the system can survive at most one drive failure. We use ZFS also for full-disk encryption and volume management --- to be able to easily expand the storage space when needed.

If a drive fails, it should be replaced soon! Therefore, we use [`smartd`](https://www.smartmontools.org/) and [ZED](https://manpages.ubuntu.com/manpages/xenial/man8/zed.8.html) to alert us when something goes wrong, and [mailgun](https://www.mailgun.com/) to deliver email. We also [verify](#verify) that these measures work by intentionally messing with the system after the first install.

Since the root ZFS filesystem is encrypted, we need a separate `/boot` partition to start the boot process. However we want redundancy there too, therefore we mirror the `/boot` partition over all the drives, using the [`mirroredBoots` NixOS option](https://search.nixos.org/options?channel=21.05&show=boot.loader.grub.mirroredBoots&from=0&size=50&sort=relevance&type=packages&query=boot.loader.grub.mirroredBoots).

Finally, we setup an SSH server which runs in `initrd`, before the server fully boots, to be able to unlock the encrypted ZFS drive remotely with ease.

## Installing NixOS

### Step 1: booting into the rescue system

The installation is executed from inside the [Hetzner rescue system](https://docs.hetzner.com/robot/dedicated-server/troubleshooting/hetzner-rescue-system/). This loads a live Debian CD onto the server, from which we can setup the disks and install NixOS.

Follow the instructions in the link above to boot into it. At the date of this article, it is important to use the "Linux (old)" image, since it uses a kernel version (5.10) which is compatible with the ZFS shipped by Debian, while the newer image does not.

From the rescue system we will setup the drives, gather needed information about the system, edit `configuration.nix`, install NixOS, and then reboot into it.

### Step 2: setting up the disks

After we've logged into the rescue system, we need to install the ZFS software to be able to create ZFS pools. Following [the instructions](https://openzfs.github.io/openzfs-docs/Getting%20Started/Debian/index.html):

```
rescue# nano /etc/apt/sources.list.d/buster-backports.list # edit as instructed
rescue# nano /etc/apt/preferences.d/90_zfs # edit as instructed
rescue# apt update
rescue# apt install -y dpkg-dev linux-headers-$(uname -r) linux-image-amd64
rescue# apt install -y zfs-dkms zfsutils-linux
```

Note that in my case I got an error complaining that the module could not be built for Linux 5.13. It seems that it tries to compile the kernel module for multiple versions, and the while compiling it for the installed kernel version does work, a more recent version doesn't. This error can be ignored.

We can now format our disks. Every disk will follow the same layout:

```
1MB BIOS boot partition | 2GB boot partition | ~10TB ZFS partition
```

<div>

The [BIOS boot partition](https://en.wikipedia.org/wiki/BIOS_boot_partition) is needed by GRUB when booting from BIOS-based machines on disks with GPT partition tables, which is our case, since Hetzner uses BIOS legacy boot.[^uefi] GRUB requires a fixed size for this partition, and 1MB works well enough.

[^uefi]: UEFI [can be enabled](https://docs.hetzner.com/robot/dedicated-server/operating-systems/uefi/), but I have not tried it, since the BIOS legacy boot works well enough.

The 2GB boot partition will contain what is usually found in `/boot`. However we'll have four such partitions for redundancy. Technically two would be enough, since we don't tolerate more than two disk failures anyway, but the ZFS partitions need to be of equal size on each disk for this setup, so we have that space anyway.

The rest of the space is taken by ZFS.

First, we create the partition table and partitions for each disk (`/dev/sda` to `/dev/sdc`):

</div>

```
rescue# for disk in /dev/sda /dev/sdb /dev/sdc /dev/sdd; do
> parted --script $disk mklabel gpt
> parted --script --align optimal $disk -- mklabel gpt mkpart 'BIOS-boot' 1MB 2MB set 1 bios_grub on mkpart 'boot' 2MB 2000MB mkpart 'zfs-pool' 2000MB '100%'
> done
```

<!--
TODO why 1MB 2MB?
-->

The first command creates the partition table, the second creates the partitions. So for each disk we'll have the first partition to be the BIOS boot partition, the second the boot partition, and the third the ZFS partition.

The flags are taken from the Hetzner script [in `nixos-install-scripts`](https://github.com/nix-community/nixos-install-scripts/blob/cb09b52de856eb561d2b6db50c045c0ce58d75f5/hosters/hetzner-dedicated/hetzner-dedicated-wipe-and-install-nixos.sh#L66), which also explains where they come from.

The BIOS boot partition doesn't need a filesystem. The boot partitions do need it though:

```
rescue# mkfs.vfat /dev/sda2 && mkfs.vfat /dev/sdb2 && mkfs.vfat /dev/sdc2 && mkfs.vfat /dev/sdd2
```

Now we need to create the ZFS pool. The Archlinux wiki
[recommends](https://wiki.archlinux.org/title/ZFS#Identify_disks) to use device IDs:

```
rescue# ls -lh /dev/disk/by-id/
lrwxrwxrwx 1 root root  9 Oct 11 10:50 ata-ST10000NM0568-2H5110_ZHZ54D1C -> ../../sdb
lrwxrwxrwx 1 root root 10 Oct 11 10:50 ata-ST10000NM0568-2H5110_ZHZ54D1C-part1 -> ../../sdb1
lrwxrwxrwx 1 root root 10 Oct 11 10:51 ata-ST10000NM0568-2H5110_ZHZ54D1C-part2 -> ../../sdb2
lrwxrwxrwx 1 root root 10 Oct 11 10:50 ata-ST10000NM0568-2H5110_ZHZ54D1C-part3 -> ../../sdb3
lrwxrwxrwx 1 root root  9 Oct 11 10:50 ata-ST10000NM0568-2H5110_ZHZ54D2A -> ../../sdd
lrwxrwxrwx 1 root root 10 Oct 11 10:50 ata-ST10000NM0568-2H5110_ZHZ54D2A-part1 -> ../../sdd1
lrwxrwxrwx 1 root root 10 Oct 11 10:51 ata-ST10000NM0568-2H5110_ZHZ54D2A-part2 -> ../../sdd2
lrwxrwxrwx 1 root root 10 Oct 11 10:50 ata-ST10000NM0568-2H5110_ZHZ54D2A-part3 -> ../../sdd3
lrwxrwxrwx 1 root root  9 Oct 11 10:50 ata-ST10000NM0568-2H5110_ZHZ54D5K -> ../../sda
lrwxrwxrwx 1 root root 10 Oct 11 10:50 ata-ST10000NM0568-2H5110_ZHZ54D5K-part1 -> ../../sda1
lrwxrwxrwx 1 root root 10 Oct 11 10:51 ata-ST10000NM0568-2H5110_ZHZ54D5K-part2 -> ../../sda2
lrwxrwxrwx 1 root root 10 Oct 11 10:50 ata-ST10000NM0568-2H5110_ZHZ54D5K-part3 -> ../../sda3
lrwxrwxrwx 1 root root  9 Oct 11 10:50 ata-ST10000NM0568-2H5110_ZHZ54DBW -> ../../sdc
lrwxrwxrwx 1 root root 10 Oct 11 10:50 ata-ST10000NM0568-2H5110_ZHZ54DBW-part1 -> ../../sdc1
lrwxrwxrwx 1 root root 10 Oct 11 10:51 ata-ST10000NM0568-2H5110_ZHZ54DBW-part2 -> ../../sdc2
lrwxrwxrwx 1 root root 10 Oct 11 10:50 ata-ST10000NM0568-2H5110_ZHZ54DBW-part3 -> ../../sdc3
...
```

We then create a pool named `zroot` using the flags recommended by [this blogpost](https://elis.nu/blog/2019/08/encrypted-zfs-mirror-with-mirrored-boot-on-nixos/):

```
# zpool create \
    -O mountpoint=none -o ashift=12 -O atime=off -O acltype=posixacl -O xattr=sa -O compression=lz4 \
    -O encryption=aes-256-gcm -O keyformat=passphrase \
    zroot raidz \
    ata-ST10000NM0568-2H5110_ZHZ54D5K-part3 ata-ST10000NM0568-2H5110_ZHZ54D1C-part3 ata-ST10000NM0568-2H5110_ZHZ54DBW-part3 ata-ST10000NM0568-2H5110_ZHZ54D2A-part3
```

Apart from the "feature flags", I also enable passphrase-based encryption (`zpool` will prompt for a password), and `raidz`, which will use 1 disk for parity. This means that the total size of the pool will be of ~30TB.

If we wanted compression or encryption only for some data, you could easily do that with ZFS "storage spaces". But I kept things simple, and only used one storage space, with zpool-wide compression and encryption.

We now mount everything under /mnt:

```
rescue# zfs create -o mountpoint=legacy zroot/root
rescue# mount -t zfs zroot/root /mnt
rescue# mkdir /mnt/boot-1 && mkdir /mnt/boot-2 && mkdir /mnt/boot-3 && mkdir /mnt/boot-4
rescue# mount /dev/sda2 /mnt/boot-1 && mount /dev/sdb2 /mnt/boot-2 && mount /dev/sdc2 /mnt/boot-3 && mount /dev/sdd2 /mnt/boot-4
```

### Step 3: generating SSH keys for the `initrd` SSH server

We'll spin up a temporary SSH server in `initrd`, before the full boot happens, only for the purpose of unlocking the ZFS pool. Remember that the encrypted ZFS pool is our root filesystem, so we need to unlock it before the boot sequence can take place.

This is [all done automatically by `configuration.nix`](https://gist.github.com/bitonic/78529d3dd007d779d60651db076a321a#file-configuration-nix-L120), but we need to execute a premininary step --- generating the host keys for this temporary server:

```
rescue# ssh-keygen -t ed25519 -N "" -f /mnt/boot-1/initrd-ssh-key
rescue# cp /mnt/boot-1/initrd-ssh-key /mnt/boot-2 && cp /mnt/boot-1/initrd-ssh-key /mnt/boot-3 && cp /mnt/boot-1/initrd-ssh-key /mnt/boot-4
```

Note that the temporary server will run on port 2222, so that our SSH client won't be confused by different keys for the same host.

### Step 4: gathering networking information

Since this is a remote server, we need to make sure to configure the network correctly so that we'll be able to reach it when we reboot into NixOS.

The IPv4 and IPv6 details can be gathered in the "IPs" tab of the Hetzner control panel for the server you've rented.

The next step is understanding what interface name NixOS will pick, so that we can configure it in `configuration.nix`. NixOS uses `systemd`, and following [this article](https://major.io/2015/08/21/understanding-systemds-predictable-network-device-names/) we can reliably derive what the network interface name will be chosen by `systemd`:

```
rescue# RESCUE_INTERFACE=$(ip route get 8.8.8.8 | grep -Po '(?<=dev )(\S+)')
rescue# INTERFACE_DEVICE_PATH=$(udevadm info -e | grep -Po "(?<=^P: )(.*${RESCUE_INTERFACE})")
rescue# udevadm info --query=property "--path=$INTERFACE_DEVICE_PATH"
DEVPATH=//devices/pci0000:00/0000:00:1c.1/0000:03:00.0/net/eth0
INTERFACE=eth0
IFINDEX=2
SUBSYSTEM=net
USEC_INITIALIZED=11867565
net.ifnames=0
ID_NET_NAMING_SCHEME=v240
ID_NET_NAME_MAC=enx001e67aaeaab
ID_OUI_FROM_DATABASE=Intel Corporate
ID_NET_NAME_ONBOARD=eno1
ID_NET_LABEL_ONBOARD=enIntel i210
ID_NET_NAME_PATH=enp3s0
ID_BUS=pci
ID_VENDOR_ID=0x8086
ID_MODEL_ID=0x1533
ID_PCI_CLASS_FROM_DATABASE=Network controller
ID_PCI_SUBCLASS_FROM_DATABASE=Ethernet controller
ID_VENDOR_FROM_DATABASE=Intel Corporation
ID_MODEL_FROM_DATABASE=I210 Gigabit Network Connection
ID_PATH=pci-0000:03:00.0
ID_PATH_TAG=pci-0000_03_00_0
ID_NET_DRIVER=igb
ID_NET_LINK_FILE=/lib/systemd/network/99-default.link
SYSTEMD_ALIAS=/sys/subsystem/net/devices/eth0
TAGS=:systemd:
```

The `systemd` name will be the first match amongst  `ID_NET_NAME_FROM_DATABASE`, `ID_NET_NAME_ONBOARD`, `ID_NET_NAME_SLOT`, `ID_NET_NAME_PATH`, or `ID_NET_NAME_MAC`. In our case we have `ID_NET_NAME_ONBOARD=eno1`, therefore the interface name will be `eno1`.

*Nota bene:* multiple of the names above can be present! The one that comes first in the given list must be chosen. For example, on my machine I have both `ID_NET_NAME_PATH` and `ID_NET_NAME_ONBOARD`, but `ID_NET_NAME_ONBOARD` takes priority. See [this section of the linked article](https://major.io/2015/08/21/understanding-systemds-predictable-network-device-names/#picking-the-final-name) for more details.

We also need to find out what driver is required by our network interface, since we want to start a temporary SSH server in `initrd`, and we'll need to instruct the kernel to load the right module explicitly. This can be done through `lshw`:

```
rescue# apt install -y lshw
rescue# lshw -C network | grep -Poh 'driver=[[:alnum:]]+'
driver=igb                  
```

### Step 5: creating a mailgun domain

We'll use [mailgun](https://www.mailgun.com/) to deliver mail. I usually create one domain per server. Once a domain is setup, we'll use the SMTP details in `configuration.nix` to have our server use it to deliver mail.

### Step 6: filling in `configuration.nix`

I've prepared a [`configuration.nix` template](https://gist.github.com/bitonic/78529d3dd007d779d60651db076a321a) which contains the full configuration needed for our storage server. It specifies the functionality of the base system, and you should read it all, but the only part you have to modify is the preamble which contains the deployment-specific parameters we have gathered above:

```
hostName = "..."; # the hostname of the machine
publicKey = "..."; # your public key
# From `ls -lh /dev/disk/by-id`
sda = "ata-...";
sdb = "ata-...";
sdc = "ata-...";
sdd = "ata-...";
# See <https://major.io/2015/08/21/understanding-systemds-predictable-network-device-names/#picking-the-final-name>
# for a description on how to find out the network card name reliably.
networkInterface = "...";
# This was derived from `lshw -C network | grep -Poh 'driver=[[:alnum:]]+'`, for me it says `driver=igb`.
# Needed to load the right driver before boot for the initrd SSH session.
networkInterfaceModule = "...";
# From the Hetzner control panel
ipv4 = {
  address = "..."; # the ip address
  gateway = "..."; # the gateway ip address
  netmask = "255.255.255.0"; # the netmask -- might not be the same for you!
  prefixLength = 24; # must match the netmask, see <https://www.pawprint.net/designresources/netmask-converter.php>
};
ipv6 = {
  address = "..."; # the ipv6 addres
  gateway = "..."; # the ipv6 gateway
  prefixLength = 64; # shown in the control panel
};
# See <https://nixos.wiki/wiki/NixOS_on_ZFS> for why we need the
# hostId and how to generate it
hostId = "...";
# Mail sender / recepient
emailTo = "..."; # where to send the notifications
emailFrom = "..."; # who should be the sender in the emails
# msmtp configuration -- I use mailgun, you need to create a new
# domain and it'll show you this data.
msmtpAccount = {
  auth = "plain";
  host = "smtp.eu.mailgun.org";
  port = "587";
  user = "postmaster@...";
  password = "...";
  from = emailFrom;
};
```

Every field should be self-explanatory if you have read this far.

### Step 7: installing NixOS

We first install Nix (the software, not the operating system) on the rescue system. See the
[install script](https://github.com/nix-community/nixos-install-scripts/blob/cb09b52de856eb561d2b6db50c045c0ce58d75f5/hosters/hetzner-dedicated/hetzner-dedicated-wipe-and-install-nixos.sh#L150)
for details on all the steps described below.

```
rescue# apt-get install -y sudo
rescue# mkdir -p /etc/nix
rescue# echo "build-users-group =" > /etc/nix/nix.conf
rescue# curl -L https://nixos.org/nix/install | sh
rescue# . $HOME/.nix-profile/etc/profile.d/nix.sh
rescue# nix-channel --add https://nixos.org/channels/nixos-21.05 nixpkgs # change the config if you change this!
rescue# nix-channel --update
rescue# nix-env -iE "_: with import <nixpkgs/nixos> { configuration = {}; }; with config.system.build; [ nixos-generate-config nixos-install nixos-enter manual.manpages ]"
rescue# nixos-generate-config --root /mnt
```

Then we replace the automatically generated `/mnt/etc/nixos/configuration.nix` with the config you have filled in in the previous sections.

Finally, we can install NixOS:

```
rescue# PATH="$PATH" NIX_PATH="$NIX_PATH" `which nixos-install` --no-root-passwd --root /mnt --max-jobs 40
```

Note that this command will build ZFS, since we enable options / patches that are not present in the default NixOS ZFS distribution. See [`configuration.nix`](https://gist.github.com/bitonic/78529d3dd007d779d60651db076a321a#file-configuration-nix-L57) for details. Everything else should be downloaded from the NixOS caches.

We can now reboot, and pray:

```
rescue# reboot
```

## Logging in

If the server starts responding to `ping` (see the [next section](#when-things-go-wrong) if it doesn't) we will have to SSH into the server twice: first to unlock the ZFS drive, and then to access the server after boot.

To unlock the ZFS drive, we log into the temporary SSH server running at port 2222:

```
% ssh -p 2222 root@ip-address
client_input_hostkeys: received duplicated ssh-ed25519 host key
Enter passphrase for 'zroot':
1 / 1 key(s) successfully loaded
~ # 
```

The `received duplicated ssh-ed25519 host key` message is expected: we're providing the same key from all boot partitions for redundancy.

Afterwards, we can login as root normally. If that all works, the hard part is over!

Now you can go ahead and configure the system as described by the [NixOS manual](https://nixos.org/manual/nixos/stable/index.html#ch-configuration).

## If things go wrong while installing {#when-things-go-wrong}

Apart from rebooting into the rescue system, which can be done completely automatically at any time, you can also request a [KVM console](https://docs.hetzner.com/robot/dedicated-server/maintainance/kvm-console/) to be able to operate server directly as if you were in front of it, although it is free only up to three hours.

If all else fails, you can try to ping me on the [NixOS matrix channel](https://matrix.to/#/#nix:nixos.org) (I'm `bitonic`).

## Testing the redundancy and monitoring {#verify}

Now that our server is up and running, the first thing to do is check that our monitoring and redundancy measures are working as expected.

These tests are not as comprehensive as I would have liked, but I didn't want to sink an excessive amount of time in this project, and I will be keeping my existing NAS server for as long as I can as a mirror, so I decided to move on.

### Startup/shutdown/weekly emails

`configuration.nix` [sends an email alert](https://gist.github.com/bitonic/78529d3dd007d779d60651db076a321a#file-configuration-nix-L209) when the server powers up, shuts down, and on Monday. The weekly email is intended to make sure that we notice if the emails stop working.

This one is easy to verify --- in fact you should have already gotten an email on the first boot. They look like this:

```
Subject: <hostname> just booted 2021-10-11T14:39:51+00:00

zpool status:

  pool: zroot
 state: ONLINE
config:

	NAME                                         STATE     READ WRITE CKSUM
	zroot                                        ONLINE       0     0     0
	  raidz1-0                                   ONLINE       0     0     0
	    ata-ST10000NM0568-2H5110_ZHZ54D5K-part3  ONLINE       0     0     0
	    ata-ST10000NM0568-2H5110_ZHZ54D1C-part3  ONLINE       0     0     0
	    ata-ST10000NM0568-2H5110_ZHZ54DBW-part3  ONLINE       0     0     0
	    ata-ST10000NM0568-2H5110_ZHZ54D2A-part3  ONLINE       0     0     0

errors: No known data errors
```

If you're not getting them, something is wrong with your email setup.

### `smartd` emails

[`smartd`](https://www.smartmontools.org/) will monitor the server's hardware (most notably the disks), and send notifications when something goes wrong.

To test that it is up and running, we can instruct it to send a notification at boot, by adding

```
services.smartd.notifications.test = true;
```

After running `sudo nixos-rebuild switch`, we'll get a test notification per disk on boot, looking like this:

```
Subject: SMART error (EmailTest) detected on host: <hostname>

This message was generated by the smartd daemon running on:

   host name:  <hostname>
   DNS domain: [Empty]

The following warning/error was logged by the smartd daemon:

TEST EMAIL from smartd for device: /dev/sdc [SAT]

Device info:
ST10000NM0568-2H5110, S/N:ZHZ54DBW, WWN:5-000c50-0c3c8276d, FW:SS02, 10.0 TB

For details see host's SYSLOG.

...
```

### ZED emails

[ZED](https://manpages.ubuntu.com/manpages/xenial/man8/zed.8.html), the "ZFS Event Daemon", picks up ZFS events and does something with them. In our case we have configured it to send emails in various circumstances --- see `configuration.nix` and the scripts in `/etc/zfs/zed.d` for details.

An easy test to see if ZED is up and running involves [creating a test ZFS filesystem and scrubbing it](https://www.reddit.com/r/zfs/comments/fb8utq/how_to_test_zed_notification_emails/fj5b9ks/):

```
% cd /tmp
% dd if=/dev/zero of=sparse_file bs=1 count=0 seek=512M
% sudo zpool create test /tmp/sparse_file
% sudo zpool scrub test
```

Which results in the following email:

```
Subject: ZFS scrub_finish event for test on <hostname>

ZFS has finished a scrub:

   eid: 48
 class: scrub_finish
  host: <hostname>
  time: 2021-10-11 15:27:11+0000
  pool: test
 state: ONLINE
  scan: scrub repaired 0B in 00:00:00 with 0 errors on Mon Oct 11 15:27:11 2021
config:

	NAME                STATE     READ WRITE CKSUM
	test                ONLINE       0     0     0
	  /tmp/sparse_file  ONLINE       0     0     0

errors: No known data errors
```

We can then remove the test zpool:

```
% sudo zpool export test
% rm sparse_file
```

ZED will also notify us on failures, as we'll see in the next section.

### Removed disk after boot

We now want to test what happens when things go wrong. **Do not run these tests on a production system!** They might render your machine unusable, or worse.

One useful test is to just kill one of the drives. [This StackOverflow answer describes various ways of doing it.](https://askubuntu.com/a/554417) The first method makes one drive disappear after boot:

```
% sudo sh -c 'echo 1 > /sys/block/sda/device/delete' # remove sda
```

We immediately get an email telling us that trouble is brewing:

```
Subject: ZFS device fault for pool 0x268AD36B11F9E2AF on <hostname>

ZFS has detected that a device was removed.

 impact: Fault tolerance of the pool may be compromised.
    eid: 56
  class: statechange
  state: UNAVAIL
   host: <hostname>
   time: 2021-10-11 16:40:58+0000
  vpath: /dev/disk/by-id/ata-ST10000NM0568-2H5110_ZHZ54D5K-part3
  vguid: 0x62259E52F503B645
   pool: 0x268AD36B11F9E2AF
```

We can verify that things are not going well with `zpool status`:

```
% zpool status
  pool: zroot
 state: DEGRADED
status: One or more devices could not be used because the label is missing or
	invalid.  Sufficient replicas exist for the pool to continue
	functioning in a degraded state.
action: Replace the device using 'zpool replace'.
   see: https://openzfs.github.io/openzfs-docs/msg/ZFS-8000-4J
config:

	NAME                                         STATE     READ WRITE CKSUM
	zroot                                        DEGRADED     0     0     0
	  raidz1-0                                   DEGRADED     0     0     0
	    ata-ST10000NM0568-2H5110_ZHZ54D5K-part3  UNAVAIL      4    51     0
	    ata-ST10000NM0568-2H5110_ZHZ54D1C-part3  ONLINE       0     0     0
	    ata-ST10000NM0568-2H5110_ZHZ54DBW-part3  ONLINE       0     0     0
	    ata-ST10000NM0568-2H5110_ZHZ54D2A-part3  ONLINE       0     0     0

errors: No known data errors
```

Rebooting the system will restore the device.

Two caveats:

* The email notification for state changes is sent by one of the default "zedlets" for ZED, [`statechange-notify.sh`](https://github.com/openzfs/zfs/blob/a5b464263bc1f9fd833272a9e691cd479bf47d5d/cmd/zed/zed.d/statechange-notify.sh). However, [I had to amend it](https://github.com/openzfs/zfs/pull/12630) so that it also picks up `UNAVAIL` state changes, since...
* ...ZFS seems to currently not emit state changes for the pool itself, just for the devices. So we do not get a notification for the change from `ONLINE` to `DEGRADED`. I believe this to be a bug, or at the very least surprising, [you can check out the corresponding issue on GitHub I opened.](https://github.com/openzfs/zfs/issues/12629)

The patch described above is [automatically applied to the stock ZFS in `configuration.nix`](https://gist.github.com/bitonic/78529d3dd007d779d60651db076a321a#file-configuration-nix-L57).

### Removed disk on boot

A more trying test is to boot the system without one of the drives. The easiest way to do this would be to physically remove it, but we don't have physical access to the machine.

We could probably pay one of Hetzner's technicians to do it, but we can approximate removing the disk physically by using the `libata.force` [kernel command line option](https://www.kernel.org/doc/Documentation/admin-guide/kernel-parameters.txt), which can be used to completely disable select ATA devices.

First we need to identify which ATA ports corresponds to our disks:

```
% dmesg | grep ' ata'                  
...
[    0.949987] ata3.00: ATA-11: ST10000NM0568-2H5110, SS02, max UDMA/133
[    0.950040] ata1.00: ATA-11: ST10000NM0568-2H5110, SS02, max UDMA/133
[    0.950056] ata2.00: ATA-11: ST10000NM0568-2H5110, SS02, max UDMA/133
[    0.950125] ata4.00: ATA-11: ST10000NM0568-2H5110, SS02, max UDMA/133
...
```

<div>
For example we can banish ATA device `1.00` by adding by adding `libata.force=1.00:disable` to the kernel command line arguments. The disks are all the same model, so it's not easy to distinguish them, but to be sure to disable the one our system is actually booting from we can disable them all, one at a time.

The easiest way to temporarily add command line arguments to the kernel is by pressing `e` in the GRUB menu when selecting what to boot. This can be done on hetzner by [request a KVM console](https://docs.hetzner.com/robot/dedicated-server/maintainance/kvm-console/) and operating the server from there.[^boot-parameters]

[^boot-parameters]: You can also add the command line arguments through `boot.kernelParameters` in `configuration.nix`, but then it is quite annoying to perform `nixos-rebuild switch` again, as opposed to just rebooting.

Once the system is started without one of the drives, it might take a while before the unlock prompt is available, since the ZFS drive will take longer to mount without one of the drives:[^longer-mount]

[^longer-mount]: In my tests it took a few minutes for the unlock prompt to appear. My guess is that ZFS is waiting for the drive to come up, and then giving up after some timeout.

</div>

```
% ssh -p 2222 root@ip-address
client_input_hostkeys: received duplicated ssh-ed25519 host key
zfs not running -- maybe the pool is taking some time to load for some unforseen reason.
~ # 
```

Once the system is booted, the boot mail alert already indicates that something is wrong:

```
Subject: <hostname> just booted 2021-10-11T17:43:49+00:00

zpool status:

  pool: zroot
 state: DEGRADED
status: One or more devices could not be used because the label is missing or
	invalid.  Sufficient replicas exist for the pool to continue
	functioning in a degraded state.
action: Replace the device using 'zpool replace'.
   see: https://openzfs.github.io/openzfs-docs/msg/ZFS-8000-4J
  scan: resilvered 3.12M in 00:00:00 with 0 errors on Mon Oct 11 16:43:46 2021
config:

	NAME                                         STATE     READ WRITE CKSUM
	zroot                                        DEGRADED     0     0     0
	  raidz1-0                                   DEGRADED     0     0     0
	    7072232868990400069                      UNAVAIL      0     0     0  was /dev/disk/by-id/ata-ST10000NM0568-2H5110_ZHZ54D5K-part3
	    ata-ST10000NM0568-2H5110_ZHZ54D1C-part3  ONLINE       0     0     0
	    ata-ST10000NM0568-2H5110_ZHZ54DBW-part3  ONLINE       0     0     0
	    ata-ST10000NM0568-2H5110_ZHZ54D2A-part3  ONLINE       0     0     0

errors: No known data errors
```

Unfortunately we do not get ZED events since there is no "state change" happening --- the pool is `DEGRADED` from the get go. I considered writing a job polling the pool state, but for now I am OK with the boot mail alert notification.

### Bad IO

ZFS ships with a useful utility, `zinject`, which artificially simulates IO failures. For example, [we can instruct `zinject` to fail 100% of the IO calls to a disk in our pool `zroot`](https://github.com/openzfs/zfs/issues/4511#issuecomment-434122340):

```
% sudo zinject -d ata-ST10000NM0568-2H5110_ZHZ54D5K-part3 -e io -T all -f 100 zroot
Added handler 1 with the following properties:
  pool: zroot
  vdev: 5f989b46a9a47c64
```

The pool won't fail immediately, but it will notice the mounting number of errors:

```
% zpool status
  pool: zroot
 state: ONLINE
status: One or more devices has experienced an unrecoverable error.  An
	attempt was made to correct the error.  Applications are unaffected.
action: Determine if the device needs to be replaced, and clear the errors
	using 'zpool clear' or replace the device with 'zpool replace'.
   see: https://openzfs.github.io/openzfs-docs/msg/ZFS-8000-9P
config:

	NAME                                         STATE     READ WRITE CKSUM
	zroot                                        ONLINE       0     0     0
	  raidz1-0                                   ONLINE       0     0     0
	    ata-ST10000NM0568-2H5110_ZHZ54D5K-part3  ONLINE       0   130     0
	    ata-ST10000NM0568-2H5110_ZHZ54D1C-part3  ONLINE       0     0     0
	    ata-ST10000NM0568-2H5110_ZHZ54DBW-part3  ONLINE       0     0     0
	    ata-ST10000NM0568-2H5110_ZHZ54D2A-part3  ONLINE       0     0     0

errors: No known data errors
```

Eventually ZFS will mark the pool as `DEGRADED`, after which we'll get an email from ZED:

```
Subject: ZFS device fault for pool 0x194686BA25635A3D on <hostname>

The number of I/O errors associated with a ZFS device exceeded
acceptable levels. ZFS has marked the device as faulted.

 impact: Fault tolerance of the pool may be compromised.
    eid: 371
  class: statechange
  state: FAULTED
   host: <hostname>
   time: 2021-10-12 09:22:10+0000
  vpath: /dev/disk/by-id/ata-ST10000NM0568-2H5110_ZHZ54D5K-part3
  vguid: 0x5F989B46A9A47C64
   pool: 0x194686BA25635A3D
```

We can then clear the artificial faults like so:

```
% sudo zinject -c all
removed all registered handlers
% sudo zpool clear zroot ata-ST10000NM0568-2H5110_ZHZ54D5K-part3
```

## Closing thoughts

I've only been using ZFS for a few days, so I can't speak of its performance or long term reliability. From my first impressions it seems like a solid piece of technology. It's a shame that [it will probably never mainlined in Linux.](https://www.realworldtech.com/forum/?threadid=189711&curpostid=189841)

Installing it on NixOS was very easy --- much more direct than setting up my older ext4-LVM-LUKS-RAID stack. The only part that seemed rough around the edges is ZED, which maybe is to be expected given that it is a much newer component. That said [the PR I submitted](https://github.com/openzfs/zfs/pull/12630) was approved the day after I sent it, and hopefully [the issue](https://github.com/openzfs/zfs/issues/12629) will be resolved soon.

## Acknowledgements and resources {#acknowledgements}

These are the main resources I drew from:

* The [install script](https://github.com/nix-community/nixos-install-scripts/blob/cb09b52de856eb561d2b6db50c045c0ce58d75f5/hosters/hetzner-dedicated/hetzner-dedicated-wipe-and-install-nixos.sh) for NixOS on Hetzner dedicated servers.
* [An helpful blog post](https://elis.nu/blog/2019/08/encrypted-zfs-mirror-with-mirrored-boot-on-nixos/) on setting up encrypted ZFS on NixOS.
* The ZFS page [on the NixOS wiki](https://nixos.wiki/wiki/NixOS_on_ZFS).
* The ZFS page [on the ArchWiki](https://wiki.archlinux.org/title/ZFS).
* [Niklas'](https://github.com/nh2) knowledge about all things NixOS, Hetzner, and systems in general.
* [Patrick's](https://github.com/chpatrick) NixOS mailgun setup.
* `PMT` on [`#zfs`](irc://libera.chat/zfs) for tips regarding ZED and zedlets.

Many thanks to Alexandru Scvorţov for reviewing drafts of this article.