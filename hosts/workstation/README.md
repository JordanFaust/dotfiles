# Thelio Workstation

# Installation

## Set up root file system

The following partitions are created and labeled
* nixos - 200GiB
* projects - 100GiB
* home - 800GiB

```bash
parted /dev/nvme0n1 -- mklabel gpt
parted /dev/nvme0n1 -- mkpart ESP fat32 1Mib 512Mib
parted /dev/nvme0n1 -- mkpart primary 512Mib 200GiB
parted /dev/nvme0n1 -- mkpart primary 200GiB 300GiB
parted /dev/nvme0n1 -- mkpart pirmary 300GiB 100%
mkfs.fat -F32 -n BOOT /dev/nvme0n1p1
mkfs.ext4 -L nixos /dev/nvme0n1p2
mkfs.ext4 -L work /dev/nvme0n1p3
mkfs.ext4 -L home /dev/nvme0n1p4
```

## Mount drives

```bash
mount /dev/disk/by-label/nixos /mnt

mkdir -p /mnt/{home,boot,media/procore}
mount /dev/disk/by-label/BOOT /mnt/boot
mount /dev/disk/by-label/work /mnt/media/work
mount /dev/disk/by-label/home /mnt/home
```
