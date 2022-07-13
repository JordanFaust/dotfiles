# Dotfiles (Jordan Faust)

My Linux dotfiles.

Focused on Linux Mint XFCE with Openbox as the WM

## How to install


```bash
$ bash -c "$(curl -fsSL raw.github.com/JordanFaust/dotfiles/master/bin/install) --all"
```

Run the dotfiles command:

```bash
$ install --dependencies --software --credentials --dotfiles
```

Options:

<table>
    <tr>
        <td><code>--all</code></td>
        <td>Installs all dotfiles, dependencies, additional software, and prompts for credentials to store in the keyring</td>
    </tr>
    <tr>
        <td><code>--dotfiles</code></td>
        <td>Create symlinks to all configuration files</td>
    </tr>
    <tr>
        <td><code>--dependencies</code></td>
        <td>Installs all apt dependencies</td>
    </tr>
    <tr>
        <td><code>--software</code></td>
        <td>Installs all software not managed by apt</td>
    </tr>
    <tr>
        <td><code>--credentials</code></td>
        <td>Prompts for credentials used by various systems. All credentials store in gnome-keyring or another compatible keyring store</td>
    </tr>
</table>

## Openbox

## Lightdm

See https://www.reddit.com/r/unixporn/comments/6xdbo6/lightdm_showing_my_login_some_love/

LightDM background is overriden by AccountServices. Edit the /var/lib/AccountService/users/jfaust file to set the login background

## Vagrant Testing

Testing changes within this repo can be done with the Vagrant nixos image

### Building the Base Image

The base image is built using nixos-generator command

``` bash
nixos-generate --format vagrant-virtualbox --system x86_64-linux
```

### Starting the base image

``` bash
vagrant init nixos/nixos-18.09-x86_64
```


## Firefox

The firefox theme is taken from FlyingFox. For installation details see 
https://github.com/akshat46/FlyingFox/wiki/%23-Installation

The repo is not actively maintained. My modifications have been copied out into
the firefox directory within this repo. Some of the features are limited or broken
from the original repo with the release of the proton UI for firefox.

### Update about:config settings

From https://support.mozilla.org/en-US/questions/1325862

  > By default, userChrome.css modifications are disabled in Firefox. You need to make sure that on the about:config page in Firefox, the toolkit.legacyUserProfileCustomizations.stylesheets preference is set to true and then restart the browser. 
  
To make the FlyingFox UI work there are a few setting that will need to be changes:

```
toolkit.legacyUserProfileCustomizations.stylesheets = true
browser.proton.enable = false
```

These settings disable the proton UI and allow configuring firefox via the userChrome file
  
### Finding Profile Location
  
The userChrome config will need to be placed in the appropriate folder for the firefox profile.
The easiest way to discover this path is to open the 'about:profiles' page after installing Firefox.

### Copying files to firefox profile

Replace the {{CHROME_PROFILE}} below with the value you get from the about:profiles page described above

``` bash
export CHROME_PROFILE={{CHROME_PROFILE}}
cp ~/.dotfiles/firefox/user.js ${CHROME_PROFILE}/user.js
cp -R ~/.dotfiles/firefox/chrome ${CHROME_PROFILE}/chrome
```

### Configure UI For Tree Style Tabs

Follow these instructions to configrue the look of treestyle tabs:
https://github.com/akshat46/FlyingFox/wiki/%23-Installation#treestyletab-css

This is done from the extenstion page for the Tree Style Tab extension

### Extensions

- Facebook container
- Firefox Multi-Account Containers
- Hacker News Enhancement suite
- nightTab
- Okta Browser Plugin
- OneTab
- Privacy Badger
- Tree Style Tab
