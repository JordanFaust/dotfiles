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
