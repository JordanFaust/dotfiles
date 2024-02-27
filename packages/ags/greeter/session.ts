import GLib from "gi://GLib?version=2.0"
import AccountsService from "gi://AccountsService?version=1.0"

const { userName } = AccountsService.UserManager.get_default().list_users()[0]

declare global {
    const WALLPAPER: string
}

Object.assign(globalThis, {
    TMP: `/var/cache/greeter`,
    OPTIONS: "/var/cache/greeter/options.json",
    WALLPAPER: "/var/cache/greeter/background",
    // TMP: "/tmp/ags",
    // OPTIONS: Utils.CACHE_DIR + "/options.json",
    // WALLPAPER: Utils.HOME + "/.config/background",
    USER: userName,
})

print(`TMP=${TMP}`)
print(`OPTIONS=${OPTIONS}`)
print(`WALLPAPER=${WALLPAPER}`)
print(`USER=${USER}`)

// Utils.ensureDirectory(TMP)
