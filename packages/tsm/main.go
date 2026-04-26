package main

import (
	"fmt"
	"os"
)

func main() {
	if len(os.Args) < 2 {
		fmt.Fprintln(os.Stderr, "tsm: no command given")
		os.Exit(1)
	}
	fmt.Println("tsm stub — not yet implemented")
}
