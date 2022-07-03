import std/[os, strformat]
import pkg/illwill


proc exitProc() {.noconv.} =
  illwillDeinit()
  showCursor()
  quit(0)

proc main() =
  illwillInit(fullscreen=true)
  setControlCHook(exitProc)
  hideCursor()

  while true:
    var tb = newTerminalBuffer(terminalWidth(), terminalHeight())

    var key = getKey()
    case key
    of Key.Escape, Key.Q: exitProc()
    else: discard

    tb.setForegroundColor(fgYellow)
    tb.drawRect(0, 0, tb.width-1, tb.height-1)

    tb.setBackgroundColor(bgRed)
    tb.setForegroundColor(fgWhite, bright=true)
    tb.write(1, 1, fmt"Width:  {tb.width}")
    tb.write(1, 2, fmt"Height: {tb.height}")

    tb.resetAttributes()
    tb.write(1, 4, "Press Q, Esc or Ctrl-C to quit")
    tb.write(1, 5, "Resize the terminal window and see what happens :)")

    tb.display()

    sleep(20)

main()