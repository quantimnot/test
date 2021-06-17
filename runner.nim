
macro test*(body: untyped): untyped =
  return body

when isMainModule:
  proc a() {.test.} =
  #  test "lock":
  #    check 1 == 1
    discard
  #a()