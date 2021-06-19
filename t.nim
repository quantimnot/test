import macros

macro test*(name: string, exprs: untyped) =
  result = newStmtList()
  for expr in exprs.children:
    case expr.kind
    of nnkCall, nnkCommand:
      case $expr[0]
      of "it":
        if expr.len > 1 and expr[1].kind == nnkStrLit:
          echo $expr[1]
    else:
      echo $expr.kind
      result.add expr

template test*(exprs: untyped) =
  test "UNNAMED":
    exprs

macro it*(name: string, group = "", exprs: untyped) =
  return newStmtList()

#macro describe*(meta: tuple[name: string, group: string], exprs: untyped) =
macro describe*(name: string, exprs: untyped) =
  result = newStmtList()
  #var group: string
  for expr in exprs.children:
    case expr.kind
    of nnkCall, nnkCommand:
      case $expr[0]
      of "it":
        if expr.len > 1 and expr[1].kind == nnkStrLit:
          echo $expr[1]
    else:
      echo $expr
      result.add expr

when defined dumpTree:
  dumpTree:
    describe "commandline arguments":
      it "runs nim programs":
        echo NimVersion
        assert 1 == 1
      it "runs shell programs":
        let program = "echo $SHELL"
        var stdout = StringStream()
        assert posix_shell(program, stdout)

when isMainModule:
  dumpTree:
    test with (command: "nim r -"):
      quit 1
#  describe (name: "commandline arguments", group: ""):
#  describe "commandline arguments":
#    it "runs nim programs", "":
#      echo NimVersion
#      assert 1 == 1
#    it "runs shell programs", "":
#      let program = "echo $SHELL"
#      var stdout = StringStream()
#      assert posix_shell(program, stdout)
