import nake
import os, times
import godotapigen

proc genGodotApi() =
  let godotBin =
    if (let x = getEnv("GODOT_BIN"); x != ""): x
    else:
      when defined(linux): "/bin/godot3-bin"
      else:
        echo "Invalid GODOT_BIN path: " & godotBin
        quit(-1)

  if godotBin.len == 0:
    echo "GODOT_BIN environment variable is not set"
    quit(-1)
  if not fileExists(godotBin):
    echo "Invalid GODOT_BIN path: " & godotBin
    quit(-1)

  const targetDir = "src"/"godotapi"
  createDir(targetDir)
  const jsonFile = targetDir/"api.json"
  if not fileExists(jsonFile) or
     godotBin.getLastModificationTime() > jsonFile.getLastModificationTime():
    direShell(godotBin, "--gdnative-generate-json-api", getCurrentDir()/jsonFile)
    if not fileExists(jsonFile):
      echo "Failed to generate api.json"
      quit(-1)

    genApi(targetDir, jsonFile)

task "build", "Build module for the current platform":
  genGodotApi()
  let bitsPostfix = when sizeof(int) == 8: "_64" else: "_32"
  let libFile =
    when defined(windows):
      "nim" & bitsPostfix & ".dll"
    elif defined(ios):
      "nim_ios" & bitsPostfix & ".dylib"
    elif defined(macosx):
      "nim_mac.dylib"
    elif defined(android):
      "libnim_android.so"
    elif defined(linux):
      "nim_linux" & bitsPostfix & ".so"
    else: nil
  createDir("_dlls")
  withDir "src":
    direShell(["nimble", "c", "--app:lib", "--noMain", ".."/"src"/"nimmod.nim", "-o:.."/"_dlls"/libFile])

task "buildWindows", "Cross-compile module from linux to windows":
  genGodotApi()
  let bitsPostfix = when sizeof(int) == 8: "_64" else: "_32"
  let libFile = "nim" & bitsPostfix & ".dll"
  createDir("_dlls")
  withDir "src":
    direShell(["nimble", "c", "--app:lib", "--noMain", "-d:mingw", "--os:windows", "--cc:gcc", "--gcc.exe:/usr/bin/x86_64-w64-mingw32-gcc", "--gcc.linkerexe:/usr/bin/x86_64-w64-mingw32-gcc", ".."/"src"/"nimmod.nim", "-o:.."/"_dlls"/libFile])

task "clean", "Remove files produced by build":
  removeDir(".nimcache")
  removeDir("src"/".nimcache")
  removeDir("src"/"godotapi")
  removeDir("_dlls")
  removeFile("nakefile")
  removeFile("nakefile.exe")
