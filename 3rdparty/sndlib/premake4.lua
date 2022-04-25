-- sndlib premake4
--   requires premake-4.4 (for os.is64bit)
--   currently assumes you want s7 (scheme) as the extension language


--------------------------------------------------------------------------------
--                              Command Line
--------------------------------------------------------------------------------

newoption({trigger = "with-g++", description = "Optionally use g++ compiler."})

if (not _ACTION) then 
  if (os.is("windows")) then 
     _ACTION = "vs2010" 
  else 
     _ACTION = "gmake" 
  end 
end 


--------------------------------------------------------------------------------
--                              Global Config
--------------------------------------------------------------------------------

--General
DebugFlags = {"Symbols", "NoPCH", "NoManifest"}
ReleaseFlags = {"NoPCH", "NoManifest"}
SpeedFlags = {"OptimizeSpeed", "NoPCH", "NoManifest"}

--Warnings
StandardGCCWarnings = {"-Wall"}

--Mac
MacFrameworks = {"-framework CoreAudio", "-framework CoreFoundation", "-framework CoreMidi"}
MacTarget = "-mmacosx-version-min=10.6"

--------------------------------------------------------------------------------
--                                  Paths
--------------------------------------------------------------------------------

PathToRoot = ""
PathToSrc = "./"
PathToLib = "lib/"  -- folder to save libs in
PathToBin = "bin/"  -- folder to save apps in
PathToObj = "obj/"  -- intermediate dir for object files

--------------------------------------------------------------------------------
--
--------------------------------------------------------------------------------

solution("sndlib")
--Create a release, debug, and speed configuration for each project.
configurations({"Release", "Debug", "Speed"})

--------------------------------------------------------------------------------
--                     project sndlib: create static libsndlib
--------------------------------------------------------------------------------

project("sndlib")

-- optionally use g++ compiler for .c files
if (_OPTIONS["with-g++"]) then
   language("C++")
--   buildoptions( {"-x c++"})
-- for clang on osx?
else
   language("C")
end

if (_OPTIONS["with-gsl"]) then
   defines("HAVE_GSL")
   links({"gsl", "gslcblas"})
end

defines("WITH_AUDIO")
defines("HAVE_SCHEME")
defines("HAVE_PREMAKE")

if (os.get() == "macosx") then
--   links({"dl"})
   linkoptions(MacFrameworks)
else
   if (os.get() == "windows") then
      links("winmm")
   else
      if (os.get() == "linux") then
	 defines("HAVE_ALSA")
      else
	 -- I tried FreeBSD (had to remove -ldl from LIBS in gmake.unix), but
	 --   premake died with some lua error.
      end
   end
end

if (os.is64bit()) then
   defines("SIZEOF_VOID_P=8")
else
   defines("SIZEOF_VOID_P=4")
end


-- TODO: WORDS_BIGENDIAN:
-- I got this from some Lua mailing list -- I have no idea what it does!
--   apparently it returns true if little-endian
--
-- function endian()
--   return string.byte(string.dump(function() end),7)
-- end
--
-- if (!endian()) then
--    defines("WORDS_BIGENDIAN")
-- end
-- until I have a test case, I think I'll leave it little-endian by default
-- it's not clear that Lua works in big-endian machines


kind("StaticLib")
flags({"StaticRuntime"})
includedirs({PathToSrc})
objdir(PathToObj)
targetdir(PathToLib)
files({"headers.*", "audio.*", "io.*", "sound.*", "xen.*", "vct.*", "clm.*", "sndlib2xen.*", "clm2xen.*", "s7.*"})
defines({"HAVE_CONFIG_H=1"})

-- ADD WHATEVER OTHER PROJECTS YOU WANT, EG:
-- project("s7")
-- kind("ConsoleApp")
-- ...

configuration "Debug" flags(DebugFlags) defines("DEBUG")
configuration "Release" flags(ReleaseFlags) defines({"NDEBUG", "_NDEBUG"})
configuration "Speed" flags(SpeedFlags) defines({"NDEBUG", "_NDEBUG"})


-- to find a library os.findlib("X11"), nil if not found
-- os.execute to run external prog
-- os.isfile("path") true if file exists, else false
-- os.outputof("command")


