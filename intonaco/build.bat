@ECHO OFF
set LUA_PATH=../lualib/?.lua
scripts\aio.exe http://www.hgamer3d.org/component/Lua scripts\build.lua %1 %2 %3 %4 %5 %6
