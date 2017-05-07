local glue = require("glue")
glue.luapath(glue.bin .. "/lualib")

require("os_arch")
require("util")
require("lfs")

local function getIntonacoVersion()
	local p = debug.getinfo(1).source:gsub('\\', '/'):match("@?(.*/)")
	if p then
		io.input(p.."../Cargo.toml")
	else
		io.input("../Cargo.toml")
	end
	while true do
		local line = io.read()
		if line == nil then break end
		local r = string.match(line, "version = \"(.*)\"")
		if r then
			return r
		end
	end
end

local function packageIntonaco()
	-- clean output directory
	os.execute("rm -rf package")
	-- create directory 
	local dir = getPlatString("intonaco", getIntonacoVersion())
	lfs.mkdir("package")
	lfs.mkdir("package/" .. dir)

	-- copy toml file
	os.execute("cp arriccio.toml package/arriccio.toml")

	-- copy executable
	local fs = assert(io.popen("ls target/debug"), "ls not working on your system")
	local s = nil
	while true do
		s = fs:read()
		if s then
			local m = s:match("intonaco")
			if m then
				os.execute("cp target/debug/" .. s .. " package/" .. dir .. "/intonaco.gio")
				break
			end
		else
			break
		end
	end
	fs:close()
end

local function buildIntonaco()
	-- build executable
	lfs.chdir("intonaco")
	os.execute("cargo build")
	packageIntonaco()
end

local function buildArriccio()
	lfs.chdir("arriccio")
	o, a = getOS()
	if o == "windows" then
		os.execute("go build -o aio.exe")
	else
		os.execute("go build -o aio")
	end
end

--[[
local function runDownload()
	-- download cbor-codec
	local cborDownloadPath = absPath(relToScriptPath("../cbor-codec"))
	local cmd = "wget -P " .. cborDownloadPath .. " -nv https://gitlab.com/twittner/cbor-codec/repository/archive.zip?ref=v0.7.1"
	os.execute(cmd)
	os.execute("cd " .. cborDownloadPath .. " && mv archive.zip* cbor-codec-0.7.1.zip")
end
--]]

-- do something else, if parameter says so
if #arg > 0 then
	if arg[1] == "intonaco" then
		buildIntonaco()
		os.exit(0)

	elseif arg[1] == "arriccio" then
		buildArriccio()
		os.exit(0)

	elseif arg[1] == "intonacoVersion" then
		intonacoVersion()
		os.exit(0)

	elseif arg[1] == "register-intonaco" then
		os.execute("scripts" .. pathSep() .. "aio local http://www.hgamer3d.org/component/Intonaco.0517 intonaco/package")
		os.exit(0)

	elseif arg[1] == "unregister-intonaco" then
		os.execute("scripts" .. pathSep() .. "aio remove-local http://www.hgamer3d.org/component/Intonaco.0517")
		os.exit(0)
	end

	print("wrong argument to build script:", arg[1])
	os.exit(-1)  -- wrong argument given

-- give hints about commands
else
	print([[

fresco build script, commands:

  intonaco - build intonaco
  register-intonaco - register local version in aio
  unregister-intonaco - remove registration in aio

  arriccio - build arriccio
	]])
	os.exit(0)
end

