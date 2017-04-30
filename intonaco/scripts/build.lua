local glue = require("glue")
glue.luapath(glue.bin .. "/../../lualib")

require("os_arch")
require("util")
require("lfs")

local function getVersion()
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


-- start script
local var version = getVersion()

local function runBuild()
	-- build executable
	os.execute("cargo build")
end

local function runPackage()
	-- clean output directory
	os.execute("rm -rf package")
	-- create directory 
	local dir = getPlatString("intonaco", version)
	lfs.mkdir("package")
	lfs.mkdir("package/" .. dir)
	-- os.execute("mkdir intonaco/package")
	-- os.execute("mkdir intonaco/package/" .. dir)

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

local function runDownload()
	-- download cbor-codec
	local cborDownloadPath = absPath(relToScriptPath("../cbor-codec"))
	local cmd = "wget -P " .. cborDownloadPath .. " -nv https://gitlab.com/twittner/cbor-codec/repository/archive.zip?ref=v0.7.1"
	os.execute(cmd)
	os.execute("cd " .. cborDownloadPath .. " && mv archive.zip* cbor-codec-0.7.1.zip")
end

-- do something else, if parameter says so
if #arg > 0 then
	if arg[1] == "build" then
		runBuild()
		os.exit(0)
	elseif arg[1] == "package" then
		runPackage()
		os.exit(0)
	elseif arg[1] == "update" then
		runBuild()
		runPackage()
		os.exit(0)
	elseif arg[1] == "version" then
		io.write(version)
		os.exit(0)
	elseif arg[1] == "platform" then
		io.write(getPlatString("", version))
		os.exit(0)
	elseif arg[1] == "register" then
		os.execute("scripts" .. pathSep() .. "aio local http://www.hgamer3d.org/component/Intonaco package")
		os.exit(0)
	elseif arg[1] == "unregister" then
		os.execute("scripts" .. pathSep() .. "aio remove-local http://www.hgamer3d.org/component/Intonaco")
		os.exit(0)
	end
	print("wrong argument to build script:", arg[1])
	os.exit(-1)  -- wrong argument given

-- give hints about commands
else
	print([[

fresco-intonaco build script, commands:

  build - build library
  package - create package after build
  update - rebuild and repackage
  version - give version of library
  register - register local version in aio
  unregister - remove registration in aio
	]])
	os.exit(0)
end

