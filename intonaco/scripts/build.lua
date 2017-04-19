require("os_arch")
require("util")

local function getVersion()
	local p = debug.getinfo(1).source:gsub('\\', '/'):match("@?(.*/)")
	if p then
		io.input(p.."../intonaco/Cargo.toml")
	else
		io.input("../intonaco/Cargo.toml")
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
	os.execute("cd intonaco && cargo build")
end

local function runPackage()
	-- clean output directory
	os.execute("rm -rf intonaco/package")
	-- create directory 
	local dir = getPlatString("intonaco", version)
	os.execute("mkdir intonaco/package")
	os.execute("mkdir intonaco/package/" .. dir)

	-- copy toml file
	os.execute("cp intonaco/arriccio.toml intonaco/package/arriccio.toml")

	-- copy executable
	local fs = assert(io.popen("ls intonaco/target/debug"), "ls not working on your system")
	local s = nil
	while true do
		s = fs:read()
		if s then
			local m = s:match("intonaco")
			if m then
				os.execute("cp intonaco/target/debug/" .. s .. " intonaco/package/" .. dir .. "/intonaco.gio")
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
	elseif arg[1] == "register" then
		os.execute("aio local http://www.hgamer3d.org/component/Intonaco intonaco/package")
		os.exit(0)
	elseif arg[1] == "unregister" then
		os.execute("aio remove-local http://www.hgamer3d.org/component/Intonaco")
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

