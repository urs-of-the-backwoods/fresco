local glue = require("glue")
glue.luapath(glue.bin .. "/lualib")

require("os_arch")
require("util")
require("lfs")

-- local functions, utilities
local function aioString()
	o, a = getOS()
	if o == "windows" then
		return  glue.bin .. "\\win\\aio.exe"	
	elseif o == "darwin" then
		return glue.bin .. "/darwin/aio"	
	elseif o == "linux" then
		return glue.bin .. "/linux/aio"	
	end
end

local function getIntonacoVersion()
	io.input("../intonaco/Cargo.toml")
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
	o, a = getOS()
	if o == "windows" then
		local fs = assert(io.popen(".." .. pathSep() .. aioString() .. " http://www.hgamer3d.org/tools/DetectVS.0717"), "detect vs not working")
		local msDir = fs:read()
		fs:close()
		msCmd = msDir .. "\\VC\\Auxiliary\\Build\\vcvars64.bat"
		os.execute("call \"" .. msCmd .. "\" && cargo build")
	else
		os.execute("cargo build")
	end
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

local function testSinopia()
	lfs.chdir("sinopia")
	os.execute(".." .. pathSep() .. aioString() .. " http://www.hgamer3d.org/tools/Stack.0617 test")
end

local function buildSinopia()
	lfs.chdir("sinopia")
	os.execute(".." .. pathSep() .. aioString() .. " http://www.hgamer3d.org/tools/Stack.0617 install --local-bin-path .")
end

local function helpText()
	print([[
fresco build script, usage:

build <command>

<command> might be:
  intonaco
  arriccio
  sinopia
  sinopia-test
  register-intonaco - register local version in aio
  unregister-intonaco - remove registration in aio
	]])
end

-- main script, check argument

if #arg > 0 then
	if arg[1] == "intonaco" then
		buildIntonaco()
		os.exit(0)

	elseif arg[1] == "arriccio" then
		buildArriccio()
		os.exit(0)

	elseif arg[1] == "sinopia" then
		buildSinopia()
		os.exit(0)

	elseif arg[1] == "sinopia-test" then
		testSinopia()
		os.exit(0)

	elseif arg[1] == "intonacoVersion" then
		intonacoVersion()
		os.exit(0)

	elseif arg[1] == "register-intonaco" then
		os.execute(aioString() .. " local http://www.hgamer3d.org/component/Intonaco.0517 intonaco/package")
		os.exit(0)

	elseif arg[1] == "unregister-intonaco" then
		os.execute(aioString() .. " remove-local http://www.hgamer3d.org/component/Intonaco.0517")
		os.exit(0)
	end

	print("wrong argument to build script:", arg[1])
end

-- in case no command exits, still give help and exit then
helpText()
os.exit(-1)

