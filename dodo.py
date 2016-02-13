#
#  Fresco Framework for Multi-Language Programming
#  Copyright 2015-2016 Peter Althainz
#    
#  Distributed under the Apache License, Version 2.0
#  (See attached file LICENSE or copy at 
#  http:#www.apache.org/licenses/LICENSE-2.0)
# 
#  file: dodo.py
#

import os, os.path, platform
from doit.tools import config_changed

def get_os():
	os = platform.system()
	if os == "Linux":
		return "linux"
	if os == "Windows":
		return "windows"
	return "???"

def get_arch():
	ar = platform.machine()
	if ar == "x86_64":
		return "amd64"
	if ar == "i386":
		return "386"


version_intonaco = "0.1.0"
arch_os = get_arch() + "-" + get_os()

def copy_file_replace(file_in, replace_in, replace_out, targets):
	with open(targets[0], "wt") as fout:
	    with open(file_in, "rt") as fin:
	        for line in fin:
	            fout.write(line.replace(replace_in, replace_out))

def task_intonaco():

	yield {
		'name' : 'compile',
	    'actions': ['cd intonaco && cargo build'],
	    'file_dep': ['intonaco/src/lib.rs'],
	    'targets': ['intonaco/target/debug/libintonaco.so'],
	} 

	yield {
		'name' : 'build-dir',
		'actions' : ['mkdir -p build-intonaco',
					 'mkdir -p build-intonaco/intonaco-' + arch_os + '-' + version_intonaco,
					 'cp intonaco/target/debug/libintonaco.so build-intonaco/intonaco-' + arch_os + '-' + version_intonaco,
		],
	    'file_dep': [
			'intonaco/target/debug/libintonaco.so',
			],
		'targets' : ['build-intonaco/intonaco-' + arch_os + '-' + version_intonaco + '/libintonaco.so',
	    		   ],
	}

	yield {
		'name' : 'arriccio',
	    'actions': [
	    	(copy_file_replace, ['interface/Intonaco', '{version}', version_intonaco]),
			'aio local http://www.hgamer3d.org/interface/Intonaco build-intonaco || true',
	    ],
	    'targets': ['build-intonaco/arriccio.toml'], 
	    'uptodate': [config_changed(version_intonaco)],
	    'file_dep': [
			'interface/Intonaco',
		]
	} 

def task_tests():

	yield {
		'name' : 'compile cube',
	    'actions': ['cd test_hg3d/cube-rust/ && cargo build'],
	    'file_dep': ['test_hg3d/cube-rust/src/main.rs'],
	    'targets': ['test_hg3d/cube-rust/target/debug/cube-rust'],
	} 

	yield {
		'name' : 'build-dir',
		'actions' : ['mkdir -p build-tests',
					 'cp test_hg3d/cube-rust/target/debug/cube-rust build-tests',
					 'cp interface/Cube build-tests',
		],
	    'file_dep': [
			'test_hg3d/cube-rust/target/debug/cube-rust',
			'interface/Cube',
			],
		'targets' : ['build-tests/cube-rust',
					 'build-tests/Cube',
		]
	}

def task_cube():
	return {
	    'actions': ['cd build-tests && 0install run Cube'],
	} 

def task_sinopia():
	return {
	    'actions': ['cd sinopia && stack build'],

	    'file_dep': [
			'sinopia/fresco-sinopia.cabal',
			'sinopia/src/sinopia.hs',
			],
		'targets' : ['sinopia/.stack-work/install/x86_64-linux/lts-3.5/7.10.2/bin/sinopia',
		]
	} 

def task_arriccio():
	os.environ['GOPATH'] =os.path.abspath('../go-home')
	return {
	    'actions': ['cd arriccio && go build -o aio main.go && cp aio ~/.local/bin/aio'],

	    'file_dep': [
			'arriccio/main.go',
			],
		'targets' : ['arriccio/aio',
					 '~/.local/bin/aio'
		]
	} 

 
#@task()
#def hg3d():
#    """compiles the hg3d test samples and data files"""
#    runCmd("mkdir -p test_hg3d/output")
#    for f in [
#                "Graphics3DConfig", 
#                "Graphics3DCommand",
#                "Camera",
#                "Light",
#                "Angle",
#                "Colour",
#                "Material",
#                "Geometry",
#                "Vec3"
#                ]:
#        runCmd("cd test_hg3d && sinopia -i patterns/" + f + ".sp -o output/" + f + ".hs -g Haskell")
#        runCmd("cd test_hg3d && sinopia -i patterns/" + f + ".sp -o output/" + f + ".rs -g Rust")

