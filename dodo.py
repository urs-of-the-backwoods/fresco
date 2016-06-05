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
from doit.tools import config_changed, run_once

def make_dir(d, targets):
	d = d.replace("/", os.sep)
	if not os.path.exists(d):
		os.makedirs(d)

def get_os():
	os = platform.system()
	if os == "Linux":
		return "linux"
	if os == "Windows":
		return "windows"
	if os == "Darwin":
		return "darwin"
	print "don't know, which platform this is: ", os
	os.exit()

def get_arch():
	ar = platform.machine()
	if ar == "x86_64":
		return "amd64"
	if ar == "AMD64":
		return "amd64"
	if ar == "i386":
		return "386"
	print "don't know, which arch this is: ", ar
	os.exit()


version_intonaco = "0.1.1"
version_haskell_fresco = "0.1.1"


arch_os = get_arch() + "-" + get_os()

def copy_file_replace(file_in, replace_dict, targets):
	with open(targets[0], "wt") as fout:
	    with open(file_in, "rt") as fin:
	        for line in fin:
	        	line_out = line
	        	for k in replace_dict.keys():
	        		line_out = line_out.replace(k, replace_dict[k])
		        fout.write(line_out)

def task_intonaco():
	if get_os() == "windows":
		intonaco_lib = "intonaco.dll"
	elif  get_os() == "darwin":
		intonaco_lib = "libintonaco.dylib"
	else:
		intonaco_lib = "libintonaco.so"
	yield {
		'name' : 'compile',
	    'actions': ['cd intonaco && cargo build'],
	    'file_dep': ['intonaco/src/lib.rs', 'intonaco/src/lockfree_value.rs', 'intonaco/src/thread_guard.rs'],
	    'targets': ['intonaco/target/debug/libintonaco.so'],
	} 

	yield {
		'name' : 'build-dir',
		'actions' : [
	    			 (make_dir, ['build-intonaco']),
	    			 (make_dir, ['build-intonaco/intonaco-' + arch_os + '-' + version_intonaco]),
					 'cp intonaco/target/debug/' + intonaco_lib + ' build-intonaco/intonaco-' + arch_os + '-' + version_intonaco + '/intonaco.gio',
		],
	    'file_dep': [
			'intonaco/target/debug/' + intonaco_lib,
			],
		'targets' : ['build-intonaco/' + intonaco_lib + '-' + arch_os + '-' + version_intonaco + '/intonaco.gio',
	    		   ],
	}

	yield {
		'name' : 'arriccio',
	    'actions': [
	    	(copy_file_replace, ['component/Intonaco', {'{version}' : version_intonaco} ]),
			'aio local http://www.hgamer3d.org/component/Intonaco build-intonaco || true',
	    ],
	    'targets': ['build-intonaco/arriccio.toml'], 
	    'uptodate': [config_changed(version_intonaco)],
	    'file_dep': [
			'component/Intonaco',
		]
	} 

def task_haskell():

	yield {
		'name' : 'cabal',
	    'actions': [
	    	(copy_file_replace, ['haskell/fresco-binding.cabal.tmpl', {'{version}' : version_haskell_fresco} ]),
	    ],
	    'targets': ['haskell/fresco-binding.cabal'],
	    'uptodate': [config_changed(version_haskell_fresco)],
	    'file_dep': [
			'haskell/fresco-binding.cabal.tmpl',
		]
	} 

	yield {
		'name' : 'library',
		'actions' : [
					 'cd haskell && stack build',
					 'cd haskell && stack sdist',
	    			 (make_dir, ['build-haskell']),
					 'cd haskell && bash -c "cp `find .stack-work | grep .tar.gz` ../build-haskell"'
					],
		'targets' : ['build-haskell/fresco-binding-' + version_haskell_fresco + '.tar.gz'],
	    'file_dep': [
			'haskell/Fresco/Component.hs',
			'haskell/Fresco/Entity.hs',
			'haskell/Fresco/System.hs',
			'haskell/fresco-binding.cabal',
			'haskell/Fresco.hs',
			'haskell/LICENSE',
			'haskell/stack.yaml',
		],
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
		'actions' : [
	    			 (make_dir, ['build-tests']),
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
	os.environ['GOPATH'] = os.path.abspath('../go-home')
	if platform.system() == "Windows":
		copy_target = os.environ['APPDATA'] + '/local/bin/aio.exe'
	else:
		copy_target = '~/.local/bin/aio'


	yield {
		'name': 'golibs',
		'actions': [
   			 (make_dir, ['../go-home']),
			'go get golang.org/x/crypto/ripemd160',
			'go get github.com/BurntSushi/toml',
			'go get github.com/asaskevich/govalidator',
		],
		'targets': [
					'../go-home',
					'../go-home/src/github.com/asaskevich',
					'../go-home/src/github.com/BurntSushi',
					'../go-home/src/golang.org/x/crypto/ripemd160',
		],
        'uptodate': [run_once],
	}

	yield {
		'name': 'compile',
	    'actions': ['cd arriccio && go build -o aio main.go && cp aio ' + copy_target],  # -ldflags -H=windowsgui 
		'targets' : [copy_target,
					 'arriccio/aio'
		],
	    'file_dep': [
			'arriccio/main.go',
			],
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

