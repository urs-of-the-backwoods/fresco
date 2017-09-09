// 
//  Fresco Framework for Multi-Language Programming
//  Copyright 2015-2017 Peter Althainz
//    
//  Distributed under the Apache License, Version 2.0
//  (See attached file LICENSE or copy at 
//  http://www.apache.org/licenses/LICENSE-2.0)
// 
//  file: arriccio/main.go
//

// arriccio - implemented as an executable named "aio" - is a component loader with dependency injection.
// Components are described in a toml file and stored together with their implementation files on the web.
// On invocation aio will download and execute them.
// It is possible to alias local directories for a component, to enable local development and testing.

package main

import (

	"log"
	"bytes"
	"os"
	"os/exec"
	"io"
	"io/ioutil"
    "bufio"
	"fmt"
	"runtime"
	"path/filepath"
	"strings"
	"encoding/hex"
    "encoding/base64"

    "crypto/rand"
    "crypto/sha256"

	"archive/tar"
	"compress/gzip"
	"net/http"

    "golang.org/x/crypto/ssh"
	"golang.org/x/crypto/ripemd160"

	"github.com/BurntSushi/toml"
	"github.com/asaskevich/govalidator"
)

//
// version
//

var version_aio = "0.2.2"

// version remarks
// 0.2.2
//	  new names for all the sections, to make arriccio.toml files clearer
//
// 0.2.0
//	  removed all version checks, simplified config file
//	  one ssh key for all downloads of one package

//
// General helper routines
//

func createClient() *http.Client {
	tr := &http.Transport{
		DisableCompression: true,
	}
	client := &http.Client{Transport: tr}
	return client
}

// global client
var httpClient *http.Client

// isUrlValid checks if url is a valid one, only check http:
func isUrlValid(url string) bool {
	return len(url) >= 5 && url[:5] == "http:" && govalidator.IsURL(url)
}

func checkNameUrl(cmd string, db AliasDB) {
	if isUrlValid(cmd) {
		return 
	}
	if _, ok := db.Commands[cmd]; ok {
		return 
	}
	log.Fatal("need <url> or <name> not: ", cmd)
}
// isLocalDirValid checks if the path is pointing to a directory.
// It returns the absolute path to that dir and a success indicator (bool).
func isLocalDirValid(dir string) (string, bool) {
	src, err := os.Stat(dir)
	if err == nil {
		abspath, _ := filepath.Abs(dir)
		return abspath, src.IsDir()
	}
	return dir, false
}

// getUserHomeDir gives back the home directory of the user.
// It should work in all targeted os.
func getUserHomeDir() string {
	if runtime.GOOS == "windows" {
		home := os.Getenv("HOMEDRIVE") + os.Getenv("HOMEPATH")
		if home == "" {
			home = os.Getenv("USERPROFILE")
		}
		return home
	}
	return os.Getenv("HOME")
}

// matchArchAndOs compares a give os and architecture string with the values 
// obtained from platform at runtime and returns true if they match.
func matchArchAndOs(arch string, os string) bool {
	// architecture can be 386, amd64, amd64p32, ppc64 or arm
	// os can be darwin, freebsd, linux, windows
//	println("match os, target: ", os, " runtime: ", runtime.GOOS)
//	println("match arch, target: ", arch, " runtime: ", runtime.GOARCH)
	return (arch == "*" || arch == runtime.GOARCH) && 
		   (os == "*" || os == runtime.GOOS)
}

// extractTarGzFile extracts a gzipped tar file into output directory.
func extractTarGzFile(sourcefile string, outdir string) {

		os.MkdirAll(outdir, 0770)
//		 println("extract tar gz file: ", sourcefile)

         if sourcefile == "" {
                 log.Fatal("extract tgz file: empty filename")
         }

         file, err := os.Open(sourcefile)

         if err != nil {
                 fmt.Println(err)
                 os.Exit(1)
         }

         defer file.Close()

         var fileReader io.ReadCloser = file

         // add a filter to handle gzipped file
         if fileReader, err = gzip.NewReader(file); err != nil {

                 fmt.Println(err)
                 os.Exit(1)
         }
         defer fileReader.Close()

         tarBallReader := tar.NewReader(fileReader)

         // Extracting tarred files

         for {
                 header, err := tarBallReader.Next()
                 if err != nil {
                         if err == io.EOF {
                                 break
                         }
                         fmt.Println(err)
                         os.Exit(1)
                 }

                 // get the individual filename and extract to the current directory
                 filename := filepath.Join(outdir, header.Name)

                 switch header.Typeflag {
                 case tar.TypeDir:
                         // handle directory
//                         fmt.Println("Creating directory :", filename)
                         err = os.MkdirAll(filename, os.FileMode(header.Mode)) // or use 0755 if you prefer

                         if err != nil {
                                 fmt.Println(err)
                                 os.Exit(1)
                         }

                 case tar.TypeReg:
                         // handle normal file
//                         fmt.Println("Untarring :", filename)
                         writer, err := os.Create(filename)

                         if err != nil {
                                 fmt.Println(err)
                                 os.Exit(1)
                         }

                         io.Copy(writer, tarBallReader)

                         err = os.Chmod(filename, os.FileMode(header.Mode))

                         if err != nil {
                                 fmt.Println(err)
                                 os.Exit(1)
                         }

                         writer.Close()

		case tar.TypeLink:
			dest := filepath.Join(outdir, header.Linkname)
			if err := os.Link(dest, filename); err != nil {
                         	fmt.Printf("error in untar type : %c in file %s", header.Typeflag, filename)
				return 
			}

		case tar.TypeSymlink:
			if err := os.Symlink(header.Linkname, filename); err != nil {
                         	fmt.Printf("error in untar type : %c in file %s", header.Typeflag, filename)
				return 
		}

                 default:
                         fmt.Printf("Unable to untar type : %c in file %s", header.Typeflag, filename)
                 }
         }

}

func compileHash(fname string) []byte {

    hasher := sha256.New()

    f, err := os.Open(fname)
    if err != nil {
        log.Fatal("error open file to hash: ", err)
    }
    defer f.Close()
    if _, err := io.Copy(hasher, f); err != nil {
        log.Fatal(err)
    }

    return hasher.Sum(nil)
}

func signFile(fname string, keyFile string) {
    hs := compileHash(fname)
    signer := loadPrivateKey(keyFile)
    signed, err := signer.Sign(rand.Reader, hs)
    if err != nil {
        log.Fatal("could not sign: ", err)
    }
    writeSignature(fname + ".sig", *signed)
}

func verifyFile(fname string, sigFile string, keyFile string) bool {
    hs := compileHash(fname)
    publicKey := loadPublicKey(keyFile)
    s := readSignature(sigFile)
    err := publicKey.Verify(hs, &s)
    if err == nil {
        return true
    } else {
        return false
    }
}

func writeSignature(fname string, sig ssh.Signature) {
    f, err := os.Create(fname)
    if err != nil {
    	log.Fatal("error creating signature file: ", err)
    }
    defer f.Close()
    f.WriteString(sig.Format)
    f.WriteString("\n")
    f.WriteString(base64.StdEncoding.EncodeToString(sig.Blob))
    f.WriteString("\n")
} 

func readSignature(fname string) ssh.Signature {
    f, err := os.Open(fname)
    if err != nil {
    	log.Fatal("error open signature file: ", err)
    }
    defer f.Close()
    s := bufio.NewScanner(f)
    var sig ssh.Signature
    s.Scan()
    sig.Format = s.Text()
    s.Scan()
    sig.Blob, _ = base64.StdEncoding.DecodeString(s.Text())
    return sig
} 


// loadPrivateKey loads an parses a PEM encoded private key file.
func loadPublicKey(path string) ssh.PublicKey {

    bs, err := ioutil.ReadFile(path)
    if err != nil {
    	log.Fatal("error open public key file: ", err)
    }
    rsa, _, _, _, err2 := ssh.ParseAuthorizedKey(bs)
    if err2 != nil {
    	log.Fatal("error cannot parse public key file: ", err2)
    }
    return rsa
}


// loadPrivateKey loads an parses a PEM encoded private key file.
func loadPrivateKey(path string) ssh.Signer {
    bs, err := ioutil.ReadFile(path)
    if err != nil {
    	log.Fatal("error open private key file: ", err)
    }
    rsa, err2 := ssh.ParsePrivateKey(bs)
    if err2 != nil {
    	log.Fatal("error cannot parse private key file: ", err2)
    }
    return rsa
}




//
// main function
//


// Main entry point of arriccio tool.
// Fulfills two purposes: manages the database of aliases and local url substitutes and
// runs a command with dependency injection from configuration files (arriccio.toml).
func main() {

	db := readAliasDB()
	httpClient = createClient()

	if len(os.Args) == 1 {
		// give help
		println("\naio (arriccio, all in one) command:\n")
		println("  aio alias <name> <url>           - gives an alias to a target url")
		println("  aio list alias                   - list given alias")
		println("  aio remove-alias <name> <url>    - removes an alias")
		println("")
		println("  aio local <url> <local-dir>      - caches a local implementation dir for a target url")
		println("  aio list local                   - list given local dirs")
		println("  aio remove-local <url>           - removes a local implementation dir")
		println("")
		println("  aio sign <file> <private key>    - create signature for file with private key")
		println("  aio verify <file> <public key>   - verify if signature is correct")
		println("")
		println("  aio deps <name> | <url>          - prints dependencies")
		println("  aio unsafe <name> | <url> [args] - process a target url without asking for confirmation")
		println("  aio version                      - displays version information")
		println("")
		println("  aio info <name> | <url>          - prints information about a component")
		println("  aio license <name> | <url>       - prints detailed license information about a component")
		println("")
		println("  aio update <name> | <url> [args] - updates a target component - re-read url from internet")
		println("  aio start <name> | <url> [args]  - executes a target component - no console")
		println("  aio <name> | <url> [args]        - executes a target component - with console i/o")
		println("")
	} else {

		// command processing

		switch os.Args[1] {

		case "version":
			{
				println("aio version " + version_aio + " running on", runtime.GOARCH + "-" + runtime.GOOS);
			}
		// aio alias <name> <url>
		// aio remove-alias <name>
		case "alias":
			{
				if len(os.Args) == 4 {
					url := os.Args[3]
					if isUrlValid(url) {
						db.Commands[os.Args[2]] = url
						writeAliasDB(db)
					} else {
						log.Fatal("url: ", url, " is not valid!")
					}
				} else {
					log.Fatal("alias needs two parameters: aio alias <name> <url>")
				}
			}
		case "remove-alias":
			{
				if len(os.Args) == 3 {
					delete(db.Commands, os.Args[2])
					writeAliasDB(db)
				} else {
				 log.Fatal("remove-alias needs one parameter: aio remove-alias <name>")
			    }
			}

			// aio local <url> local-dir
			// aio remove-local <url>
		case "local":
			{
				if len(os.Args) == 4 {
					url := os.Args[2]
					if isUrlValid(url) {
						abs, ok := isLocalDirValid(os.Args[3])
						if ok {
							db.Locals[url] = abs
							writeAliasDB(db)
						} else {
							log.Fatal("dir: ", os.Args[3], " is not valid!")
						}
					} else {
						log.Fatal("url: ", url, " is not valid!")
					}
				} else {
					log.Fatal("local needs two parameters: aio local <url> <local-dir>")
				}
			}
		case "remove-local":
			{
				if len(os.Args) == 3 {
					delete(db.Locals, os.Args[2])
					writeAliasDB(db)
				} else {
					log.Fatal("remove-local needs one parameter: aio remove-local <url>")
				}
			}

		// aio list alias, aio list local
		case "list":
			{
				if len(os.Args) == 3 {
					switch os.Args[2] {
					case "alias":
						for k, v := range db.Commands {
							println(k, " - ", v)
						}
					case "local":
						for k, v := range db.Locals {
							println(k, " - ", v)
						}
					default:
						log.Fatal("list needs either alias or list as parameter")
					}
				} else {
					log.Fatal("list needs one parameter, either aio list alias or aio list local")
				}
			}

		// aio sign <file> <private key>    - create signature for file with private key
		case "sign":
			{
				if len(os.Args) == 4 {
			        signFile(os.Args[2], os.Args[3])
				} else {
					log.Fatal("sign needs two parameters, aio sign <file> <private key>")
				}
			}

		// aio verify <file> <public key>   - verify if signature is correct
		case "verify":
			{
				if len(os.Args) == 4 {
			        if verifyFile(os.Args[2], os.Args[2] + ".sig", os.Args[3]) {
			            println("file is correctly signed")
			            os.Exit(0)
			        } else {
			            println("file is not properly signed, signature does not fit")
			            os.Exit(-1)
			        }
				} else {
					log.Fatal("verify needs two parameters, aio verify <file> <public key>")
				}
			}

		// aio deps <name | url>
		case "deps":
			{
				if len(os.Args) == 3 {
					checkNameUrl(os.Args[2], db)
					runComponentWithDependencies(os.Args[2], db, getArriccioDir(), os.Args[3:], true, false, true, false)
				} else {
					log.Fatal("deps needs one parameter, aio deps <name | url>")
				}
			}

		// aio unsafe <name | url>
		case "unsafe":
			{
				if len(os.Args) >= 3 {
					checkNameUrl(os.Args[2], db)
					runComponentWithDependencies(os.Args[2], db, getArriccioDir(), os.Args[3:], false, true, true, false)
				} else {
				log.Fatal("unsafe needs one parameter, aio unsafe <name | url>")
				}
			}

		case "info":
			{
				if len(os.Args) == 3 {
					checkNameUrl(os.Args[2], db)
					showComponentInfo(os.Args[2], db);
				} else {
					log.Fatal("info needs one parameter, aio info <name | url>")
				}
			}

		case "license":
			{
				if len(os.Args) == 3 {
					checkNameUrl(os.Args[2], db)
					showLicenseInfo(os.Args[2], db);
				} else {
					log.Fatal("license needs one parameter, aio license <name | url>")
				}
			}

		// aio start <name | url>
		case "start":
			if len(os.Args) >= 3 {
				checkNameUrl(os.Args[2], db)
				runComponentWithDependencies(os.Args[2], db, getArriccioDir(), os.Args[3:], false, false, false, false)
			} else {
				log.Fatal("start needs one parameter, aio start <name | url>")
			}

		// aio start <name | url>
		case "update":
			if len(os.Args) == 3 {
				checkNameUrl(os.Args[2], db)
				runComponentWithDependencies(os.Args[2], db, getArriccioDir(), os.Args[3:], false, false, false, true)
			} else {
				log.Fatal("update needs one parameter, aio update <name | url>")
			}

		// aio <name | url>
		default:
			if len(os.Args) >= 2 {
				checkNameUrl(os.Args[1], db)
				runComponentWithDependencies(os.Args[1], db, getArriccioDir(), os.Args[2:], false, false, true, false)
			} else {
				log.Fatal("aio needs at least one parameter, aio <name | url>")
			}

		}

	} 
}

//
// some basic arriccio specific file handling
//

func getArriccioDir() string {
	dir := getUserHomeDir()
	arrdir := filepath.Join(dir, ".aio")
	_, ok := isLocalDirValid(arrdir)
	//	println("getArriccioDir: ", arrdir, ok)
	if !ok {
		os.MkdirAll(arrdir, 0770)
		os.MkdirAll(filepath.Join(arrdir, "cache"), 0770)
		os.MkdirAll(filepath.Join(arrdir, "impl"), 0770)
	}
	return arrdir
}

//		fname, isCached := checkUrlIsChached(el.impl.Location)

func checkUrlIsCached(url string) (string, bool) {
	// check if file is in cache
	cdir := filepath.Join(getArriccioDir(), "cache")
	h := ripemd160.New()
	h.Write([]byte(url))
	hh := h.Sum(nil)
	fname := filepath.Join(cdir, hex.EncodeToString(hh[:]))
	_, err := os.Stat(fname)
	return fname, (err == nil)
}	

func getImplFName(fname string) string {
	// get filename from path
	base := filepath.Base(fname)
	idir :=  filepath.Join(getArriccioDir(), "impl")
	iname := filepath.Join(idir, base) + ".i"
	return iname
}

func getUrlAsCachedFile(urln string, update bool) (string, bool) {
	// check if file is in cache
	fname, isCached := checkUrlIsCached(urln)
	if update || !isCached {
		// download
		resp, err1 := httpClient.Get(urln)
		if err1 == nil {
			// check response for 404 error
			if resp.StatusCode == http.StatusOK {
				fo, err2 := os.Create(fname)
				if err2 == nil {
					io.Copy(fo, resp.Body)
					fo.Close()
				} else {
					log.Fatal("cannot open file: ", fname)
				}
			} else {
				log.Fatal("cannot download urln: ", urln, ", http error: ", resp.StatusCode)
			}
			resp.Body.Close()
		} else {
			log.Fatal("cannot open url: ", urln)
		}
	} else {
//		println("take file from cache: ", fname)
	}
	abs, _ := filepath.Abs(fname)
	return abs, isCached
}

func getRemoteComponent(urln string, update bool) (string, Component, bool) {
	fname, isCached := getUrlAsCachedFile(urln, update)
	dat, _ := ioutil.ReadFile(fname)
	aif := readComponent(string(dat))
	if !isCached {
		// check url is consistent with id
		if urln != aif.Id {
			os.Remove(fname)
			log.Fatal("downloaded component description has not correct id!\n   url:", urln, "\n   id:", aif.Id)
		}
		// check key is https - important for security !!!
		if len(aif.SigningKey) < 9 || aif.SigningKey[0:8] != "https://" {
			log.Fatal("key file not provided or not to be downloaded over https: ", aif.SigningKey)
		}
		// verify signature
		fsig, _ :=  getUrlAsCachedFile(urln + ".sig", update)
		fkey, _ :=  getUrlAsCachedFile(aif.SigningKey, update)
		if (!verifyFile(fname, fsig, fkey)) {
			os.Remove(fname)
			os.Remove(fsig)
			log.Fatal("downloaded component description not correctly signed: ", urln)
		}
	}
	return fname, aif, !isCached
}

func getRemoteFile(urln string, urlkey string, update bool) string {
	fname, isCached := getUrlAsCachedFile(urln, update)
	if !isCached {
		fsig, _ :=  getUrlAsCachedFile(urln + ".sig", update)
		fkey, _ :=  getUrlAsCachedFile(urlkey, update)
		if (!verifyFile(fname, fsig, fkey)) {
			os.Remove(fname)
			os.Remove(fsig)
			log.Fatal("downloaded component description not correctly signed: ", urln)
		}
	}
	return fname
}

//
// Database of command aliases and local substitute aliases
//

type AliasDB struct {
	Commands map[string]string
	Locals   map[string]string
}

func readAliasDB() AliasDB {
	fname := filepath.Join(getArriccioDir(), "arr_db")
	dat, err := ioutil.ReadFile(fname)
	if err == nil {
		var db AliasDB
		_, err := toml.Decode(string(dat), &db)
		if err == nil {
			return db
		} else {
			fmt.Println(err)
			log.Fatal("cannot read alias db")
		}
	}
	return AliasDB{make(map[string]string), make(map[string]string)}
}

func writeAliasDB(db AliasDB) {
	fname := filepath.Join(getArriccioDir(), "arr_db")

	var buf bytes.Buffer
	e := toml.NewEncoder(&buf)
	err := e.Encode(db)
	if err != nil {
		fmt.Println(err)
		log.Fatal("cannot write alias db")
	} else {
		ioutil.WriteFile(fname, buf.Bytes(), 0660)
	}
}


//
// Run a command with dependency injection 
//
// main complexity of arriccio command
// contains:
//	data format for dependency description
//  mechanism to read those descriptions from file or url
//  mechanism to resolve dependencies from multiple implementation options
//  mechanism to finally run the program with all required dependencies
//

//
// Components
//

// Components describe some functionality or data. They can be implemented for different platforms, os.
type Component struct {
	Id          string `toml:"id-url"` // Url as id
	Description string `toml:"description"` // description
	SigningKey	string `toml:"signing-key"` // https location of public key for signature
	License     string `toml:"license-short"` // License type
	FullLicenseText string `toml:"license-full"`  // full text of license, included in component description
	Implementations []ComponentImplementation `toml:"implementation"`
}

// ComponentImplementation describes the properties of an implementation of a component.
// In addition to metadata it also contains the components, it depends on, the dependencies.
type ComponentImplementation struct {
	Architecture string `toml:"architecture"`
	OS           string `toml:"operating-system"`
	Location     string `toml:"archive-download-location"` // download Url tgz
	Command      string `toml:"start-local-command"`
	Environment  []string `toml:"environment-settings"`
	Dependencies []ImplementationDependency `toml:"dependency"`
}

// A dependency describes on which other components an implementation of a component depends on.
// There is a version constraint, which says which range of versions is acceptable for the implementation.
type ImplementationDependency struct {
	Id                string `toml:"id-url"`
	Environment       []string `toml:"environment-settings"`
}

// routines to manage components

func readComponent(dat string) Component {
	var db Component
	_, err := toml.Decode(dat, &db)
	if err != nil {
		log.Fatal("cannot read component ", err)
	}
	return db
}

func writeComponent(db Component, fname string) {
	var buf bytes.Buffer
	e := toml.NewEncoder(&buf)
	err := e.Encode(db)
	if err != nil {
		log.Fatal("cannot write component ", err)
	} else {
		ioutil.WriteFile(fname, buf.Bytes(), 0660)
	}
}

func getComponentFromUrl(db AliasDB, url string, update bool) (Component, string, bool) {

	fname := ""
	ifloc := ""  // component location, directory if locally found

	// check url
	var dat []byte
	var aif Component
	var isDownload bool = false

	if isUrlValid(url) {
		// file is available locally, since alias is defined
		if val, ok := db.Locals[url]; !update && ok {
			// check valid path
			abspath, ok := isLocalDirValid(val)
			if !ok {
				log.Fatal("Local arriccio file not valid: ", url, fname)
			}
			ifloc = abspath
			fname = filepath.Join(abspath, "arriccio.toml")
			if _, err := os.Stat(fname); os.IsNotExist(err) {
				log.Fatal("Local arriccio file not valid: ", fname)
			}
			dat, _ = ioutil.ReadFile(fname)
			aif = readComponent(string(dat))
		// file needs to be downloaded or taken from cache
		} else {
			fname, aif, isDownload = getRemoteComponent(url, update)
		}
	} else {
		log.Fatal("Component id is not a valid url: ", url)
	}
	// check, if url is correct
	if url != aif.Id {
		log.Fatal("downloaded component description has not correct id!\n   url:", url, "\n   id:", aif.Id)
	}

	return aif, ifloc, isDownload
}


//
// Processing Components, running commands with dependency injection
//

// We process dependency resolution in steps, each step adds some information to the results.
type DependencyProcessingInfo struct {
	comp Component
	impl ComponentImplementation
	installdir string
	settings []string
}

func printDepProcInfo(ri DependencyProcessingInfo) {
	println("command: ", ri.impl.Command)
	println("location: ", ri.impl.Location)
	println("installdir: ", ri.installdir)
	println("settings: ")
	for _, s := range ri.settings {
		println("   ", s)
	}
}

// install info
type InstallInfo struct {
	url string
	key string
	license string
	installdir string
}

// resolveDependencies takes a url (the "command") and resolves all dependencies. This means 
// gathering all needed implementations and find suitable versions, as well as putting together
// the needed environment setting, which are used to run the command.
// The output is a bool stating if the resolution was successful (true) and a list of 
// gathered processing information for the next step - running the command.
func resolveDependencies(db AliasDB, cmd string, thisdep []ImplementationDependency, update bool) (bool, []DependencyProcessingInfo) {

//	println("resolve Dependencies for: ", cmd)

	// get aif and process
	// url is either given or taken from alias database
	url := cmd
	// check, do we have an alias for command
	if val, ok := db.Commands[cmd]; ok {
		url = val
	}
	// load toml file, returns directory, if locally found
	aif, ifloc, isDownload := getComponentFromUrl(db, url, update)

	// resultlist
	rlist := []DependencyProcessingInfo{}

	// build list of impl, sorted by version
	ilist := []ComponentImplementation{}
	for _, impl := range aif.Implementations {
		if matchArchAndOs(impl.Architecture, impl.OS) {
			ilist = append(ilist, impl)
		}
	}

	// go through implementations
	implok := false
	for _, impl := range ilist {

		// check dependencies are ok
		rdeps := []DependencyProcessingInfo{}
		depsok := true
		for _, dep := range impl.Dependencies {
			// get valid impl
			ok, r := resolveDependencies(db, dep.Id, []ImplementationDependency{dep}, update || isDownload)
			if ok {
				for _, el := range r {
					rdeps = append(rdeps, el)
				}
			} else {
				depsok = false
				break
			}
		}

		if depsok {
			location := ""
			// special handling of local directories: if component was found locally and subdirectory of
			// implementation exists, then set location to this subdirectory.
			if ifloc != "" {
				// get last part of url
				parts := strings.Split(impl.Location, "/")
				dname := parts[len(parts)-1]
				// remove .tar.gz from name
				dname = strings.Replace(dname, ".tar.gz", "", -1)
				// see if directory exists
				fdir := filepath.Join(ifloc, dname)
				abs, ok := isLocalDirValid(fdir)
//				println(fdir, " ", abs, " ", ok)
				if ok {
					location = abs
				}
			}

			// impl found, now put together return values
			implok = true
			var newd DependencyProcessingInfo
			if len(thisdep) > 0 {
				newd = DependencyProcessingInfo{
					aif,
					impl,
					location,
					append(impl.Environment, thisdep[0].Environment...), 			// this is important, here happens dep. injections, we transfer dep env to settings
				} 
			} else {
				newd = DependencyProcessingInfo{
					aif,
					impl,
					location,
					impl.Environment,
				} 
			}
			rlist = append(rlist, newd)
			for _, el := range rdeps {
				rlist = append(rlist, el)
			}
			break
		}
	}

	return implok, rlist
}

func enrichDepProcInfoWithInstallDir(db AliasDB, depi []DependencyProcessingInfo) ([]DependencyProcessingInfo, []InstallInfo) {

	// go ahead and install
	var rlist []DependencyProcessingInfo
	var ilist []InstallInfo

	for _, el := range depi {

		// check if needed, or if local dir already exists
		locdir := el.installdir
		if locdir != "" { 
			rlist = append(rlist, el)
		} else if len(el.impl.Location) > 0 {

			fname, isCached := checkUrlIsCached(el.impl.Location)
			if !isCached {
				ilist = append(ilist, InstallInfo{el.impl.Location, el.comp.SigningKey, el.comp.License, fname})
			}

			// enrich output with directory name
			el.installdir = getImplFName(fname)
			rlist = append(rlist, el) 
		}
	}

	return rlist, ilist
}

func installDownloads(infos []InstallInfo, unsafe bool) {
	// ask user if downloads can be accepted
	if len(infos) == 0 {return}
	inline := "unsafe"

	if !unsafe {
		println("\narriccio is going to download and install the following files:\n" +
				"--------------------------------------------------------------\n" +
				"(more license info can be obtained by using the \"aio license\" cmd)\n")

		for _, ii := range infos {
			println("file: ", ii.url, "\n signing key: ", ii.key, "\n license: ", ii.license, "\n")
		}

		print("please confirm download with \"yes\": ")
		reader := bufio.NewScanner(os.Stdin)
		reader.Scan()
		inline = reader.Text()
	}

	if unsafe || inline == "yes" {
		for _, el := range infos {
			// download and install procedure
			url := el.url
			key := el.key

			// check key is https - important for security !!!
			if len(key) < 9 || key[0:8] != "https://" {
				log.Fatal("key file not provided or not to be downloaded over https: ", key)
			}

			print("downloading: ", url)
			// download data file, signature and key
			fname := getRemoteFile(url, key, false)
			println(" - done")

			// extract, if signature is correct
			dname := getImplFName( fname)
			// check if already in cache, extracted
			_, err := os.Stat(dname)
			if err != nil {
				// else extract
				extractTarGzFile(fname, dname)
			}
		}
	} else {
		log.Fatal("download not authorized")
	}
}

func evaluateEnvSetting(env []string, settings []string, installdir string) []string {
	// interpretation of environment commmand settings
	// possible commands
	//
	// add-path ENVVAR rel-path [sep]
	// add-val ENVVAR val [sep]
	//

	// create map
	m := make(map[string]string)
	orig_key := make(map[string]string)
	for _, e := range env {
		kvs := strings.SplitN(e, "=", 2)
		k := strings.TrimSpace(kvs[0])
		uk := strings.ToUpper(k)
		m[uk] = strings.TrimSpace(kvs[1])
		orig_key[uk] = k
	}

	// modify map
	for _, s := range settings {
		fs := strings.Fields(s)
		if len(fs) == 3 || len(fs) == 4 {
			v, ok := m[fs[1]]
			val := ""
			if fs[0] == "add-path" {
				val1, err := filepath.Abs(filepath.Join(installdir, fs[2]))
				if err != nil {
					log.Fatal("wrong relative path given: ", fs[2], err)
				}
				val = val1
			} else if fs[0] == "set-value" {
				val = fs[2]
			} else {
				log.Fatal("wrong command in setting: ", fs[0])
			}

			// check for separator
			s := string(os.PathListSeparator)
			if len(fs) == 4 { s = fs[3] }

			// compose result
			if ok {
				m[strings.ToUpper(fs[1])] = v + s + val
			} else {
				m[strings.ToUpper(fs[1])] = val
			}

//			println("changed env: ", fs[1])
//			println("value: ", m[fs[1]])
		} else {
			log.Fatal("wrong number of arguments in setting: ", s)			
		}
	}

	// create output environment
	out := []string{}
	for k, v := range m {
		if val, ok := orig_key[k]; ok {
			out = append(out, val + "=" + v)
		} else {
			out = append(out, k + "=" + v)
		}
	}
	return out
}

func composeEnvironmentAndRunCommand(depi []DependencyProcessingInfo, args []string, console bool) {
	// path handling: path of command will be added at the end of PATH
	// other env: will be added or set, depending on previous setting

	// adapt environment, set binary
	env := os.Environ()
	binary := ""
	arglist := args

	for i := len(depi)-1; i >= 0; i-- {  // reversed loop, to start with lowest dependency
		el := depi[i]

		env = evaluateEnvSetting(env, el.settings, el.installdir)

		// command handling, binary will include all commands separated by space to allow command chaining
		if len(el.impl.Command) != 0 {
			bparts := strings.Fields(el.impl.Command)
			newcmd := el.installdir + string(os.PathSeparator) + bparts[0]  // append installdir to command

			if len(binary) != 0 {	// if there is already a command, push this one to arglist
				a := make([]string, 1)
				a[0] = newcmd
				arglist = append(a, arglist...)
			} else {
				binary = newcmd
				arglist = append(bparts[1:], arglist...)
			}
		}
	}	

/*
	print(binary)
	for _,a := range arglist {
		print(" ", a)
	}
	println()
*/
	
	// if binary still empty, take first argument as binary
	if binary == "" && len(arglist) > 0 {
		binary = arglist[0] 
		arglist = arglist[1:]
	}

	// run command
	cmd := exec.Command(binary, arglist...)
	cmd.Env = env

	if console {
	    cmd.Stdout = os.Stdout
	    cmd.Stderr = os.Stderr
	    cmd.Stdin = os.Stdin
		cmd.Run()
	} else {
//		cmd.SysProcAttr = &syscall.SysProcAttr{HideWindow: true}
//		cmd.SysProcAttr = &syscall.SysProcAttr{Setpgid: false}
		cmd.Start()
	}
}

func showComponentInfo(cmd string, db AliasDB) {
	// url is either given or taken from alias database
	url := cmd
	// check, do we have an alias for command
	if val, ok := db.Commands[cmd]; ok {
		url = val
	}

	aif, _, _ := getComponentFromUrl(db, url, false)

//	fmt.Printf("%+v", aif)

	println("Component Info on:", aif.Id, "\n")
	println("Description:")
	println(aif.Description, "\n")
	println("License:")
	println(aif.License, "\n")
}

func showLicenseInfo(cmd string, db AliasDB) {

	// url is either given or taken from alias database
	url := cmd
	// check, do we have an alias for command
	if val, ok := db.Commands[cmd]; ok {
		url = val
	}

	// resolve dependencies
	ok, rlist := resolveDependencies(db, url, []ImplementationDependency{}, false)
	if !ok {
		log.Fatal("Could not resolve dependencies for license info on:", url)
	}

	aif, _, _ := getComponentFromUrl(db, url, false)

	println("License info on component:", aif.Id)
	println("(including licenses of all subcomponents)\n")

	for _, ri := range rlist {
		println("LICENSE FOR COMPONENT:", ri.comp.Id)
		println("----------------------")
		println("----------------------")
		println(ri.comp.FullLicenseText, "\n")
	}
}

// aio run <name | url>, cmd = <name | url>

func runComponentWithDependencies(cmd string, db AliasDB, workDir string, args []string, debug bool, unsafe bool, console bool, update bool) {

	// url is either given or taken from alias database
	url := cmd
	// check, do we have an alias for command
	if val, ok := db.Commands[cmd]; ok {
		url = val
	}

	// resolve dependencies
	ok, rlist := resolveDependencies(db, url, []ImplementationDependency{}, update)
	if !ok {
		log.Fatal("Could not resolve dependencies for cmd: ", url)
	}

	// check install dirs and install missing software
	rlist2, ilist := enrichDepProcInfoWithInstallDir(db, rlist)

	// install files
	installDownloads(ilist, unsafe)

	if debug {
		for _, el := range rlist2 {
			printDepProcInfo(el)
		}
	} else {
		// build run command and start it
		composeEnvironmentAndRunCommand(rlist2, args, console)
	}
}
