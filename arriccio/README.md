# Arriccio

Tool for dependency management of binary components, with implementations on multiple platforms. Arriccio works, by reading a a configuration file and a good start is to understand the format of this file.

## Configuration File

The configuration file is written in TOML and contains the following information:

- `id-url` the principal id of the component, given as a URL.
- `description` a short description of the component.
- `license-short` short version of license
- `license-full` full version of license
- `signing-key` url (https) for the signing key
- `implementation` platform specific implementations (details below)
- `implementation.dependencies` dependenies of specific implementation

## Implementation

There can be multiple implementations, so this is given as an array of tables (see TOML syntax guide). Contents of an Implemenation can be:

- `architecture` the underlying machine architecture for this implementation, possible values are `amd64` and `*`
- `operating-system` the OS for this implementation, values can be: `windows`, `darwin` or `linux`
- `archive-download-location` the url of an downloadable archive, containing the implementation itself
- `archive-signing-key` signature key for the archive
- `start-local-command` command to start in target folder of unpacked archive
- `environment-settings` array of commands to modify environement, see below

## Implementation Dependencies

The implementation dependencies are also given as an array of tables below each (!) implementation, since they might be implementation specific. They can contain the following keys:

- `id-url` the id of the dependency
- `environment-settings` settings for the dependency in this specific context (dependency injection)

## Environment Settings

For each component environment settings can be specified, either directly in the component itself, or in the dependency section. Those environemnt settings are given as an array of strings, each string being a single command. The following commands are possible:

- `add-path SAMPLE_PATH ./bin` add-path adds the target location as an absolute path to the given environment variable. The path is taken local to the target install location of the component and appended as an absolute path.
- `set-value VALUE val` the corresponding value is set in the environment.

The environment settings are used to let components find each others locations, so that for example an executable is able to load a needed runtime library, or similar.






