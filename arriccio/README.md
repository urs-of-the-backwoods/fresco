Arriccio
--------

Tool for dependency management of binary components, with implementations on multiple platforms. Arriccio works, by reading a a configuration file and a good start is to understand the format of this file.

Configuration File
++++++++++++++++++

The configuration file is written in TOML and contains the following information:

- `url-id` the principal id of the component, given as a URL.
- `description` a short description of the component.
- `license-short` short version of license
- `licnese-full` full version of license
- `signing-key` url (https) for the signing key
- `implementation` platform specific implementations (details below)
- `implementation.dependencies` dependenies of specific implementation

Implementation
**************




