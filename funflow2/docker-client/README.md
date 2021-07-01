# docker-client

Docker client library used internally by Funflow.

## Build and Installation

### Dependencies

This library's default connection manager expects the Docker Engine service to be available at runtime. For example, on linux and osx, it expects the default docker unix socket to exist. See the documentation at https://docs.docker.com/config/daemon/ for more details on how to
start up docker.

### Build

`docker-client` can be built using the repository's nix packaging:


**Library:** 
```bash
$ nix-build -A docker-client ../default.nix 
```


**Tests:** 
```bash
# Test executable
$ nix-build -A docker-client-tests ../default.nix 
# Unit tests
$ result/bin/primary --pattern 'unit'
# Integration tests
$ result/bin/primary --pattern 'integration'
# All tests
$ result/bin/primary
```


## Usage

This library exposes a simple API via the Docker.API.Client module.

### Example: Running a container

```haskell
import Docker.API.Client 

import System.Info (os)

main :: IO ()
main = do
  manager <- newDefaultDockerManager os
  uid <- getEffectiveUserID
  gid <- getEffectiveGroupID
  let container =
        (defaultContainerSpec "ubuntu:latest")
          { cmd = ["bash", "-c", "echo foo > /host/test.txt"],
            hostVolumes = ["/home/dorran/Desktop/test-bind:/host"]
          }
  result <- runExceptT $ do 
    containerId <- runContainer manager container
    -- You can either start the container and let it run asynchronously, or 
    -- wait for it to exit using awaitContainer:
    awaitContainer manager containerId
  case result of
    Left err -> print err
    Right containerId -> print "Run succeeded! The container id was " ++ containerId
```
