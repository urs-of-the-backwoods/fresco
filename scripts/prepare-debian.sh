apt-get update
apt-get install -y wget
apt-get install -y git
wget https://storage.googleapis.com/golang/go1.6.2.linux-amd64.tar.gz
tar -C /usr/local -xzf go1.6.2.linux-amd64.tar.gz
export PATH=$PATH:/usr/local/go/bin
go version
mkdir /go-path
export GOPATH=/go-path

