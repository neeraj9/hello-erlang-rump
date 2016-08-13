# Hello Erlang Rump

[![Build Status](https://travis-ci.org/neeraj9/hello-erlang-rump.svg?branch=master)](https://travis-ci.org/neeraj9/hello-erlang-rump)
[![Software License (3-Clause BSD)](https://img.shields.io/badge/license-BSD%203--Clause-blue.svg?style=flat-square)](http://opensource.org/licenses/BSD-3-Clause)

Say hello to Erlang Microkernel powered by Rumprun unikernel. This project
is apart from many things an example of running Erlang microkernel powered
by the Rumprun unikernel.

The project is under active development and hence lacks details on what
else this project is supposed to do. If I were to give keywords to this
project then it will be.

* Erlang
* Rumprun
* Unikernel
* Microservices Architecture
* AI (huh! sort of)
* HTML5
* Javascript

## Software Dependencies

This project requires a *Gnu/Linux* or *Mac OS X* with a modern gnu c/c++
compiler (preferably v5.3.0+ because I use that one and tested with it).
Some of the software dependencies are as listed below. Note that the
build scripts are setup to take the latest Rumprun unikernel and build
the software, so in case you dont like it edit the scripts appropriately.

* [g++](https://gcc.gnu.org/) v3.8+
* [openssl](http://www.openssl.org/)

## A Sneak Peak

## Development Environment Setup

Although the installation for various GNU/Linux distributions differ but
the dependencies are easily available on any one of them.

### Ubuntu or Debian

The following commands were tested on Ubuntu 14.04 but things should be similar
(if not same) on other releases and Debian.

The following commands needs to be as a system administrator or with sudo
(as shown below) so that the relavent packages are installed in the
system.

If your system is not updated then probably it is a good idea to do that
before installing anything else.

    sudo apt-get update

In case you prefer the GNU GCC compler then install g++ and openssl dev.

    sudo apt-get install -y g++ libssl-dev

### CentOS / Fedora / Redhat

The following commands were tested on Centos 7 but things should be similar
(if not same) on other releases and Fedora or Redhat.

If your system is not updated then probably it is a good idea to do that
before installing anything else.

    sudo yum update

In case you prefer the GNU GCC compler then install g++ and openssl

    sudo yum install -y gcc-c++ openssl-devel

> The versions gcc (g++) and clang which are installed in your (rpm based)
> operating needs to meet the previously mentioned criteria. At the time of
> this writing CentOS 7, Fedora 23, RHEL 7 are the latest releases and works
> well. Anything earlier may not work for you unless you are willing to
> get under the hood and build things on your own.

### Archlinux

If your system is not updated then probably it is a good idea to do that
before installing anything else.

    sudo pacman -Syu

In case you prefer the GNU GCC compler then install g++ and openssl

    sudo pacman -Sy gcc-multilib openssl

## Motivation

The motivation as indicated earlier is partly to demonstrate how Erlang
applications can be built and run over the Erlang microkernel (powered by
Rumprun unikernel) and partly for other reasons which will be discussed
once the projects starts taking shape. Stay tuned!

## Build

After you are done setting up the development environment the build is
pretty straight-forward (see below).

    git clone https://github.com/neeraj9/hello-erlang-rump
    cd hello-erlang-rump
    make


## Thanks

Thanks for evaluating this project and hope you find it useful.
Feel free to create issues for bugs or new features.

## Authors

* Neeraj Sharma {[github: neeraj9](https://github.com/neeraj9)}

