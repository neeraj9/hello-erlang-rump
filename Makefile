# Copyright (c) 2016, Neeraj Sharma <neeraj.sharma@alumni.iitg.ernet.in>.
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are
# met:
#
# * Redistributions of source code must retain the above copyright
#   notice, this list of conditions and the following disclaimer.
#
# * Redistributions in binary form must reproduce the above copyright
#   notice, this list of conditions and the following disclaimer in the
#   documentation and/or other materials provided with the distribution.
#
# * The names of its contributors may not be used to endorse or promote
#   products derived from this software without specific prior written
#   permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
# A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
# OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
# SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
# LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
# DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
# THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
# (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
# OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

MICROKERNELS = \
 helloer \
 uclimate \
 ustockmarket \
 uwiki \
 uworldtime


.PHONY: all $(MICROKERNELS)

SRC_DEPS = $(shell find apps)
SRC_DEPS += $(shell find config)
SRC_DEPS += rebar.config


all: $(MICROKERNELS)


helloer: helloer-0.1.0.iso

uclimate: uclimate-0.1.0.iso

ustockmarket: ustockmarket-0.1.0.iso

uwiki: uwiki-0.1.0.iso

uworldtime: uworldtime-0.1.0.iso


%.iso:
	name=$(shell echo $@ | rev | cut -f2- -d'-' | rev); \
	vsn=$(shell echo $@ | rev | cut -f1 -d'-' | cut -f2- -d'.' | rev); \
	$(MAKE) -f Makefile.gen PKGNAME=$$name PKGVSN=$$vsn \
		SRC_DEPS="$(SRC_DEPS)"
