#!/bin/bash

sudo /sbin/ifdown enp2s0
nmcli conn up "Imperial-WPA"
