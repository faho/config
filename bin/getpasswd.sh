#!/bin/bash
sed -n -e "1,/$1/d" -e "s/password //p" ~/.netrc
