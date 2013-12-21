#!/bin/zsh
./rebar compile && ./rebar generate && rel/barenode/bin/barenode console
