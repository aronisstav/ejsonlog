# ejsonlog

A drop-in formatter for Erlang's logging module that outputs messages
as line delimited JSON.

## Why?

This formatter is heavily inspired by [flatlog][flatlog], but it is
meant to produce JSON logs and to that end it sanitizes its input
accordingly.

## Usage

Just like [flatlog][flatlog], this formatter should not be added as a
dependency in individual libraries.

## Build

    $ rebar3 compile

<!-- Links -->
[flatlog]: https://github.com/ferd/flatlog
