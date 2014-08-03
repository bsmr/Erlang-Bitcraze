# Erlang Bitcraze: Joystick

The Joystick application is an "interface" to the Linux joystick API for
Erlang/OTP.
In the development process a Sony Playstation 3 Dualshock Sixaxis Joystick
is used for testing with the help of ```sixad```.

## Features

The Joystick application handles the Linux Kernel Joystick API, based on
an evented model on the Erlang/OTP side.

## Implementation

The Joystick application uses a NIF library to interface a few Linux
system calls (open, close, read, ioctl) to access device nodes using
the Linux Joystick API.
