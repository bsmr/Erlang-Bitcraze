# Erlang Bitcraze: Joystick C-Node

The Joystick C-Node support Erlang/OTP as an "interface" to
Linux joysticks. For the development a Sony Playstation 3
Dualshock Sixaxis Joystick is used.

## Features

The Joystick C-Node handes the Linux Kernel Joystick API.


## Implementation

The Joystick C-Node consists of several stages/processes:
- The main entry point, to handle command line arguments, and
  setup the environment.
- A process to handle the communication with other Erlang nodes.
- A process to handle the joystick device.

