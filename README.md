# lucid-gtk
A simple GTK UI application for converting rich html to lucid markup

![image](https://github.com/user-attachments/assets/be9bfc6c-32a9-4a57-b77c-8ef90563a820)



## Building from source

### Installing GTK Dev libraries

This project requires several GTK development libraries to be on the system.

#### On Ubuntu 24.04

```
sudo apt install libgtk-4-dev libadwaita-1-dev libgtksourceview-5-dev libgirepository1.0-dev libxml2-utils
```
#### Building

Make targets are provided to build lucid-gtk.

To build both the library and the ui projects:

```
make build
```

To build both projects and install them on the machine:

```
make install
```

To build and run the UI

```
make run
```

