# Core-Catcher

A multiplayer game server and frontend, written in Haskell and Elm.


## Setup Frontend

It is required to install the programming language elm for the frontend.
To do this, one enters manually 

```bash
sudo npm -g install elm
```

For an improved developer experience, the following installations are recommended as well:

```bash
sudo npm -g install elm-live elm-format
```

The latter one is to format your document according to the elm conventions, while the first executable replaces `elm-make` with an auto reloading build tool. 

To view the frontend, the following command can be used:

```bash
cd path/to/installation/
cd frontend
elm-live
```

Afterwards, the website can be viewed in the browser on port `8080`.

## Setup Backend

To execute the backend, it is required to have `stack` the Haskell build tool installed. This can be done via the officical website [[1]](https://docs.haskellstack.org/en/stable/README/).

To build and execute the server:

```bash
stack build 
stack exec core-catcher-exe
```