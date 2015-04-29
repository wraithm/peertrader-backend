PeerTrader
----------

To get PeerTrader up and runnging:

### Clone peertrader, notescript, and groundhog-utils repositories

```
$ git clone git@github.com:WraithM/notescript.git
$ git clone git@github.com:WraithM/peertrader-backend.git
$ git clone git@github.com:Soostone/groundhog-utils.git
$ git clone git@github.com:ocharles/snaplet-ekg.git
```

### Create a cabal sandbox in notescript

```
$ cd notescript
$ cabal sandbox init
```


### Create a sandbox in peertrader, link up notescript, and build

```
$ cd ../peertrader
$ cabal sandbox init
$ cabal sandbox add-source ../notescript
$ cabal sandbox add-source ../groundhog-utils
$ cabal sandbox add-source ../snaplet-ekg
$ cabal install -j
```

(hint: It might be helpful to install Snap first. `cabal install snap -j`)

### Assuming that building was successful, create a peertrader database and load the schema.

Run peertrader (`.cabal-sandbox/bin/peertrader`), and the program will probably
fail with various postgres errors. Allow peertrader to connect to postgres, and
create the `snap_auth_user` table.

```
$ createdb peertrader
$ createuser -s -U postgres --interactive
$ psql -d peertrader -U postgres < sql/createtables.sql
```

### Configure your market data account

Open prosper.cfg:

```
prosper {
    apiurl = "api.prosper.com"
    username = "prosper@username.com"
    password = "ProsperAPIPassword"

    log {
        logger = "MarketData"
        enabled = false # You can enable market data to be logged to a file
        location = "log/prosper.log"
        level = "DEBUG"
    }
}
```

Open devel.cfg:

```
groundhog {
    host = "localhost"
    name = "peertrader"
    user = "postgres"
}

p2ppicks {
    apikey = "YourAPIKey"
    apisecret = "YourAPISecret"
    email = "yourp2ppicksemail"
    password = "p2ppickspassword"
}
```

### You're ready to run PeerTrader!

```
$ .cabal-sandbox/bin/peertraderOps
$ .cabal-sandbox/bin/peertrader -p 8000 +RTS -N
```


### Trademark notice

'PeerTrader' is a trademark of Deck Technologies, Inc.
