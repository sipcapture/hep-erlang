hep_udp_listener example
========================

To try this example, you need GNU `make` and `git` in your path.

To build this example, run the following command:

``` bash
$ make
```
Start the example in the foreground, as you want to look at the console for
messages:

``` bash
$ ./_rel/bin/hep_udp_listener console
```

Then point some HEP generating source to your IP on port 9060/udp and do some
calls. You should see some HEP messages appear on your console.
