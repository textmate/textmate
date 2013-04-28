title: Semantic Classes

# Events / Filters

## Events

Presently the only event fired is `event.document.did_save`.

Catching an event is done by adding an `eventSpecifier` key to a regular command and setting the value to the event which should be caught.

An example of this is the “Make Script Executable” in the included Avian bundle. This is scoped to `source.ruby` so if you create a new ruby file and save it, it should automatically get the executable bit set.

## Filters

Filters are like events but their output is used. Examples of filters are in the included Avian bundle, there are filters to decompile AppleScript, pretty print tmCommand files, and encrypt/decrypt the content.
