@0xbe61cff8e1b1801e;

struct Entry {
	path @0 :Text;

	type :union {
		file      @1 :File;
		directory @2 :Directory;
		link      @3 :Text;
		missing   @4 :Void;
	}

	struct File {
		content  @0 :List(Pair);
		modified @1 :UInt64;

		struct Pair {
			key   @0 :Text;
			value @1 :Text;
			plist @2 :Data;
		}
	}

	struct Directory {
		items   @0 :List(Text);
		glob    @1 :Text;
		eventId @2 :UInt64 = 0;
	}
}

struct Cache {
	entries @0 :List(Entry);
	version @1 :UInt32 = 2;
}
