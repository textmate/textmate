@0xbe61cff8e1b1801e;

struct Entry {
	path @0 :Text;

	type @1 union {
		file      @2 :File;
		directory @3 :Directory;
		link      @4 :Text;
		missing   @5 :Void;
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
	version @1 :UInt32 = 1;
}
