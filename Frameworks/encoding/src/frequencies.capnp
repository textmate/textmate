@0xf07cdbe73cbefea0;

struct Charset {
	charset @0 :Text;
	words   @1 :List(Pair);
	bytes   @2 :List(Pair);

	struct Pair {
		type :union {
			word @0 :Text;
			byte @1 :UInt8;
		}
		count @2 :UInt64;
	}
}

struct Frequencies {
	version  @0 :UInt32 = 1;
	charsets @1 :List(Charset);
}
