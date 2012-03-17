#include <theme/theme.h>
#include <oak/duration.h>

class SpeedTests : public CxxTest::TestSuite
{
	struct npow2 {
		enum {one = 1ll <<40};
		unsigned long long backing;
		npow2():backing(0) {}

		npow2(int exponent) {
			backing = one>>exponent;
		}
		bool operator <(const npow2& rhs) const{
			return backing < rhs.backing;
		}
		npow2& operator -= (npow2 rhs) {
			backing -=rhs.backing;
			return *this;
		}
		npow2& operator += (npow2 rhs) {
			backing +=rhs.backing;
			return *this;
		}
	};
	int repeat = 1;
	public:
	struct pathx_t;
	struct pathx_t {
		std::map<std::string, pathx_t> _path;
		bool match;
		int possible;
		int route;
		int id;
		pathx_t& operator[](std::string str) {
			return _path[str];
		}
		pathx_t& set(bool b,int m, int r, int i) {
			match = b;
			possible = m;
			route = r;
			id = i;
			return *this;
		}

		std::string to_s () const {
			std::string res = "< match=";
			res+= (match?"true":"false" );
			std::stringstream sstream;
			sstream << "\n  route=0x" << std::hex << route << "\n  possible=0x" << std::hex << possible;
			std::string result = sstream.str();
			res+= result;
			
			res+= "\n {";
			iterate(it, _path)
				res += it->first+"= "+ it->second.to_s();
		  
			res += "\n}>";
			return res;
		}
	};

	pathx_t* next(std::string& str, pathx_t& path) {
		std::map<std::string, pathx_t>::iterator it = path._path.find(str);
		if(it != path._path.end()) 
			return &it->second;
		it = path._path.find("*");
		if(it != path._path.end()) 
			return &it->second;
		
		return NULL; 
	}

	
	struct rules_t {
		rules_t(size_t sz) : matchingSelectors(sz){}
		struct internal_t {
			struct pattern_t{
				int pattern;
				int mask;
				int size;
			}; 
			int id;
			std::vector<pattern_t>  matchingSelectors;
			pattern_t operator[](size_t idx) { return matchingSelectors[idx];}
			internal_t& add(int p, int m, int s) {
				matchingSelectors.push_back(pattern_t());
				matchingSelectors.back().pattern = p;
				matchingSelectors.back().mask = m;
				matchingSelectors.back().size = s;
				
				return *this;
			}
			internal_t& add(int p, int m) {
				return add(p, m, route_length(p));
			}			
			size_t size() {
				return matchingSelectors.size();
			}
		} internal;
		std::vector<internal_t> matchingSelectors;
		
		internal_t& operator[](size_t idx) {
			return matchingSelectors[idx];
		}
	};
	void test_speed1_theme_benchmark ()
	{
		rules_t matchingSelectors(8);
		// path - mask
		//"text.* markup.bold", 
		matchingSelectors[0].add(3<<4|1<<1,7<<4|1<<1).add( 5<<4|3<<1, 7<<4|7<<1).id = 0;
		//"text markup.bold"
		matchingSelectors[1].add(3<<4, 7<<4).add( 5<<4|3<<1, 7<<4|7<<1).id = 1;
		//"text.html meta.*.markdown markup"
		matchingSelectors[3].add(3<<4|5<<1, 7<<4|7<<1).add( 7<<4|1<<1|1<<0, 7<<4|1<<1|1<<0).add( 5<<4, 7<<4).id= 3;
		//"text.html meta.* markup"
		matchingSelectors[4].add(3<<4|5<<1, 7<<4|7<<1).add( 7<<4|1<<1, 7<<4|1<<1).add( 5<<4, 7<<4).id=4;
		//"text.html * markup" 
		matchingSelectors[5].add(3<<4|5<<1, 7<<4|7<<1).add( 1<<4, 1<<4).add( 5<<4,7<<4).id=5;
		 //"text.html markup"
		matchingSelectors[6].add(3<<4|5<<1, 7<<4|7<<1).add( 5<<4, 7<<4).id=6;
		//"text markup"
		matchingSelectors[7].add(3<<4, 7<<4).add( 5<<4, 7<<4).id=7; 
		// [text 3<<4,markup 5<<4,meta 7<<4,* 1<<4][bold 3<<1,html 5<<1,* 1<<1][markdown 1]
		
		static std::string const selectorStrings[] =
		{
			"text.* markup.bold", // 0
			"text markup.bold",
			"markup.bold",
			"text.html meta.*.markdown markup",
			"text.html meta.* markup",
			"text.html * markup", // 5
			"text.html markup",
			"text markup",
			"markup",
			"text.html",
			"text" //10
		};
		
		pathx_t root;
		root["text"].set(true,1<<1|1<<7, 3<<4, 10)["html"]
			.set(true,1<<0|1<<3|1<<4|1<<5|1<<6|  1<<1|1<<7, 3<<4|5<<1,9);//1<<0*
		root["text"]["*"].set(false,1<<0  |1<<1|1<<7, 3<<4|1<<1,-1);
		int help = 1 << 3 | 1 << 4 | 1 << 5 | 1 << 6| 1<<7;
		root["markup"].set(true, help, 5<<4, 8)["bold"]
			.set(true,1<<0|1<<1|help, 5<<4|3<<1,2);
		root["*"].set(false,1<<5,1<<4,-1);
		root["meta"]["*"].set(false,1<<4,7<<4|1<<1,-1)["markdown"].set(false,1<<3  |1<<4, 7<<4|1<<1|1<<0,-1);
		
		auto p = std::vector<std::vector<std::string> >();
		{
			std::string const cl[] = {"text","html","markdown"};
			p.push_back(std::vector<std::string> ());
			iterate(c, cl)
				p.back().push_back(*c);				
		}
		
		{
			std::string const cl[] = {"meta","paragraph","markdown"};
			p.push_back(std::vector<std::string> ());
			iterate(c, cl)
				p.back().push_back(*c);				
		}
		
		{
			std::string const cl[] = {"markup","bold","markdown"};
			p.push_back(std::vector<std::string> ());
			iterate(c, cl)
				p.back().push_back(*c);				
		}

		//f// printf(stderr, "pathx: %s\n", root.to_s().c_str());
		npow2 total;		
		oak::duration_t timer2;
		
		int computed = 0;
		for(size_t times = 0 ; times < repeat ;times++){
			total = npow2();
			
			size_t s = p.size();
			std::vector<npow2> collector(11); 
			std::multimap<npow2, std::string> ordering;
			rules_t::internal_t v;
			int power = 0;
			npow2 sco(1);
			printf("score 1 b:%llu \n", sco.backing);
			
		while(s--)
		{
			pathx_t* path = &root;
			int j = 0;
			int sz = p[s].size();
			power += sz;

			while(pathx_t* current = next(p[s][j], *path))
			{
				j++;
				path = current;
				if(path->match)
				{

					npow2 score(power - j);
					printf("ffs p:%d j:%d b:%llu s:%s\n",power, j, score.backing, p[s][j-1].c_str());

					score-=npow2(power);
					printf("ffs b:%llu \n", score.backing);
					
					collector[path->id] = score;
				}
			}
			computed = computed | path->possible;
			v.add(path->route, 0, sz);
			
		} 
		/*
		iterate(it, v) {
			// printf("vector item: %#x \n", *it);
		}
		*/
		
		while(int idx = ffs(computed)) {
			// printf("ffs %d %s\n",idx, selectorStrings[idx-1].c_str());
			
			npow2 rank;
			if(does_match(v, matchingSelectors[idx - 1],&rank) ) {
				npow2& val = collector[idx-1];
				if( val < rank)
					val = rank;
					
				// printf("---ffs %d\n matched",idx);

			}
			computed &= ~(1<<idx-1);
		}
		int index = 0;
		iterate(it, collector) {
			printf("collector item: %llu %d\n", it->backing, index);

			ordering.insert(std::make_pair(*it, selectorStrings[index++]));
		}
		
		iterate(it, ordering) {
			printf("item:  %llu %s\n", it->first.backing, it->second.c_str());
			total += it->first;
		}
	   		
		}
		
		printf("%.4f seconds to computed path_x:%llu \n", timer2.duration(), total.backing);
	}
	
	void test_speed2_theme_benchmark ()
	{
		rules_t matchingSelectors(8);
		// path - mask
		//"text.* markup.bold", 
		matchingSelectors[0].add(3<<4|1<<1,7<<4|1<<1).add( 5<<4|3<<1, 7<<4|7<<1).id = 0;
		//"text markup.bold"
		matchingSelectors[1].add(3<<4, 7<<4).add( 5<<4|3<<1, 7<<4|7<<1).id = 1;
		//"text.html meta.*.markdown markup"
		matchingSelectors[3].add(3<<4|5<<1, 7<<4|7<<1).add( 7<<4|1<<1|1<<0, 7<<4|1<<1|1<<0).add( 5<<4, 7<<4).id= 3;
		//"text.html meta.* markup"
		matchingSelectors[4].add(3<<4|5<<1, 7<<4|7<<1).add( 7<<4|1<<1, 7<<4|1<<1).add( 5<<4, 7<<4).id=4;
		//"text.html * markup" 
		matchingSelectors[5].add(3<<4|5<<1, 7<<4|7<<1).add( 1<<4, 1<<4).add( 5<<4,7<<4).id=5;
		 //"text.html markup"
		matchingSelectors[6].add(3<<4|5<<1, 7<<4|7<<1).add( 5<<4, 7<<4).id=6;
		//"text markup"
		matchingSelectors[7].add(3<<4, 7<<4).add( 5<<4, 7<<4).id=7; 
		// [text 3<<4,markup 5<<4,meta 7<<4,* 1<<4][bold 3<<1,html 5<<1,* 1<<1][markdown 1]
		
		static std::string const selectorStrings[] =
		{
			"text.* markup.bold", // 0
			"text markup.bold",
			"markup.bold",
			"text.html meta.*.markdown markup",
			"text.html meta.* markup",
			"text.html * markup", // 5
			"text.html markup",
			"text markup",
			"markup",
			"text.html",
			"text" //10
		};
		
		pathx_t root;
		root["text"].set(true,1<<1|1<<7, 3<<4, 10)["html"]
			.set(true,1<<0|1<<3|1<<4|1<<5|1<<6|  1<<1|1<<7, 3<<4|5<<1,9);//1<<0*
		root["text"]["*"].set(false,1<<0  |1<<1|1<<7, 3<<4|1<<1,-1);
		int help = 1 << 3 | 1 << 4 | 1 << 5 | 1 << 6| 1<<7;
		root["markup"].set(true, help, 5<<4, 8)["bold"]
			.set(true,1<<0|1<<1|help, 5<<4|3<<1,2);
		root["*"].set(false,1<<5,1<<4,-1);
		root["meta"]["*"].set(false,1<<4,7<<4|1<<1,-1)["markdown"].set(false,1<<3  |1<<4, 7<<4|1<<1|1<<0,-1);
		
		auto p = std::vector<std::vector<std::string> >();
		{
			std::string const cl[] = {"text","html","markdown"};
			p.push_back(std::vector<std::string> ());
			iterate(c, cl)
				p.back().push_back(*c);				
		}
		
		{
			std::string const cl[] = {"meta","paragraph","markdown"};
			p.push_back(std::vector<std::string> ());
			iterate(c, cl)
				p.back().push_back(*c);				
		}
		
		{
			std::string const cl[] = {"markup","bold","markdown"};
			p.push_back(std::vector<std::string> ());
			iterate(c, cl)
				p.back().push_back(*c);				
		}

		//f// printf(stderr, "pathx: %s\n", root.to_s().c_str());
		npow2 total;		
		oak::duration_t timer2;
		
		int computed = 0;
		for(size_t times = 0 ; times < repeat ;times++){
			total = npow2();
			
			size_t s = p.size();
			std::vector<npow2> collector(11); 
			std::multimap<npow2, std::string> ordering;
			rules_t::internal_t v;
			int power = 0;
			npow2 sco(1);
			printf("score 1 b:%llu \n", sco.backing);
			
		while(s--)
		{
			pathx_t* path = &root;
			int j = 0;
			int sz = p[s].size();
			power += sz;

			while(pathx_t* current = next(p[s][j], *path))
			{
				j++;
				path = current;
				if(path->match)
				{

					npow2 score(power - j);
					printf("ffs p:%d j:%d b:%llu s:%s\n",power, j, score.backing, p[s][j-1].c_str());

					score-=npow2(power);
					printf("ffs b:%llu \n", score.backing);
					
					collector[path->id] = score;
				}
			}
			computed = computed | path->possible;
			v.add(path->route, 0, sz);
			
		} 
		/*
		iterate(it, v) {
			// printf("vector item: %#x \n", *it);
		}
		*/
		
		while(int idx = ffs(computed)) {
			// printf("ffs %d %s\n",idx, selectorStrings[idx-1].c_str());
			
			npow2 rank;
			if(does_match(v, matchingSelectors[idx - 1],&rank) ) {
				npow2& val = collector[idx-1];
				if( val < rank)
					val = rank;
					
				// printf("---ffs %d\n matched",idx);

			}
			computed &= ~(1<<idx-1);
		}
		int index = 0;
		iterate(it, collector) {
			printf("collector item: %llu %d\n", it->backing, index);

			ordering.insert(std::make_pair(*it, selectorStrings[index++]));
		}
		
		iterate(it, ordering) {
			printf("item:  %llu %s\n", it->first.backing, it->second.c_str());
			total += it->first;
		}
	   		
		}
		
		printf("%.4f seconds to computed path_x:%llu \n", timer2.duration(), total.backing);
	}
	
	static size_t route_length(const int& route) {
		if(route & 2){ // 1<<1
			if(route & 1) {
				return 3;
			}
			return 2;
		}
		return 1;
	}
	bool does_match (rules_t::internal_t& path, rules_t::internal_t& scopes, npow2* rank ) const
	{

		size_t i = path.size(); // “source.ruby string.quoted.double constant.character”
		size_t j = scopes.size();      // “string > constant $”
		size_t s = i;
		// printf("path size: %zu\n", i);
		// printf("size: %zu\n", j);

		npow2 score;
		int power = 0;
		while(j <= i && j)
		{
			// if(anchor_to_bol)
			// if(anchor_to_next)
			power += path[s-i].size;//route_length(path[s-i]);
			// printf("scope:%#x path:%#x mask:%#x masked:%#x i=%zu j=%zu\n ", scopes[j-1].pattern, path[s-i], scopes[j-1].mask, path[s-i] & scopes[j-1].mask, i, j);

			if(scopes[j-1].pattern  == (path[s-i].pattern & scopes[j-1].mask))
			{
				/*for(size_t k = 0; k < scopes[j-1].size; ++k)
					score += 1 / pow(2, power - k);
				*/
				npow2 temp(power - scopes[j-1].size);
				temp-=npow2(power);
				score +=temp;
				--j;
			}
			--i;
		}

		// if(anchor_to_eol)
		if(j == 0 && rank)
			*rank = score;
		return j == 0;
	}
	
};
