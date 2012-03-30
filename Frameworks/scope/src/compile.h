#ifndef COMPILE_H_5XWUY4P8
#define COMPILE_H_5XWUY4P8
#include "scope.h"
#include <oak/oak.h>

namespace scope
{
	namespace types
	{
		typedef std::string atom_t;
		struct selector_t;
	}
	
	namespace compressed
	{
		struct path_t;
		struct composite_t;
	}

	namespace compile
	{


		typedef std::vector<scope::types::atom_t> scopex;
		typedef std::vector<scopex> scopesx;
		typedef unsigned long long bits_t;
		struct PUBLIC analyze_t
		{	
			std::map<std::string, analyze_t> path;
			scopesx or_paths;
			scopesx not_paths;
			std::set<int> simple;
			std::map<int, int> multi_part;
			int hash;
			int mask;
			const analyze_t& operator[](std::string key) const
			{
				return path.at(key);
			}
			void calculate_bit_fields ();
			bool has_any ();
			void clear ();
			std::string to_s (int indent=0) const;
		};
		
		struct sub_rule_t
		{
			typedef std::tr1::shared_ptr<compressed::composite_t> composite_ptr;
			composite_ptr composite;
			int rule_id;
		};
		
		PUBLIC std::map<int, double> match (context_t const& scope, const compressor_t& compressor, const std::vector<sub_rule_t>& expressions, size_t backing_size);
		
		class PUBLIC compressor_t
		{
			std::vector<int> simple;
			std::vector<bits_t> possible;
			bool match;
			int hash;
			friend std::map<int, double> match (context_t const& scope, const compressor_t& compressor, const std::vector<sub_rule_t>& expressions, size_t backing_size);
		public:
			std::map<std::string, compressor_t> path;
			static compressor_t& setup(analyze_t const& analyze, compressor_t& compressor);
			compressor_t (size_t sz): possible(sz) {}
		};

		template<typename T>
		class PUBLIC compiled_t {
			
			compressor_t compressor;
			std::vector<sub_rule_t> expressions;
			size_t blocks_needed;
			std::vector<T> rules;
		public:
			compiled_t(const analyze_t& analyze, std::vector<T> const& rules, std::vector<sub_rule_t> expressions, size_t blocks_needed): compressor(blocks_needed), blocks_needed(blocks_needed){
				this->rules = rules;
				this->expressions = expressions;
				compressor_t::setup(analyze, compressor);
			}
			bool match (context_t const& scope, std::multimap<double, T>& ordered) const
			{
				size_t before = ordered.size();
				std::map<int, double> matched = ::scope::compile::match(scope, compressor, expressions, blocks_needed);
				iterate(it, matched)
					ordered.insert(std::make_pair(it->first, rules[it->second]));
				return ordered.size() - before != 0;
			}
		};

		PUBLIC void compress (const compile::analyze_t& root, const selector_t& selector, int rule_id, int sub_id, std::vector<sub_rule_t>& expressions);
		PUBLIC void expand_wildcards (analyze_t& root);
		// T must support:
		// +, scope::selector_t scope_selector

		PUBLIC void graph (analyze_t& root, const selector_t& selector, int& rule_id, int& sub_rule_id,  std::multimap<int,int>& sub_rule_mapping);
		template<typename T>
		const compiled_t<T> compile (std::vector<T> const& rules)
		{
			analyze_t root;
			int rule_id = 0;
			int sub_rule_id=0;
			std::multimap<int, int> sub_rule_mapping;
			iterate(iter, rules)
			{
				auto selector = iter->scope_selector;
				graph(root, selector, rule_id, sub_rule_id, sub_rule_mapping);
				rule_id++;
			}
			// add * to all paths
			expand_wildcards(root);
			// populate bit fields
			root.calculate_bit_fields();
			
			// break out all selectors into its compressed composites
			std::vector<sub_rule_t> expressions;
			iterate(r_id, sub_rule_mapping)
			{
				//printf("rule :%d sub:%d\n", r_id->first, r_id->second);

				auto selector = rules[r_id->first].scope_selector;
				compress(root, selector, r_id->first, r_id->second, expressions);
			}

			//printf("root:\n");
			//printf("%s\n", root.to_s().c_str());
			size_t sz = sizeof(bits_t)*CHAR_BIT;
			auto r = rules;
			return compiled_t<T>(root, r, expressions, sub_rule_id/sz + (sub_rule_id%sz > 0 ? 1 :0));
		}
	}
}

#endif /* end of include guard: COMPILE_H_5XWUY4P8 */