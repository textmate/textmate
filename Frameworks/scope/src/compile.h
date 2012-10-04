#ifndef COMPILE_H_5XWUY4P8
#define COMPILE_H_5XWUY4P8
#include "scope.h"
#include <oak/oak.h>
#include "immutable_map.h"

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
			struct paths_t
			{
				scopesx or_paths;
				scopesx not_paths;
				void clear()
				{
					or_paths.clear();
					not_paths.clear();
				}
			} left, right;
			bool needs_right;
			void clear()
			{
				needs_right = false;
				left.clear();
				right.clear();
			}
		};
		struct interim_t;
		typedef std::unique_ptr<interim_t> interim_unique_ptr;
		struct PUBLIC interim_t {
			std::map<std::string, interim_unique_ptr > path;

			std::set<int> simple;
			std::map<int, int> multi_part;
			int hash;
			int mask;
			bool needs_right;

			void calculate_bit_fields ();
			bool has_any ();
			std::string to_s (int indent=0) const;
			
		};
		
		struct sub_rule_t
		{
			typedef std::shared_ptr<compressed::composite_t> composite_ptr;
			composite_ptr composite;
			int rule_id;
		};
		
		class PUBLIC matcher_t
		{
			std::vector<sub_rule_t> expressions;
			size_t blocks_needed;
			mutable std::vector<scope::compile::bits_t> palette;
		public:
			matcher_t () {}
			matcher_t (std::vector<sub_rule_t> expressions, size_t blocks_needed): expressions(expressions), blocks_needed(blocks_needed), palette(blocks_needed) {}
			scope::compressed::path_t lookup (scope::types::path_ptr const& scope, const scope::compile::compressor_t& compressor, std::vector<scope::compile::bits_t>& palette, std::map<int, double>& ruleToRank, bool& needs_right) const;
			std::map<int, double> match (context_t const& scope, compressor_t const& compressor, compressor_t const& r_compressor) const;
		};

		class compressor_t;
		typedef std::unique_ptr<compressor_t> compressor_unique_ptr;
		class PUBLIC compressor_t
		{
			struct converter
			{
				size_t sz;
	         typedef std::pair<std::string, compressor_unique_ptr> result_type;				
				converter(size_t sz):sz(sz) {}
				result_type operator()(std::pair<std::string, interim_unique_ptr const&> const& pair) const
				{ 
					return std::make_pair(std::move(pair.first), compressor_unique_ptr(new compressor_t(*pair.second, sz)));
				}
			};
			//typedef immutable_map<std::string, scope::compile::compressor_t::compressor_t> map_type;
			typedef std::map<std::string, compressor_unique_ptr> map_type; 			
			std::vector<int> simple;
			std::vector<bits_t> possible;
			int hash;
			bool match;
			bool needs_right;
			friend class matcher_t;
			map_type path;
			
		public:
			compressor_unique_ptr const& next (std::string const& str) const;
			compressor_t (interim_t const& analyze, size_t sz);
			compressor_t (){}
			compressor_t (compressor_t&& rhs) = default;
			compressor_t& operator=(compressor_t&& rhs) = default;
			//compressor_t (compressor_t&& rhs) : simple(rhs.simple), possible(rhs.possible), hash(rhs.hash), match(rhs.match), needs_right(rhs.needs_right), path(std::move(rhs.path)){}

			//~compressor_t()
			//{
			//	fprintf(stderr, "delete!!!!!!!!!!!!!!!!!!!!!!,");
			//	
			//	for(auto pair : path) {
			//		delete pair.second;}
			//}
		};

		template<typename T>
		class PUBLIC compiled_t {
			compressor_t l_compressor;
			compressor_t r_compressor;

			matcher_t matcher;
			std::vector<T> rules;
		public:
			compiled_t<T>& operator=(compiled_t<T>&& rhs) = default;
			compiled_t(compiled_t<T>&& rhs) : l_compressor(std::move(rhs.l_compressor)), r_compressor(std::move(rhs.r_compressor)), matcher(rhs.matcher), rules(std::move(rhs.rules)) {}
			compiled_t (const interim_t& interim, const interim_t& r_interim, std::vector<T> const& rules, const std::vector<sub_rule_t>& expressions, size_t blocks_needed): l_compressor(interim, blocks_needed), r_compressor(r_interim, blocks_needed), matcher(expressions, blocks_needed), rules(rules) 
				{}
			compiled_t() {}

			bool match (context_t const& scope, std::multimap<double, const T&>& ordered) const
			{
				size_t before = ordered.size();
				std::map<int, double> matched = matcher.match(scope, l_compressor, r_compressor);
				iterate(it, matched)
					ordered.insert(std::make_pair<double, const T&>(it->second, rules[it->first]));
				return ordered.size() - before != 0;
			}

			T styles_for_scope (context_t const& scope, T base_style) const
			{
				std::multimap<double, int> ordered;
				std::map<int, double> matched = matcher.match(scope, l_compressor, r_compressor);
				iterate(it, matched)
					ordered.insert(std::make_pair(it->second, it->first));
				iterate(it, ordered) 
					base_style+= rules[it->second];
				
				return base_style;
			}
		};
		
		class PUBLIC compiler_t
		{
			analyze_t _analyzer;
			interim_t root;
			interim_t right_root;
			std::multimap<int, int> sub_rule_mapping;
			std::vector<sub_rule_t> _expressions;
		public:
			void compress (const selector_t& selector, int rule_id, int composite_index);
			void expand_wildcards () { expand_wildcards(root);}
			void graph (const selector_t& selector, int& rule_id, int& sub_rule_id);
			std::multimap<int, int>& sub_rule_mappings () { return sub_rule_mapping;}
			std::vector<sub_rule_t> expressions () { return _expressions; }
			analyze_t& analyzer () { return _analyzer;}
			interim_t& interim () { return root;}
			interim_t& right_interim () { return right_root;}
			void calculate_bit_fields () { root.calculate_bit_fields();}
			std::string to_s () { return root.to_s(); }
			
		private:
			void expand_wildcards (interim_t& analyzer);
		};

		// T must support:
		// +, scope::selector_t scope_selector

		template<typename T>
		compiled_t<T> compile (std::vector<T> const& rules)
		{
			compiler_t compiler;
			int rule_id = 0;
			int sub_rule_id=0;
			iterate(iter, rules)
			{
				//auto selector = iter->scope_selector;
				compiler.graph(iter->scope_selector, rule_id, sub_rule_id);
				rule_id++;
			}
			// add * to all paths
			compiler.expand_wildcards();
			// populate bit fields
			compiler.calculate_bit_fields();
			
			// break out all selectors into its compressed composites
			iterate(r_id, compiler.sub_rule_mappings())
			{
				//printf("rule :%d sub:%d\n", r_id->first, r_id->second);
				//auto selector = rules[r_id->first].scope_selector;
				compiler.compress(rules[r_id->first].scope_selector, r_id->first, r_id->second);
			}

			//printf("root:\n");
			//printf("%s\n", compiler.to_s().c_str());
			size_t sz = sizeof(bits_t)*CHAR_BIT;
			return compiled_t<T>(compiler.interim(), compiler.right_interim(), rules, compiler.expressions(), sub_rule_id/sz + (sub_rule_id%sz > 0 ? 1 :0));
		}
	}
}

#endif /* end of include guard: COMPILE_H_5XWUY4P8 */