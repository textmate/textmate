#ifndef THEME_EDITOR_SAMPLES_H
#define THEME_EDITOR_SAMPLES_H

#include <string>
#include <vector>

namespace theme_editor
{
	struct sample_t
	{
		std::string name;
		std::string file;
		std::vector<std::string> scopes;
	};

	inline std::vector<sample_t> const& samples ()
	{
		static std::vector<sample_t> const values = {
			{ "JavaScript", "JavaScript.js", { "source.js" } },
			{ "TypeScript", "TypeScript.ts", { "source.ts", "source.ts.typescript" } },
			{ "TypeScript / React", "TypeScriptReact.tsx", { "source.tsx" } },
			{ "Python", "Python.py", { "source.python" } },
			{ "Swift", "Swift.swift", { "source.swift" } },
			{ "Rust", "Rust.rs", { "source.rust" } },
			{ "Go", "Go.go", { "source.go" } },
			{ "C", "C.c", { "source.c" } },
			{ "C++", "CPlusPlus.cpp", { "source.c++", "source.cpp" } },
			{ "C#", "CSharp.cs", { "source.cs", "source.csharp" } },
			{ "Java", "Java.java", { "source.java" } },
			{ "Ruby", "Ruby.rb", { "source.ruby" } },
			{ "PHP", "PHP.php", { "text.html.php", "source.php" } },
			{ "SQL", "SQL.sql", { "source.sql" } },
			{ "Bash", "Bash.sh", { "source.shell", "source.shell.bash" } },
			{ "HTML", "HTML.html", { "text.html.basic", "text.html" } },
			{ "CSS", "CSS.css", { "source.css" } },
			{ "JSON", "JSON.json", { "source.json" } },
			{ "Markdown", "Markdown.md", { "text.html.markdown", "text.markdown" } },
		};
		return values;
	}
}

#endif
