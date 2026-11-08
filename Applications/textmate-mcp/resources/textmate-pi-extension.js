// Loaded by tm-pi with `pi --extension`. Registers TextMate's editor MCP server
// for this session only; nothing is written to the user's mcp.json.
export default function (pi) {
	const command = process.env.TM_MCP_SERVER;
	if (!command) return;
	pi.registerMcpServer("textmate", {
		command,
		args: [],
		// References, not values: the token stays in the environment, out of any configuration.
		env: {
			TM_CONTEXT_SOCKET: "${TM_CONTEXT_SOCKET}",
			TM_CONTEXT_SESSION: "${TM_CONTEXT_SESSION}",
			TM_CONTEXT_TOKEN: "${TM_CONTEXT_TOKEN}",
		},
		// Declared to the model like built-in tools. The default, codemode, hides them behind scripts.
		exposure: "direct",
		description: "Live TextMate editor buffers, selections, diagnostics, and navigation for this terminal's window.",
	});
}
