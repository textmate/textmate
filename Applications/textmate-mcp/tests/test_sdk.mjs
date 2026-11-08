// Run with: node test_sdk.mjs /path/to/sdk/dist/esm /path/to/textmate-mcp
import assert from 'node:assert/strict';
import { pathToFileURL } from 'node:url';
import { createServer } from 'node:net';
import { mkdtemp, rm } from 'node:fs/promises';
import { join, resolve } from 'node:path';
const root = resolve(process.argv[2]);
const { Client } = await import(pathToFileURL(join(root, 'client/index.js')));
const { StdioClientTransport } = await import(pathToFileURL(join(root, 'client/stdio.js')));
const { ResourceUpdatedNotificationSchema } = await import(pathToFileURL(join(root, 'types.js')));
const directory = await mkdtemp('/tmp/tm-sdk-');
const socket = join(directory, 's');
let requests = 0;
let change = 0;
const server = createServer(connection => {
  let input = '';
  connection.on('data', chunk => {
    input += chunk.toString();
    if (!input.includes('\n')) return;
    const message = JSON.parse(input.split('\n')[0]);
    assert.equal(message.session, 'sdk-session');
    assert.equal(message.token, 'sdk-token');
    requests++;
    if (message.method === 'get_editor_change_token') {
      connection.end(JSON.stringify({cursor: `sdk/${change}`, windowId: 'sdk-window'})+'\n');
      return;
    }
    if (message.method === 'navigate_editor') {
      assert.deepEqual(message.arguments, {documentId: 'sdk-document', line: 2, column: 2, endLine: 2, endColumn: 4, focus: false});
      connection.end(JSON.stringify({status: 'navigated', opened: false, focused: false})+'\n');
      return;
    }
    connection.end(JSON.stringify({windowId: 'sdk-window', document: {name: 'unsaved.txt'}, selections: [{text: 'live 😀'}]})+'\n');
  });
});
await new Promise((resolve, reject) => { server.once('error', reject); server.listen(socket, resolve); });
const client = new Client({name: 'textmate-compatibility-test', version: '1.0'});
const transport = new StdioClientTransport({command: resolve(process.argv[3]),
  env: {...process.env, TM_CONTEXT_SOCKET: socket, TM_CONTEXT_SESSION: 'sdk-session', TM_CONTEXT_TOKEN: 'sdk-token'}, stderr: 'pipe'});
try {
  await client.connect(transport);
  const list = await client.listTools();
  assert.deepEqual(list.tools.map(tool => tool.name), ['get_editor_context','list_open_documents','read_document','get_diagnostics','get_definition','find_references','get_document_symbols','search_workspace_symbols','get_hover','navigate_editor','propose_document_edits','get_edit_proposal_status','get_editor_changes']);
  assert.equal(list.tools.find(tool => tool.name === 'navigate_editor').annotations.readOnlyHint, false);
  const result = await client.callTool({name: 'get_editor_context', arguments: {}});
  assert.equal(result.isError, false);
  assert.equal(JSON.parse(result.content[0].text).selections[0].text, 'live 😀');
  assert.equal(requests, 1);
  const navigation = await client.callTool({name: 'navigate_editor', arguments: {documentId: 'sdk-document', line: 2, column: 2, endLine: 2, endColumn: 4, focus: false}});
  assert.equal(navigation.isError, false);
  assert.equal(JSON.parse(navigation.content[0].text).status, 'navigated');
  assert.equal(requests, 2);
  const uri = 'textmate://editor/context';
  assert.equal((await client.listResources()).resources[0].uri, uri);
  assert.deepEqual((await client.listResourceTemplates()).resourceTemplates, []);
  const notifications = [];
  client.setNotificationHandler(ResourceUpdatedNotificationSchema, notification => { notifications.push(notification); });
  async function waitForUpdates(count) {
    const deadline = Date.now()+5000;
    while(notifications.length < count && Date.now() < deadline) await new Promise(resolve => setTimeout(resolve, 20));
    assert.ok(notifications.length >= count, 'Expected a resource update notification');
  }
  await client.subscribeResource({uri});
  await waitForUpdates(1);
  change++;
  await waitForUpdates(2);
  assert.equal(notifications[1].params.uri, uri);
  const resource = await client.readResource({uri});
  assert.equal(JSON.parse(resource.contents[0].text).selections[0].text, 'live 😀');
  await client.unsubscribeResource({uri});
  await client.ping();
  console.log('Official MCP SDK: initialization, discovery, tools, resource read/subscribe/update/unsubscribe, Unicode and ping passed.');
} finally {
  await client.close();
  await new Promise(resolve => server.close(resolve));
  await rm(directory, {recursive: true, force: true});
}
