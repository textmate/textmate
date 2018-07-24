<?php
error_reporting(E_ALL | E_STRICT);

function fail ($http_response, $message = '') {
	$reasons = array(
		200 => 'OK',
		201 => 'Created',
		302 => 'Found',
		303 => 'See Other',
		304 => 'Not Modified',
		400 => 'Bad Request',
		401 => 'Unauthorized',
		403 => 'Forbidden',
		404 => 'Not Found',
		405 => 'Method Not Allowed',
		429 => 'Too Many Requests',
		500 => 'Internal Server Error'
	);

	header("HTTP/1.0 {$http_response} {$reasons[$http_response]}");
	if($http_response == 401)
		header('WWW-Authenticate: Basic');

	if($message) {
		header('Content-Type: application/json');
		echo json_encode(array('error' => $message)) . "\n";
	}

	exit;
}

function load_settings ($filename) {
	if($settings = parse_ini_file($filename, true)) {
		if($database = $settings['database']) {
			define('DB_ACCESS', $database['access']);
			define('DB_USER',   $database['username']);
			define('DB_PASS',   $database['password']);

			return true;
		}
	}
	return false;
}

function dispatch ($uri, $routes) {
	$http_response = 404; // Not Found

	foreach($routes as $route) {
		$route_method  = $route[0];
		$route_uri     = $route[1];
		$route_handler = $route[2];

		$delim = '&';
		$regex = preg_replace('/\\\:(\w+)/', '(?<\1>.+?)', preg_quote($route_uri, $delim));
		if(preg_match("{$delim}^{$regex}\${$delim}", $uri['path'], $matches)) {
			if(preg_match("/{$route_method}/", $uri['method']) == 1) {
				call_user_func($route_handler, $matches, $uri['method']);
				exit;
			}
			else {
				$http_response = 405; // Method Not Allowed
			}
		}
	}

	fail($http_response);
}

function get_current_uri () {
	// $_SERVER['SERVER_PORT']
	$method = filter_input(INPUT_POST, '_method', FILTER_VALIDATE_REGEXP, array('options' => array('regexp' => '/^(POST|GET|PUT|DELETE)$/')));
	return array(
		'method' => $method ? $method : (isset($_SERVER['REQUEST_METHOD']) ? $_SERVER['REQUEST_METHOD'] : 'GET'),
		'scheme' => isset($_SERVER['HTTPS']) ? 'https' : 'http',
		'host'   => $_SERVER['HTTP_HOST'],
		'path'   => current(explode('?', $_SERVER['REQUEST_URI'], 2)),
		'query'  => $_SERVER['QUERY_STRING'],
	);
}

// =====================
// = Support Functions =
// =====================

function fulfills_requirements ($table, $program)
{
	$pattern = '/(?x)\G
		\s* (\w+) \s* \(
			((?:
				\s* (?: [<>]=?|=|<> )
				\s* (?: [A-Za-z0-9\-+.]+ )
				\s* (?: [,|] | (?=\)) )
			)+)
		\)
		\s* ( [,|] | $ )
	/';

	$subpattern = '/(?x)\G
		\s* ( [<>]=?|=|<> )
		\s* ( [A-Za-z0-9\-+.]+ )
		\s* ( [,|] | $ )
	/';

	$eqMap = array('=' => '==', '<>' => '!=');
	$opMap = array(',' => '&&', '|' => '||');

	$all_checks = array();

	preg_match_all($pattern, $program, $matches, PREG_SET_ORDER);
	foreach($matches as $match) {
		$checks = array();

		$variable = $table[$match[1]];
		preg_match_all($subpattern, $match[2], $requirenents, PREG_SET_ORDER);
		foreach($requirenents as $expr) {
			$operator = isset($eqMap[$expr[1]]) ? $eqMap[$expr[1]] : $expr[1];
			$value = $expr[2];

			if(is_string($variable) && preg_match('/^\d+(\.|$)/', $variable) == 1)
				array_push($checks, "version_compare('{$variable}', '{$value}', '{$operator}')");
			else if(is_string($variable))
				array_push($checks, "'{$variable}' {$operator} '{$value}'");
			else
				array_push($checks, "{$variable} {$operator} '{$value}'");

			if(isset($opMap[$expr[3]]))
				array_push($checks, $opMap[$expr[3]]);
		}

		array_push($all_checks, '(' . implode(' ', $checks) . ')');

		if(isset($opMap[$match[3]]))
			array_push($all_checks, $opMap[$match[3]]);
	}

	$code = implode(' ', $all_checks);
	return eval("return {$code};");
}

// =================
// = URI Callbacks =
// =================

function latest_version ($arguments) {
	try {
		$table = array(
			'os'     => isset($_REQUEST['os']) && preg_match('/^\d+(\.\d+)+$/', $_REQUEST['os'], $matches) ? $matches[0] : '10.9',
			'app'    => isset($_REQUEST['v']) ? $_REQUEST['v'] : '0.0',
			'label'  => $arguments['label'],
			'client' => NULL,
			'rand'   => mt_rand() / mt_getrandmax()
		);

		$builds = array();

		$db = new PDO(DB_ACCESS, DB_USER, DB_PASS);
		foreach($db->query('SELECT build_id, name, version, depends, label, signee, signature, url FROM releases LEFT JOIN builds ON (build_id = builds.id)') as $row) {
			if(fulfills_requirements($table, $row['depends']) && fulfills_requirements($table, $row['label']))
				$builds[$row['version']] = $row;
		}
		uksort($builds, 'version_compare');

		if($version = end($builds)) {
			$build_id = $version['build_id'];
			$filename = basename($version['url']);
			$url      = "https://genie.macromates.com/builds/{$build_id}/{$filename}";

			header('Content-Type: application/json');
			echo json_encode(array(
				'version' => $version['version'],
				'signee'  => $version['signee'],
				'url'     => $url,
			)) . "\n";
		}
		else {
			fail(404);
		}

	} catch(PDOException $e) {
		echo "Error!: {$e->getMessage()}<br/>\n";
		die();
	}
}

function get_build ($arguments, $method) {
	try {
		$db = new PDO(DB_ACCESS, DB_USER, DB_PASS);

		$stmt = $db->prepare('SELECT url, signee, signature FROM builds WHERE id = :build_id');
		if($stmt->execute(array('build_id' => $arguments['build_id']))) {
			while($row = $stmt->fetch()) {
				if($method == 'GET') {
					$stmt = $db->prepare('UPDATE builds SET downloads = downloads + 1 WHERE id = :build_id');
					$stmt->execute(array('build_id' => $arguments['build_id']));
				}

				header("x-amz-meta-x-signee: {$row['signee']}");
				header("x-amz-meta-x-signature: {$row['signature']}");
				header("Location: {$row['url']}");
				fail(302);
			}
		}
		fail(404);

	} catch(PDOException $e) {
		echo "Error!: {$e->getMessage()}<br/>\n";
		die();
	}
}

// ========
// = Main =
// ========

load_settings('../secrets.ini') or fail(500);

dispatch(get_current_uri(), array(
	array('GET|OPTIONS',      '/version/:label',               'latest_version'),
	array('GET|OPTIONS|HEAD', '/builds/:build_id/:build_name', 'get_build'),
));
