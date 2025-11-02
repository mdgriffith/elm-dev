var fs = require('fs');
var package = require('./package.json');
var path = require('path');



// MAIN
//
// This function is used by install.js and by the bin/elm backup that gets
// called when --ignore-scripts is enabled.


module.exports = function () {
	// figure out package of binary
	var version = package.version.replace(/^(\d+\.\d+\.\d+).*$/, '$1'); // turn '1.2.3-alpha' into '1.2.3'
	var subPackageName = '@elm_dev_binaries/' + process.platform + '_' + process.arch;

	verifyPlatform(version, subPackageName);

	var fileName = process.platform === 'win32' ? 'elm-dev.exe' : 'elm-dev';

	try {
		var subBinaryPath = require.resolve(subPackageName + '/' + fileName);
	}
	catch (error) {
		if (error && error.code === 'MODULE_NOT_FOUND') {
			exitFailure(version, missingSubPackageHelp(subPackageName));
		}
		else {
			exitFailure(version, 'I had trouble requiring the binary package for your platform (' + subPackageName + '):\n\n' + error);
		}
	}

	// Yarn 2 and later ("Berry") always invokes `node` (regardless of configuration)
	// so we cannot do any optimizations there
	var isYarnBerry = /\byarn\/(?!1\.)/.test(process.env.npm_config_user_agent || "");

	// as mentioned in bin/elm we cannot do any optimizations on Windows
	if (process.platform === 'win32' || isYarnBerry) {
		return subBinaryPath;
	}

	// figure out where to put the binary
	var binaryPath = path.resolve(__dirname, package.bin["elm-dev"]);
	var tmpPath = binaryPath + '.tmp';

	// optimize by replacing the JS bin/elm with the native binary directly
	try {
		// atomically replace the file with a hard link to the binary
		fs.linkSync(subBinaryPath, tmpPath);
		fs.renameSync(tmpPath, binaryPath);

		// Also place the proxy binary next to elm-dev so the runtime can find it
		var proxyFileName = process.platform === 'win32' ? 'elm-dev-proxy.exe' : 'elm-dev-proxy';
		var subProxyPath = path.join(path.dirname(subBinaryPath), proxyFileName);
		var proxyDest = path.resolve(__dirname, 'bin', proxyFileName);
		try {
			if (fs.existsSync(subProxyPath)) {
				var tmpProxy = proxyDest + '.tmp';
				// remove any existing to avoid EXDEV surprises
				try { fs.unlinkSync(tmpProxy); } catch (_) { }
				try { fs.unlinkSync(proxyDest); } catch (_) { }
				fs.linkSync(subProxyPath, tmpProxy);
				fs.renameSync(tmpProxy, proxyDest);
				try { fs.chmodSync(proxyDest, 0o755); } catch (_) { }
			}
		} catch (e) {
			// Non-fatal; fallback paths will still work
		}
	}
	catch (error) {
		exitFailure(version, 'I had some trouble writing file to disk. It is saying:\n\n' + error);
	}

	return binaryPath;
}

// Expose helper to locate platform package path. Downstream scripts may use this to locate
// additional artifacts (like elm-dev-proxy) placed in the same subpackage folder.
module.exports.subPackageFolder = function () {
	var version = package.version.replace(/^(\d+\.\d+\.\d+).*$/, '$1');
	var subPackageName = '@elm_dev_binaries/' + process.platform + '_' + process.arch;
	verifyPlatform(version, subPackageName);
	var resolved = require.resolve(subPackageName + '/' + (process.platform === 'win32' ? 'elm-dev.exe' : 'elm-dev'));
	return path.dirname(resolved);
}



// VERIFY PLATFORM


function verifyPlatform(version, subPackageName) {
	if (subPackageName in package.optionalDependencies) return;

	var situation = process.platform + '_' + process.arch;
	console.error(
		'-- ERROR -----------------------------------------------------------------------\n\n'
		+ 'I am detecting that your computer (' + situation + ') may not be compatible with any\n'
		+ 'of the official pre-built binaries.\n\n'
		+ 'I recommend against using the npm installer for your situation. Check out the\n'
		+ 'alternative installers at https://github.com/mdgriffith/elm-dev/releases/tag/' + version + '\n'
		+ 'to see if there is something that will work better for you.\n\n'
		+ 'From there I recommend asking for guidance on Slack or Discourse to find someone\n'
		+ 'who can help with your specific situation.\n\n'
		+ '--------------------------------------------------------------------------------\n'
	);
	process.exit(1);
}



// EXIT FAILURE


function exitFailure(version, message) {
	console.error(
		'-- ERROR -----------------------------------------------------------------------\n\n'
		+ message
		+ '\n\nNOTE: You can avoid npm entirely by downloading directly from:\n'
		+ 'https://github.com/mdgriffith/elm-dev/releases/tag/' + version + '\n'
		+ 'All this package does is distribute a file from there.\n\n'
		+ '--------------------------------------------------------------------------------\n'
	);
	process.exit(1);
}



// MISSING SUB PACKAGE HELP


function missingSubPackageHelp(subPackageName) {
	return (
		'I tried to get `elm-dev` from ' + subPackageName + ', but something went wrong.\n'
		+ 'This can happen if you use the "--omit=optional" or "--no-optional" npm flag, or\n'
		+ 'if your "node_modules" folder was copied over from a different computer (or VM).\n'
	);
}
