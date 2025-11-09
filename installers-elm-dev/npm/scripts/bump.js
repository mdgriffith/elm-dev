#!/usr/bin/env node
const fs = require('fs');
const path = require('path');

function fail(msg) {
    console.error(msg);
    process.exit(1);
}

const newVersion = process.argv[2];
if (!newVersion) {
    fail('Usage: node scripts/bump.js <new-version>');
}

const npmRoot = path.join(__dirname, '..');
const repoRoot = path.join(__dirname, '..', '..', '..');
const packagesDir = path.join(npmRoot, 'packages');
const rootPkgJsonPath = path.join(npmRoot, 'package.json');
const mainDevPath = path.join(repoRoot, 'ext-dev', 'MainDev.hs');

function readJson(filePath) {
    try {
        const raw = fs.readFileSync(filePath, 'utf8');
        return JSON.parse(raw);
    } catch (err) {
        fail(`Failed to read or parse JSON at ${filePath}: ${err.message}`);
    }
}

function writeJson(filePath, json) {
    try {
        const content = JSON.stringify(json, null, 2) + '\n';
        fs.writeFileSync(filePath, content, 'utf8');
    } catch (err) {
        fail(`Failed to write JSON at ${filePath}: ${err.message}`);
    }
}

function bumpRootPackageJson() {
    const pkg = readJson(rootPkgJsonPath);
    const before = pkg.version;
    pkg.version = newVersion;

    if (pkg.dependencies) {
        for (const dep of Object.keys(pkg.dependencies)) {
            pkg.dependencies[dep] = newVersion;
        }
    }
    if (pkg.optionalDependencies) {
        for (const dep of Object.keys(pkg.optionalDependencies)) {
            pkg.optionalDependencies[dep] = newVersion;
        }
    }
    if (pkg.devDependencies) {
        for (const dep of Object.keys(pkg.devDependencies)) {
            pkg.devDependencies[dep] = newVersion;
        }
    }

    writeJson(rootPkgJsonPath, pkg);
    console.log(`Updated ${rootPkgJsonPath}: ${before} -> ${newVersion}`);
}

function bumpPackages() {
    if (!fs.existsSync(packagesDir)) return;
    const entries = fs.readdirSync(packagesDir, { withFileTypes: true });
    for (const entry of entries) {
        if (!entry.isDirectory()) continue;
        const pkgJsonPath = path.join(packagesDir, entry.name, 'package.json');
        if (!fs.existsSync(pkgJsonPath)) continue;
        const pkg = readJson(pkgJsonPath);
        const before = pkg.version;
        pkg.version = newVersion;
        writeJson(pkgJsonPath, pkg);
        console.log(`Updated ${pkgJsonPath}: ${before} -> ${newVersion}`);
    }
}

function bumpMainDevVersion() {
    if (!fs.existsSync(mainDevPath)) {
        console.warn(`Skipping MainDev version update; file not found: ${mainDevPath}`);
        return;
    }
    const src = fs.readFileSync(mainDevPath, 'utf8');
    // Replace: version = "x.y.z" keeping spacing intact
    const replaced = src.replace(
        /(version\s*=\s*")([^"]*)(")/,
        (_, pre, _old, post) => `${pre}${newVersion}${post}`
    );
    if (replaced === src) {
        console.warn('Warning: No version assignment found in MainDev.hs to update.');
    } else {
        fs.writeFileSync(mainDevPath, replaced, 'utf8');
        console.log(`Updated ${mainDevPath} version string -> ${newVersion}`);
    }
}

function main() {
    bumpRootPackageJson();
    bumpPackages();
    bumpMainDevVersion();
    console.log('Version bump complete.');
}

main();


