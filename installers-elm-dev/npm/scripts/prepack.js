const fs = require('fs');
const path = require('path');

function copyFileSync(src, dest) {
    fs.mkdirSync(path.dirname(dest), { recursive: true });
    fs.copyFileSync(src, dest);
}

function copyDirSync(srcDir, destDir) {
    if (!fs.existsSync(srcDir)) return;
    for (const entry of fs.readdirSync(srcDir, { withFileTypes: true })) {
        const srcPath = path.join(srcDir, entry.name);
        const destPath = path.join(destDir, entry.name);
        if (entry.isDirectory()) {
            copyDirSync(srcPath, destPath);
        } else if (entry.isFile()) {
            copyFileSync(srcPath, destPath);
        }
    }
}

const repoRoot = path.join(__dirname, '..', '..', '..');
const src = path.join(repoRoot, 'apps', 'ts-tool');
const dest = path.join(__dirname, '..', 'ts-tool');

copyDirSync(src, dest);

console.log('Copied ts-tool into npm package');


