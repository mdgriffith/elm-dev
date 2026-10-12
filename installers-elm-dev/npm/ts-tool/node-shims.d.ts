declare module 'node:child_process' {
    import { ChildProcess } from 'child_process';
    export function spawn(command: string, args?: readonly string[], options?: any): ChildProcess;
}

declare module 'node:fs' {
    export function existsSync(path: string): boolean;
}

declare module 'node:fs/promises' {
    export function readFile(path: string, encoding: 'utf8'): Promise<string>;
    export function readdir(path: string, options: { withFileTypes: true }): Promise<Array<{
        name: string;
        isDirectory(): boolean;
        isFile(): boolean;
    }>>;
}

declare module 'node:module' {
    export function createRequire(filename: string): { resolve(id: string): string };
}

declare module 'node:path' {
    import * as path from 'path';
    export default path;
}

// Minimal Node globals and optional deps used by vite.mjs
declare const process: any;
declare function fetch(input: any, init?: any): Promise<any>;

declare module 'ws' {
    const _default: any;
    export default _default;
}
