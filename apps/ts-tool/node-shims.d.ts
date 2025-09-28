declare module 'node:child_process' {
    import { ChildProcess } from 'child_process';
    export function spawn(command: string, args?: readonly string[], options?: any): ChildProcess;
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


