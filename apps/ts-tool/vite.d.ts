export type ElmDevMode = "debug" | "dev" | "optimize" | "O2" | "O3";

export interface ElmDevPluginOptions {
    mode?: ElmDevMode;
    useDevServer?: boolean;
    advancedDebugger?: boolean;
}

declare const elmDevPlugin: (options?: ElmDevPluginOptions) => any;
export default elmDevPlugin;
