export type ElmDevMode = "debug" | "dev" | "optimize" | "O2" | "O3";

export interface ElmDevPluginOptions {
    /** Defaults to dev for serve, optimize for build. Build rejects debug. */
    mode?: ElmDevMode;
    /** Development only. Builds always invoke elm-dev directly. */
    useDevServer?: boolean;
    /** Development only. Enabling this during build is an error. */
    advancedDebugger?: boolean;
}

declare const elmDevPlugin: (options?: ElmDevPluginOptions) => any;
export default elmDevPlugin;
