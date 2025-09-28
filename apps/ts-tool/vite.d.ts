export interface ElmDevPluginOptions {
    debug?: boolean;
    optimize?: boolean;
    useDevServer?: boolean;
}

declare const elmDevPlugin: (options?: ElmDevPluginOptions) => any;
export default elmDevPlugin;


