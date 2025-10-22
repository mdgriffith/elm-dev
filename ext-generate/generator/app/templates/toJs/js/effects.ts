/// <reference types="vite/client" />

type EffectModule = { connect: (app: any, data: any) => void };

function isEffectModule(value: unknown): value is EffectModule {
  return (
    typeof value === 'object' &&
    value !== null &&
    'connect' in value &&
    typeof (value as any).connect === 'function'
  );
}

// This imports all effect modules in the effect directory
// and calls connect on them.
export function connect(app: any, data: any) {
  const modules = import.meta.glob("./effect/*.ts", { eager: true });
  for (const [path, mod] of Object.entries(modules)) {
    if (isEffectModule(mod)) {
      mod.connect(app, data);
    } else {
      console.warn(`${path} does not export a 'connect' function`);
    }
  }
}
