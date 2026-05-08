/// <reference types="vite/client" />

// This imports all effect modules in the effect directory
// and calls connect on them.
export function connect(app: any, data: any) {
  const modules = import.meta.glob("./effect/*.ts", { eager: true });
  for (const module of Object.values(modules)) {
    if (isEffectModule(module)) {
      module.connect(app, data);
    } else {
      console.log(`${module} does not have a connect function`);
    }
  }
}

function isEffectModule(module: unknown): module is { connect: (app: any, data: any) => void } {
  return typeof module === "object"
    && module !== null
    && "connect" in module
    && typeof module.connect === "function";
}
