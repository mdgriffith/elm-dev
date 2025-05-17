/// <reference types="vite/client" />

// This imports all effect modules in the effect directory
// and calls connect on them.
export function connect(app: any, data: any) {
  const modules = import.meta.glob("./effect/*.ts", { eager: true });
  for (const module of Object.values(modules)) {
    if ("connect" in module && module.connect && typeof module.connect === 'function') {
      module.connect(app, data);
    } else {
      console.log(`${module} does not have a connect function`);
    }
  }
}
