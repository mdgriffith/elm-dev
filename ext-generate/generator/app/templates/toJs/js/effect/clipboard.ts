export const connect = (app: any, data: any) => {
  app.ports?.clipboard?.subscribe?.((message: any) => {
    copy(message);
  });
};


export function copy(text: string) {
  const clipboard = navigator.clipboard;
  if (!clipboard) {
    return;
  }
  clipboard.writeText(text);
}
